type 'number t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of 'number
  | `Object of (string * 'number t) list
  | `Array of 'number t list
  ]

module Or_raw = struct
  type ('number, 't) t =
    [< `Raw_json_string of string
    | `Null
    | `False
    | `True
    | `String of string
    | `Number of 'number
    | `Object of (string * 't) list
    | `Array of 't list
    ]
    as
    't
end

module Parser = struct
  type 'a json = 'a t

  open Angstrom

  let string_ignored s = string s *> return ()

  let ws =
    skip_while (function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false)
  ;;

  let fail_word =
    (* Used for bare words like true, false, and null. Ideally we would parse as much of
       the word as we could, then fail on the next character, but Angstrom will always
       backtrack on failures. *)
    take_while1 (function
      | 'a' .. 'z' -> true
      | _ -> false)
    >>= fun s -> fail (Printf.sprintf "unexpected string: '%s'" s)
  ;;

  let lchar c = ws *> char c
  let rsb = lchar ']'
  let rcb = lchar '}'
  let ns, vs = lchar ':', lchar ','
  let quo = lchar '"'

  let word s (res : [< `True | `False | `Null ]) =
    (string_ignored s >>| (fun () -> res) <|> fail_word
      : [< `True | `False | `Null ] Angstrom.t
      :> _ json Angstrom.t)
  ;;

  let _false = word "false" `False
  let _true = word "true" `True
  let _null = word "null" `Null

  let num number =
    let%bind.Angstrom s =
      take_while1 (function
        | '-' | '+' | '0' .. '9' | '.' | 'e' | 'E' -> true
        | _ -> false)
    in
    match number s with
    | Ok x -> return (`Number x)
    | Error msg -> fail msg
  ;;

  let create_without_trailing_whitespace (type number) number =
    let advance1 = advance 1 in
    let pair (x : string) (y : number json) = x, y in
    let str = Json_string.parse in
    fix (fun (json : number json Angstrom.t) ->
      let mem = lift2 pair (quo *> str <* ns) json in
      let obj : number json t =
        advance1 *> sep_by vs mem <* rcb >>| fun ms -> `Object ms
      in
      let obj = obj <?> "object" in
      let arr : number json t =
        advance1 *> sep_by vs json <* rsb >>| fun vs -> `Array vs
      in
      let arr = arr <?> "array" in
      let str : number json t = advance1 *> str >>| fun s -> `String s in
      let str = str <?> "string" in
      let num = num number <?> "number" in
      let fail_char ?hint char =
        let message = Printf.sprintf "unexpected character: '%c'" char in
        match hint with
        | Some hint -> fail (Printf.sprintf "%s (%s)" message hint)
        | None -> fail message
      in
      commit *> ws *> peek_char_fail
      >>= function
      | 'f' -> _false
      | 'n' -> _null
      | 't' -> _true
      | '{' -> obj
      | '[' -> arr
      | '"' -> str
      | '-' | '+' | '0' .. '9' | '.' ->
        (* strictly speaking, we should only allow '-' or '0' .. '9' to start numbers. *)
        num
      | '<' -> fail_char '<' ~hint:"does your string contain HTML instead of JSON?"
      | c -> fail_char c)
    <?> "json"
  ;;

  let create number = create_without_trailing_whitespace number <* ws
end

module Serializer = struct
  let spaces = String.init 100 (fun _ -> ' ')

  let rec write_spaces faraday num =
    if num > String.length spaces
    then (
      Faraday.write_string faraday spaces;
      write_spaces faraday (num - String.length spaces))
    else Faraday.write_string faraday spaces ~len:num
  ;;

  let maybe_newline_and_indent ~spaces faraday indent =
    if spaces > 0
    then (
      Faraday.write_char faraday '\n';
      write_spaces faraday indent)
  ;;

  let rec list_iter_local xs ~f =
    match xs with
    | [] -> ()
    | x :: xs ->
      f x;
      list_iter_local xs ~f
  ;;

  let rec serialize_hum' t ~indent ~spaces serialize_number faraday : unit =
    match t with
    | `Null -> Faraday.write_string faraday "null"
    | `False -> Faraday.write_string faraday "false"
    | `True -> Faraday.write_string faraday "true"
    | `String string -> Json_string.serialize faraday string
    | `Number number -> serialize_number faraday number
    | `Object items ->
      serialize_list items ~indent ~spaces serialize_number faraday "{}" serialize_kv
    | `Array items ->
      serialize_list items ~indent ~spaces serialize_number faraday "[]" serialize_hum'
    | `Raw_json_string string -> Faraday.write_string faraday string

  and serialize_list
    : 'a 'b.
    'b list
    -> indent:int
    -> spaces:int
    -> 'a
    -> Faraday.t
    -> string
    -> ('b -> indent:int -> spaces:int -> 'a -> Faraday.t -> unit)
    -> unit
    =
    fun items ~indent ~spaces serialize_number faraday brackets serialize_item ->
    match items with
    | [] -> Faraday.write_string faraday brackets
    | item :: items ->
      Faraday.write_char faraday brackets.[0];
      let indent = indent + spaces in
      maybe_newline_and_indent ~spaces faraday indent;
      serialize_item item ~indent ~spaces serialize_number faraday;
      list_iter_local items ~f:(fun item ->
        Faraday.write_char faraday ',';
        maybe_newline_and_indent ~spaces faraday indent;
        serialize_item item ~indent ~spaces serialize_number faraday);
      let indent = indent - spaces in
      maybe_newline_and_indent ~spaces faraday indent;
      Faraday.write_char faraday brackets.[1]

  and serialize_kv (k, v) ~indent ~spaces serialize_number faraday =
    Json_string.serialize faraday k;
    Faraday.write_char faraday ':';
    if spaces > 0 then Faraday.write_char faraday ' ';
    serialize_hum' v ~indent ~spaces serialize_number faraday
  ;;

  (* need to eta-expand to avoid the value restriction *)
  let create s t faraday = serialize_hum' t ~indent:0 ~spaces:0 s faraday
  let create_hum ~spaces s t faraday = serialize_hum' t ~indent:0 ~spaces s faraday
end
