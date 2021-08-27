module List = ListLabels

type 'number t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of 'number
  | `Object of (string * 'number t) list
  | `Array of 'number t list
  ]

module Parser = struct
  open Angstrom

  let ws =
    skip_while (function
      | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
      | _ -> false)
  ;;

  let lchar c = ws *> char c
  let rsb = lchar ']'
  let rcb = lchar '}'
  let ns, vs = lchar ':', lchar ','
  let quo = lchar '"'
  let _false = string "false" *> return `False
  let _true = string "true" *> return `True
  let _null = string "null" *> return `Null

  let num number =
    take_while1 (function
      | '-' | '+' | '0' .. '9' | '.' | 'e' | 'E' -> true
      | _ -> false)
    >>= fun s ->
    match number s with
    | Ok x -> return (`Number x)
    | Error msg -> fail msg
  ;;

  let t number =
    let open Angstrom in
    let advance1 = advance 1 in
    let pair x y = x, y in
    let buf = Buffer.create 0x1000 in
    let str = Json_string.parse buf in
    fix (fun json ->
      let mem = lift2 pair (quo *> str <* ns) json in
      let obj = advance1 *> sep_by vs mem <* rcb >>| fun ms -> `Object ms in
      let arr = advance1 *> sep_by vs json <* rsb >>| fun vs -> `Array vs in
      let str = advance1 *> str >>| fun s -> `String s in
      let fail_word =
        (* Used for bare words like true, false, and null. Ideally we would parse as
           much of the word as we could, then fail on the next character, but Angstrom
           will always backtrack on failures. *)
        take_while1 (function
          | 'a' .. 'z' -> true
          | _ -> false)
        >>= fun s -> fail (Printf.sprintf "unexpected string: '%s'" s)
      in
      let fail_char ?hint char =
        let message = Printf.sprintf "unexpected character: '%c'" char in
        match hint with
        | Some hint -> fail (Printf.sprintf "%s (%s)" message hint)
        | None -> fail message
      in
      commit *> ws *> peek_char_fail
      >>= function
      | 'f' -> _false <|> fail_word
      | 'n' -> _null <|> fail_word
      | 't' -> _true <|> fail_word
      | '{' -> obj <?> "object"
      | '[' -> arr <?> "array"
      | '"' -> str <?> "string"
      | '-' | '+' | '0' .. '9' | '.' ->
        (* strictly speaking, we should only allow '-' or '0' .. '9' to start numbers. *)
        num number <?> "number"
      | '<' -> fail_char '<' ~hint:"does your string contain HTML instead of JSON?"
      | c -> fail_char c)
    <?> "json"
    <* ws
  ;;

  let run number_of_string string =
    Angstrom.(parse_string ~consume:All (t number_of_string)) string
  ;;

  let run_many number_of_string string =
    Angstrom.(parse_string ~consume:All (many (t number_of_string))) string
  ;;
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

  let rec serialize_hum' ~indent ~spaces serialize_number t faraday =
    match t with
    | `Null -> Faraday.write_string faraday "null"
    | `False -> Faraday.write_string faraday "false"
    | `True -> Faraday.write_string faraday "true"
    | `String string -> Json_string.serialize faraday string
    | `Number number -> serialize_number faraday number
    | `Object items ->
      serialize_list ~indent ~spaces serialize_number faraday "{}" serialize_kv items
    | `Array items ->
      serialize_list ~indent ~spaces serialize_number faraday "[]" serialize_hum' items

  and serialize_list :
    'a 'b.
      indent:int
    -> spaces:int
    -> 'a
    -> Faraday.t
    -> string
    -> (indent:int -> spaces:int -> 'a -> 'b -> Faraday.t -> unit)
    -> 'b list
    -> unit
    =
    fun ~indent ~spaces serialize_number faraday brackets serialize_item items ->
      match items with
      | [] -> Faraday.write_string faraday brackets
      | item :: items ->
        Faraday.write_char faraday brackets.[0];
        let indent = indent + spaces in
        maybe_newline_and_indent ~spaces faraday indent;
        serialize_item ~indent ~spaces serialize_number item faraday;
        List.iter items ~f:(fun item ->
          Faraday.write_char faraday ',';
          maybe_newline_and_indent ~spaces faraday indent;
          serialize_item ~indent ~spaces serialize_number item faraday);
        let indent = indent - spaces in
        maybe_newline_and_indent ~spaces faraday indent;
        Faraday.write_char faraday brackets.[1]

  and serialize_kv ~indent ~spaces serialize_number (k, v) faraday =
    Json_string.serialize faraday k;
    Faraday.write_char faraday ':';
    if spaces > 0 then Faraday.write_char faraday ' ';
    serialize_hum' ~indent ~spaces serialize_number v faraday
  ;;

  (* need to eta-expand to avoid the value restriction *)
  let serialize s t = serialize_hum' ~indent:0 ~spaces:0 s t
  let serialize_hum ~spaces s t = serialize_hum' ~indent:0 ~spaces s t

  let run serialize_number t =
    let faraday = Faraday.create 0x1000 in
    serialize serialize_number t faraday;
    Faraday.serialize_to_string faraday
  ;;

  let run_hum ~spaces serialize_number t =
    let faraday = Faraday.create 0x1000 in
    serialize_hum ~spaces serialize_number t faraday;
    Faraday.serialize_to_string faraday
  ;;
end
