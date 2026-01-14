module Bytes = BytesLabels

module Parse = struct
  open Angstrom

  type t =
    [ `Unescaped
    | `Escaped
    | `UTF8 of char list
    | `UTF16 of int * [ `S | `U | `C of char list ]
    | `Error of string
    | `Done
    ]

  let is_invalid c = c <= '\031'

  let unescaped buf = function
    | '"' -> `Terminate
    | '\\' -> `Escaped
    | c ->
      if is_invalid c
      then `Error (Printf.sprintf "unexpected character '%c'" c)
      else (
        Buffer.add_char buf c;
        `Unescaped)
  ;;

  let escaped buf = function
    | '\x22' ->
      Buffer.add_char buf '\x22';
      `Unescaped
    | '\x5c' ->
      Buffer.add_char buf '\x5c';
      `Unescaped
    | '\x2f' ->
      Buffer.add_char buf '\x2f';
      `Unescaped
    | '\x62' ->
      Buffer.add_char buf '\x08';
      `Unescaped
    | '\x66' ->
      Buffer.add_char buf '\x0c';
      `Unescaped
    | '\x6e' ->
      Buffer.add_char buf '\x0a';
      `Unescaped
    | '\x72' ->
      Buffer.add_char buf '\x0d';
      `Unescaped
    | '\x74' ->
      Buffer.add_char buf '\x09';
      `Unescaped
    | '\x75' -> `UTF8 []
    | _ -> `Error "invalid escape sequence"
  ;;

  let hex c =
    match c with
    | '0' .. '9' -> Char.code c - 0x30 (* '0' *)
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _ -> 255
  ;;

  let utf_8 buf d = function
    | [ c; b; a ] ->
      let a = hex a
      and b = hex b
      and c = hex c
      and d = hex d in
      if a lor b lor c lor d = 255
      then `Error "invalid hex escape"
      else (
        let cp = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if cp >= 0xd800 && cp <= 0xdbff
        then `UTF16 (cp, `S)
        else (
          if cp lsr 7 = 0
          then Buffer.add_char buf (Char.unsafe_chr cp)
          else if cp lsr 11 = 0
          then (
            Buffer.add_char
              buf
              (Char.unsafe_chr (0b11000000 lor ((cp lsr 6) land 0b00011111)));
            Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (cp land 0b00111111))))
          else if cp lsr 16 = 0
          then (
            Buffer.add_char
              buf
              (Char.unsafe_chr (0b11100000 lor ((cp lsr 12) land 0b00001111)));
            Buffer.add_char
              buf
              (Char.unsafe_chr (0b10000000 lor ((cp lsr 6) land 0b00111111)));
            Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (cp land 0b00111111))))
          else assert false;
          `Unescaped))
    | cs -> `UTF8 (d :: cs)
  ;;

  let utf_16 buf d x s =
    match s, d with
    | `S, '\\' -> `UTF16 (x, `U)
    | `U, 'u' -> `UTF16 (x, `C [])
    | `C [ c; b; a ], _ ->
      let a = hex a
      and b = hex b
      and c = hex c
      and d = hex d in
      if a lor b lor c lor d = 255
      then `Error "invalid hex escape"
      else (
        let y = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if y >= 0xdc00 && y <= 0xdfff
        then (
          let hi = x - 0xd800 in
          let lo = y - 0xdc00 in
          let cp = 0x10000 + ((hi lsl 10) lor lo) in
          Buffer.add_char
            buf
            (Char.unsafe_chr (0b11110000 lor ((cp lsr 18) land 0b00000111)));
          Buffer.add_char
            buf
            (Char.unsafe_chr (0b10000000 lor ((cp lsr 12) land 0b00111111)));
          Buffer.add_char
            buf
            (Char.unsafe_chr (0b10000000 lor ((cp lsr 6) land 0b00111111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (cp land 0b00111111)));
          `Unescaped)
        else `Error "invalid escape sequence for utf-16 low surrogate")
    | `C cs, _ -> `UTF16 (x, `C (d :: cs))
    | _, _ -> `Error "invalid escape sequence for utf-16 low surrogate"
  ;;

  module Buffer_pool = struct
    (* A global pool of buffers shared across all string parsers, of which multiple may be
       running in parallel. Buffers automatically grow as needed but shrink back to the
       initial size when reset. *)
    let initial_size = 4096
    let max_entries = 256
    let pool = Stack.create ()

    let allocate () =
      if Stack.is_empty pool then Buffer.create initial_size else Stack.pop pool
    ;;

    let free buf =
      if Stack.length pool < max_entries
      then (
        Buffer.reset buf;
        Stack.push buf pool)
    ;;
  end

  (* Slow path in the presence of an escape: we need to transform the string with an
     intermediate buffer *)
  let str_slow ~prefix =
    let buf = Buffer_pool.allocate () in
    let state : t ref = ref `Unescaped in
    skip_while (fun c ->
      match
        match !state with
        | `Unescaped -> unescaped buf c
        | `Escaped -> escaped buf c
        | `UTF8 cs -> utf_8 buf c cs
        | `UTF16 (x, cs) -> utf_16 buf c x cs
        | (`Error _ | `Done) as state -> state
      with
      | `Error _ | `Done -> false
      | `Terminate ->
        state := `Done;
        true
      | #t as state' ->
        state := state';
        true)
    >>= fun () ->
    match !state with
    | `Done ->
      let prefix_len = String.length prefix in
      let buf_len = Buffer.length buf in
      let result = Bytes.create (prefix_len + buf_len) in
      (* blit: src src_pos dst dst_pos len *)
      String.blit prefix 0 result 0 prefix_len;
      Buffer.blit buf 0 result prefix_len buf_len;
      Buffer_pool.free buf;
      return (Bytes.unsafe_to_string result)
    | `Error msg ->
      Buffer_pool.free buf;
      fail msg
    | `Unescaped | `Escaped | `UTF8 _ | `UTF16 _ ->
      Buffer_pool.free buf;
      fail "unterminated string"
  ;;

  let str =
    let%bind.Angstrom prefix =
      (* This is only unsafe if we modify the buffer or return a reference to the buffer *)
      Unsafe.take_till
        (function
          | '"' | '\\' -> true
          | c when is_invalid c -> true
          | _ -> false)
        (fun bstr ~off ~len -> Bigstringaf.substring bstr ~off ~len)
    and next = peek_char in
    match next with
    | None | Some '\n' -> fail "unterminated string"
    | Some '"' -> advance 1 *> return prefix
    | Some c when is_invalid c -> fail (Printf.sprintf "unexpected character '%c'" c)
    | Some _ -> str_slow ~prefix
  ;;
end

let parse = Parse.str
let to_hex_digit i = if i < 10 then i + 48 else i + 87

module Int64 = struct
  include Int64

  module O = struct
    let ( land ) = Int64.logand
    let ( lxor ) = Int64.logxor
    let ( - ) = Int64.sub
    let lognot = Int64.lognot
  end
end

(* We apply an optimization here to determine if any batch of 8 characters needs to be
   escaped; if not we can write the 8 characters directly.

   [can_skip_escape_check_for_8_bytes s] returns false if any of the following holds:
   a. One of the 8 bytes of [s] starting at [off] is <32
   b. One of the 8 bytes of [s] starting at [off] is '"', 0x22
   c. One of the 8 bytes of [s] starting at [off] is '\\', 0x5c

   Each of the checks is done by first constructing some int64 which has 0 in any byte in
   which the condition holds, and second doing a bitwise trick with [has_zero_byte] below.
*)
let[@zero_alloc] [@inline always] has_zero_byte bytes =
  let open Int64.O in
  (* Why does this function work?

     Consider where [(v - x01s) & x80s] has bits set. If a byte of [v] is 0 then in
     [v - x01s] it will be 0xff or 0xfe depending on borrow. Both those cases will have
     the 0x80 bit set. Otherwise, that bit may only be set if it was already set in [v].
     So by anding with [~v], we remove the cases where the byte was not equal to 0. *)
  (bytes - 0x0101010101010101L) land lognot bytes land 0x8080808080808080L <> 0L
;;

let[@zero_alloc] [@inline always] any_next_8_bytes_need_escaping s ~off =
  let open Int64.O in
  let bytes = String.get_int64_ne s off in
  let anything_lt_32 = has_zero_byte (bytes land 0xe0e0e0e0e0e0e0e0L) in
  let any_double_quote = has_zero_byte (bytes lxor 0x2222222222222222L) in
  let any_backslash = has_zero_byte (bytes lxor 0x5c5c5c5c5c5c5c5cL) in
  anything_lt_32 || any_double_quote || any_backslash
;;

let[@inline always] can_skip_8 s ~off ~strlen =
  if off + 8 > strlen then false else not (any_next_8_bytes_need_escaping s ~off)
;;

let[@inline always] can_skip_8 s ~off ~strlen =
  match Sys.backend_type, Sys.word_size with
  | Native, 64 -> can_skip_8 s ~off ~strlen
  | _ ->
    (* [can_skip_8] will be slow/allocation heavy on platforms other than 64-bit native
       (e.g. js_of_ocaml) so we skip the optimisation on them. *)
    false
;;

let escape_byte_table =
  (* The [i]th byte says what to do when want to represent [Char.chr i] in a json string:
     0 if we don’t escape [Char.chr i] 128 if we \u... escape it some other char if we use
     that for the escape
  *)
  String.init 256 (fun i ->
    match Char.chr i with
    | '\n' -> 'n'
    | '\r' -> 'r'
    | '\t' -> 't'
    | '\012' -> 'f'
    | '\b' -> 'b'
    | '"' -> '"'
    | '\\' -> '\\'
    | '\000' .. '\031' -> '\128'
    | '\032' .. '\255' -> '\000')
;;

let serialize t s =
  let open Faraday in
  let flush t s ~off ~len = if len <> 0 then write_string t ~off ~len s in
  let rec go8 t s ~off ~len ~strlen =
    if strlen = off + len
    then flush t s ~off ~len
    else if (can_skip_8 [@inlined]) s ~off:(off + len) ~strlen
    then go8 t s ~off ~len:(len + 8) ~strlen
    else go1 t s ~off ~len ~strlen ~n:8
  and go1 t s ~off ~len ~strlen ~n =
    if strlen = off + len
    then flush t s ~off ~len
    else if n = 0
    then go8 t s ~off ~len ~strlen
    else (
      let i = off + len in
      let c = String.unsafe_get s i in
      let char_code = Char.code c in
      let rule = String.get escape_byte_table char_code in
      if rule = '\000'
      then go1 t s ~off ~len:(len + 1) ~strlen ~n:(n - 1)
      else (
        flush t s ~off ~len;
        if rule = '\128'
        then (
          write_string t "\\u00";
          write_uint8 t (to_hex_digit (char_code lsr 4));
          write_uint8 t (to_hex_digit (char_code land 0xf)))
        else (
          write_char t '\\';
          write_char t rule);
        go1 t s ~off:(i + 1) ~len:0 ~strlen ~n:(n - 1)))
  in
  write_char t '"';
  go8 t s ~off:0 ~len:0 ~strlen:(String.length s);
  write_char t '"'
;;
