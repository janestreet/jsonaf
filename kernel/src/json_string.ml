module Bytes = BytesLabels

module Parse = struct
  open Angstrom

  (* To parse a string with escaping we do two passes:
     1. count how many output bytes we will need
     2. allocate the output buffer and unescape into it

     For the latter part, the code ends up being straightforward as angstrom gives us the
     whole buffer. For the former part, we need to write a char-at-a-time state machine.
     The counting is also quite complicated: not only do we need to recognise escape
     sequences, but we are outputting UTF-8 so some codepoints may be wider than others,
     and unicode escapes are in UTF-16 so we need to recognise surrogate pairs as these
     end up combining and mapping to 4 UTF-8 output bytes. We solve this with a state
     lookup table. Each state gives us (i) the number of output bytes to add to our
     counter and (ii) a table to get the next state for a given char.
  *)

  module Output_byte_counting = struct
    (* Most states here corresponds to the prefix (base = no prefix, escape = '\\' prefix,
       U... = '\\u...' prefix). The _plusN states correspond to how they modify our
       running total of output bytes. We explain the state machine in a little more detail
       where we define the transition matrix below. *)

    type t =
      | Base
      | Escape
      | U
      | U0
      | U00
      | UXXX_plus0
      | UXXX_plus1
      | UXX_plus1
      | UXX_plus2
      | UX
      | UD
      | Terminate
      | Fail

    let initial_state = Base

    let is_terminal = function
      | Terminate | Fail -> true
      | Base
      | Escape
      | U
      | U0
      | UXX_plus1
      | UXXX_plus0
      | UXX_plus2
      | U00
      | UXXX_plus1
      | UX
      | UD -> false
    ;;

    let is_fail = function
      | Fail -> true
      | _ -> false
    ;;

    let error_message_if_transition_to_fail = function
      | Fail | Terminate -> "Unknown invalid string"
      | Base -> "Invalid string char"
      | Escape -> "Invalid char after '\\'"
      | U -> "Invalid char after '\\u'"
      | U0 -> "Invalid char after '\\u0'"
      | UD -> "Invalid char after '\\uD'"
      | UX -> "Invalid char after '\\uX'"
      | U00 -> "Invalid char after '\\u00'"
      | UXX_plus2 -> "Invalid char after '\\uXX'"
      | UXX_plus1 -> "Invalid char after '\\uXX'"
      | UXXX_plus0 -> "Invalid char after '\\uXXX'"
      | UXXX_plus1 -> "Invalid char after '\\uXXX'"
    ;;

    let num_extra_output_bytes_on_entry = function
      | Base | UXX_plus1 | UXXX_plus1 -> 1
      | UXX_plus2 -> 2
      | Escape | U | U0 | U00 | UXXX_plus0 | UX | UD | Terminate | Fail -> 0
    ;;

    (* So here are the possible escape sequences in a json string and the number of output
       bytes:
       - <normal char> -> 1 output byte (we don't validate that the string is valid UTF-8)
       - <named backslash escape> -> 1 UTF-8 byte
       - <\u0000 - \u007F> -> 1 UTF-8 byte
       - <\u0080 - \u07FF> -> 2 UTF-8 bytes
       - <\u0800 - \uD7FF> -> 3 UTF-8 bytes
       - <\uD800 - \uDBFF> <\uDC00 - \uDFFF> -> 4 UTF-8 bytes (surrogate pair)
       - <\uE000 - \uFFFF> -> 3 UTF-8 bytes

       Here are the states we go through for different valid encodings (starting and
       ending in the base state):
       {v
       -   Base                                  = +1 (ordinary)
       -   Base Escape                           = +1 (named escape)
       -   Base Escape U U0 U00       UXXX_plus0 = +1 (\u0000 - \u007F)
       -   Base Escape U U0 U00       UXXX_plus1 = +2 (\u0080 - \u00FF)
       -   Base Escape U U0 UXX_plus1 UXXX_plus0 = +2 (\u0100 - \u07FF)
       -   Base Escape U U0 UXX_plus2 UXXX_plus0 = +3 (\u0800 - \u0FFF)
       -   Base Escape U UX UXX_plus2 UXXX_plus0 = +3 (\u1000 - \uCFFF)
       -   Base Escape U UD UXX_plus2 UXXX_plus0 = +3 (\uD000 - \uD7FF)
       -   Base Escape U UD UXX_plus1 UXXX_plus0 ...
         + Base Escape U UD UXX_plus1 UXXX_plus0 = +4 (\uD800 - \uDFFF; \uD800 - \uDFFF)†
       -   Base Escape U UX UXX_plus2 UXXX_plus0 = +3 (\uE000 - \uFFFF)
       v}

       † Note that this includes invalid surrogate pairs – the first entry should be in
       \uD800 - \uDBFF and the second in \uDC00 - \uDFFF. Some JSON parsers will accept
       these mapping to 6 UTF-8 codepoints but we do not (because we undercount how many
       bytes we will need). *)
    let transition ~current_state ~char:c =
      let if_hex c x =
        match c with
        | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> x
        | _ -> Fail
      in
      match current_state with
      | Fail | Terminate -> Fail
      | Base ->
        (match c with
         | '\\' -> Escape
         | '"' -> Terminate
         | '\x00' .. '\x1f' -> Fail
         | _ -> Base)
      | Escape ->
        (match c with
         | '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' -> Base
         | 'u' -> U
         | _ -> Fail)
      | U ->
        (match c with
         | '0' -> U0
         | 'D' | 'd' -> UD
         | c -> if_hex c UX)
      | UX -> if_hex c UXX_plus2
      | UXX_plus1 | UXX_plus2 -> if_hex c UXXX_plus0
      | UXXX_plus0 | UXXX_plus1 -> if_hex c Base
      | U0 ->
        (match c with
         | '0' -> U00
         | '1' .. '7' -> UXX_plus1
         | c -> if_hex c UXX_plus2)
      | U00 ->
        (match c with
         | '0' .. '7' -> UXXX_plus0
         | c -> if_hex c UXXX_plus1)
      | UD ->
        (match c with
         | '0' .. '7' -> UXX_plus2
         | c -> if_hex c UXX_plus1)
    ;;
  end

  let is_invalid c = c <= '\031'

  let hex c =
    match c with
    | '0' .. '9' -> Char.code c - 0x30 (* '0' *)
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _ -> 255
  ;;

  (** All three of the following unicode helpers assume the input is in the inclusive
      range [0,0xFFFF] and will otherwise return garbage. *)

  let is_valid_high_surrogate i = i >= 0xD800 && i <= 0xDBFF
  let is_valid_low_surrogate i = i >= 0xDC00 && i <= 0xDFFF
  let is_valid_bmp_codepoint i = i < 0xD800 || i >= 0xE000

  let codepoint_of_surrogate_pair ~high ~low =
    0x10000 + (((high - 0xd800) lsl 10) lor (low - 0xdc00))
  ;;

  type result_without_argments =
    | Ok
    | Error

  let unescape_blit =
    let getch ~buf ~off = Bigstringaf.unsafe_get buf off in
    let read_4_hex ~buf ~off =
      let u1 = hex (getch ~buf ~off:(off + 0)) in
      let u2 = hex (getch ~buf ~off:(off + 1)) in
      let u3 = hex (getch ~buf ~off:(off + 2)) in
      let u4 = hex (getch ~buf ~off:(off + 3)) in
      (u1 lsl 12) lor (u2 lsl 8) lor (u3 lsl 4) lor u4
    in
    let rec base buf ~off ~len ~output ~dst_pos =
      if len = 0
      then if dst_pos = Bytes.length output then Ok else Error
      else (
        let len_to_blit =
          let rec loop ~buf ~off ~len ~acc =
            if acc = len
            then acc
            else (
              match getch ~buf ~off:(off + acc) with
              | '\\' -> acc
              | _ -> loop ~buf ~off ~len ~acc:(acc + 1))
          in
          loop ~buf ~off ~len ~acc:0
        in
        if len_to_blit > 0
        then
          Bigstringaf.blit_to_bytes
            buf
            ~src_off:off
            output
            ~dst_off:dst_pos
            ~len:len_to_blit;
        if len_to_blit = len
        then Ok
        else
          escape
            buf
            ~off:(off + len_to_blit + 1)
            ~len:(len - len_to_blit - 1)
            ~output
            ~dst_pos:(dst_pos + len_to_blit))
    and escape buf ~off ~len ~output ~dst_pos =
      if len = 0
      then Error
      else (
        let[@local] putch char =
          Bytes.set output dst_pos char;
          base buf ~off:(off + 1) ~len:(len - 1) ~output ~dst_pos:(dst_pos + 1)
        in
        match getch ~buf ~off with
        | 'n' -> putch '\n'
        | 'r' -> putch '\r'
        | 't' -> putch '\t'
        | '"' -> putch '"'
        | '\\' -> putch '\\'
        | '/' -> putch '/'
        | 'b' -> putch '\x08'
        | 'f' -> putch '\x0c'
        | 'u' -> u_escape buf ~off:(off + 1) ~len:(len - 1) ~output ~dst_pos
        | _ -> Error)
    and u_escape buf ~off ~len ~output ~dst_pos =
      if len < 4
      then Error
      else (
        let codepoint = read_4_hex ~buf ~off in
        if is_valid_bmp_codepoint codepoint
        then put_utf8 codepoint buf ~off:(off + 4) ~len:(len - 4) ~output ~dst_pos
        else
          utf_16_surrogate_pair
            codepoint
            buf
            ~off:(off + 4)
            ~len:(len - 4)
            ~output
            ~dst_pos)
    and utf_16_surrogate_pair leading_surrogate buf ~off ~len ~output ~dst_pos =
      if len < 6
      then Error
      else (
        match getch ~buf ~off, getch ~buf ~off:(off + 1) with
        | '\\', 'u' ->
          let off = off + 2 in
          let len = len - 2 in
          let trailing_surrogate = read_4_hex ~buf ~off in
          if not
               (is_valid_high_surrogate leading_surrogate
                && is_valid_low_surrogate trailing_surrogate)
          then Error
          else (
            let codepoint =
              codepoint_of_surrogate_pair ~high:leading_surrogate ~low:trailing_surrogate
            in
            put_utf8 codepoint buf ~off:(off + 4) ~len:(len - 4) ~output ~dst_pos)
        | _ -> Error)
    and put_utf8 codepoint buf ~off ~len ~output ~dst_pos =
      let wrote = Bytes.set_utf_8_uchar output dst_pos (Uchar.unsafe_of_int codepoint) in
      base buf ~off ~len ~output ~dst_pos:(dst_pos + wrote)
    in
    base
  ;;

  let str_slow ~prefix =
    let unescaped_bytes = ref 0 in
    let state : Output_byte_counting.t ref = ref Output_byte_counting.initial_state in
    (* two pass: first work out how many bytes we need for the buffer, then allocate and
       fill it. *)
    let%bind.Angstrom res =
      Unsafe.take_while
        (fun c ->
          let next_state =
            Output_byte_counting.transition ~current_state:!state ~char:c
          in
          unescaped_bytes
          := !unescaped_bytes
             + Output_byte_counting.num_extra_output_bytes_on_entry next_state;
          (* We don’t transition to a Fail state so that we can have a nice error message
             based on the state we start in. *)
          if not (Output_byte_counting.is_fail next_state) then state := next_state;
          not (Output_byte_counting.is_terminal next_state))
        (fun buf ~off ~len ->
          let unescaped_bytes = !unescaped_bytes in
          if not (Output_byte_counting.is_terminal !state)
          then
            ""
            (* This happens when counting fails for invalid string syntax. We generate an
               error message below *)
          else if unescaped_bytes = len
          then (
            let prefix_len = String.length prefix in
            if prefix_len = 0
            then Bigstringaf.substring buf ~off ~len
            else (
              let output = Bytes.create (prefix_len + unescaped_bytes) in
              Bytes.blit_string
                ~src:prefix
                ~src_pos:0
                ~len:prefix_len
                ~dst:output
                ~dst_pos:0;
              Bigstringaf.blit_to_bytes buf ~src_off:off output ~dst_off:prefix_len ~len;
              Bytes.unsafe_to_string output))
          else (
            let prefix_len = String.length prefix in
            let output = Bytes.create (prefix_len + unescaped_bytes) in
            Bytes.blit_string
              ~src:prefix
              ~src_pos:0
              ~len:prefix_len
              ~dst:output
              ~dst_pos:0;
            (match unescape_blit buf ~off ~len ~output ~dst_pos:prefix_len with
             | Ok -> ()
             | Error -> state := Fail);
            Bytes.unsafe_to_string output))
    and next = peek_char in
    match next with
    (* Special cases to give a nicer error message. *)
    | None | Some '\n' -> fail "unterminated string"
    | _ ->
      if not (Output_byte_counting.is_terminal !state)
      then fail (Output_byte_counting.error_message_if_transition_to_fail !state)
      else if Output_byte_counting.is_fail !state
      then fail "Failed to convert string to correct length. Invalid string of jsonaf bug"
      else (
        match next with
        | Some '"' -> advance 1 *> return res
        | _ -> fail "Unexpected end of string")
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
