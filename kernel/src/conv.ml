open StdLabels
open MoreLabels
include Conv_intf

type t = Type.t

let jsonaf_of_unit () : t = `Null

let jsonaf_of_bool b : t =
  match b with
  | true -> `True
  | false -> `False
;;

let jsonaf_of_string str : t = `String str
let jsonaf_of_bytes bytes : t = `String (Bytes.to_string bytes)
let jsonaf_of_char c : t = `String (String.make 1 c)
let jsonaf_of_int n : t = `Number (string_of_int n)

let jsonaf_of_float n : t =
  (* JSON cannot represent [nan], [infinity], or [neg_infinity] *)
  if not (Float.is_finite n)
  then failwith (Printf.sprintf "Cannot represent non-finite float as JSON: %f" n);
  (* The logic below was stolen from the js_of_ocaml compiler. It ensure roundtripability
     of floats while keeping small string representation *)
  let f12 = Printf.sprintf "%.12g" n in
  if Float.equal n (float_of_string f12)
  then `Number f12
  else (
    let f15 = Printf.sprintf "%.15g" n in
    if Float.equal n (float_of_string f15)
    then `Number f15
    else (
      let f18 = Printf.sprintf "%.18g" n in
      `Number f18))
;;

let jsonaf_of_int32 (n : Int32.t) : t = `Number (Int32.to_string n)
let jsonaf_of_int64 (n : Int64.t) : t = `Number (Int64.to_string n)
let jsonaf_of_nativeint n : t = `Number (Nativeint.to_string n)
let jsonaf_of_ref jsonaf_of__a rf = jsonaf_of__a !rf
let jsonaf_of_lazy_t jsonaf_of__a lv = jsonaf_of__a (Lazy.force lv)

let jsonaf_of_option jsonaf_of__a = function
  | Some x -> jsonaf_of__a x
  | None -> `Null
;;

let jsonaf_of_pair jsonaf_of__a jsonaf_of__b (a, b) =
  `Array [ jsonaf_of__a a; jsonaf_of__b b ]
;;

let jsonaf_of_triple jsonaf_of__a jsonaf_of__b jsonaf_of__c (a, b, c) =
  `Array [ jsonaf_of__a a; jsonaf_of__b b; jsonaf_of__c c ]
;;

(* List.rev (List.rev_map ...) is tail recursive, the OCaml standard library List.map is
   NOT. *)
let jsonaf_of_list jsonaf_of__a lst = `Array (List.rev (List.rev_map ~f:jsonaf_of__a lst))

let jsonaf_of_array jsonaf_of__a ar =
  let lst_ref = ref [] in
  for i = Array.length ar - 1 downto 0 do
    lst_ref := jsonaf_of__a ar.(i) :: !lst_ref
  done;
  `Array !lst_ref
;;

let jsonaf_of_hashtbl jsonaf_of_key jsonaf_of_val htbl =
  let coll ~key:k ~data:v acc = `Array [ jsonaf_of_key k; jsonaf_of_val v ] :: acc in
  `Array (Hashtbl.fold htbl ~init:[] ~f:coll)
;;

let jsonaf_of_opaque _ = `String "<opaque>"
let jsonaf_of_fun _ = `String "<fun>"

exception Of_jsonaf_error of exn * t @@ contended portable

let record_check_extra_fields = ref true
let of_jsonaf_error_exn exc jsonaf = raise (Of_jsonaf_error (exc, Type.mode_cross jsonaf))

let of_jsonaf_error what jsonaf =
  raise (Of_jsonaf_error (Failure what, Type.mode_cross jsonaf))
;;

let unit_of_jsonaf jsonaf =
  match jsonaf with
  | `Null -> ()
  | _ -> of_jsonaf_error "unit_of_jsonaf: `Null needed" jsonaf
;;

let bool_of_jsonaf jsonaf =
  match jsonaf with
  | `True -> true
  | `False -> false
  | _ -> of_jsonaf_error "bool_of_jsonaf: true/false needed" jsonaf
;;

let string_of_jsonaf jsonaf =
  match jsonaf with
  | `String str -> str
  | _ -> of_jsonaf_error "string_of_jsonaf: string needed" jsonaf
;;

let bytes_of_jsonaf jsonaf =
  match jsonaf with
  | `String str -> Bytes.of_string str
  | _ -> of_jsonaf_error "bytes_of_jsonaf: string needed" jsonaf
;;

let char_of_jsonaf jsonaf =
  match jsonaf with
  | `String str ->
    if String.length str <> 1
    then of_jsonaf_error "char_of_jsonaf: string must contain one character only" jsonaf;
    str.[0]
  | _ -> of_jsonaf_error "char_of_jsonaf: string of size one needed" jsonaf
;;

let look_like_int s =
  let r = ref true in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '+' | '-' | '0' .. '9' -> ()
    | _ -> r := false
  done;
  !r
;;

let int_of_jsonaf jsonaf =
  match jsonaf with
  | `Number v when look_like_int v -> int_of_string v
  | _ -> of_jsonaf_error "int_of_jsonaf: integer needed" jsonaf
;;

let float_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str -> float_of_string str
  | _ -> of_jsonaf_error "float_of_jsonaf: float needed" jsonaf
;;

let int32_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str when look_like_int str -> Int32.of_string str
  | _ -> of_jsonaf_error "int32_of_jsonaf: integer needed" jsonaf
;;

let int64_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str when look_like_int str -> Int64.of_string str
  | _ -> of_jsonaf_error "int64_of_jsonaf: integer needed" jsonaf
;;

let nativeint_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str when look_like_int str -> Nativeint.of_string str
  | _ -> of_jsonaf_error "nativeint_of_jsonaf: integer needed" jsonaf
;;

let ref_of_jsonaf a__of_jsonaf jsonaf = ref (a__of_jsonaf jsonaf)
let lazy_t_of_jsonaf a__of_jsonaf jsonaf = Lazy.from_val (a__of_jsonaf jsonaf)

let option_of_jsonaf a__of_jsonaf jsonaf =
  match jsonaf with
  | `Null -> None
  | el -> Some (a__of_jsonaf el)
;;

let pair_of_jsonaf a__of_jsonaf b__of_jsonaf jsonaf =
  match jsonaf with
  | `Array [ a_jsonaf; b_jsonaf ] ->
    let a = a__of_jsonaf a_jsonaf in
    let b = b__of_jsonaf b_jsonaf in
    a, b
  | _ -> of_jsonaf_error "pair_of_jsonaf: invalid format" jsonaf
;;

let triple_of_jsonaf a__of_jsonaf b__of_jsonaf c__of_jsonaf jsonaf =
  match jsonaf with
  | `Array [ a_jsonaf; b_jsonaf; c_jsonaf ] ->
    let a = a__of_jsonaf a_jsonaf in
    let b = b__of_jsonaf b_jsonaf in
    let c = c__of_jsonaf c_jsonaf in
    a, b, c
  | _ -> of_jsonaf_error "triple_of_jsonaf: invalid format" jsonaf
;;

let list_of_jsonaf a__of_jsonaf jsonaf =
  match jsonaf with
  | `Array lst ->
    let rev_lst = List.rev_map lst ~f:a__of_jsonaf in
    List.rev rev_lst
  | _ -> of_jsonaf_error "list_of_jsonaf: list needed" jsonaf
;;

let array_of_jsonaf a__of_jsonaf jsonaf =
  match jsonaf with
  | `Array [] -> [||]
  | `Array (h :: t) ->
    let len = List.length t + 1 in
    let res = Array.make len (a__of_jsonaf h) in
    let rec loop i = function
      | [] -> res
      | h :: t ->
        res.(i) <- a__of_jsonaf h;
        loop (i + 1) t
    in
    loop 1 t
  | _ -> of_jsonaf_error "array_of_jsonaf: list needed" jsonaf
;;

let hashtbl_of_jsonaf key_of_jsonaf val_of_jsonaf jsonaf =
  match jsonaf with
  | `Array lst ->
    let htbl = Hashtbl.create 0 in
    let act = function
      | `Array [ k_jsonaf; v_jsonaf ] ->
        Hashtbl.add htbl ~key:(key_of_jsonaf k_jsonaf) ~data:(val_of_jsonaf v_jsonaf)
      | _ -> of_jsonaf_error "hashtbl_of_jsonaf: tuple list needed" jsonaf
    in
    List.iter lst ~f:act;
    htbl
  | _ -> of_jsonaf_error "hashtbl_of_jsonaf: list needed" jsonaf
;;

let opaque_of_jsonaf jsonaf =
  of_jsonaf_error "opaque_of_jsonaf: cannot convert opaque values" jsonaf
;;

let fun_of_jsonaf jsonaf =
  of_jsonaf_error "fun_of_jsonaf: cannot convert function values" jsonaf
;;

module Primitives = struct
  let jsonaf_of_array = jsonaf_of_array
  let array_of_jsonaf = array_of_jsonaf
  let jsonaf_of_bool = jsonaf_of_bool
  let bool_of_jsonaf = bool_of_jsonaf
  let jsonaf_of_char = jsonaf_of_char
  let char_of_jsonaf = char_of_jsonaf
  let jsonaf_of_float = jsonaf_of_float
  let float_of_jsonaf = float_of_jsonaf
  let jsonaf_of_int = jsonaf_of_int
  let int_of_jsonaf = int_of_jsonaf
  let jsonaf_of_int32 = jsonaf_of_int32
  let int32_of_jsonaf = int32_of_jsonaf
  let jsonaf_of_int64 = jsonaf_of_int64
  let int64_of_jsonaf = int64_of_jsonaf
  let jsonaf_of_list = jsonaf_of_list
  let list_of_jsonaf = list_of_jsonaf
  let jsonaf_of_nativeint = jsonaf_of_nativeint
  let nativeint_of_jsonaf = nativeint_of_jsonaf
  let jsonaf_of_option = jsonaf_of_option
  let option_of_jsonaf = option_of_jsonaf
  let jsonaf_of_ref = jsonaf_of_ref
  let ref_of_jsonaf = ref_of_jsonaf
  let jsonaf_of_string = jsonaf_of_string
  let string_of_jsonaf = string_of_jsonaf
  let jsonaf_of_bytes = jsonaf_of_bytes
  let bytes_of_jsonaf = bytes_of_jsonaf
  let jsonaf_of_unit = jsonaf_of_unit
  let unit_of_jsonaf = unit_of_jsonaf
  let jsonaf_of_lazy_t = jsonaf_of_lazy_t
  let lazy_t_of_jsonaf = lazy_t_of_jsonaf
  let jsonaf_of_hashtbl = jsonaf_of_hashtbl
  let hashtbl_of_jsonaf = hashtbl_of_jsonaf
end
