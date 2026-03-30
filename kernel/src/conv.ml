open StdLabels
open MoreLabels
include Conv_intf

type t = Type.t

external string_length : string -> int = "%string_length"
external string_get : string -> int -> char = "%string_safe_get"
external int_of_string : string -> int = "caml_int_of_string"
external float_of_string : string -> float = "caml_float_of_string"

external int32_of_string
  :  string
  -> (int32[@unboxed])
  = "caml_int32_of_string" "caml_dummy_int32_of_string_unboxed"

external int64_of_string
  :  string
  -> (int64[@unboxed])
  = "caml_int64_of_string" "caml_dummy_int64_of_string_unboxed"

external nativeint_of_string
  :  string
  -> (nativeint[@unboxed])
  = "caml_nativeint_of_string" "caml_dummy_nativeint_of_string_unboxed"

external create_local_bytes : int -> bytes = "caml_create_bytes"

external unsafe_blit_string
  :  src:string
  -> src_pos:int
  -> dst:bytes
  -> dst_pos:int
  -> len:int
  -> unit
  = "caml_blit_string"
[@@noalloc]

[%%template
let[@mode global] bytes_of_string str = Bytes.of_string str

let[@mode local] bytes_of_string str =
  let len = string_length str in
  let bytes = create_local_bytes len in
  unsafe_blit_string ~src:str ~src_pos:0 ~dst:bytes ~dst_pos:0 ~len;
  bytes
;;]

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

let jsonaf_of_or_null jsonaf_of__a = function
  | Basement.Or_null_shim.This x -> jsonaf_of__a x
  | Null -> `Null
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

exception Of_jsonaf_error of exn * t

let record_check_extra_fields = ref true

[%%template
let[@mode global] maybe_globalize t = t
let[@mode local] maybe_globalize t = Type.globalize t

[@@@mode.default m = (local, global)]

let of_jsonaf_error_exn exc jsonaf =
  raise (Of_jsonaf_error (exc, Type.mode_cross ((maybe_globalize [@mode m]) jsonaf)))
;;

let of_jsonaf_error what jsonaf =
  raise
    (Of_jsonaf_error (Failure what, Type.mode_cross ((maybe_globalize [@mode m]) jsonaf)))
;;]

let look_like_int s =
  let r = ref true in
  for i = 0 to string_length s - 1 do
    match string_get s i with
    | '+' | '-' | '0' .. '9' -> ()
    | _ -> r := false
  done;
  !r
;;

[%%template
let rec map_list (f : Type.t -> 'a) (xs : Type.t list) =
  match[@exclave_if_stack a] xs with
  | [] -> []
  | x :: xs ->
    (* Some tests are sensitive to evaluation order so always map first. This isn’t
       tail-recursive but I think not allocating the list twice on the local stack is a
       reasonable advantage here. *)
    let fx = f x in
    fx :: (map_list [@alloc a]) f xs
[@@alloc a @ m = stack_local]
;;

let map_list f xs = List.map ~f xs [@@alloc heap]

[@@@alloc.default a @ m = (heap_global, stack_local)]

let unit_of_jsonaf jsonaf =
  match jsonaf with
  | `Null -> ()
  | _ -> (of_jsonaf_error [@mode m]) "unit_of_jsonaf: `Null needed" jsonaf
;;

let bool_of_jsonaf jsonaf =
  match jsonaf with
  | `True -> true
  | `False -> false
  | _ -> (of_jsonaf_error [@mode m]) "bool_of_jsonaf: true/false needed" jsonaf
;;

let string_of_jsonaf jsonaf =
  match jsonaf with
  | `String str -> str
  | _ -> (of_jsonaf_error [@mode m]) "string_of_jsonaf: string needed" jsonaf
;;

let bytes_of_jsonaf jsonaf =
  match jsonaf with
  | `String str -> (bytes_of_string [@mode m]) str [@exclave_if_stack a]
  | _ -> (of_jsonaf_error [@mode m]) "bytes_of_jsonaf: string needed" jsonaf
;;

let char_of_jsonaf jsonaf =
  match jsonaf with
  | `String str ->
    if String.length str <> 1
    then
      (of_jsonaf_error [@mode m])
        "char_of_jsonaf: string must contain one character only"
        jsonaf;
    str.[0]
  | _ -> (of_jsonaf_error [@mode m]) "char_of_jsonaf: string of size one needed" jsonaf
;;

let int_of_jsonaf jsonaf =
  match jsonaf with
  | `Number v when look_like_int v -> int_of_string v
  | _ -> (of_jsonaf_error [@mode m]) "int_of_jsonaf: integer needed" jsonaf
;;

let float_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str -> float_of_string str
  | _ -> (of_jsonaf_error [@mode m]) "float_of_jsonaf: float needed" jsonaf
;;

let int32_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str when look_like_int str -> int32_of_string str
  | _ -> (of_jsonaf_error [@mode m]) "int32_of_jsonaf: integer needed" jsonaf
;;

let int64_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str when look_like_int str -> int64_of_string str
  | _ -> (of_jsonaf_error [@mode m]) "int64_of_jsonaf: integer needed" jsonaf
;;

let nativeint_of_jsonaf jsonaf =
  match jsonaf with
  | `Number str when look_like_int str -> nativeint_of_string str
  | _ -> (of_jsonaf_error [@mode m]) "nativeint_of_jsonaf: integer needed" jsonaf
;;

let ref_of_jsonaf (a__of_jsonaf : Type.t -> 'a) jsonaf =
  ref (a__of_jsonaf jsonaf) [@exclave_if_stack a]
;;

let option_of_jsonaf (a__of_jsonaf : Type.t -> 'a) jsonaf =
  match[@exclave_if_stack a] jsonaf with
  | `Null -> None
  | el -> Some (a__of_jsonaf el)
;;

let or_null_of_jsonaf a__of_jsonaf (jsonaf : Type.t) =
  match[@exclave_if_stack a] jsonaf with
  | `Null -> Basement.Or_null_shim.Null
  | el -> This (a__of_jsonaf el)
;;

let pair_of_jsonaf (a__of_jsonaf : Type.t -> 'a) (b__of_jsonaf : Type.t -> 'b) jsonaf =
  match jsonaf with
  | `Array [ a_jsonaf; b_jsonaf ] ->
    (let a = a__of_jsonaf a_jsonaf in
     let b = b__of_jsonaf b_jsonaf in
     a, b)
    [@exclave_if_stack a]
  | _ -> (of_jsonaf_error [@mode m]) "pair_of_jsonaf: invalid format" jsonaf
;;

let triple_of_jsonaf
  (a__of_jsonaf : Type.t -> 'a)
  (b__of_jsonaf : Type.t -> 'b)
  (c__of_jsonaf : Type.t -> 'c)
  jsonaf
  =
  match jsonaf with
  | `Array [ a_jsonaf; b_jsonaf; c_jsonaf ] ->
    (let a = a__of_jsonaf a_jsonaf in
     let b = b__of_jsonaf b_jsonaf in
     let c = c__of_jsonaf c_jsonaf in
     a, b, c)
    [@exclave_if_stack a]
  | _ -> (of_jsonaf_error [@mode m]) "triple_of_jsonaf: invalid format" jsonaf
;;

let list_of_jsonaf (a__of_jsonaf : Type.t -> 'a) jsonaf =
  match jsonaf with
  | `Array lst -> (map_list [@alloc a]) a__of_jsonaf lst [@exclave_if_stack a]
  | _ -> (of_jsonaf_error [@mode m]) "list_of_jsonaf: list needed" jsonaf
;;]

let lazy_t_of_jsonaf a__of_jsonaf jsonaf = Lazy.from_val (a__of_jsonaf jsonaf)

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

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

let opaque_of_jsonaf jsonaf =
  (of_jsonaf_error [@mode m]) "opaque_of_jsonaf: cannot convert opaque values" jsonaf
;;

let fun_of_jsonaf jsonaf =
  (of_jsonaf_error [@mode m]) "fun_of_jsonaf: cannot convert function values" jsonaf
;;]

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
  let jsonaf_of_or_null = jsonaf_of_or_null
  let or_null_of_jsonaf = or_null_of_jsonaf
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
