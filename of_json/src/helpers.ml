open Core
open Type

module Context_stack : sig
  (* A [Context_stack] is a nonempty accumulator that supports traversal in insertion
     order. Its semantics fit the way raising through a series of exception handlers
     constructs a stack by slipping things under the bottom. *)

  type 'a t

  val singleton : 'a -> 'a t
  val add_caller : 'a t -> 'a -> 'a t
  val pop_caller : 'a t -> 'a * 'a t option

  (* [iter] and [to_list] traverse in callee-before-caller order. *)

  val iter : 'a t -> f:('a -> unit) -> unit

  (* The farthest-up caller comes out last and has [depth = 0]. *)
  val to_list : 'a t -> f:(depth:int -> 'a -> 'b) -> 'b list
end = struct
  type 'a t = 'a Fdeque.t

  let singleton = Fdeque.singleton
  let add_caller = Fdeque.enqueue_back

  let pop_caller t =
    Fdeque.dequeue_back_exn t
    |> Tuple2.map_snd ~f:(fun rest -> Option.some_if (not (Fdeque.is_empty rest)) rest)
  ;;

  let iter = Fdeque.iter

  let to_list t ~f =
    let length = Fdeque.length t - 1 in
    Fdeque.to_list t |> List.mapi ~f:(fun i a -> f ~depth:(length - i) a)
  ;;
end

module Conv_failure = struct
  module Context = struct
    type t =
      { json : Json.t
      ; location : string option
      }

    let to_exn_sexp { json; location } ~depth =
      let tag = [%string "json context [%{depth#Int}]"] in
      let tag =
        match location with
        | None -> tag
        | Some location -> [%string "%{tag}, at %{location}"]
      in
      [%sexp (tag : string), (json : Json.t)]
    ;;

    let to_string_hum_fmt fmt { json; location } =
      Option.iter location ~f:(Format.fprintf fmt "at %s ");
      (* <0 4> means we're indenting by 4 spaces, and <v 0> means we have a vertical box
         and each new line is not indented. This is what we want because the [Jsonaf]
         returns the correct spacing already. *)
      Format.fprintf fmt "in json:@;@;<0 4>@[<v 0>";
      (* Even though we have [Jsonaf.pp], it just writes out raw newlines, which is never
         what you want with [Format]. We break down the string here and use @; to delimit
         lines. *)
      List.iter
        (String.split_lines (Jsonaf.to_string_hum json))
        ~f:(Format.fprintf fmt "%s@;");
      Format.fprintf fmt "@]@;@;"
    ;;
  end

  type t =
    { exn : Exn.t
    ; context_stack : Context.t Context_stack.t
    }

  let context_sexp context_stack =
    Context_stack.to_list context_stack ~f:(fun ~depth context ->
      Context.to_exn_sexp context ~depth)
  ;;

  (* This [sexp_of_t] exists only to give a slightly nicer pretty-printing sexp for the
     exception [Of_json_conv_failed] below. See tests for formatting examples. *)
  let sexp_of_t { exn; context_stack } =
    [%sexp
      "Of_json failed to convert"
      :: (exn : Exn.t)
      :: (context_sexp context_stack : Sexp.t list)]
  ;;

  let to_string_hum_fmt fmt { exn; context_stack } =
    Format.fprintf fmt "@[<v>";
    Exn.pp fmt exn;
    Format.fprintf fmt "@;";
    Context_stack.iter context_stack ~f:(Context.to_string_hum_fmt fmt);
    Format.fprintf fmt "@]"
  ;;

  let to_string_hum t =
    let buffer = Buffer.create 1024 (* arbitrary *) in
    let fmt = Format.formatter_of_buffer buffer in
    to_string_hum_fmt fmt t;
    Format.pp_print_flush fmt ();
    Buffer.contents buffer
  ;;

  let extract_exn { exn; _ } = exn
end

exception Of_json_conv_failed of Conv_failure.t [@@deriving sexp]

let reraise exn ~context =
  match exn with
  | Of_json_conv_failed { exn = inner_exn; context_stack } ->
    let context_stack = Context_stack.add_caller context_stack context in
    raise (Of_json_conv_failed { exn = inner_exn; context_stack })
  | _ ->
    raise (Of_json_conv_failed { exn; context_stack = Context_stack.singleton context })
;;

let annotate ?location t json =
  try run t json with
  | exn -> reraise exn ~context:{ json; location }
;;

(* Equivalent to [map], but [reraise]s exceptions only from the application of [f] with
   context. *)
let annotated_map ?location t ~f json =
  let result = run t json in
  try f result with
  | exn -> reraise exn ~context:{ json; location }
;;

let lookup key = Json.member key

let lookup_exn key json =
  match lookup key json with
  | Some value -> value
  | None -> raise_s [%message "Key not in object" (key : string)]
;;

let using key t =
  (* We annotate the key access and run separately so that we only provide a location
     indicator if it exists *)
  annotate (lookup_exn key) |> annotated_map ~f:(run t) ~location:[%string "key [%{key}]"]
;;

let using_opt key t =
  annotate
    (fun json -> Option.map (lookup key json) ~f:(run t))
    ~location:[%string {|key [%{key}]|}]
;;

let map_object ~f =
  annotate ?location:None
  @@ fun json ->
  List.map (Json.keys json) ~f:(fun key -> run (f key) (lookup_exn key json))
;;

let safe t json =
  try Some (run t json) with
  | _ex -> None
;;

module Alternative_error = struct
  module Context = Conv_failure.Context

  type branch =
    { exn : Exn.t
    ; context_stack : Context.t Context_stack.t option
    }

  type t =
    { branches : branch Appendable_list.t
    ; context : Context.t
    }

  let of_conv_failure (err : Conv_failure.t) =
    let local, rest = Context_stack.pop_caller err.context_stack in
    { branches = Appendable_list.singleton { exn = err.exn; context_stack = rest }
    ; context = local
    }
  ;;

  let combine t1 t2 =
    { t1 with branches = Appendable_list.append t1.branches t2.branches }
  ;;

  let sexp_of_t { branches; context } : Sexp.t =
    let branches =
      Sequence.mapi (Appendable_list.to_sequence branches) ~f:(fun i branch ->
        let tag = [%string "branch [%{i#Int}]"] in
        let context =
          match branch.context_stack with
          | None -> []
          | Some stack -> Conv_failure.context_sexp stack
        in
        [%sexp (tag : string) :: (branch.exn : Exn.t) :: (context : Sexp.t list)])
    in
    List
      (List.concat
         [ [ [%sexp "expected one non-failure"] ]
         ; Sequence.to_list branches
         ; [ [%sexp "branch context", (context.json : Json.t)] ]
         ])
  ;;
end

exception Alternative_error of Alternative_error.t [@@deriving sexp]

let combined_exns exn1 exn2 ~context =
  let conv_exn = function
    | Alternative_error e -> e
    | Of_json_conv_failed e -> Alternative_error.of_conv_failure e
    | exn ->
      let context_stack = Context_stack.singleton context in
      Alternative_error.of_conv_failure { exn; context_stack }
  in
  Alternative_error (Alternative_error.combine (conv_exn exn1) (conv_exn exn2))
;;

let ( <|> ) a b json =
  try run a json with
  | left_exn ->
    (try run b json with
     | right_exn ->
       raise (combined_exns left_exn right_exn ~context:{ json; location = None }))
;;

let choice ts =
  if List.is_empty ts
  then failwith "Expected at least one [of_json] to choose from"
  else List.reduce_exn ts ~f:( <|> )
;;

let ( @. ) = using
let ( @? ) = using_opt
let ( @> ) t f = annotated_map t ~f
let ( >>> ) a b = Fn.compose b a
let json = Fn.id
let int = annotate Json.as_int
let float = annotate Json.as_float
let number = annotate Json.as_number
let string = annotate Json.as_string
let bool = annotate Json.as_bool
let list t = annotate (Json.as_list @> List.map ~f:(run t))
let option t = annotate (Json.as_option @> Option.map ~f:(run t))
let ( @?? ) key t = using_opt key (option t) @> Option.join
let as_sexp of_sexp = string @> (Sexp.of_string >>> of_sexp)

(** These are for sloppy APIs which sometimes double quotes values. *)
let number_string = annotate (Json.as_number <|> Json.as_string)

let float_string = number_string @> Float.of_string
let int_string = number_string @> Int.of_string

module Array_as_tuple = struct
  module T = struct
    type 'a t = Json.t list -> 'a * Json.t list

    let run t jsons = t jsons

    let bind t ~f jsons =
      let x, jsons = run t jsons in
      run (f x) jsons
    ;;

    let return a jsons = a, jsons

    let map t ~f jsons =
      let x, jsons = run t jsons in
      f x, jsons
    ;;

    let apply af ax = bind af ~f:(fun f -> map ax ~f)
    let map = `Custom map
  end

  include T
  include Applicative.Make (T)

  (* This module is supposed to be used in an inline open, so we have to do some
     functor/signature dance to get the correct [Let_syntax] module out. *)
  module M = Monad.Make (T)
  include (M : Monad.S_without_syntax with type 'a t := 'a t)
  module Let_syntax = M.Let_syntax.Let_syntax

  let run_exhaustively (t : 'a t) jsons : 'a =
    let a, jsons = t jsons in
    match jsons with
    | [] -> a
    | elems ->
      raise_s [%message "array_as_tuple has unparsed elements" (elems : Json.t list)]
  ;;

  let shift of_json : _ t = function
    | [] -> raise_s [%message "ran out of elements while parsing tuple"]
    | hd :: tl -> Type.run of_json hd, tl
  ;;

  let drop_rest : unit t = fun _jsons -> (), []
end

let tuple m = list json @> Array_as_tuple.run_exhaustively m
