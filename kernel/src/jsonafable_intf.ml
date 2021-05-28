module type S = sig
  type t

  val t_of_jsonaf : Type.t -> t
  val jsonaf_of_t : t -> Type.t
end

module type S1 = sig
  type 'a t

  val t_of_jsonaf : (Type.t -> 'a) -> Type.t -> 'a t
  val jsonaf_of_t : ('a -> Type.t) -> 'a t -> Type.t
end

module type S2 = sig
  type ('a, 'b) t

  val t_of_jsonaf : (Type.t -> 'a) -> (Type.t -> 'b) -> Type.t -> ('a, 'b) t
  val jsonaf_of_t : ('a -> Type.t) -> ('b -> Type.t) -> ('a, 'b) t -> Type.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val t_of_jsonaf
    :  (Type.t -> 'a)
    -> (Type.t -> 'b)
    -> (Type.t -> 'c)
    -> Type.t
    -> ('a, 'b, 'c) t

  val jsonaf_of_t
    :  ('a -> Type.t)
    -> ('b -> Type.t)
    -> ('c -> Type.t)
    -> ('a, 'b, 'c) t
    -> Type.t
end
