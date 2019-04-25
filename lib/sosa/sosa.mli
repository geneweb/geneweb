(* Copyright (c) 1998-2007 INRIA *)

type t

val zero : t
val one : t
val eq : t -> t -> bool
val gt : t -> t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val twice : t -> t
val half : t -> t
val even : t -> bool
val inc : t -> int -> t
val mul : t -> int -> t
val exp : t -> int -> t
val div : t -> int -> t
val modl : t -> int -> t
val gen : t -> int

(** [branches sosa]
    Return the path to follow in order to reach [sosa]
    It is encoded as a list of int representing the acendant to choose at each generation.
    [0] if you have to follow the father branch, [1] if it is the mother branch.
*)
val branches : t -> int list

val of_int : int -> t
val of_string : string -> t
val to_string : t -> string

(** See {!val:Mutil.string_of_int_sep} *)
val to_string_sep : string -> t -> string
