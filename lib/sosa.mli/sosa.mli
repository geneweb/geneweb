(* Copyright (c) 1998-2007 INRIA *)

type t

(** Initial sosa value *)
val zero : t

(** Sosa number describing the subject itself *)
val one : t

(** Equality between 2 sosa *)
val eq : t -> t -> bool

(** Tells if one sosa number is greater then another *)
val gt : t -> t -> bool

(** Comparison function between sosa numbers *)
val compare : t -> t -> int

(** Addition of 2 sosa numbers *)
val add : t -> t -> t

(** Substraction of 2 sosa numbers *)
val sub : t -> t -> t

(** Returns sosa number multiplied by 2. Represents father's sosa number of
    person with the giving sosa. *)
val twice : t -> t

(** Returns sosa number divided by 2. If person has a child then result number 
    will be child's sosa number. *)
val half : t -> t

(** Tells if sosa number is even. Even numbers describe fathers, odd - mothers for each generation. *)
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
