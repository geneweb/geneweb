(* Copyright (c) 1998-2007 INRIA *)

type t

val zero : t
(** Initial sosa value *)

val one : t
(** Sosa number describing the subject itself *)

val eq : t -> t -> bool
(** Equality between 2 sosa *)

val gt : t -> t -> bool
(** Tells if one sosa number is greater then another *)

val compare : t -> t -> int
(** Comparison function between sosa numbers *)

val add : t -> t -> t
(** Addition of 2 sosa numbers *)

val sub : t -> t -> t
(** Substraction of 2 sosa numbers *)

val twice : t -> t
(** Returns sosa number multiplied by 2. Represents father's sosa number of
    person with the giving sosa. *)

val half : t -> t
(** Returns sosa number divided by 2. If person has a child then result number
    will be child's sosa number. *)

val even : t -> bool
(** Tells if sosa number is even. Even numbers describe fathers, odd - mothers
    for each generation. *)

val inc : t -> int -> t
(** Addition of sosa number with a integer *)

val mul : t -> int -> t
(** Multiply sosa number with an integer *)

val exp : t -> int -> t
(** The power of the sosa number *)

val div : t -> int -> t
(** Divide sosa number by an integer *)

val modl : t -> int -> t
(** Calculate modulo of sosa number comparing to integer *)

val gen : t -> int
(** Returns generation of sosa number. *)

val branches : t -> int list
(** [branches sosa] Return the path to follow in order to reach [sosa] It is
    encoded as a list of int representing the acendant to choose at each
    generation. [0] if you have to follow the father branch, [1] if it is the
    mother branch. *)

val of_int : int -> t
(** Converts sosa from integer *)

val of_string : string -> t
(** Converts sosa from string *)

val to_string : t -> string
(** Converts sosa to string *)

val to_string_sep : string -> t -> string
(** See {!val:Mutil.string_of_int_sep} *)
