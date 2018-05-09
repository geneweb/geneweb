(* camlp5r *)
(* Copyright (c) 1998-2007 INRIA *)

type t = 'a;

value zero : t;
value one : t;
value eq : t -> t -> bool;
value gt : t -> t -> bool;
value add : t -> t -> t;
value sub : t -> t -> t;
value twice : t -> t;
value half : t -> t;
value even : t -> bool;
value inc : t -> int -> t;
value mul : t -> int -> t;
value exp : t -> int -> t;
value div : t -> int -> t;
value modl : t -> int -> int;
value gen : t -> int;
value branch : t -> char;
value sosa_gen_up : t -> t;
value print : (string -> unit) -> string -> t -> unit;
value of_int : int -> t;
value to_int : t -> int;
value of_string : string -> t;
value to_string : t -> string;
value to_string_sep : string -> t -> string;
value to_string_sep_base : string -> int -> t -> string;
