(* $Id: num.mli,v 1.1.1.1 1998-09-01 14:32:05 ddr Exp $ *)

type t = 'a;

value zero : t;
value one : t;
value eq : t -> t -> bool;
value twice : t -> t;
value half : t -> t;
value even : t -> bool;
value inc : t -> int -> t;
value div : t -> int -> t;
value modl : t -> int -> int;
value print : string -> t -> unit;
value of_string : string -> t;
value to_string : t -> string;
