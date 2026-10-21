type desc = private
  | Atext of string
  | Avar of string * string list
  | Atransl of bool * string * string
  | Awid_hei of string
  | Aif of t * t list * t list
  | Aforeach of (string * string list) * t list list * t list
  | Afor of string * t * t * t list
  | Adefine of string * (string * t option) list * t list * t list
  | Aapply of string * (string option * t list) list
  | Alet of string * t list * t list
  | Aop1 of string * t
  | Aop2 of string * t * t
  | Aint of string
  | Ainclude of [ `File of string | `Raw of string ]
  | Apack of t list

and t = private { desc : desc; loc : Loc.t }

val equal : t -> t -> bool
(** [equal t1 t2] checks if the ast node [t1] and [t2] are equal. *)

val pp : t Fmt.t
(** [pp ppf t] prints the ast node [t] on the formatter [ppf] for debugging
    purpose. *)

val mk_text : ?loc:Loc.t -> string -> t
val mk_var : ?loc:Loc.t -> string -> string list -> t
val mk_transl : ?loc:Loc.t -> bool -> string -> string -> t
val mk_wid_hei : ?loc:Loc.t -> string -> t
val mk_if : ?loc:Loc.t -> t -> t list -> t list -> t

val mk_foreach :
  ?loc:Loc.t -> string * string list -> t list list -> t list -> t

val mk_for : ?loc:Loc.t -> string -> t -> t -> t list -> t

val mk_define :
  ?loc:Loc.t -> string -> (string * t option) list -> t list -> t list -> t

val mk_apply : ?loc:Loc.t -> string -> (string option * t list) list -> t
val mk_let : ?loc:Loc.t -> string -> t list -> t list -> t
val mk_op1 : ?loc:Loc.t -> string -> t -> t
val mk_op2 : ?loc:Loc.t -> string -> t -> t -> t
val mk_int : ?loc:Loc.t -> string -> t
val mk_include : ?loc:Loc.t -> [ `File of string | `Raw of string ] -> t
val mk_pack : ?loc:Loc.t -> t list -> t

val subst : (string -> string) -> t -> t
(** [subst f t] applies the substitution [f] on all variables of the ast node
    [t]. *)
