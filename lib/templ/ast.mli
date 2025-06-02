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

and t = private { desc : desc; loc : Geneweb_loc.t }

val equal : t -> t -> bool
(** [equal t1 t2] checks if the ast node [t1] and [t2] are equal. *)

val pp : t Fmt.t
(** [pp ppf t] prints the ast node [t] on the formatter [ppf] for debugging
    purpose. *)

val mk_text : ?loc:Geneweb_loc.t -> string -> t
val mk_var : ?loc:Geneweb_loc.t -> string -> string list -> t
val mk_transl : ?loc:Geneweb_loc.t -> bool -> string -> string -> t
val mk_wid_hei : ?loc:Geneweb_loc.t -> string -> t
val mk_if : ?loc:Geneweb_loc.t -> t -> t list -> t list -> t

val mk_foreach :
  ?loc:Geneweb_loc.t -> string * string list -> t list list -> t list -> t

val mk_for : ?loc:Geneweb_loc.t -> string -> t -> t -> t list -> t

val mk_define :
  ?loc:Geneweb_loc.t ->
  string ->
  (string * t option) list ->
  t list ->
  t list ->
  t

val mk_apply :
  ?loc:Geneweb_loc.t -> string -> (string option * t list) list -> t

val mk_let : ?loc:Geneweb_loc.t -> string -> t list -> t list -> t
val mk_op1 : ?loc:Geneweb_loc.t -> string -> t -> t
val mk_op2 : ?loc:Geneweb_loc.t -> string -> t -> t -> t
val mk_int : ?loc:Geneweb_loc.t -> string -> t
val mk_include : ?loc:Geneweb_loc.t -> [ `File of string | `Raw of string ] -> t
val mk_pack : ?loc:Geneweb_loc.t -> t list -> t

val subst : (string -> string) -> t -> t
(** [subst f t] applies the substitution [f] on all variables of the ast node
    [t]. *)
