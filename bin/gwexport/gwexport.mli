type gwexport_charset = Ansel | Ansi | Ascii | Utf8

type gwexport_opts = {
  asc : int option; (* Maximum generation of the root's ascendants *)
  ascdesc : int option;
      (* Maximum generation of the root's ascendants descendants *)
  censor : int; (* Censors the base for 'n' years *)
  charset : gwexport_charset; (* The charset of the export *)
  desc : int option; (* Maximum generation of the root's descendants *)
  img_base_path : string; (* Unused by this module (and not set by options) *)
  keys : string list; (* Key reference of additional persons to select *)
  mem : bool; (* Unused by this module *)
  notes : bool; (* true if we export notes *)
  base_notes : bool; (* true if we export base_notes *)
  no_picture : bool; (* Unused by this module *)
  oc : string * (string -> unit) * (unit -> unit); (* Unused by this module *)
  parentship : bool;
      (* If asc, ascdesc and desc are not set & parenting = true, then
         select individuals involved in parentship between pair of keys
         (/!\ assumes the input are pairs of keys) *)
  picture_path : bool; (* Unused by this module *)
  source : string option; (* Unused by this module *)
  surnames : string list; (* Used to select persons by their surname *)
  verbose : bool; (* Unused by this module *)
}

val default_opts : gwexport_opts
(** Default set of options *)

val speclist : gwexport_opts ref -> (Arg.key * Arg.spec * Arg.doc) list
(** Given a set of options, returns default command line arguments for selecting
    elements from a base. The output of this function is the first input of
    Arg.parse.
*)
(* Used for gwd2ged and gwu. *)

val errmsg : Arg.usage_msg
(** Default error message.
    This is the third argument of Arg.parse. *)

val select :
  Gwdb.base ->
  gwexport_opts ->
  Gwdb.iper list ->
  (Gwdb.iper -> bool) * (Gwdb.ifam -> bool)
(** [select base opts ips] returns filters for [iper] and [ifam] to be used
    when exporting a portion of the [base]. *)
