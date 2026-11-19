type line = { lang : string; text : string; lnum : int }
(** A translation line [lang: text] at line [lnum]; [lang] is ["->"] for an
    alias. *)

type entry = { key : string; key_lnum : int; lines : line list }
(** An entry: its key, the line number of the key and its translation lines. *)

type t = {
  entries : entry list;  (** Entries in file order. *)
  errors : (int * string) list;  (** Syntax errors with their line number. *)
  crlf : bool;  (** The file uses CRLF line endings. *)
  source : string;  (** Content of the file. *)
}

val read : string -> t
(** [read fname] parses the lexicon [fname]. Blank lines separate the entries; a
    key line starts with four spaces. *)

val sort : entry list -> entry list
(** [sort entries] orders [entries] by key, ignoring ASCII case first, and the
    lines of each entry by language. Both sorts are stable: duplicates keep
    their relative order, hence their precedence in gwd. *)

val to_string : crlf:bool -> entry list -> string
(** [to_string ~crlf entries] prints [entries] in the lexicon format, separated
    by a single blank line. *)

val duplicates : entry list -> (int * string) list
(** [duplicates entries] reports the keys defined more than once and the
    languages repeated within an entry. *)

val format_errors : entry list -> (int * string) list
(** [format_errors entries] reports the translations whose conversions ([%s],
    [%d], [%t], in order) or positional arguments ([%1] to [%9]) differ from the
    English ones, as a whole or variant by variant. *)

val languages : entry list -> string list
(** [languages entries] lists the language codes used in [entries]. *)

val missing : entry list -> string list -> string
(** [missing entries langs] prints, after the English and French references, an
    empty line [lang:] for each language of [langs] missing in an entry, and the
    line of each translation whose number of [/]-separated variants differs from
    the English one. Aliases are skipped. *)
