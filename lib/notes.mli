(* $Id: notes.mli,v 5.6 2007-09-12 09:58:44 ddr Exp $ *)

open Config
open Gwdb
open NotesLinks

val file_path : config -> base -> string -> string
val read_notes : base -> string -> (string * string) list * string

val print : config -> base -> unit
val print_mod : config -> base -> unit
val update_notes_links_db : config -> page -> string -> unit
val print_mod_ok : config -> base -> unit

val print_misc_notes : config -> base -> unit
val print_misc_notes_search : config -> base -> unit
val print_linked_list : config -> base -> page list -> unit

val merge_possible_aliases : config -> notes_links_db -> notes_links_db
