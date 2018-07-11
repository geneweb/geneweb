(* camlp5r *)
(* $Id: notes.mli,v 5.6 2007-09-12 09:58:44 ddr Exp $ *)

open Config;
open Gwdb;

value file_path : config -> base -> string -> string;
value read_notes : base -> string -> (list (string * string) * string);

value print : config -> base -> unit;
value print_mod : config -> base -> unit;
value update_notes_links_db : config -> NotesLinks.page -> string -> unit;
value print_mod_ok : config -> base -> unit;

value print_misc_notes : config -> base -> unit;
value print_misc_notes_search : config -> base -> unit;
value print_linked_list : config -> base -> list NotesLinks.page -> unit;

value merge_possible_aliases :
  config -> NotesLinks.notes_links_db -> NotesLinks.notes_links_db;
