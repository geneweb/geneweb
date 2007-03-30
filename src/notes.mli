(* camlp4r *)
(* $Id: notes.mli,v 5.5 2007-03-30 18:57:19 ddr Exp $ *)

open Config;
open Gwdb;
open NotesLinks;

value file_path : config -> base -> string -> string;
value read_notes : base -> string -> (list (string * string) * string);

value print : config -> base -> unit;
value print_mod : config -> base -> unit;
value update_notes_links_db : config -> page -> string -> unit;
value print_mod_ok : config -> base -> unit;

value print_misc_notes : config -> base -> unit;
value print_misc_notes_search : config -> base -> unit;
value print_linked_list : config -> base -> list page -> unit;

value merge_possible_aliases : config -> notes_links_db -> notes_links_db;
