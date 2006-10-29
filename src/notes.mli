(* camlp4r *)
(* $Id: notes.mli,v 5.2 2006-10-29 20:49:57 ddr Exp $ *)

open Config;
open Gwdb;
open NotesLinks;

value file_path : config -> base -> string -> string;
value read_notes : base -> string -> (list (string * string) * string);

value print : config -> base -> unit;
value print_mod : config -> base -> unit;
value update_notes_links_db : config -> page -> string -> bool -> unit;
value print_mod_ok : config -> base -> unit;

value print_misc_notes : config -> base -> unit;
