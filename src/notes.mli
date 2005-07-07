(* camlp4r *)
(* $Id: notes.mli,v 4.10 2005-07-07 11:24:41 ddr Exp $ *)

open Config;
open Def;
open NotesLinks;

value file_path : config -> string -> string;
value syntax_links : config -> string -> (string -> string) -> string -> string;
value lines_list_of_string : string -> (list string * bool);
value insert_sub_part : string -> int -> string -> string;
value rev_extract_sub_part : string -> int -> list string;
value html_of_tlsw_lines :
  config -> string -> string -> int -> bool -> list string ->
    list string -> list string;
value html_with_summary_of_tlsw :
  config -> string -> (string -> string) -> string -> string -> string;
value print_sub_part : config -> string -> string -> int -> string -> unit;

value print : config -> base -> unit;
value print_mod_page :
  config -> string -> string -> (bool -> unit) -> string -> string -> unit;
value print_mod : config -> base -> unit;
value update_notes_links_db : config -> page -> string -> bool -> unit;
value print_mod_ok : config -> base -> unit;

value print_misc_notes : config -> base -> unit;
