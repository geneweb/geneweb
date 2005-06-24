(* camlp4r *)
(* $Id: notes.mli,v 4.1 2005-06-24 08:05:12 ddr Exp $ *)

open Config;
open Def;

value check_file_name : string -> bool;
value file_path : config -> string -> string;
value syntax_links :
  config -> string -> (Config.config -> string -> string) -> string ->
    string;
value lines_list_of_string : string -> list string;
value insert_sub_part : string -> int -> string -> string;
value rev_extract_sub_part : string -> int -> list string;
value html_of_tlsw_lines :
  config -> string -> string -> int -> bool -> list string ->
    list string -> list string;
value html_with_summary_of_tlsw :
  config -> string -> (config -> string -> string) -> string -> string ->
    string;
value print_sub_part : config -> string -> string -> int -> string -> unit;

value print : config -> base -> unit;
value print_mod_page :
  config -> string -> string -> (bool -> unit) -> string -> unit;
value print_mod : config -> base -> unit;
value update_notes_links_db : config -> string -> string -> unit;
value print_mod_ok : config -> base -> unit;
