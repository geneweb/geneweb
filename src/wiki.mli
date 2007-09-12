(* camlp5r *)
(* $Id: wiki.mli,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;

type wiki_info =
  { wi_mode : string;
    wi_file_path : string -> string;
    wi_cancel_links : bool;
    wi_person_exists : (string * string * int) -> bool;
    wi_always_show_link : bool }
;

value syntax_links : config -> wiki_info -> string -> string;

value html_of_tlsw : config -> string -> list string;
value html_with_summary_of_tlsw :
  config -> wiki_info -> option (bool * string * string) -> string -> string;

value extract_sub_part : string -> int -> list string;
value split_title_and_text : string -> (list (string * string) * string);

value print_sub_part :
  config -> wiki_info -> bool -> string -> string -> int ->
    list string -> unit;
value print_mod_view_page :
  config -> bool -> string -> string -> (bool -> unit) ->
    list (string * string) -> string -> unit;
value print_mod_ok :
  config -> wiki_info -> (string -> option string) ->
    (option string -> string) ->
    (string -> (list (string * string) * string)) ->
    (string -> string -> unit) -> (string -> string) ->
    bool -> unit;

value notes_aliases : config -> list (string * string);
value map_notes : list (string * string) -> string -> string;
