(* camlp4r *)
(* $Id: wiki.mli,v 5.3 2006-10-30 21:11:10 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;

value syntax_links :
  config -> string -> (string -> string) -> string -> string;

value html_of_tlsw : config -> string -> list string;
value html_with_summary_of_tlsw :
  config -> string -> (string -> string) -> option (bool * string * string) ->
    string -> string;

value extract_sub_part : string -> int -> list string;
value split_title_and_text : string -> (list (string * string) * string);

value print_sub_part :
  config -> bool -> (string -> string) -> string -> string -> string -> int ->
    list string -> unit;
value print_mod_view_page :
  config -> bool -> string -> string -> (bool -> unit) ->
    list (string * string) -> string -> unit;
value print_mod_ok :
  config -> (string -> option string) -> string -> (option string -> string) ->
    (string -> (list (string * string) * string)) ->
    (string -> string -> unit) -> (string -> string) ->
    (string -> string) -> bool -> unit;

value notes_aliases : config -> list (string * string);
value map_notes : list (string * string) -> string -> string;
