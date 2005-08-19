(* camlp4r *)
(* $Id: wiki.mli,v 4.9 2005-08-19 01:39:30 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;

value syntax_links : config -> string -> (string -> string) -> string -> string;

value html_of_tlsw : config -> string -> list string;
value html_with_summary_of_tlsw :
  config -> string -> (string -> string) -> option (string * string) ->
    string -> string;

value extract_sub_part : string -> int -> list string;
value split_title_and_text :
  string -> (string * list (string * string) * string);

value print_sub_part :
  config -> bool -> (string -> string) -> string -> string -> string -> int ->
    list string -> unit;
value print_mod_page :
  config -> string -> string -> (bool -> unit) ->
    (string * list (string * string)) -> string -> unit;
value print_mod_ok :
  config -> (string -> option string) -> string -> (option string -> string) ->
    (string -> (string * list (string * string) * string)) ->
    (config -> string -> string -> unit) -> (string -> string) ->
    (string -> string) -> bool -> unit;

value notes_aliases : config -> list (string * string);
value map_notes : list (string * string) -> string -> string;
