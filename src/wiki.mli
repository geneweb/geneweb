(* camlp4r *)
(* $Id: wiki.mli,v 4.5 2005-07-14 22:52:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;

value syntax_links : config -> string -> (string -> string) -> string -> string;

value html_of_tlsw : config -> string -> list string;
value html_with_summary_of_tlsw :
  config -> string -> (string -> string) -> option (string * string) ->
    string -> string;

value extract_sub_part : string -> int -> list string;
value split_title_and_text : string -> (string * string);

value print_sub_part :
  config -> bool -> (string -> string) -> string -> string -> string -> int ->
    list string -> unit;
value print_mod_page :
  config -> string -> string -> (bool -> unit) -> string -> string -> unit;
value print_mod_ok :
  config -> (string -> option string) -> string -> (option string -> string) ->
    (string -> (string * string)) ->
    (config -> string -> string -> unit) -> (string -> string) ->
    (string -> string) -> unit;
