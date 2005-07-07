(* camlp4r *)
(* $Id: wiki.mli,v 4.1 2005-07-07 18:32:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;

value lines_list_of_string : string -> (list string * bool);

value syntax_links : config -> string -> (string -> string) -> string -> string;
value html_of_tlsw_lines :
  config -> string -> string -> int -> bool -> list string ->
    list string -> list string;
value html_with_summary_of_tlsw :
  config -> string -> (string -> string) -> string -> string -> string;
value rev_extract_sub_part : string -> int -> list string;
value insert_sub_part : string -> int -> string -> string;
value split_title_and_text : string -> (string * string);

value print_sub_part :
  config -> (string -> string) -> string -> string -> int ->
    list string -> (string -> string) -> unit;
value print_mod_page :
  config -> string -> string -> (bool -> unit) -> string -> string -> unit;
