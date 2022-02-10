(* $Id: wiki.mli,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config

(* TLSW: Text Language Stolen to Wikipedia
   = title level 1 =
   == title level 2 ==
   ...
   ====== title level 6 ======
   * list ul/li item
   * list ul/li item
   ** list ul/li item 2nd level
   ** list ul/li item 2nd level
   ...
   # list ol/li item
   : indentation list dl/dd item
   ; list dl dt item ; dd item
   ''italic''
   '''bold'''
   '''''bold+italic'''''
   [[first_name/surname/oc/text]] link; 'text' displayed
   [[first_name/surname/text]] link (oc = 0); 'text' displayed
   [[first_name/surname]] link (oc = 0); 'first_name surname' displayed
   [[[notes_subfile/text]]] link to a sub-file; 'text' displayed
   [[[notes_subfile]]] link to a sub-file; 'notes_subfile' displayed
   empty line : new paragraph
   lines starting with space : displayed as they are (providing 1/ there
     are at least two 2/ there is empty lines before and after the group
     of lines).
   __TOC__ : summary
   __SHORT_TOC__ : short summary (unnumbered)
   __NOTOC__ : no (automatic) numbered summary *)

type wiki_info =
  { wi_mode : string;
    wi_file_path : string -> string;
    wi_person_exists : string * string * int -> bool;
    wi_always_show_link : bool }

val syntax_links : config -> wiki_info -> string -> string

(** Parses a whole TLSW text to a list of strings *)
val html_of_tlsw : config -> string -> string list

(** HTML displaying a table of content for a TLSW file *)
val html_with_summary_of_tlsw :
  config -> wiki_info -> (bool * string * string) option -> string -> string

(** [extract_sub_part tlsw i]
    Extracts the `i`th first TLSW sections of `tlsw` *)
val extract_sub_part : string -> int -> string list

(**
   The argument is expected to have the form "KEY=value\n"...
   This function calculates each Key/value pair and puts it in a list;
   except for the key TITLE, that is the second element of the returned tuple.
   If there is no title defined, checks if the first line is not empty and does
   not start with '=' nor contains '<' nor '[', in which case it is choosen as a
   first line. Otherwise, the title is the empty string.
*)
val split_title_and_text : string -> (string * string) list * string

(** Prints an exctracted sub part *)
val print_sub_part :
  config -> wiki_info -> bool -> string -> string -> int -> string list ->
  unit

(** Prints an editable part *)
val print_mod_view_page :
  config -> bool -> string -> string -> (bool -> unit) ->
    (string * string) list -> string -> unit

(** Commits the changes of a page *)
val print_mod_ok :
  config -> wiki_info -> (string -> string option) ->
    (string option -> string) ->
    (string -> (string * string) list * string) ->
    (string -> string -> unit) -> (string -> string) -> bool -> unit

(*S: shouldn't the following functions be defined elsewhere? *)

(** Reads the notes alias file (conf.base_env.notes_alias_file or base_path/notes.alias).
    File format is "KEY value\n...", returns the list of (KEY,value) *)
val notes_aliases : config -> (string * string) list

(** Given an alias list, finds the corresponding alias for a given string *)
val map_notes : (string * string) list -> string -> string
