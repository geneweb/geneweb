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

type wiki_info = {
  wi_mode : string;
  wi_file_path : string -> string;
  wi_person_exists : string * string * int -> bool;
  wi_mark_if_not_public : string * string * int -> bool;
  wi_always_show_link : bool;
}
(** wi_mark_if_not_public: if the person designated by fn * sn * oc is not
    public display it in red. This function is used by Roglo to identify persons
    that should be public and are not *)

val syntax_links : config -> wiki_info -> string -> string
val bold_italic_syntax : string -> string

val make_edit_button :
  Config.config -> ?mode:string -> string -> ?cnt:int option -> unit -> string

val html_of_tlsw : config -> string -> string list
(** Parses a whole TLSW text to a list of strings *)

val html_with_summary_of_tlsw :
  config -> wiki_info -> (bool * string * string) option -> string -> string
(** HTML displaying a table of content for a TLSW file *)

val extract_sub_part : string -> int -> string list
(** [extract_sub_part tlsw i] Extracts the `i`th first TLSW sections of `tlsw`
*)

val split_title_and_text : string -> (string * string) list * string
(** The argument is expected to have the form "KEY=value\n"... This function
    calculates each Key/value pair and puts it in a list; except for the key
    TITLE, that is the second element of the returned tuple. If there is no
    title defined, checks if the first line is not empty and does not start with
    '=' nor contains '<' nor '\[', in which case it is choosen as a first line.
    Otherwise, the title is the empty string. *)

val print_sub_part :
  config -> wiki_info -> bool -> string -> string -> int -> string list -> unit
(** Prints an exctracted sub part *)

val print_mod_view_page :
  config (* conf *) ->
  bool (* can_edit *) ->
  Adef.encoded_string (* mode *) ->
  string (* fname *) ->
  (bool -> unit) ->
  (* title *)
  (string * string) list (* env *) ->
  string (* s *) ->
  unit
(** [print_mod_view_page conf can_edit mode fname title env s] Prints an
    editable part *)

val print_mod_ok :
  config (* conf *) ->
  wiki_info (* wi *) ->
  (string -> string option) ->
  (* edit_mode *)
  (string option -> string) ->
  (* fname *)
  (string -> (string * string) list * string) ->
  (* read_string *)
  (string -> string -> unit) ->
  (* commit *)
  (string -> string) ->
  (* string_filter *)
  bool (* title_is_1st *) ->
  unit
(** [print_mod_ok conf wi edit_mode fname read_string commit string_filter
     title_is_1st] Commits the changes of a page *)

(*S: shouldn't the following functions be defined elsewhere? *)

val notes_aliases : config -> (string * string) list
(** Reads the notes alias file (conf.base_env.notes_alias_file or
    base_path/notes.alias). File format is "KEY value\n...", returns the list of
    (KEY,value) *)

val map_notes : (string * string) list -> string -> string
(** Given an alias list, finds the corresponding alias for a given string *)
