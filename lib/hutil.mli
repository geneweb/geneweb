(* Copyright (c) 2007 INRIA *)

open Config

val header_without_http_nor_home : config -> (bool -> unit) -> unit

val header_with_title :
  ?error:bool -> ?fluid:bool -> config -> (bool -> unit) -> unit
(** Prints HTTP header, HTML page header with <!DOCTYPE>, <head>, <body>, home
    template, container <div>, and <h1> title.
    @param error red title if [true]
    @param fluid container-fluid if [true] *)

val header_with_adaptive_title : ?fluid:bool -> config -> string -> unit
(** Header with adaptive title size based on content length. Uses h3/h4/h5 class
    on <h1> for long titles. *)

val header_fluid : config -> (bool -> unit) -> unit
(** Shortcut for [header_with_title ~fluid:true]. *)

val header_with_conf_title : config -> (bool -> unit) -> unit
(** Same as [header] but takes page title from [conf.env]. *)

val header_without_title : config -> unit
(** Similar to [header] but without any <h1> title element. *)

val header_without_home : config -> (bool -> unit) -> unit
(** Like [header_with_title] but without home.txt inclusion. *)

val header : ?error:bool -> ?fluid:bool -> config -> (bool -> unit) -> unit
(** Main header. [title true] prints in <title>, [title false] prints in <h1>.
*)

val rheader : config -> (bool -> unit) -> unit
(** Same as [header ~error:true]. *)

val trailer : config -> unit
(** Prints trl, copyr, closes container, js, timing, </body>. *)

val trailer_with_extra_js : config -> string -> unit
(** Like [trailer] but injects extra JS before main js.txt. Sources separated by
    ['|']: .js file paths or template names for inline <script> blocks. *)

val link_to_referer : config -> Adef.safe_string
(** HTML link to previous page (referer). Empty if none. *)

val incorrect_request : ?comment:string -> config -> unit
(** Sends HTTP 400 Bad Request. *)

val error_cannot_access : config -> string -> unit
(** Sends HTTP 404 for inaccessible template file. *)

val include_home_template : config -> unit
(** Includes home.txt template via Templ.output. Shows error page if template
    cannot be loaded. *)
