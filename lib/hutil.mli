(* Copyright (c) 2007 INRIA *)

type meta = { name : string; content : string }

val header_without_http :
  Config.config -> (bool -> unit) -> ?meta:meta list -> unit -> unit
(** [header_without_http conf title] prints HTML page header in the body of the
    current response on the socket. HTML page header consists of :

    - <!DOCTYPE> Declaration
    - <head> tag where :

    - content of <title> tag is printed with [title true]
    - <meta> and <link> tags are filled due to [conf]
    - content of {i css.txt} template is evaluated and printed
    - content of {i hed.txt} template is evaluated and printed

    - Opening <body> tag with its attributes
    - If user is a wizard or a friend, then includes all messages send to him.
*)

val header_without_page_title :
  Config.config -> (bool -> unit) -> ?meta:meta list -> unit -> unit
(** Calls for [Util.html] to print HTTP header and for [header_without_http] to
    print HTML page header. Additionaly prints opening container <div> tag on
    the socket. *)

val header_with_meta : Config.config -> (bool -> unit) -> meta list -> unit

val header : Config.config -> (bool -> unit) -> unit
(** [header conf title] calls for [header_without_page_title] to print HTTP
    header and HTML page header. Additionaly prints page title with [title true]
    (false to print browser tab title). *)

val header_no_page_title : Config.config -> (bool -> unit) -> unit
(** Same as [header] but takes page title from [conf.env]. *)

val header_fluid : Config.config -> (bool -> unit) -> unit
(** Prints HTML page header (without HTTP headers) and opens fluid container
    <div> (see Bootstrap). *)

val header_link_welcome : Config.config -> (bool -> unit) -> unit
(** Same as [header] but insert links to previous and home pages (with
    [print_link_to_welcome]) before page title. *)

val trailer : Config.config -> unit
(** [trailer conf] prints HTML page trailer in the body of the current response
    on the socket. HTML page trailer consists of :

    - Copyright message from template {i copyr.txt} with inserted logo
    - Scripts JS from template {i js.txt}
    - Closing <body> and <html> tags *)

val rheader : Config.config -> (bool -> unit) -> unit
(** Same as [header] except page's title informs about an occured error (red
    title). *)

val link_to_referer : Config.config -> Adef.safe_string
(** Returns the HTML link to the previous (referer) page *)

val gen_print_link_to_welcome : (unit -> unit) -> Config.config -> bool -> unit
(** [gen_print_link_to_welcome f conf right_alined] prints links to previous and
    to home pages. [f] is used to print additional content before links. *)

val print_link_to_welcome : Config.config -> bool -> unit
(** Calls [gen_print_link_to_welcome] with empty function [f]. *)

val incorrect_request : Config.config -> unit
(** Sends [Bad Request] HTTP response (same as
    [GWPARAM.output_error conf Bad_Request]) *)

(* TODOOCP *)
val interp :
  Config.config ->
  string ->
  ('a, 'b) Templ.interp_fun ->
  'a Templ.env ->
  'b ->
  unit

val interp_no_header :
  Config.config ->
  string ->
  ('a, 'b) Templ.interp_fun ->
  'a Templ.env ->
  'b ->
  unit
