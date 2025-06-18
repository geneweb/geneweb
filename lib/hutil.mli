(* Copyright (c) 2007 INRIA *)

open Config

val header_without_http_nor_home : config -> (bool -> unit) -> unit

val header_with_title :
  ?error:bool -> ?fluid:bool -> config -> (bool -> unit) -> unit
(** Calls for [Util.html] to print HTTP header and for [header_without_http] to
    print HTML page header. HTML page header consists of :
    - <!DOCTYPE> Declaration
    - <head> tag where :
    - content of <title> tag is printed with [title true]
    - <meta> and <link> tags are filled due to [conf]
    - content of {i css.txt} template is evaluated and printed
    - content of {i hed.txt} template is evaluated and printed
    - Opening <body> tag with its attributes
    - If user is a wizard or a friend, then includes messages sent to them.
      Additionaly opens a <div> container (see Bootstrap). *)

val header_fluid : config -> (bool -> unit) -> unit
(** Calls header_with_title and opens a <div> container-fluid (see Bootstrap).
*)

val header_with_conf_title : config -> (bool -> unit) -> unit
(** Same as [header] but takes page title from [conf.env]. *)

val header_without_title : config -> unit
(** Similar to [header] but without any <h1> title element. Only prints HTTP
    header, HTML page header, and opens a <div> container. Useful when you need
    to handle title display separately. *)

val header_without_home : config -> (bool -> unit) -> unit
(** calls header_with_title, but gets its <h1> title from conf.env "p_title" *)

val header : ?error:bool -> ?fluid:bool -> config -> (bool -> unit) -> unit
(** [header conf title] calls for [header_with_title] to print HTTP header and
    HTML page header. Additionaly prints page title with [title true] (false to
    print browser tab title).
    - error : select a red color
    - fluid : open a container-fluid *)

val rheader : config -> (bool -> unit) -> unit
(** Same as [header] except page's title informs about an occured error (red
    title). *)

val trailer : config -> unit
(** [trailer conf] prints HTML page trailer in the body of the current response
    on the socket. HTML page trailer consists of :
    - content of {i trl.txt} file
    - Copyright message from template {i copyr.txt} with inserted logo
    - Scripts JS from template {i js.txt}
    - Closing <body> and <html> tags *)

val link_to_referer : config -> Adef.safe_string
(** Returns the HTML link to the previous (referer) page *)

val incorrect_request : ?comment:string -> config -> unit
(** Sends [Bad Request] HTTP response (same as
    [GWPARAM.output_error conf Bad_Request]) *)

val print_calendar : config -> Geneweb_db.Driver.base -> unit
(** Displays the calendar; if no key is set, it will use today's date. Based on
    template file calendar.txt *)
