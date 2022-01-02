(* $Id: hutil.mli,v 5.4 2007-01-17 15:07:26 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config

(** [header_without_http conf title] pritns HTML page header in the body of the current response on the socket. 
    HTML page header consists of :
    
    - <!DOCTYPE> Declaration
    - <head> tag where :
      
      - content of <title> tag is get with [title true]
      - <meta> and <link> tags are filled due to [conf]
      - content of <style> tag is evaluated and send by interpretation of template {i etc/css.txt}
      
    - Opening <body> tag with its attributes
    - If user is a wizard or a friend, then includes all messages send to him. *)
val header_without_http : config -> (bool -> unit) -> unit

(** Calls for [Util.html] to print HTTP header and for [header_without_http] to print HTML page header. Additionaly
    prints opening container <div> tag on the socket. *)
val header_without_page_title : config -> (bool -> unit) -> unit

val header_no_page_title : config -> (bool -> unit) -> unit

(** [header conf title] calls for [header_without_page_title] to print HTTP header and HTML page header. Additionaly
    prints page title with [title true] (false to print browser tab title). *)
val header : config -> (bool -> unit) -> unit
val header_fluid : config -> (bool -> unit) -> unit
val header_link_welcome : config -> (bool -> unit) -> unit
val trailer : config -> unit

(** Same as [header] except page's title informs about an occured error. *)
val rheader : config -> (bool -> unit) -> unit

(** Returns the HTML link to the previous (referer) page *)
val link_to_referer : config -> string

(** [gen_print_link_to_welcome f conf right_alined] prints links to previous and to home page. [f] is used to print additional
    content. *)
val gen_print_link_to_welcome : (unit -> unit) -> config -> bool -> unit

(** Calls [gen_print_link_to_welcome] with empty function [f]. *)
val print_link_to_welcome : config -> bool -> unit

val gen_trailer : bool -> config -> unit

val incorrect_request : config -> unit

val interp :
  config -> string -> ('a, 'b) Templ.interp_fun -> 'a Templ.env -> 'b -> unit

val interp_no_header :
  config -> string -> ('a, 'b) Templ.interp_fun -> 'a Templ.env -> 'b -> unit

(** Displays the calendar; if no key is set, it will use today's date =.
    Based on template file calendar.txt *)
val print_calendar : config -> unit
