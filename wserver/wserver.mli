(* $Id: wserver.mli,v 1.1.1.1 1998-09-01 14:32:14 ddr Exp $ *)

(* module [Wserver]: elementary web service *)

value f :
  int -> int -> option int -> option (int * int) ->
    ((Unix.sockaddr * list string) -> string -> unit) -> unit
;
   (* [Wserver.f port tmout maxc robot_xcl g] starts an elementary httpd
       server at port [port] in the current machine. The port number is any
       number greater than 1024 (to create a client < 1024, you must be
       root). At each connection, the function [g] is called:
       [g (addr, request) s] where [addr] is the client
       identification socket, [request] the browser request, and [s]
       the string request itself (extracted from [request]). The function
       [g] has [tmout] seconds to answer some text on standard output.
       If [maxc] is [Some n], maximum [n] clients can be treated at the
       same time; [None] means no limit. See the example below.
       If [robot_xcl] is [Some (cnt, sec)], robots attacks are excluded,
       i.e. connexions calling more than [cnt] request in [sec] consecutive
       seconds; if [None], no robot exclusion. *)

value wprint : format 'a out_channel unit -> 'a;
    (* To be called to print page contents. *)

value wflush : unit -> unit;
    (* To flush page contents print. *)

value html : unit -> unit;
    (* [Wserver.html ()] specifies that the text will be HTML. *)

value encode : string -> string;
    (* [Wserver.encode s] encodes the string [s] in another string
       where spaces and special characters are coded. This allows
       to put such strings in html links <a href=...>. This is
       the same encoding done by Web browsers in forms. *)

value decode : string -> string;
    (* [Wserver.decode s] does the inverse job than [Wserver.code],
       restoring the initial string. *)

value extract_param : string -> char -> list string -> string;
    (* [extract_param name stopc request] can be used to extract some
       parameter from a browser [request] (list of strings); [name]
       is a string which should match the beginning of a request line,
       [stopc] is a character ending the request line. For example, the
       string request has been obtained by: [extract_param "GET /" ' '].
       Answers the empty string if the parameter is not found. *)

(* Example:

   - Source program "foo.ml":
        Wserver.f 2368 60 0
           (fun _ s -> Wserver.html (); Printf.printf "You said: %s...\n" s);;
   - Compilation:
        ocamlc -custom unix.cma -cclib -lunix wserver.cmo foo.ml
   - Run:
        ./a.out
   - Launch a Web browser and open the location:
        http://localhost:2368/hello   (but see the remark below)
   - You should see a new page displaying the text:
        You said: hello...

  Possible problem: if the browser says that it cannot connect to
      "localhost:2368",
  try:
      "localhost.domain:2368" (the domain where your machine is)
      "127.0.0.1:2368"
      "machine:2368"          (your machine name)
      "machine.domain:2368"   (your machine name)
      "addr:2368"             (your machine internet address)

*)
