open Server

type t = unit

let of_out_channel ~cgi _oc = is_cgi := cgi
let close = close_connection
let header () = header
let http () = http
let woc = woc
let wsocket = wsocket
let wflush = wflush
let http_redirect_temporarily () = http_redirect_temporarily
let printf () = printf
let print_string () = print_string
let pp_sockaddr = Server.pp_sockaddr
let is_lan_candidate = Server.is_lan_candidate
