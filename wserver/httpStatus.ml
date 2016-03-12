(* camlp5o *)

type t =
| OK (* 200 *)
| Unauthorized (* 401 *)
| Forbidden (* 403 *)

let to_string = function
| OK -> "200 OK"
| Unauthorized -> "401 Unauthorized"
| Forbidden -> "403 Forbidden"
