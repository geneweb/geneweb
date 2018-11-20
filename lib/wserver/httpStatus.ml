(* camlp5o *)

type t =
| OK (* 200 *)
| Moved_Temporarily (* 302 *)
| Bad_Request (* 400 *)
| Unauthorized (* 401 *)
| Forbidden (* 403 *)
| Not_Found (* 404 *)

let to_string = function
| OK -> "200 OK"
| Moved_Temporarily -> "302 Moved Temporarily"
| Bad_Request -> "400 Bad Request"
| Unauthorized -> "401 Unauthorized"
| Forbidden -> "403 Forbidden"
| Not_Found -> "404 Not Found"
