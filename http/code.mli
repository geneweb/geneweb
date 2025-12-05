type status =
  | OK (* 200 *)
  | Moved_Temporarily (* 302 *)
  | Bad_Request (* 400 *)
  | Unauthorized (* 401 *)
  | Forbidden (* 403 *)
  | Not_Found (* 404 *)
  | Conflict (* 409 *)
  | Internal_Server_Error (* 500 *)
  | Service_Unavailable (* 503 *)

val status_code : status -> int
val pp : status Fmt.t
val to_string : status -> string
