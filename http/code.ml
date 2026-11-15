type status =
  | OK
  | Moved_Temporarily
  | Bad_Request
  | Unauthorized
  | Forbidden
  | Not_Found
  | Conflict
  | Payload_Too_Large
  | Unsupported_Media_Type
  | Internal_Server_Error
  | Service_Unavailable

let status_code s =
  match s with
  | OK -> 200
  | Moved_Temporarily -> 302
  | Bad_Request -> 400
  | Unauthorized -> 401
  | Forbidden -> 403
  | Not_Found -> 404
  | Conflict -> 409
  | Payload_Too_Large -> 413
  | Unsupported_Media_Type -> 415
  | Internal_Server_Error -> 500
  | Service_Unavailable -> 503

let pp ppf s =
  match s with
  | OK -> Fmt.pf ppf "200 OK"
  | Moved_Temporarily -> Fmt.pf ppf "302 Moved Temporarily"
  | Bad_Request -> Fmt.pf ppf "400 Bad Request"
  | Unauthorized -> Fmt.pf ppf "401 Unauthorized"
  | Forbidden -> Fmt.pf ppf "403 Forbidden"
  | Not_Found -> Fmt.pf ppf "404 Not Found"
  | Conflict -> Fmt.pf ppf "409 Conflict"
  | Payload_Too_Large -> Fmt.pf ppf "413 Payload Too Large"
  | Unsupported_Media_Type -> Fmt.pf ppf "415 Unsupported Media Type"
  | Internal_Server_Error -> Fmt.pf ppf "500 Internal Server Error"
  | Service_Unavailable -> Fmt.pf ppf "503 Service Unavailable"

let to_string = Fmt.to_to_string pp
