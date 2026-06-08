type status =
  | OK
  | Moved_Temporarily
  | Bad_Request
  | Unauthorized
  | Forbidden
  | Not_Found
  | Service_Unavailable

let status_code s =
  match s with
  | OK -> 200
  | Moved_Temporarily -> 302
  | Bad_Request -> 400
  | Unauthorized -> 401
  | Forbidden -> 403
  | Not_Found -> 404
  | Service_Unavailable -> 503

let pp ppf s =
  match s with
  | OK -> Fmt.pf ppf "200 OK"
  | Moved_Temporarily -> Fmt.pf ppf "302 Moved Temporarily"
  | Bad_Request -> Fmt.pf ppf "400 Bad Request"
  | Unauthorized -> Fmt.pf ppf "401 Unauthorized"
  | Forbidden -> Fmt.pf ppf "403 Forbidden"
  | Not_Found -> Fmt.pf ppf "404 Not Found"
  | Service_Unavailable -> Fmt.pf ppf "503 Service Unavailable"

let to_string = Fmt.to_to_string pp
