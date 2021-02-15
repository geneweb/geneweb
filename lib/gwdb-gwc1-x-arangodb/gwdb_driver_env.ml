let aux s =
  try Sys.getenv s
  with Not_found ->
    prerr_endline @@
    "error: environment variable \"" ^ s ^ "\" not defined" ;
    raise Not_found

let user = aux "GW_ARANGODB_USER"
let password = aux "GW_ARANGODB_PASSWORD"
let server = aux "GW_ARANGODB_SERVER"
let port = aux "GW_ARANGODB_PORT"
let database = aux "GW_ARANGODB_DATABASE"
let url =
  "http://" ^ user ^ ":" ^ password ^ "@" ^ server ^ ":" ^ port ^ "/_db/" ^ database ^ "/"
