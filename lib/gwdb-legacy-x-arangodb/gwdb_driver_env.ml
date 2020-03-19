let user = Sys.getenv "GW_ARANGODB_USER"
let password = Sys.getenv "GW_ARANGODB_PASSWORD"
let server = Sys.getenv "GW_ARANGODB_SERVER"
let port = Sys.getenv "GW_ARANGODB_PORT"
let database = Sys.getenv "GW_ARANGODB_DATABASE"
let url =
  "http://" ^ user ^ ":" ^ password ^ "@" ^ server ^ ":" ^ port ^ "/_db/" ^ database ^ "/"
