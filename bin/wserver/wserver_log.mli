val log_request_infos :
  request:string list ->
  path:string ->
  query:Adef.encoded_string ->
  resp_status:Def.httpStatus option ->
  unit
