let string_of_status = function
  | Def.OK -> "200 OK"
  | Def.Moved_Temporarily -> "302 Moved Temporarily"
  | Def.Bad_Request -> "400 Bad Request"
  | Def.Unauthorized -> "401 Unauthorized"
  | Def.Forbidden -> "403 Forbidden"
  | Def.Not_Found -> "404 Not Found"
  | Def.Conflict -> "409 Conflict"
  | Def.Internal_Server_Error -> "500 Internal Server Error"
  | Def.Service_Unavailable -> "503 Service Unavailable"
  | Def.Gateway_Timeout -> "504 Gateway Timeout"

let json_entry key value =
  Printf.sprintf "\"%s\":\"%s\"" (String.escaped key) (String.escaped value)

let json_of_request_infos tm pid request path query resp_status =
  let utime = Printf.sprintf "\"utime\": %f" tm.Unix.tms_utime in
  let stime = Printf.sprintf "\"stime\": %f" tm.tms_stime in
  let pid = Printf.sprintf "\"pid\": %d" pid in
  let user_agent = Mutil.extract_param "user-agent: " '\n' request in
  let resp_status = Option.value ~default:"" @@ Option.map string_of_status resp_status in
  "{"
  ^ String.concat ","
      [
        json_entry "status" resp_status;
        pid;
        utime;
        stime;
        json_entry "path" path;
        json_entry "user_agent" user_agent;
        json_entry "query" (Adef.as_string query);
      ]
  ^ "}"

let log_request_infos ~request ~path ~query ~resp_status =
  let tm = Unix.times () in
  let pid = Unix.getpid () in
  let json = json_of_request_infos tm pid request path query resp_status in
  Printf.eprintf "GW_REQUEST_INFO : %s" json
