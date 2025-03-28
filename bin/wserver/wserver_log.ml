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

type json_value = JsonString of string | JsonFloat of float | JsonInt of int

let json_entry key value =
  match value with
  | JsonString value ->
      Printf.sprintf "\"%s\":\"%s\"" (String.escaped key) (String.escaped value)
  | JsonFloat value -> Printf.sprintf "\"%s\":%f" (String.escaped key) value
  | JsonInt value -> Printf.sprintf "\"%s\":%d" (String.escaped key) value

let json_string_entry key value = json_entry key (JsonString value)
let json_float_entry key value = json_entry key (JsonFloat value)
let json_int_entry key value = json_entry key (JsonInt value)

let json_of_request_infos ~curr_tm ~tm ~request ~path ~resp_status ~length =
  let resp_status =
    Option.value ~default:"" @@ Option.map string_of_status resp_status
  in
  let mode, url =
    match Mutil.extract_param "GET /" ' ' request with
    | "" -> ("POST", Mutil.extract_param "Referer: " '\n' request)
    | url -> ("GET", url)
  in
  "{"
  ^ String.concat ","
      [
        json_string_entry "date" curr_tm;
        json_string_entry "request" url;
        json_string_entry "mode" mode;
        json_string_entry "status" resp_status;
        json_int_entry "resp_length" length;
        json_float_entry "utime" tm.Unix.tms_utime;
        json_float_entry "stime" tm.Unix.tms_stime;
        json_string_entry "path" path;
      ]
  ^ "}"

let log_request_infos ~request ~path ~resp_status ~length =
  let tm = Unix.times () in
  let curr_tm = Ext_unix.sprintf_date Unix.(time () |> localtime) in
  let json =
    json_of_request_infos ~curr_tm ~tm ~request ~path ~resp_status ~length
  in
  Printf.eprintf "GW_REQUEST_INFO : %s\n" json
