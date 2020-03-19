let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let send ?(tmout = 10000) header m url write : unit =
  try
    let connection = Curl.init () in
    Curl.set_httpheader connection header;
    Curl.set_httpversion connection Curl.HTTP_VERSION_1_1 ;
    Curl.set_writefunction connection write ;
    Curl.set_errorbuffer connection @@ ref "" ;
    Curl.set_followlocation connection true ;
    if tmout > 0 then Curl.set_timeoutms connection tmout ;
    Curl.set_url connection url ;
    begin match m with
      | `GET -> ()
      | `DELETE data ->
        Curl.set_customrequest connection "DELETE" ;
        if data <> "" then Curl.set_postfields connection data
      | `POST data ->
        Curl.set_post connection true ;
        Curl.set_postfields connection data ;
      | `PUT data ->
        Curl.set_customrequest connection "PUT" ;
        Curl.set_postfields connection data ;
    end ;
    Curl.perform connection ;
    Curl.cleanup connection
  with Curl.CurlException (_, _, s) -> failwith s
