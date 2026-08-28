let src = Logs.Src.create ~doc:"CONN" "CONN"

module Log = (val Logs.src_log src : Logs.LOG)

type state = Ready | Header | Body | Closed

type t = {
  oc : out_channel;
  ic : in_channel;
  is_cgi : bool;
  is_socket : bool;
  mutable state : state;
}

let[@inline] of_channels ~cgi oc ic =
  { oc; ic; is_cgi = cgi; is_socket = false; state = Ready }

let[@inline] of_socket fd =
  let oc = Unix.out_channel_of_descr fd in
  let ic = Unix.in_channel_of_descr fd in
  { oc; ic; is_cgi = false; is_socket = true; state = Ready }

let[@inline] wflush { oc; _ } = flush oc
let[@inline] wsocket { oc; _ } = Unix.descr_of_out_channel oc
let[@inline] woc { oc; _ } = oc
let[@inline] wic { ic; _ } = ic

let drain_input ic =
  let buf_size = 4_096 in
  let buf = Bytes.create buf_size in
  let fd = Unix.descr_of_in_channel ic in
  let rec loop () =
    match Unix.select [ fd ] [] [] 0. with
    | [], _, _ -> ()
    | _ -> (
        match input ic buf 0 buf_size with
        | 0 | (exception Sys_error _) -> ()
        | _ -> loop ())
  in
  loop ()

let close t =
  match t.state with
  | Closed -> ()
  | Ready | Header | Body ->
      flush t.oc;
      if t.is_socket then Unix.shutdown (wsocket t) Unix.SHUTDOWN_SEND;
      drain_input t.ic;
      if t.is_socket then Unix.shutdown (wsocket t) Unix.SHUTDOWN_RECEIVE;
      close_out t.oc;
      close_in_noerr t.ic;
      t.state <- Closed

let close_noerr t = try close t with _ -> ()
let output t fmt = Printf.fprintf t.oc fmt

let http t status =
  match t.state with
  | Header | Body | Closed ->
      Log.err (fun k -> k "Attempted to write status on a closed connection.")
  | Ready ->
      t.state <- Header;
      if t.is_cgi then output t "Status: %a\r\n" Code.output status
      else output t "HTTP/1.0 %a\r\n" Code.output status

let header t s =
  match t.state with
  | Closed ->
      Log.err (fun k -> k "Attempted to write header on a closed connection.")
  | Body -> Log.err (fun k -> k "Attempted to write header after content.")
  | Ready ->
      Log.warn (fun k -> k "Sending a header before setting the status code.");
      http t Code.OK;
      output t "%s\r\n" s
  | Header -> output t "%s\r\n" s

let printf t fmt =
  match t.state with
  | Closed ->
      Log.err (fun k -> k "Attempted to write body on a closed connection.");
      Printf.ifprintf t.oc fmt
  | Ready ->
      Log.warn (fun k ->
          k
            "Writing body content without explicitly sending an HTTP header \
             first.");
      http t Code.OK;
      output t "\r\n";
      t.state <- Body;
      output t fmt
  | Header ->
      output t "\r\n";
      t.state <- Body;
      output t fmt
  | Body -> output t fmt

let print_string t s = printf t "%s" s

let http_redirect_temporarily t url =
  http t Code.Moved_Temporarily;
  output t "Location: %s\r\n\r\n" url;
  wflush t
