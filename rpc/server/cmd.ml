open Cmdliner
open Cmdliner.Term.Syntax

type dflag = TLS | RPC

type cfg = {
  interface : string;
  port : int;
  max_connection : int option;
  idle_timeout : float option;
  task_timeout : float option;
  index_dir : string;
  index_fuel : int;
  tls : (string * string) option;
  dflags : dflag list;
}

let default_interface = "localhost"
let default_port = 8080
let default_tls_port = 8443
let default_index_dir = "etc"
let default_index_fuel = 50
let conn_sec = "CONNECTION"
let idx_sec = "INDEX"

let dflags =
  let all = [ TLS; RPC ] in
  let show = function TLS -> "tls" | RPC -> "rpc" in
  let enum_conv = List.map (fun v -> (show v, v)) all |> Arg.enum in
  let doc =
    Fmt.str "Set the debugging flags, $(docv) must be %s."
      (Arg.doc_alts (List.map show all))
  in
  Arg.(value & opt_all enum_conv [] & info [ "d"; "debug" ] ~docv:"DEBUG" ~doc)

let interface =
  let doc = "Specify the network interface to listen on" in
  Arg.(
    value
    & opt string default_interface
    & info [ "i"; "interface" ] ~docs:conn_sec ~docv:"INTERFACE" ~doc)

let port =
  let doc =
    Fmt.str
      "Specify the port to listen on. Defaults: %d without TLS and %d with TLS"
      default_port default_tls_port
  in
  Arg.(
    value
    & opt (some int) None
    & info [ "p"; "port" ] ~docs:conn_sec ~docv:"PORT" ~doc)

let max_connection =
  let doc =
    "Limit the maximum number of simultaneous connections. $(docv) must be a \
     positive integer."
  in
  Arg.(
    value
    & opt (some int) None
    & info [ "m"; "max-connection" ] ~docs:conn_sec ~docv:"INT" ~doc)

let idle_timeout =
  let doc =
    "Define the idle timeout (in seconds) for closing client connection. \
     $(docv) must be a positive number."
  in
  Arg.(
    value
    & opt (some float) None
    & info [ "idle-timeout" ] ~docs:conn_sec ~docv:"FLOAT" ~doc)

let crt =
  let doc = "Path to the TLS certificate file." in
  Arg.(
    value
    & opt (some filepath) None
    & info [ "c"; "crt" ] ~docs:conn_sec ~docv:"PATH" ~doc)

let key =
  let doc = "Path to the private key file for TLS." in
  Arg.(
    value
    & opt (some filepath) None
    & info [ "k"; "key" ] ~docs:conn_sec ~docv:"PATH" ~doc)

let parse_connection_opt interface port max_connection idle_timeout crt key =
  let port =
    match (port, crt) with
    | None, Some _ -> default_tls_port
    | None, None -> default_port
    | Some p, _ -> p
  in
  match (crt, key) with
  | Some crt, Some key ->
      `Ok (interface, port, max_connection, idle_timeout, Some (crt, key))
  | None, None -> `Ok (interface, port, max_connection, idle_timeout, None)
  | _ ->
      `Error
        ( true,
          "You must specify both a TLS certificate and key to enable \
           connection encryption, or neither to leave it disabled." )

let connection =
  Term.ret
  @@
  let+ interface = interface
  and+ port = port
  and+ max_connection = max_connection
  and+ idle_timeout = idle_timeout
  and+ crt = crt
  and+ key = key in
  parse_connection_opt interface port max_connection idle_timeout crt key

let index_dir =
  let doc = "Specify the directory for index files." in
  Arg.(
    value
    & opt dirpath default_index_dir
    & info [ "idx"; "index-dir" ] ~docs:idx_sec ~docv:"DIR" ~doc)

let index_fuel =
  let doc = "Maximum number of answers per index request." in
  Arg.(
    value & opt int default_index_fuel
    & info [ "index-fuel" ] ~docs:idx_sec ~docv:"INT" ~doc)

let task_timeout =
  let doc =
    "Define the task timeout (in seconds) per task. $(docv) must be a positive \
     number."
  in
  Arg.(
    value & opt (some float) None & info [ "task-timeout" ] ~docv:"FLOAT" ~doc)

let cfg =
  let doc = "Remote Procedure Call (RPC) server for Geneweb." in
  Cmd.make (Cmd.info "geneweb-rpc-server" ~version:"%%VERSION%%" ~doc)
  @@
  let+ interface, port, max_connection, idle_timeout, tls = connection
  and+ index_dir = index_dir
  and+ index_fuel = index_fuel
  and+ task_timeout = task_timeout
  and+ dflags = dflags in
  {
    interface;
    port;
    max_connection;
    idle_timeout;
    tls;
    index_dir;
    index_fuel;
    task_timeout;
    dflags;
  }
