module C = Cmdliner

type dflag = TLS | RPC

type cfg = {
  interface : string;
  port : int;
  max_connection : int option;
  idle_timeout : float option;
  task_timeout : float option;
  base_dir : string;
  index_dir : string;
  tls : (string * string) option;
  dflags : dflag list;
}

let default_interface = "localhost"
let default_port = 8080
let default_tls_port = 8443
let default_base_dir = "bases"
let default_index_dir = "etc"

module Dflag = struct
  let all = [ TLS; RPC ]
  let show = function TLS -> "tls" | RPC -> "rpc"

  let t =
    let enum_conv = List.map (fun v -> (show v, v)) all |> C.Arg.enum in
    let doc =
      Fmt.str "Set the debugging flags, $(docv) must be %s."
        (C.Arg.doc_alts (List.map show all))
    in
    C.Arg.(
      value & opt_all enum_conv [] & info [ "d"; "debug" ] ~docv:"DEBUG" ~doc)
end

module Connection = struct
  let interface =
    let doc = "Specify the network interface to listen on" in
    C.Arg.(
      value
      & opt string default_interface
      & info [ "i"; "interface" ] ~docv:"INTERFACE" ~doc)

  let port =
    let doc =
      Fmt.str
        "Specify the port to listen on. Defaults: %d without TLS and %d with \
         TLS"
        default_port default_tls_port
    in
    C.Arg.(value & opt (some int) None & info [ "p"; "port" ] ~docv:"PORT" ~doc)

  let max_connection =
    let doc =
      "Limit the maximum number of simultaneous connections. $(docv) must be a \
       positive integer."
    in
    C.Arg.(
      value
      & opt (some int) None
      & info [ "m"; "max-connection" ] ~docv:"INT" ~doc)

  let idle_timeout =
    let doc =
      "Define the idle timeout (in seconds) for closing client connection. \
       $(docv) must be a positive number."
    in
    C.Arg.(
      value & opt (some float) None & info [ "idle-timeout" ] ~docv:"FLOAT" ~doc)

  let crt =
    let doc = "Path to the TLS certificate file." in
    C.Arg.(
      value & opt (some string) None & info [ "c"; "crt" ] ~docv:"CERT" ~doc)

  let key =
    let doc = "Path to the private key file for TLS." in
    C.Arg.(
      value & opt (some string) None & info [ "k"; "key" ] ~docv:"KEY" ~doc)

  (* TODO: add a custom parser to emit an error if the user only specify
     a certificate key or a private key. *)
  let parse_connection_opt interface port max_connection idle_timeout crt key =
    let port =
      match (port, crt, key) with
      | None, None, None -> default_port
      | None, Some _, Some _ -> default_tls_port
      | None, _, _ -> assert false
      | Some p, _, _ -> p
    in
    let tls =
      match (crt, key) with
      | Some crt, Some key -> Some (crt, key)
      | None, None -> None
      | _ -> assert false
    in
    (interface, port, max_connection, idle_timeout, tls)

  let t =
    C.Term.(
      const parse_connection_opt $ interface $ port $ max_connection
      $ idle_timeout $ crt $ key)
end

let base_dir_t =
  let doc = "Specify the directory for bases." in
  C.Arg.(
    value
    & opt string default_base_dir
    & info [ "b"; "base-dir" ] ~docv:"DIR" ~doc)

let index_dir_t =
  let doc = "Specify the directory for index files" in
  C.Arg.(
    value
    & opt string default_index_dir
    & info [ "idx"; "index-dir" ] ~docv:"DIR" ~doc)

let task_timeout_t =
  let doc =
    "Define the task timeout (in seconds) per task. $(docv) must be a positive \
     number."
  in
  C.Arg.(
    value & opt (some float) None & info [ "task-timeout" ] ~docv:"FLOAT" ~doc)

let mk_cfg (interface, port, max_connection, idle_timeout, tls) base_dir
    index_dir task_timeout dflags =
  {
    interface;
    port;
    max_connection;
    idle_timeout;
    tls;
    base_dir;
    index_dir;
    task_timeout;
    dflags;
  }

let parse () =
  let doc = "Remote Procedure Call (RPC) server for Geneweb" in
  let info = C.Cmd.info "geneweb-rpc-server" ~version:"dev" ~doc in
  let cmd =
    C.Cmd.v info
      C.Term.(
        const mk_cfg $ Connection.t $ base_dir_t $ index_dir_t $ task_timeout_t
        $ Dflag.t)
  in
  match C.Cmd.eval_value cmd with
  | Ok (`Ok cfg) -> cfg
  | Ok (`Version | `Help) -> exit 0
  | Error `Parse -> exit 124
  | Error `Exn -> exit 125
  | Error `Term -> (* TODO: check this rc code. *) exit 1
