module C = Cmdliner

type dflag = TLS | RPC

type cfg = {
  interface : string;
  port : int;
  max_connection : int option;
  idle : float option;
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
    let doc = "Listen on the interface" in
    C.Arg.(
      value
      & opt string default_interface
      & info [ "i"; "interface" ] ~docv:"INTERFACE" ~doc)

  let port =
    let doc =
      Fmt.str
        "Listen on the port.The port %d is the default without TLS and %d is \
         the default with TLS"
        default_port default_tls_port
    in
    C.Arg.(value & opt (some int) None & info [ "p"; "port" ] ~docv:"PORT" ~doc)

  let max_connection =
    let doc = "Limit the maximum number of connections" in
    C.Arg.(
      value
      & opt (some int) None
      & info [ "m"; "max-connection" ] ~docv:"INT" ~doc)

  let idle =
    let doc = "Close idle client connection" in
    C.Arg.(value & opt (some float) None & info [ "idle" ] ~docv:"FLOAT" ~doc)

  let crt =
    let doc = "Certificate" in
    C.Arg.(
      value & opt (some string) None & info [ "c"; "crt" ] ~docv:"CERT" ~doc)

  let key =
    let doc = "Private key" in
    C.Arg.(
      value & opt (some string) None & info [ "k"; "key" ] ~docv:"KEY" ~doc)

  (* TODO: add a custom parser to emit an error if the user only specify
     a certificate key or a private key. *)
  let parse_connection_opt interface port max_connection idle crt key =
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
    (interface, port, max_connection, idle, tls)

  let t =
    C.Term.(
      const parse_connection_opt $ interface $ port $ max_connection $ idle
      $ crt $ key)
end

let base_dir_t =
  let doc = "Base directory" in
  C.Arg.(
    value
    & opt string default_base_dir
    & info [ "b"; "base-dir" ] ~docv:"DIR" ~doc)

let index_dir_t =
  let doc = "Index directory" in
  C.Arg.(
    value
    & opt string default_index_dir
    & info [ "idx"; "index-dir" ] ~docv:"DIR" ~doc)

let mk_cfg (interface, port, max_connection, idle, tls) base_dir index_dir
    dflags =
  { interface; port; max_connection; idle; tls; base_dir; index_dir; dflags }

let parse () =
  let doc = "Geneweb RPC server" in
  let info = C.Cmd.info "geneweb-rpc-server" ~version:"dev" ~doc in
  let cmd =
    C.Cmd.v info
      C.Term.(const mk_cfg $ Connection.t $ base_dir_t $ index_dir_t $ Dflag.t)
  in
  match C.Cmd.eval_value cmd with
  | Ok (`Ok cfg) -> cfg
  | Ok (`Version | `Help) -> exit 0
  | Error `Parse -> exit 124
  | Error `Exn -> exit 125
  | Error `Term -> (* TODO: check this rc code. *) exit 1
