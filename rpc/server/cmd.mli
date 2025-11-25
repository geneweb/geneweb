type dflag = private TLS | RPC

type cfg = private {
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

val cfg : cfg Cmdliner.Cmd.t
