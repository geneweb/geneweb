type dflag = private TLS | RPC

type cfg = private {
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

val parse : unit -> cfg
