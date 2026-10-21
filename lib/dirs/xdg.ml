type one = Var.one
type many = Var.many

type t = {
  data_home : one Var.t;
  config_home : one Var.t;
  state_home : one Var.t;
  cache_home : one Var.t;
  runtime_dir : one Var.t option;
  data_dirs : many Var.t;
  config_dirs : many Var.t;
}

let make_one_path ~getenv ~default name =
  let v =
    match getenv name with
    | Some v when Filename.is_relative v -> default
    | None -> default
    | Some v -> v
  in
  { Var.name; content = One v }

let parse s =
  let l = String.split_on_char ':' s in
  List.filter (fun x -> not @@ Filename.is_relative x) l

let make_many_path ~getenv ~default name =
  let l = match getenv name with Some s -> parse s | None -> default in
  { Var.name; content = Many l }

let ( // ) = Filename.concat
let root_dir = "/"
let[@inline] default_data_home home = home // ".local" // "share"
let[@inline] default_config_home home = home // ".config"
let[@inline] default_state_home home = home // ".local" // "state"
let[@inline] default_cache_home home = home // ".cache"

let default_data_dirs =
  [ root_dir // "usr" // "local" // "share"; root_dir // "usr" // "share" ]

let default_config_dirs = [ root_dir // "etc" // "xdg" ]

let make ?runtime_dir ~getenv () =
  let home = Option.get @@ getenv "HOME" in
  if Filename.is_relative home then failwith "make";
  let data_home =
    make_one_path ~getenv ~default:(default_data_home home) "XDG_DATA_HOME"
  in
  let config_home =
    make_one_path ~getenv ~default:(default_config_home home) "XDG_CONFIG_HOME"
  in
  let state_home =
    make_one_path ~getenv ~default:(default_state_home home) "XDG_STATE_HOME"
  in
  let cache_home =
    make_one_path ~getenv ~default:(default_cache_home home) "XDG_CACHE_HOME"
  in
  let runtime_dir =
    let name = "XDG_RUNTIME_DIR" in
    let o =
      match getenv name with
      | Some s -> Some (Var.One s)
      | None -> Option.bind runtime_dir @@ fun s -> Some (Var.One s)
    in
    Option.bind o (fun content -> Some { Var.name; content })
  in
  let data_dirs =
    make_many_path ~getenv ~default:default_data_dirs "XDG_DATA_DIRS"
  in
  let config_dirs =
    make_many_path ~getenv ~default:default_config_dirs "XDG_CONFIG_DIRS"
  in
  {
    data_home;
    config_home;
    state_home;
    cache_home;
    runtime_dir;
    data_dirs;
    config_dirs;
  }

let[@inline] data_home { data_home; _ } = data_home
let[@inline] config_home { config_home; _ } = config_home
let[@inline] state_home { state_home; _ } = state_home
let[@inline] cache_home { cache_home; _ } = cache_home
let[@inline] runtime_dir { runtime_dir; _ } = runtime_dir
let[@inline] data_dirs { data_dirs; _ } = data_dirs
let[@inline] config_dirs { config_dirs; _ } = config_dirs
