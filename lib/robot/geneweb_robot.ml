let magic_robot = "GWRB0008"

let file () =
  String.concat Filename.dir_sep [ Secure.bases_dir (); "cnt"; "robot" ]

let lock_file () =
  let dir = Filename.dirname (file ()) in
  (if not (Sys.file_exists dir) then
     try Filesystem.create_dir ~parent:true ~required_perm:0o755 dir
     with Sys_error _ -> ());
  Filename.concat dir "robot.lck"

module W = Map.Make (struct
  type t = string

  let compare = compare
end)

type norfriwiz = Normal | Friend of string | Wizard of string

let who_key ip = function
  | Normal -> ip
  | Friend n -> ip ^ " F:" ^ n
  | Wizard n -> ip ^ " W:" ^ n

let ip_of_key k =
  match String.index_opt k ' ' with Some i -> String.sub k 0 i | None -> k

let remove_ip ip m = W.filter (fun k _ -> ip_of_key k <> ip) m

module S = Set.Make (String)

let nb_ips m =
  W.fold (fun k _ s -> S.add (ip_of_key k) s) m S.empty |> S.cardinal

type who = {
  acc_times : float list;
  oldest_time : float;
  nb_connect : int;
  nbase : string;
  utype : norfriwiz;
}

type excl = {
  mutable excl : (string * int ref) list;
  mutable who : who W.t;
  mutable max_conn : int * string;
  mutable last_summary : float;
}

let empty () =
  { excl = []; who = W.empty; max_conn = (0, ""); last_summary = 0.0 }

let output_excl oc xcl =
  output_string oc magic_robot;
  output_value oc (xcl : excl)
