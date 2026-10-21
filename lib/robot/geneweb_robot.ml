let magic_robot = "GWRB0008"

module W = Map.Make (struct
  type t = string

  let compare = compare
end)

type norfriwiz = Normal | Friend of string | Wizard of string

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
