(* On-disk state of the robot detector (file cnt/robot).

   This file is written by gwd and read or rewritten by gwrobot with
   [output_value]/[input_value], which perform no type check: both programs
   must use these very definitions. Change [magic_robot] whenever a type
   below changes. *)

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

(* A function, not a constant: the record is mutable. *)
let empty () =
  { excl = []; who = W.empty; max_conn = (0, ""); last_summary = 0.0 }

let output_excl oc xcl =
  output_string oc magic_robot;
  output_value oc (xcl : excl)
