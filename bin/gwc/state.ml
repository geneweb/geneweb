type t = {
  just_comp : bool;
  out_file : string;
  force : bool;
  separate : bool;
  (* TODO use type for bnotes *)
  bnotes : string;
  shift : int;
  no_fail : bool;
  no_picture : bool;
  no_public : bool;
  mutable create_all_keys : bool;
  mutable files : (string * bool * string * int) list;
  mutable line_cnt : int;
  default_source : string;
  do_check : bool;
  do_consang : bool;
  pr_stats : bool;
  particules_file : string;
}

let default =
  {
    just_comp = false;
    out_file = Filename.concat Filename.current_dir_name "a";
    force = false;
    separate = false;
    bnotes = "merge";
    shift = 0;
    files = [];
    no_fail = false;
    no_picture = false;
    no_public = false;
    create_all_keys = false;
    line_cnt = 0;
    default_source = "";
    do_check = true;
    do_consang = false;
    pr_stats = false;
    particules_file = "";
  }
