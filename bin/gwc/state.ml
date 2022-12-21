type t = {
  just_comp : bool;
  out_file : string;
  force : bool;
  separate : bool;
  (* TODO use type for bnotes *)
  bnotes : string;
  shift : int;
  no_fail : bool;
      (** Do not raise exception if syntax error occured.
    Instead print error information on stdout *)
  no_picture : bool;  (** Save path to the images *)
  mutable create_all_keys : bool;
      (** Forces to create all the keys for every persons (even for ? ?).
    Enabled for gwplus format. *)
  mutable files : (string * bool * string * int) list;
  mutable line_cnt : int;  (** Line counter while reading .gw file *)
  default_source : string;
      (** Default source field for persons and families without source data *)
  do_check : bool;  (** Base consistency check *)
  do_consang : bool;  (** Compute consanguinity *)
  pr_stats : bool;  (** Print base's statistics *)
  particules_file : string;  (** File containing the particles to use *)
}

(** Default state *)
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
    create_all_keys = false;
    line_cnt = 0;
    default_source = "";
    do_check = true;
    do_consang = false;
    pr_stats = false;
    particules_file = "";
  }
