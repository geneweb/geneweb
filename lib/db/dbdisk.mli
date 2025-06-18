type dsk_person = (int, int, int) Def.gen_person
(** Extended person's entry in the base *)

type dsk_ascend = int Def.gen_ascend
(** Person's ascendants entry in the base *)

type dsk_union = int Def.gen_union
(** Person's union entry in the base *)

type dsk_family = (int, int, int) Def.gen_family
(** Family's entry in the base *)

type dsk_couple = int Def.gen_couple
(** Family's couple entry in the base *)

type dsk_descend = int Def.gen_descend
(** Family's descendants entry in the base *)

type dsk_title = int Def.gen_title
(** Nobility title in the base *)

type 'a record_access = {
  (* Load array in the memory and cache it so it could be accessed
     instantly by other functions unless [clear_array] is called. *)
  load_array : unit -> unit;
  (* Get the nth element of array. In details, it searches for an element in
     the following order:
     - Search inside the pending patches
     - Search inside the commited patches
     - Search insede the loaded in memory array
     - Search inside the "base" file *)
  get : int -> 'a;
  (* Same as [get] but doesn't consider pending patches *)
  get_nopending : int -> 'a;
  (* Return length of an array that by default takes into account
     commited patches *)
  mutable len : int;
  (* Output array with applied commited patches to the giving chanel *)
  output_array : out_channel -> unit;
  (* Remove array from the memory *)
  clear_array : unit -> unit;
}
(** Type that define the functions to use to access and manipulate with database
    arrays. *)

type string_person_index = {
  (* Find all person's ids that has giving surname/first name. *)
  find : int -> int list;
  (* Return surname's/first name's id. If it doen't present return id of the next
     name by alphabetical order *)
  cursor : string -> int;
  (* Return surname's/first name's id. If it doen't present return id of the next
     name by alphabetical order *)
  next : int -> int;
}
(** Data structure for optimised search throughout index by name (surname or
    first name). Considers also patched persons. *)

type visible_record_access = {
  v_write : unit -> unit;
  v_get : (dsk_person -> bool) -> int -> bool;
}

type perm = RDONLY | RDRW

type base_data = {
  (* Array of persons *)
  persons : dsk_person record_access;
  (* Array of persons' ascendants *)
  ascends : dsk_ascend record_access;
  (* Array of persons' unions *)
  unions : dsk_union record_access;
  (* unused by default *)
  visible : visible_record_access;
  (* Array of families *)
  families : dsk_family record_access;
  (* Array of families' couples *)
  couples : dsk_couple record_access;
  (* Array of families' descendants *)
  descends : dsk_descend record_access;
  (* Array of strings *)
  strings : string record_access;
  (* Array of autorised to use surname's particles *)
  particles_txt : string list;
  (* Regular expression that matches particles in [particles_txt]  *)
  particles : Re.re Lazy.t;
  (* Data base notes and extended page structure *)
  bnotes : Def.base_notes;
  (* Directory where database's files are stored *)
  bdir : string;
  perm : perm;
}
(** Data part of database *)

type base_func = {
  (* Return person's id from the giving key (first name, surname and occurene number).
     If person doesn't exists return None. Doesn't consider pending patches *)
  person_of_key : string -> string -> int -> int option;
  (* Return list of person ids that have giving name
     (could be one of the mix). Doesn't consider pending patches *)
  persons_of_name : string -> int list;
  (* Return list of surnames (string ids) that contain giving person's surname or surname substring.
     Consider also surnames of pathed persons. Doesn't consider pending patches *)
  strings_of_sname : string -> int list;
  (* Return list of first names (string ids) that contain giving person's first name or first name's
     substring. Consider also first names of pathed persons. Doesn't consider pending patches *)
  strings_of_fname : string -> int list;
  (* Search functionalities throughout index by surname *)
  persons_of_surname : string_person_index;
  (* Search functionalities throughout index by first name *)
  persons_of_first_name : string_person_index;
  (* Insert or modify person with a giving id (add to pending patches). *)
  patch_person : int -> dsk_person -> unit;
  (* Insert or modify ascendants of a person with a giving id (add to pending patches). *)
  patch_ascend : int -> dsk_ascend -> unit;
  (* Insert or modify union of a person with a giving id (add to pending patches). *)
  patch_union : int -> dsk_union -> unit;
  (* Insert or modify family with a giving id (add to pending patches). *)
  patch_family : int -> dsk_family -> unit;
  (* Insert or modify couple of a family with a giving id (add to pending patches). *)
  patch_couple : int -> dsk_couple -> unit;
  (* Insert or modify descendants of a family with a giving id (add to pending patches). *)
  patch_descend : int -> dsk_descend -> unit;
  (* Associate person to [name] inside the index.
     Added directly inside commited patches. *)
  patch_name : string -> int -> unit;
  (* Insert new string inside the pending patches and returns its id.
     If string already exists return its id. *)
  insert_string : string -> int;
  (* Commit pending patches and write a patches' new state inside "patches"
     file. "nb_persons" is also updated. *)
  commit_patches : unit -> unit;
  (* Update content (second arg) of the notes' file (first arg) if exists. *)
  commit_notes : string -> string -> unit;
  (* Update content (second arg) of the notes' file (first arg) if exists. *)
  commit_wiznotes : string -> string -> unit;
  (* Close every opened channel. *)
  nb_of_real_persons : unit -> int;
  (* Tells if person with giving id exists in the base.
     Pending patches are also considered. *)
  iper_exists : int -> bool;
  (* Tells if family with giving id exists in the base.
     Pending patches are also considered. *)
  ifam_exists : int -> bool;
}
(** Functionality part of database. Every modification of the base is stored in
    {i patches} file. Note that, every modification firstly is pendent and
    should be commited to apply them and to update {i patches} file with
    [commit_patches]. *)

(** Geneweb database version *)
type base_version = GnWb0020 | GnWb0021 | GnWb0022 | GnWb0023 | GnWb0024

type dsk_base = { data : base_data; func : base_func; version : base_version }
(** Database representation: data and basic requests over this data. *)
