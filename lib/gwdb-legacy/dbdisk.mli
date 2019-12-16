open Def

type dsk_person = (int, int, int) gen_person
type dsk_ascend = int gen_ascend
type dsk_union = int gen_union
type dsk_family = (int, int, int) gen_family
type dsk_couple = int gen_couple
type dsk_descend = int gen_descend

type dsk_title = int gen_title

type 'a record_access =
  { load_array : unit -> unit;
    get : int -> 'a;
    set : int -> 'a -> unit;
    mutable len : int;
    output_array : out_channel -> unit;
    clear_array : unit -> unit }

type string_person_index =
  { find : int -> int list
  ; cursor : string -> int
  ; next : int -> int
  }

type visible_record_access =
  { v_write : unit -> unit; v_get : (dsk_person -> bool) -> int -> bool }

type base_data =
  { persons : dsk_person record_access
  ; ascends : dsk_ascend record_access
  ; unions : dsk_union record_access
  ; visible : visible_record_access
  ; families : dsk_family record_access
  ; couples : dsk_couple record_access
  ; descends : dsk_descend record_access
  ; strings : string record_access
  ; particles : string list
  ; bnotes : base_notes
  ; bdir : string
  }

type base_func =
  { person_of_key : string -> string -> int -> int option
  ; persons_of_name : string -> int list
  ; strings_of_fsname : string -> int list
  ; persons_of_surname : string_person_index
  ; persons_of_first_name : string_person_index
  ; patch_person : int -> dsk_person -> unit
  ; patch_ascend : int -> dsk_ascend -> unit
  ; patch_union : int -> dsk_union -> unit
  ; patch_family : int -> dsk_family -> unit
  ; patch_couple : int -> dsk_couple -> unit
  ; patch_descend : int -> dsk_descend -> unit
  ; patch_name : string -> int -> unit
  ; insert_string : string -> int
  ; commit_patches : unit -> unit
  ; commit_notes : string -> string -> unit
  ; cleanup : unit -> unit
  ; nb_of_real_persons : unit -> int
  }

type dsk_base = { data : base_data; func : base_func }
