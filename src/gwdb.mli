(* $Id: gwdb.mli,v 5.1 2006-09-15 11:45:37 ddr Exp $ *)

open Def;

type person = gen_person iper istr;
type ascend = gen_ascend ifam;
type union = gen_union ifam;

type family = gen_family iper istr;
type couple = gen_couple iper;
type descend = gen_descend iper;

type relation = gen_relation iper istr;
type title = gen_title istr;

type rn_mode = [ RnAll | Rn1Ch | Rn1Ln ];

type notes =
  { nread : mutable string -> rn_mode -> string;
    norigin_file : mutable string;
    efiles  : mutable unit -> list string }
;

type cache 'a =
  { array : mutable unit -> array 'a;
    get : mutable int -> 'a;
    len : mutable int;
    clear_array : unit -> unit }
;

type istr_iper_index =
  { find : istr -> list iper;
    cursor : string -> istr;
    next : istr -> istr }
;

type visible_cache =
  { v_write : mutable unit -> unit;
    v_get : mutable (person -> bool) -> int -> bool }
;

type base_data =
  { persons : cache person;
    ascends : cache ascend;
    unions : cache union;
    visible : visible_cache;
    families : cache family;
    couples : cache couple;
    descends : cache descend;
    strings : cache string;
    particles : list string;
    bnotes : notes }
;

type base_func =
  { persons_of_name : string -> list iper;
    strings_of_fsname : string -> list istr;
    index_of_string : string -> istr;
    persons_of_surname : istr_iper_index;
    persons_of_first_name : istr_iper_index;
    patch_person : iper -> person -> unit;
    patch_ascend : iper -> ascend -> unit;
    patch_union : iper -> union -> unit;
    patch_family : ifam -> family -> unit;
    patch_couple : ifam -> couple -> unit;
    patch_descend : ifam -> descend -> unit;
    patch_string : istr -> string -> unit;
    patch_name : string -> iper -> unit;
    commit_patches : unit -> unit;
    commit_notes : string -> string -> unit;
    patched_ascends : unit -> list iper;
    is_patched_person : iper -> bool;
    cleanup : unit -> unit }
;

type base =
  { data : base_data;
    func : base_func }
;
