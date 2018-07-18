(* camlp5r ../../src/pa_lock.cmo *)
(* $Id: gw_fix_base.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Def;
open Gwdb;
open Printf;


value designation base ip p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then
    "i=" ^ string_of_int (Adef.int_of_iper ip)
  else
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)
;

value kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in
  patch_ascend base ip a
;

value check_cpl base nb_ind nb_fam =
  if nb_fam > 0 then
    (*
    let res = ref [] in
    for i = 0 to nb_ind - 1 do {
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      match get_parents p with
      [ Some ifam ->
          if ifam = Adef.ifam_of_int 0 then res.val := [ip :: res.val]
          else ()
      | None -> () ]
    };
    *)
    let ifam = Adef.ifam_of_int 0 in
    let fam = foi base ifam in
    if not (is_deleted_family fam) then
      let neg_cpl =
        List.exists
          (fun ip -> ip = Adef.iper_of_int (-1))
          (Array.to_list (get_parent_array fam))
      in
      if neg_cpl then do {
        Array.iter (kill_parents base) (get_children fam);
        delete_family base ifam;
        Gwdb.commit_patches base;
        }
      else ()
    else ()
  else ()
;

value check bname = do {
  let base = Gwdb.open_base bname in
  (*
  let nb_fam = nb_of_families base in
  if nb_fam > 0 then
    let ifam = Adef.ifam_of_int 0 in
    let fam = foi base ifam in
    if not (is_deleted_family fam) then
      let neg_cpl =
        List.exists
          (fun ip -> ip = Adef.iper_of_int (-1))
          (Array.to_list (get_parent_array fam))
      in
      if neg_cpl then do {
        eprintf "%s\n" bname;
        flush stderr;
      }
      else ()
    else ()
  else ()
  *)
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in
  check_cpl base nb_ind nb_fam;
};


(**/**)

value bname = ref "";

value speclist = [] ;
value anonfun i = bname.val := i;
value usage = "Usage: " ^ Sys.argv.(0) ^ " base";

value main () = do {
  Arg.parse speclist anonfun usage;
  if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
  lock Mutil.lock_file bname.val with
  [ Accept -> check bname.val
  | Refuse -> do {
      eprintf "Cannot lock database. Try again.\n";
      flush stderr;
    } ]
};

main ();
