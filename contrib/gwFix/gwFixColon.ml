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

value check_name base nb_ind fix = do {
  printf "Check colon\n";
  flush stdout;
  for i = 0 to nb_ind - 1 do {
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    if String.contains fn ':' || String.contains sn ':' then do {
      printf "*** bad name : %s %s (%d) => %s\n" fn sn i (designation base ip (poi base ip));
      flush stdout;
    }
    else ();
  };
};


value check bname = do {
  let base = Gwdb.open_base bname in
  let fix = ref False in
  let nb_ind = nb_of_persons base in
  check_name base nb_ind fix;
  if fix.val then Gwdb.commit_patches base
  else do {
    printf "No change\n";
    flush stdout;
  }
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
