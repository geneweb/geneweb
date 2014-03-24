(* $Id: cleopatre.ml,v 4.10 2006-12-23 22:47:37 deraugla Exp $ *)

open Def;
open Gwdb;

value main bname =
  let base = Gwdb.open_base bname in
  let ip1 = Gwlib.add_indi base ("Ptolémée", "d'Égypte", 0) Male in
  let ip2 = Gwlib.add_indi base ("Cléopâtre", "d'Égypte", 0) Female in
  do {
    (let rec loop n ip1 ip2 =
       if n = 50 then ()
       else
         let x = string_of_int n in
         let ip3 =
           Gwlib.add_indi base ("Ptolémée " ^ x, "d'Égypte", 0) Male
         in
         let ip4 =
           Gwlib.add_indi base ("Cléopâtre " ^ x, "d'Égypte", 0) Female
         in
         let _ifam = Gwlib.add_fam base ip1 ip2 [ip3; ip4] in
         loop (n + 1) ip3 ip4
     in
     loop 2 ip1 ip2);
    commit_patches base;
  }
;

main Sys.argv.(1);

