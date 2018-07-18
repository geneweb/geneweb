(* $Id: cleopatre.ml,v 4.10 2006-12-23 22:47:37 deraugla Exp $ *)

open Def
open Gwdb

let main bname =
  let base = Gwdb.open_base bname in
  let ip1 = GwBaseLib.add_indi base ("Ptol�m�e", "d'�gypte", 0) Male in
  let ip2 = GwBaseLib.add_indi base ("Cl�op�tre", "d'�gypte", 0) Female in
  begin let rec loop n ip1 ip2 =
    if n = 50 then ()
    else
      let x = string_of_int n in
      let ip3 =
        GwBaseLib.add_indi base ("Ptol�m�e " ^ x, "d'�gypte", 0) Male
      in
      let ip4 =
        GwBaseLib.add_indi base ("Cl�op�tre " ^ x, "d'�gypte", 0) Female
      in
      let _ifam = GwBaseLib.add_fam base ip1 ip2 [ip3; ip4] in
      loop (n + 1) ip3 ip4
  in
    loop 2 ip1 ip2
  end;
  commit_patches base

let _ = main Sys.argv.(1)

