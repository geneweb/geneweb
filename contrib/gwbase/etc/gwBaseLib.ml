(* $Id: gwlib.ml,v 4.21 2007-01-18 19:44:50 deraugla Exp $ *)

open Def;
open Gwdb;

value add_indi base (fn, sn, nb) sex =
  let ip = Adef.iper_of_int (nb_of_persons base) in
  let empty_string = insert_string base "" in
  let np =
    {first_name = insert_string base fn; surname = insert_string base sn;
     occ = nb; image = empty_string; public_name = empty_string;
     qualifiers = []; aliases = []; first_names_aliases = [];
     surnames_aliases = []; titles = []; rparents = []; related = [];
     occupation = empty_string; sex = sex; access = IfTitles;
     birth = Adef.codate_None; birth_place = empty_string;
     birth_src = empty_string; baptism = Adef.codate_None;
     baptism_place = empty_string; baptism_src = empty_string;
     death = NotDead; death_place = empty_string; death_src = empty_string;
     burial = UnknownBurial; burial_place = empty_string;
     burial_src = empty_string; notes = empty_string;
     psources = empty_string; key_index = ip}
  in
  let na = {parents = None; consang = Adef.fix (-1)} in
  let nu = {family = [| |]} in
  do {
    patch_person base ip np;
    patch_ascend base ip na;
    patch_union base ip nu;
    ip
  }
;

value add_fam base fath moth children =
  let ifam = Adef.ifam_of_int (nb_of_families base) in
  let empty_string = insert_string base "" in
  let fam =
    {marriage = Adef.codate_None; marriage_place = empty_string;
     marriage_src = empty_string; witnesses = [| |]; relation = Married;
     divorce = NotDivorced; comment = empty_string;
     origin_file = empty_string; fsources = empty_string; fam_index = ifam}
  in
  let cpl = Adef.couple fath moth in
  let des = {children = Array.of_list children}
  in
  let ufath = poi base fath in
  let umoth = poi base moth in
  do {
    patch_family base ifam fam;
    patch_couple base ifam cpl;
    patch_descend base ifam des;
    let ufath = {family = Array.append (get_family ufath) [| ifam |]} in
    patch_union base fath ufath;
    let umoth = {family = Array.append (get_family umoth) [| ifam |]} in
    patch_union base moth umoth;
    List.iter
      (fun ip ->
         let a = {parents = Some ifam; consang = Adef.fix (-1)} in
         patch_ascend base ip a)
      children;
    ifam
  }
;
