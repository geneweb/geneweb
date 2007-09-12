(* camlp5r ./pa_html.cmo *)
(* $Id: mergeFamOk.ml,v 5.19 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Util;

value cat_strings base is1 sep is2 =
  let n1 = sou base is1 in
  let n2 = sou base is2 in
  if n1 = "" then n2 else if n2 = "" then n1 else n1 ^ sep ^ n2
;

value merge_strings base is1 sep is2 =
  if eq_istr is1 is2 then sou base is1 else cat_strings base is1 sep is2
;

value sorp base ip =
  let p = poi base ip in
  (sou base (get_first_name p), sou base (get_surname p), get_occ p,
   Update.Link, "")
;

value merge_witnesses base wit1 wit2 =
  let list =
    List.fold_right
      (fun wit list -> if List.mem wit list then list else [wit :: list])
      (List.map (sorp base) (Array.to_list wit1))
      (List.map (sorp base) (Array.to_list wit2))
  in
  Array.of_list list
;

value reconstitute conf base ifam1 fam1 fam2 =
  let field name proj null =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    match p_getenv conf.env name with
    [ Some "1" -> x1
    | Some "2" -> x2
    | _ -> if null x1 then x2 else x1 ]
  in
  let fam =
    {marriage = field "marriage" get_marriage ( \= Adef.codate_None);
     marriage_place =
       field "marriage_place" (fun f -> sou base (get_marriage_place f))
         ( \= "");
     marriage_src =
       merge_strings base (get_marriage_src fam1) ", "
         (get_marriage_src fam2);
     witnesses =
       merge_witnesses base (get_witnesses fam1) (get_witnesses fam2);
     relation = field "relation" get_relation ( \= Married);
     divorce = field "divorce" get_divorce ( \= NotDivorced);
     comment = merge_strings base (get_comment fam1) ", " (get_comment fam2);
     origin_file = sou base (get_origin_file fam1);
     fsources =
       merge_strings base (get_fsources fam1) ", " (get_fsources fam2);
     fam_index = ifam1}
  in
  let des =
    {children =
       Array.map (UpdateFam.person_key base)
         (Array.append (get_children fam1) (get_children fam2))}
  in
  (fam, des)
;

value print_merge conf base =
  match (p_getint conf.env "i", p_getint conf.env "i2") with
  [ (Some f1, Some f2) ->
      let ifam1 = Adef.ifam_of_int f1 in
      let fam1 = foi base ifam1 in
      let fam2 = foi base (Adef.ifam_of_int f2) in
      let (sfam, sdes) = reconstitute conf base ifam1 fam1 fam2 in
      let digest =
        let ini_sfam = UpdateFam.string_family_of conf base ifam1 in
        Update.digest_family ini_sfam
      in
      let scpl =
        Futil.map_couple_p conf.multi_parents (UpdateFam.person_key base)
          (gen_couple_of_couple (foi base sfam.fam_index))
      in
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
  | _ -> incorrect_request conf ]
;

value print_mod_merge_ok conf base wl cpl des = do {
  let title _ = Wserver.wprint "%s" (capitale (transl conf "merge done")) in
  header conf title;
   print_link_to_welcome conf True;
  UpdateFamOk.print_family conf base wl cpl des;
  Merge.print_possible_continue_merging conf base;
  trailer conf;
};

value effective_mod_merge conf base sfam scpl sdes =
  match p_getint conf.env "i2" with
  [ Some i2 ->
      let ifam2 = Adef.ifam_of_int i2 in
      let fam2 = foi base ifam2 in
      do {
        UpdateFamOk.effective_del conf base (ifam2, fam2);
        let (ifam, fam, cpl, des) =
          UpdateFamOk.effective_mod conf base sfam scpl sdes
        in
        let wl =
          UpdateFamOk.all_checks_family conf base ifam fam cpl des
            (scpl, sdes, None (* should be Some *))
        in
        let ((fn, sn, occ, _, _), ip) =
          match p_getint conf.env "ip" with
          [ Some i ->
              let ip = Adef.iper_of_int i in
              (if Adef.mother cpl = ip then mother scpl else father scpl, ip)
          | None -> (father scpl, Adef.iper_of_int (-1)) ]
        in
        Util.commit_patches conf base;
        History.record conf base (fn, sn, occ, ip) "ff";
        print_mod_merge_ok conf base wl cpl des;
      }
  | None -> incorrect_request conf ]
;

value print_mod_merge o_conf base =
  let conf = Update.update_conf o_conf in
  UpdateFamOk.print_mod_aux conf base (effective_mod_merge conf base)
;
