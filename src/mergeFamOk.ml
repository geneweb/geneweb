(* camlp4r ./pa_html.cmo *)
(* $Id: mergeFamOk.ml,v 3.3 2000-05-14 19:59:36 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Config;
open Def;
open Util;
open Gutil;

value cat_strings base is1 sep is2 =
  let n1 = sou base is1 in
  let n2 = sou base is2 in
  if n1 = "" then n2
  else if n2 = "" then n1
  else n1 ^ sep ^ n2
;

value reconstitute conf base fam1 des1 fam2 des2 =
  let field name proj null =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    match p_getenv conf.env name with
    [ Some "1" -> x1
    | Some "2" -> x2
    | _ -> if null x1 then x2 else x1 ]
  in
  let fam =
    {marriage = field "marriage" (fun f -> f.marriage) (\= Adef.codate_None);
     marriage_place =
       field "marriage_place" (fun f -> sou base f.marriage_place) (\= "");
     marriage_src = cat_strings base fam1.marriage_src ", " fam2.marriage_src;
     witnesses = [| |];
     relation = field "relation" (fun f -> f.relation) (\= Married);
     divorce = field "divorce" (fun f -> f.divorce) (\= NotDivorced);
     comment = sou base fam1.comment;
     origin_file = sou base fam1.origin_file;
     fsources = cat_strings base fam1.fsources ", " fam2.fsources;
     fam_index = fam1.fam_index}
  in
  let des =
    {children =
       Array.map (UpdateFam.person_key base)
         (Array.append des1.children des2.children)}
  in
  (fam, des)
;

value print_merge1 conf base fam des fam2 digest =
  let title _ =
    let s = transl_nth conf "family/families" 1 in
    Wserver.wprint "%s # %d"
      (capitale (transl_decline conf "merge" s))
      (Adef.int_of_ifam fam.fam_index)
  in
  let cpl =
    Gutil.map_couple_p (UpdateFam.person_key base) (coi base fam.fam_index)
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=MRG_MOD_FAM_OK>\n";
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_ifam fam.fam_index);
       Wserver.wprint "<input type=hidden name=i2 value=%d>\n"
         (Adef.int_of_ifam fam2.fam_index);
       match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
       [ (Some i1, Some i2) ->
           do Wserver.wprint "<input type=hidden name=ini1 value=%d>\n" i1;
              Wserver.wprint "<input type=hidden name=ini2 value=%d>\n" i2;
           return ()
       | _ -> () ];
       match p_getenv conf.env "ip" with
       [ Some ip -> Wserver.wprint "<input type=hidden name=ip value=%s>\n" ip
       | None -> () ];
       Wserver.wprint "\n";
       UpdateFam.print_family conf base fam cpl des False;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_merge conf base =
  match (p_getint conf.env "f1", p_getint conf.env "f2") with
  [ (Some f1, Some f2) ->
      let fam1 = base.data.families.get f1 in
      let des1 = base.data.descends.get f1 in
      let fam2 = base.data.families.get f2 in
      let des2 = base.data.descends.get f2 in
      let (sfam, sdes) = reconstitute conf base fam1 des1 fam2 des2 in
      let digest = Update.digest_family fam1 (base.data.couples.get f1) des1 in
      print_merge1 conf base sfam sdes fam2 digest
  | _ -> incorrect_request conf ]
;

value print_mod_merge_ok conf base wl cpl des =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "merge done"))
  in
  do header conf title;
     print_link_to_welcome conf True;
     UpdateFamOk.print_family conf base wl cpl des;
     match (p_getint conf.env "ini1", p_getint conf.env "ini2") with
     [ (Some ini1, Some ini2) ->
         let p1 = base.data.persons.get ini1 in
         let p2 = base.data.persons.get ini2 in
         do Wserver.wprint "\n";
            html_p conf;
            stag "a" "href=%sm=MRG_IND;i=%d;i2=%d" (commd conf) ini1 ini2
            begin
              Wserver.wprint "%s" (capitale (transl conf "continue merging"));
            end;
            Wserver.wprint "\n";
            Merge.print_someone conf base p1;
            Wserver.wprint "\n%s\n" (transl conf "and");
            Merge.print_someone conf base p2;
            Wserver.wprint "\n";
         return ()
     | _ -> () ];
     trailer conf;
  return ()
;

value effective_mod_merge conf base sfam scpl sdes =
  match p_getint conf.env "i2" with
  [ Some i2 ->
      let fam2 = base.data.families.get i2 in
      do UpdateFamOk.effective_del conf base fam2; return
      let (fam, cpl, des) =
        UpdateFamOk.effective_mod conf base sfam scpl sdes
      in
      let wl = UpdateFamOk.all_checks_family conf base fam cpl des in
      let (fn, sn, occ, _) =
        match p_getint conf.env "ip" with
        [ Some i when Adef.int_of_iper cpl.mother = i -> scpl.mother
        | _ -> scpl.father ]
      in
      do base.func.commit_patches ();
         History.record conf base (fn, sn, occ) "ff";
         print_mod_merge_ok conf base wl cpl des;
      return ()
  | None -> incorrect_request conf ]
;

value print_mod_merge conf base =
  UpdateFamOk.print_mod_aux conf base (effective_mod_merge conf base)
;
