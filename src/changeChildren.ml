(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: changeChildren.ml,v 2.6 1999-07-22 14:34:02 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Config;
open Util;

value print_child_person conf base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let occ = p.occ in
  let var = "c" ^ string_of_int (Adef.int_of_iper p.cle_index) in
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "first name/first names" 0));
      end;
      tag "td" "colspan=3" begin
        Wserver.wprint "<input name=%s_first_name size=23 maxlength=200" var;
        Wserver.wprint " value=\"%s\">"
          (quote_escaped first_name);
      end;
      tag "td" "align=right" begin
        let s = capitale (transl conf "number") in
        Wserver.wprint "%s" s;
      end;
      tag "td" begin
        Wserver.wprint "<input name=%s_occ size=5 maxlength=8%s>" var
          (if occ == 0 then "" else " value=" ^ string_of_int occ);
      end;
    end;
    Wserver.wprint "\n";
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "surname/surnames" 0));
      end;
      tag "td" "colspan=5" begin
        Wserver.wprint
          "<input name=%s_surname size=40 maxlength=200 value=\"%s\">"
          var surname;
      end;
    end;
    Wserver.wprint "\n";
  end
;

value select_children_of base p =
  List.fold_right
    (fun ifam ipl ->
       let fam = foi base ifam in
       List.fold_right (fun ip ipl -> [ip :: ipl])
         (Array.to_list fam.children) ipl)
    (Array.to_list p.family) []
;

value digest_children base ipl =
  let l =
    List.map
      (fun ip ->
         let p = poi base ip in
         (p.first_name, p.surname, p.occ))
      ipl
  in
  Iovalue.digest l
;

value check_digest conf base digest =
  match p_getenv conf.env "digest" with
  [ Some ini_digest ->
      if digest <> ini_digest then Update.error_digest conf base
      else ()
  | None -> () ]
;

value print_children conf base ipl =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "child/children" 1));
     end;
     Wserver.wprint "\n<p>\n";
     tag "ul" begin
       List.iter
         (fun ip ->
            let p = poi base ip in
            do html_li conf;
               Wserver.wprint "\n%s"
                 (reference conf base p (person_text conf base p));
               Wserver.wprint "%s\n" (Date.short_dates_text conf base p);
               print_child_person conf base p;
            return ())
         ipl;
     end;
  return ()
;

value print_change conf base p =
  let title _ =
    let s = transl conf "change children's names" in
    Wserver.wprint "%s" (capitale s)
  in
  let children = select_children_of base p in
  let digest = digest_children base children in
  do header conf title;
     Wserver.wprint "%s" (reference conf base p (person_text conf base p));
     Wserver.wprint "%s\n" (Date.short_dates_text conf base p);
     Wserver.wprint "<p>\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       Wserver.wprint "\n";
       Wserver.wprint "<input type=hidden name=m value=CHG_CHN_OK>\n";
       print_children conf base children;
       Wserver.wprint "\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = poi base (Adef.iper_of_int i) in
      print_change conf base p
  | _ -> incorrect_request conf ]
;

value print_children_list conf base p =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "child/children" 1));
     end;
     Wserver.wprint "\n<p>\n";
     tag "ul" begin
       Array.iter
         (fun ifam ->
            let fam = foi base ifam in
            Array.iter
              (fun ip ->
                 let p = poi base ip in
                 do html_li conf;
                    Wserver.wprint "\n%s"
                      (reference conf base p (person_text conf base p));
                    Wserver.wprint "%s\n" (Date.short_dates_text conf base p);
                 return ())
              fam.children)
         p.family;
     end;
  return ()
;

value print_change_done conf base p =
  let title _ =
    let s = transl conf "children's names changed" in
    Wserver.wprint "%s" (capitale s)
  in
  do header conf title;
     Wserver.wprint "\n%s"
       (reference conf base p (person_text conf base p));
     Wserver.wprint "%s\n" (Date.short_dates_text conf base p);
     print_children_list conf base p;
     trailer conf;
  return ()
;

value print_conflict conf base p =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Update.print_error conf base (AlreadyDefined p);
     html_p conf;
     Wserver.wprint "<ul>\n";
     html_li conf;
     Wserver.wprint "%s: %d\n"
       (capitale (transl conf "first free number"))
       (Update.find_free_occ base (p_first_name base p) (p_surname base p)
          0);
     Wserver.wprint "</ul>\n";
     Update.print_same_name conf base p;
     trailer conf;
  return ()
;

value check_conflict conf base p key new_occ ipl =
  let name = Name.strip_lower key in
  List.iter
    (fun ip ->
       let p1 = poi base ip in
       if p1.cle_index <> p.cle_index
       && Name.strip_lower (p_first_name base p1 ^ " " ^ p_surname base p1)
          = name
       && p1.occ = new_occ then
         do print_conflict conf base p1; return raise Update.ModErr
       else ())
    ipl
;

value error_person conf base p err =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint "%s\n" (capitale err);
     trailer conf;
  return raise Update.ModErr
;

value rename_image_file conf base p (nfn, nsn, noc) =
  match auto_image_file conf base p with
  [ Some old_f ->
      let s = default_image_name_of_key nfn nsn noc in
      let f =
        List.fold_right Filename.concat [base_dir.val; "images"; conf.bname] s
      in
      let new_f =
        if Filename.check_suffix old_f ".gif" then f ^ ".gif"
        else f ^ ".jpg"
      in
      try Sys.rename old_f new_f with [ Sys_error _ -> () ]
  | _ -> () ]
;

value change_child conf base parent_surname ip =
  let p = poi base ip in
  let var = "c" ^ string_of_int (Adef.int_of_iper p.cle_index) in
  let new_first_name =
    match p_getenv conf.env (var ^ "_first_name") with
    [ Some x -> only_printable x
    | _ -> p_first_name base p ]
  in
  let new_surname =
    match p_getenv conf.env (var ^ "_surname") with
    [ Some x ->
        let x = only_printable x in
        if x = "" then parent_surname else x
    | _ -> p_surname base p ]
  in
  let new_occ =
    match p_getint conf.env (var ^ "_occ") with
    [ Some x -> x
    | _ -> 0 ]
  in
  if new_first_name = "" then
    error_person conf base p (transl conf "first name missing")
  else if new_first_name <> p_first_name base p
  || new_surname <> p_surname base p
  || new_occ <> p.occ then
    let key = new_first_name ^ " " ^ new_surname in
    let ipl = person_ht_find_all base key in
    do check_conflict conf base p key new_occ ipl;
       rename_image_file conf base p (new_first_name, new_surname, new_occ);
       p.first_name := Update.insert_string conf base new_first_name;
       p.surname := Update.insert_string conf base new_surname;
       p.occ := new_occ;
       base.func.patch_person p.cle_index p;
       person_ht_add base key p.cle_index;
       let np_misc_names = person_misc_names base p in
       List.iter (fun key -> person_ht_add base key p.cle_index)
         np_misc_names;
    return ()
  else ()
;

value print_change_ok conf base p =
  let bfile = Filename.concat Util.base_dir.val conf.bname in
  lock (Iobase.lock_file bfile) with
  [ Accept ->
      try
        let ipl = select_children_of base p in
        let parent_surname = p_surname base p in
        do check_digest conf base (digest_children base ipl);
           List.iter (change_child conf base parent_surname) ipl;
           base.func.commit_patches ();
           print_change_done conf base p;
        return ()      
      with
      [ Update.ModErr -> () ]
  | Refuse -> Update.error_locked conf base ]
;

value print_ok conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = poi base (Adef.iper_of_int i) in
      print_change_ok conf base p
  | _ -> incorrect_request conf ]
;
