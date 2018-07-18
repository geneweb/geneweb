(* camlp5r ./pa_html.cmo *)
(* $Id: changeChildren.ml,v 5.22 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Util;

value print_child_person conf base p =
  let var = "c" ^ string_of_int (Adef.int_of_iper (get_key_index p)) in
  let first_name =
    match p_getenv conf.env (var ^ "_first_name") with
    [ Some v -> v
    | None -> p_first_name base p ]
  in
  let surname =
    match p_getenv conf.env (var ^ "_surname") with
    [ Some v -> v
    | None -> p_surname base p ]
  in
  let occ =
    match p_getint conf.env (var ^ "_occ") with
    [ Some i -> i
    | None -> get_occ p ]
  in
  tag "table" "border=\"1\"" begin
    tag "tr" "align=\"%s\"" conf.left begin
      tag "td" begin
        Wserver.printf "%s"
          (capitale (transl_nth conf "first name/first names" 0));
      end;
      tag "td" "colspan=\"3\"" begin
        xtag "input"
          "name=\"%s_first_name\" class=\"form-control\" size=\"23\" maxlength=\"200\" value=\"%s\""
          var (quote_escaped first_name);
      end;
      tag "td" "align=\"%s\"" conf.right begin
        let s = capitale (transl conf "number") in Wserver.printf "%s" s;
      end;
      tag "td" begin
        xtag "input" "class=\"form-control\" name=\"%s_occ\" size=\"5\" maxlength=\"8\"%s" var
          (if occ = 0 then "" else " value=\"" ^ string_of_int occ ^ "\"");
      end;
    end;
    tag "tr" "align=\"%s\"" conf.left begin
      tag "td" begin
        Wserver.printf "%s" (capitale (transl_nth conf "surname/surnames" 0));
      end;
      tag "td" "colspan=\"5\"" begin
        xtag "input"
          "name=\"%s_surname\" class=\"form-control\" size=\"40\" maxlength=\"200\" value=\"%s\"" var
          surname;
      end;
    end;
  end
;

value select_children_of base u =
  List.fold_right
    (fun ifam ipl ->
       let des = foi base ifam in
       List.fold_right (fun ip ipl -> [ip :: ipl])
         (Array.to_list (get_children des)) ipl)
    (Array.to_list (get_family u)) []
;

value digest_children base ipl =
  let l =
    List.map
      (fun ip ->
         let p = poi base ip in
         (sou base (get_first_name p), sou base (get_surname p), get_occ p))
      ipl
  in
  Iovalue.digest l
;

value check_digest conf base digest =
  match p_getenv conf.env "digest" with
  [ Some ini_digest ->
      if digest <> ini_digest then Update.error_digest conf else ()
  | None -> () ]
;

value print_children conf base ipl =
  do {
    stagn "h4" begin
      Wserver.printf "%s" (capitale (transl_nth conf "child/children" 1));
    end;
    tag "ul" begin
      List.iter
        (fun ip ->
           let p = poi base ip in
           tag "li" begin
             Wserver.printf "%s"
               (reference conf base p (person_text conf base p));
             Wserver.printf "%s\n" (Date.short_dates_text conf base p);
             print_child_person conf base p;
           end)
        ipl;
    end;
  }
;

value print_change conf base p =
  let title _ =
    let s = transl conf "change children's names" in
    Wserver.printf "%s" (capitale s)
  in
  let children = select_children_of base p in
  let digest = digest_children base children in
  do {
    Perso.interp_notempl_with_menu title "perso_header" conf base p;
    tag "h2" begin title False; end;
    tag "p" begin
      Wserver.printf "%s" (reference conf base p (person_text conf base p));
      Wserver.printf "%s\n" (Date.short_dates_text conf base p);
    end;
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"ip\" value=\"%d\""
          (Adef.int_of_iper (get_key_index p));
        xtag "input" "type=\"hidden\" name=\"digest\" value=\"%s\"" digest;
        xtag "input" "type=\"hidden\" name=\"m\" value=\"CHG_CHN_OK\"";
      end;
      print_children conf base children;
      Wserver.printf "\n";
      tag "button" "type=\"submit\" class=\"btn btn-secondary btn-lg\"" begin 
        Wserver.printf "%s" (capitale (transl_nth conf "validate/delete" 0));
      end;
    end;
    Wserver.printf "\n";
    trailer conf;
  }
;

value print conf base =
  match p_getint conf.env "ip" with
  [ Some i ->
      let p = poi base (Adef.iper_of_int i) in
      print_change conf base p
  | _ -> incorrect_request conf ]
;

value print_children_list conf base u =
  do {
    stag "h4" begin
      Wserver.printf "%s" (capitale (transl_nth conf "child/children" 1));
    end;
    Wserver.printf "\n<p>\n";
    tag "ul" begin
      Array.iter
        (fun ifam ->
           let des = foi base ifam in
           Array.iter
             (fun ip ->
                let p = poi base ip in
                do {
                  html_li conf;
                  Wserver.printf "\n%s"
                    (reference conf base p (person_text conf base p));
                  Wserver.printf "%s\n" (Date.short_dates_text conf base p);
                })
             (get_children des))
        (get_family u);
    end;
  }
;

value print_change_done conf base p =
  let title _ =
    let s = transl conf "children's names changed" in
    Wserver.printf "%s" (capitale s)
  in
  do {
    header conf title;
    Wserver.printf "\n%s" (reference conf base p (person_text conf base p));
    Wserver.printf "%s\n" (Date.short_dates_text conf base p);
    print_children_list conf base p;
    trailer conf;
  }
;

value print_conflict conf base ip_var p =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Update.print_error conf base (AlreadyDefined p);
    let free_n =
      Gutil.find_free_occ base (p_first_name base p) (p_surname base p) 0
    in
    tag "ul" begin
      stag "li" begin
        Wserver.printf "%s%s %d.\n" (capitale (transl conf "first free number"))
          (Util.transl conf ":") free_n;
        Wserver.printf (fcapitale (ftransl conf "click on \"%s\""))
          (transl conf "create");
        Wserver.printf "%s.\n" (transl conf " to try again with this number");
      end;
      stag "li" begin
        Wserver.printf "%s " (capitale (transl conf "or"));
        Wserver.printf (ftransl conf "click on \"%s\"") (transl conf "back");
        Wserver.printf " %s %s." (transl_nth conf "and" 0)
          (transl conf "change it (the number) yourself");
      end;
    end;
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      List.iter
        (fun (x, v) ->
          xtag "input" "type=\"hidden\" name=\"%s\" value=\"%s\"" x
            (quote_escaped (decode_varenv v)))
        (conf.henv @ conf.env);
      let var = "c" ^ string_of_int (Adef.int_of_iper ip_var) in
      xtag "input" "type=\"hidden\" name=\"field\" value=\"%s\"" var;
      xtag "input" "type=\"hidden\" name=\"free_occ\" value=\"%d\"" free_n;
      tag "button" "type=\"submit\" name=\"create\" class=\"btn btn-secondary btn-lg\"" begin 
        Wserver.printf "%s" (capitale (transl conf "create"));
      end;
      tag "button" "type=\"submit\" name=\"return\" class=\"btn btn-secondary btn-lg\"" begin 
        Wserver.printf "%s" (capitale (transl conf "back"));
      end;
    end;
    Update.print_same_name conf base p;
    trailer conf;
  }
;

value check_conflict conf base p key new_occ ipl =
  let name = Name.lower key in
  List.iter
    (fun ip ->
       let p1 = poi base ip in
       if get_key_index p1 <> get_key_index p &&
          Name.lower (p_first_name base p1 ^ " " ^ p_surname base p1) =
            name &&
          get_occ p1 = new_occ then
          do {
         print_conflict conf base (get_key_index p) p1; raise Update.ModErr
       }
       else ())
    ipl
;

value error_person conf base p err =
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  do {
    rheader conf title;
    Wserver.printf "%s\n" (capitale err);
    trailer conf;
    raise Update.ModErr
  }
;

value rename_image_file conf base p (nfn, nsn, noc) =
  match auto_image_file conf base p with
  [ Some old_f ->
      let s = default_image_name_of_key nfn nsn noc in
      let f = Filename.concat (base_path ["images"] conf.bname) s in
      let new_f =
        if Filename.check_suffix old_f ".gif" then f ^ ".gif" else f ^ ".jpg"
      in
      try Sys.rename old_f new_f with [ Sys_error _ -> () ]
  | _ -> () ]
;

value change_child conf base parent_surname changed ip =
  let p = poi base ip in
  let var = "c" ^ string_of_int (Adef.int_of_iper (get_key_index p)) in
  let new_first_name =
    match p_getenv conf.env (var ^ "_first_name") with
    [ Some x -> only_printable x
    | _ -> p_first_name base p ]
  in
  let new_surname =
    match p_getenv conf.env (var ^ "_surname") with
    [ Some x ->
        let x = only_printable x in if x = "" then parent_surname else x
    | _ -> p_surname base p ]
  in
  let new_occ =
    match p_getint conf.env (var ^ "_occ") with
    [ Some x -> x
    | _ -> 0 ]
  in
  if new_first_name = "" then
    error_person conf base p (transl conf "first name missing")
  else if
    new_first_name <> p_first_name base p ||
    new_surname <> p_surname base p || new_occ <> get_occ p
  then do {
    let key = new_first_name ^ " " ^ new_surname in
    let ipl = person_ht_find_all base key in
    check_conflict conf base p key new_occ ipl;
    rename_image_file conf base p (new_first_name, new_surname, new_occ);
    (* On ajoute les enfants dans le type Change_children_name       *)
    (* pour la future mise à jour de l'historique et du fichier gwf. *)
    changed.val :=
      [((p_first_name base p, p_surname base p, get_occ p, ip),
        (new_first_name, new_surname, new_occ, ip)) :: changed.val];
    let p =
      {(gen_person_of_person p) with
       first_name = Gwdb.insert_string base new_first_name;
       surname = Gwdb.insert_string base new_surname;
       occ = new_occ}
    in
    patch_person base ip p;
    patch_key base ip new_first_name new_surname new_occ;
    person_ht_add base key ip;
    let np_misc_names = gen_person_misc_names base p (fun p -> p.titles) in
    List.iter (fun key -> person_ht_add base key p.key_index)
      np_misc_names;
  }
  else ()
;

value print_update_child conf base p digest =
  match p_getenv conf.env "m" with
  [ Some "CHG_CHN_OK" -> print conf base
  | _ -> incorrect_request conf ]
;

value print_change_ok conf base p =
  try
    let ipl = select_children_of base p in
    let parent_surname = p_surname base p in
    let changed = ref [] in
    let redisp =
      match p_getenv conf.env "return" with
      [ Some _ -> True
      | _ -> False ]
    in
    if redisp then print_update_child conf base p ""
    else do {
      check_digest conf base (digest_children base ipl);
      List.iter (change_child conf base parent_surname changed) ipl;
      Util.commit_patches conf base;
      let changed =
        U_Change_children_name
          (Util.string_gen_person base (gen_person_of_person p))
          changed.val
      in
      History.record conf base changed "cn";
      print_change_done conf base p;
    }
  with
  [ Update.ModErr -> () ]
;

value print_ok o_conf base =
  let conf = Update.update_conf o_conf in
  match p_getint conf.env "ip" with
  [ Some i ->
      let p = poi base (Adef.iper_of_int i) in
      print_change_ok conf base p
  | _ -> incorrect_request conf ]
;
