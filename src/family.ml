(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: family.ml,v 4.32 2003-03-19 13:00:08 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Gutil;
open Config;
open Util;

value person_is_std_key base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (p_first_name base p ^ " " ^ p_surname base p) then
    True
  else if
    List.exists (fun n -> Name.strip n = k) (person_misc_names base p) then
    True
  else False
;

value select_std_eq base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key base p k then [p :: pl] else pl) pl []
;

value very_unknown conf =
  match (p_getenv conf.env "n", p_getenv conf.env "p") with
  [ (Some nom, Some prenom) ->
      let title _ =
        Wserver.wprint "%s: \"%s %s\"" (capitale (transl conf "not found"))
          prenom nom
      in
      do {
        rheader conf title; print_link_to_welcome conf False; trailer conf;
      }
  | _ -> incorrect_request conf ]
;

value unknown conf n =
  let title _ =
    Wserver.wprint "%s: \"%s\"" (capitale (transl conf "not found")) n
  in
  do {
    rheader conf title; print_link_to_welcome conf False; trailer conf;
  }
;

value relation_print conf base p =
  let p1 =
    match p_getint conf.senv "ei" with
    [ Some i ->
        do {
          conf.senv := [];
          if i >= 0 && i < base.data.persons.len then
            Some (pget conf base (Adef.iper_of_int i))
          else None
        }
    | None ->
        match find_person_in_env conf base "1" with
        [ Some p1 -> do { conf.senv := []; Some p1 }
        | None -> None ] ]
  in
  Relation.print conf base p p1
;

value person_selected conf base p =
  match p_getenv conf.senv "em" with
  [ Some "R" -> relation_print conf base p
  | Some mode -> incorrect_request conf
  | None -> Perso.print conf base p ]
;

value compact_list conf base xl =
  let pl = sort_person_list base xl in
  let pl =
    List.fold_right
      (fun p pl ->
         match pl with
         [ [p1 :: _] when p.cle_index == p1.cle_index -> pl
         | _ -> [p :: pl] ])
      pl []
  in
  pl
;

value cut_words str =
  loop 0 0 where rec loop beg i =
    if i < String.length str then
      match str.[i] with
      [ ' ' ->
          if beg == i then loop (succ beg) (succ i)
          else [String.sub str beg (i - beg) :: loop (succ i) (succ i)]
      | _ -> loop beg (succ i) ]
    else if beg == i then []
    else [String.sub str beg (i - beg)]
;

value try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match lindex n1 ' ' with
  [ Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let (list, _) =
        Some.persons_of_fsname conf base base.func.persons_of_surname.find
          (fun x -> x.surname) sn
      in
      let pl =
        List.fold_left
          (fun pl (_, _, ipl) ->
             List.fold_left
               (fun pl ip ->
                  let p = pget conf base ip in
                  let fn1 =
                    Name.abbrev (Name.lower (sou base p.first_name))
                  in
                  if List.mem fn (cut_words fn1) then [p :: pl] else pl)
               pl ipl)
          [] list
      in
      pl
  | None -> [] ]
;

value name_with_roman_number str =
  loop False 0 0 where rec loop found len i =
    if i == String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
      [ '0'..'9' as c ->
          let (n, i) =
            loop (Char.code c - Char.code '0') (i + 1) where rec loop n i =
              if i == String.length str then (n, i)
              else
                match str.[i] with
                [ '0'..'9' as c ->
                    loop (10 * n + Char.code c - Char.code '0') (i + 1)
                | _ -> (n, i) ]
          in
          loop True (Buff.mstore len (roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1) ]
;

value find_all conf base an =
  let sosa_ref = Util.find_person_in_env conf base "z" in
  let sosa_nb = try Some (Num.of_string an) with [ Failure _ -> None ] in
  match (sosa_ref, sosa_nb) with
  [ (Some p, Some n) ->
      if n <> Num.zero then
        match Util.branch_of_sosa conf base p.cle_index n with
        [ Some [(ip, _) :: _] -> ([pget conf base ip], True)
        | _ -> ([], False) ]
      else ([], False)
  | _ ->
      let ipl = person_ht_find_all base an in
      let (an, ipl) =
        if ipl = [] then
          match name_with_roman_number an with
          [ Some an1 ->
              let ipl = person_ht_find_all base an1 in
              if ipl = [] then (an, []) else (an1, ipl)
          | None -> (an, ipl) ]
        else (an, ipl)
      in
      let pl = 
        List.fold_left
          (fun l ip ->
             let p = pget conf base ip in
             if is_hidden p then l else [p :: l])
        [] ipl
      in
      let spl = select_std_eq base pl an in
      let pl =
        if spl = [] then
          if pl = [] then try_find_with_one_first_name conf base an else pl
        else spl
      in
      let pl =
        if not conf.wizard && not conf.friend && conf.hide_names then
          List.fold_right
            (fun p pl ->
               if Util.fast_auth_age conf p then [p :: pl] else pl)
            pl []
        else pl
      in
      (compact_list conf base pl, False) ]
;

value specify conf base n pl =
  let title _ = Wserver.wprint "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
         let tl = ref [] in
         let add_tl t =
           tl.val :=
             let rec add_rec =
               fun
               [ [t1 :: tl1] ->
                   if t1.t_ident = t.t_ident && t1.t_place = t.t_place then
                     [t1 :: tl1]
                   else [t1 :: add_rec tl1]
               | [] -> [t] ]
             in
             add_rec tl.val
         in
         let compare_and_add t pn =
           let pn = sou base pn in
           if Name.crush_lower pn = n then add_tl t
           else
             match p.qualifiers with
             [ [nn :: _] ->
                 let nn = sou base nn in
                 if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t else ()
             | _ -> () ]
         in
         do {
           List.iter
             (fun t ->
                match (t.t_name, p.public_name) with
                [ (Tname s, _) -> compare_and_add t s
                | (_, pn) when sou base pn <> "" -> compare_and_add t pn
                | _ -> () ])
             p.titles;
           (p, tl.val)
         })
      pl
  in
  do {
    header conf title;
    conf.cancel_links := False;
    Wserver.wprint "<ul>\n";
    List.iter
      (fun (p, tl) ->
         do {
           html_li conf;
           match tl with
           [ [] ->
               Wserver.wprint "\n%s" (referenced_person_title_text conf base p)
           | [t :: _] ->
               do {
                 tag "a" "href=\"%s%s\"" (commd conf) (acces conf base p)
                 begin
                   Wserver.wprint "%s" (titled_person_text conf base p t);
                 end;
                 List.iter
                   (fun t ->
                      Wserver.wprint "%s" (one_title_text conf base p t))
                   tl;
               } ];
           Wserver.wprint "%s" (Date.short_dates_text conf base p);
           if authorized_age conf base p then
             match p.first_names_aliases with
             [ [] -> ()
             | fnal ->
                 do {
                   Wserver.wprint "\n<em>(";
                   Gutil.list_iter_first
                     (fun first fna ->
                        do {
                          if not first then Wserver.wprint ", " else ();
                          Wserver.wprint "%s" (sou base fna);
                        })
                     fnal;
                   Wserver.wprint ")</em>";
                 } ]
           else ();
           let spouses =
             List.fold_right
               (fun ifam spouses ->
                  let cpl = coi base ifam in
                  let spouse = pget conf base (spouse p.cle_index cpl) in
                  if p_surname base spouse <> "?" then [spouse :: spouses]
                  else spouses)
               (Array.to_list (uget conf base p.cle_index).family) []
           in
           match spouses with
           [ [] -> ()
           | [h :: hl] ->
               let s =
                 List.fold_left
                   (fun s h ->
                      s ^ ", " ^ transl_nth conf "and" 0 ^ "\n" ^
                        person_title_text conf base h)
                   (person_title_text conf base h) hl
               in
               Wserver.wprint ", <em>%s</em>\n"
                 (transl_a_of_gr_eq_gen_lev conf
                    (transl_nth conf "spouse" (index_of_sex p.sex)) s) ];
         })
      ptll;
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

(* Make the "special" environement; "em=mode;ei=n" *)

value set_senv conf vm vi =
  do {
    conf.senv := [("em", vm); ("ei", vi)];
    match p_getenv conf.env "image" with
    [ Some "on" -> conf.senv := conf.senv @ [("image", "on")]
    | _ -> () ];
    match p_getenv conf.env "long" with
    [ Some "on" -> conf.senv := conf.senv @ [("long", "on")]
    | _ -> () ];
    match p_getenv conf.env "spouse" with
    [ Some "on" -> conf.senv := conf.senv @ [("spouse", "on")]
    | _ -> () ];
    match p_getenv conf.env "et" with
    [ Some x -> conf.senv := conf.senv @ [("et", x)]
    | _ -> () ];
    match p_getenv conf.env "cgl" with
    [ Some "on" -> conf.senv := conf.senv @ [("cgl", "on")]
    | _ -> () ];
  }
;

value make_senv conf base =
  let get x = Util.p_getenv conf.env x in
  match (get "em", get "ei", get "ep", get "en", get "eoc") with
  [ (Some vm, Some vi, _, _, _) -> set_senv conf vm vi
  | (Some vm, None, Some vp, Some vn, voco) ->
      let voc =
        match voco with
        [ Some voc -> try int_of_string voc with [ Failure _ -> 0 ]
        | None -> 0 ]
      in
      let ip =
        try person_ht_find_unique base vp vn voc with
        [ Not_found -> do { incorrect_request conf; raise Exit } ]
      in
      let vi = string_of_int (Adef.int_of_iper ip) in set_senv conf vm vi
  | _ -> () ]
;

value family_m conf base =
  match p_getenv conf.env "m" with
  [ Some "A" ->
      match find_person_in_env conf base "" with
      [ Some p -> Ascend.print conf base p
      | _ -> very_unknown conf ]
  | Some "ADD_FAM" when conf.wizard -> UpdateFam.print_add conf base
  | Some "ADD_FAM_OK" when conf.wizard -> UpdateFamOk.print_add conf base
  | Some "ADD_IND" when conf.wizard -> UpdateInd.print_add conf base
  | Some "ADD_IND_OK" when conf.wizard -> UpdateIndOk.print_add conf base
  | Some "ADD_PAR" when conf.wizard -> UpdateFam.print_add_parents conf base
  | Some "AN" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_birth conf base (int_of_string x)
      | _ -> Birthday.print_menu_birth conf base ]
  | Some "AD" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_dead conf base (int_of_string x)
      | _ -> Birthday.print_menu_dead conf base ]
  | Some "AM" ->
      match p_getenv conf.env "v" with
      [ Some x -> Birthday.print_marriage conf base (int_of_string x)
      | _ -> Birthday.print_menu_marriage conf base ]
  | Some "AS_OK" -> AdvSearchOk.print conf base
  | Some "B" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "C" ->
      match find_person_in_env conf base "" with
      [ Some p -> Cousins.print conf base p
      | _ -> very_unknown conf ]
  | Some "CAL" -> Date.print_calendar conf base
  | Some "CHG_CHN" when conf.wizard -> ChangeChildren.print conf base
  | Some "CHG_CHN_OK" when conf.wizard -> ChangeChildren.print_ok conf base
  | Some "D" ->
      match find_person_in_env conf base "" with
      [ Some p -> Descend.print conf base p
      | _ -> very_unknown conf ]
  | Some "DAG" -> Dag.print conf base
  | Some "DEL_FAM" when conf.wizard -> UpdateFam.print_del conf base
  | Some "DEL_FAM_OK" when conf.wizard -> UpdateFamOk.print_del conf base
  | Some "DEL_IND" when conf.wizard -> UpdateInd.print_del conf base
  | Some "DEL_IND_OK" when conf.wizard -> UpdateIndOk.print_del conf base
  | Some "DEL_IMAGE" when conf.wizard && conf.can_send_image ->
      SendImage.print_del conf base
  | Some "DEL_IMAGE_OK" when conf.wizard && conf.can_send_image ->
      SendImage.print_del_ok conf base
  | Some "DOC" -> Doc.print conf
  | Some "FORUM" -> Forum.print conf base
  | Some "FORUM_ADD" -> Forum.print_add conf base
  | Some "FORUM_ADD_OK" -> Forum.print_add_ok conf base
  | Some "FORUM_DEL" -> Forum.print_del conf base
  | Some "H" ->
      match p_getenv conf.env "v" with
      [ Some f -> Srcfile.print conf base f
      | None -> Util.incorrect_request conf ]
  | Some "HIST" -> History.print conf base
  | Some "INV_FAM" when conf.wizard -> UpdateFam.print_inv conf base
  | Some "INV_FAM_OK" when conf.wizard -> UpdateFamOk.print_inv conf base
  | Some "KILL_ANC" when conf.wizard ->
      MergeInd.print_kill_ancestors conf base
  | Some "LB" when conf.wizard || conf.friend ->
      BirthDeath.print_birth conf base
  | Some "LD" when conf.wizard || conf.friend ->
      BirthDeath.print_death conf base
  | Some "LL" -> BirthDeath.print_longest_lived conf base
  | Some "LM" when conf.wizard || conf.friend ->
      BirthDeath.print_marriage conf base
  | Some "LEX" -> Srcfile.print_lexicon conf base
  | Some "MRG" when conf.wizard ->
      match find_person_in_env conf base "" with
      [ Some p -> Merge.print conf base p
      | _ -> very_unknown conf ]
  | Some "MRG_FAM" when conf.wizard -> MergeFam.print conf base
  | Some "MRG_FAM_OK" when conf.wizard -> MergeFamOk.print_merge conf base
  | Some "MRG_MOD_FAM_OK" when conf.wizard ->
      MergeFamOk.print_mod_merge conf base
  | Some "MRG_IND" when conf.wizard -> MergeInd.print conf base
  | Some "MRG_IND_OK" when conf.wizard -> MergeIndOk.print_merge conf base
  | Some "MRG_MOD_IND_OK" when conf.wizard ->
      MergeIndOk.print_mod_merge conf base
  | Some "MOD_FAM" when conf.wizard -> UpdateFam.print_mod conf base
  | Some "MOD_FAM_OK" when conf.wizard -> UpdateFamOk.print_mod conf base
  | Some "MOD_IND" when conf.wizard -> UpdateInd.print_mod conf base
  | Some "MOD_IND_OK" when conf.wizard -> UpdateIndOk.print_mod conf base
  | Some "MOD_NOTES" when conf.wizard -> Notes.print_mod conf base
  | Some "MOD_NOTES_OK" when conf.wizard -> Notes.print_mod_ok conf base
  | Some "RLM" -> Relation.print_multi conf base
  | Some "N" ->
      match p_getenv conf.env "v" with
      [ Some v -> Some.surname_print conf base Some.surname_not_found v
      | _ -> Alln.print_surnames conf base ]
  | Some "NG" ->
      match (p_getenv conf.env "n", p_getenv conf.env "select") with
      [ (Some n, Some "input" | None) ->
          match p_getenv conf.env "t" with
          [ Some "P" ->
              do {
                conf.cancel_links := False; Some.first_name_print conf base n
              }
          | Some "N" ->
              do {
                conf.cancel_links := False;
                Some.surname_print conf base Some.surname_not_found n
              }
          | _ ->
              if n = "" then unknown conf n
              else
                let (pl, soza_acc) = find_all conf base n in
                match pl with
                [ [] ->
                    do {
                      conf.cancel_links := False;
                      Some.surname_print conf base unknown n
                    }
                | [p] ->
                    if soza_acc ||
                       Gutil.person_of_key base n <> None ||
                       person_is_std_key base p n
                    then
                      person_selected conf base p
                    else specify conf base n pl
                | pl -> specify conf base n pl ] ]
      | (_, Some i) ->
          relation_print conf base
            (pget conf base (Adef.iper_of_int (int_of_string i)))
      | _ -> () ]
  | Some "NOTES" -> Notes.print conf base
  | Some "OA" when conf.wizard || conf.friend ->
      BirthDeath.print_oldest_alive conf base
  | Some "P" ->
      match p_getenv conf.env "v" with
      [ Some v -> Some.first_name_print conf base v
      | None -> Alln.print_first_names conf base ]
  | Some "PS" -> Place.print_all_places_surnames conf base
  | Some "R" ->
      match find_person_in_env conf base "" with
      [ Some p -> relation_print conf base p
      | _ -> very_unknown conf ]
  | Some "REQUEST" when conf.wizard ->
      let title _ = () in
      do {
        header conf title;
        Wserver.wprint "<pre>\n";
        List.iter (Wserver.wprint "%s\n") conf.request;
        Wserver.wprint "</pre>\n";
        trailer conf;
      }
  | Some "RL" -> RelationLink.print conf base
  | Some "SND_IMAGE" when conf.wizard && conf.can_send_image ->
      SendImage.print conf base
  | Some "SND_IMAGE_OK" when conf.wizard && conf.can_send_image ->
      SendImage.print_send_ok conf base
  | Some "SRC" ->
      match p_getenv conf.env "v" with
      [ Some f -> Srcfile.print_source conf base f
      | _ -> Util.incorrect_request conf ]
  | Some "STAT" -> BirthDeath.print_statistics conf base
  | Some "TT" -> Title.print conf base
  | Some "U" when conf.wizard ->
      match find_person_in_env conf base "" with
      [ Some p -> Update.print conf base p
      | _ -> very_unknown conf ]
  | Some "WIZNOTES" -> Wiznotes.print conf base
  | Some mode -> incorrect_request conf
  | None ->
      match find_person_in_env conf base "" with
      [ Some p -> person_selected conf base p
      | _ -> very_unknown conf ] ]
;

value print_no_index conf base =
  let title _ = Wserver.wprint "Link to use" in
  let link = url_no_index conf base in
  do {
    header conf title;
    tag "ul" begin
      html_li conf;
      tag "a" "href=\"http://%s\"" link begin Wserver.wprint "%s" link; end;
    end;
    print_link_to_welcome conf False;
    trailer conf;
  }
;

value special_vars =
  ["dsrc"; "em"; "ei"; "ep"; "en"; "eoc"; "escache"; "et"; "long"; "spouse";
   "cgl"; "iz"; "nz"; "pz"; "ocz"; "templ"; "log_uid"; "log_pwd"; "log_cnl"]
;

value only_special_env = List.for_all (fun (x, _) -> List.mem x special_vars);

value extract_henv conf base =
  do {
    match find_person_in_env conf base "z" with
    [ Some p ->
        let x =
          let first_name = p_first_name base p in
          let surname = p_surname base p in
          if conf.access_by_key &&
             not (first_name = "?" || surname = "?") then
            [("pz", code_varenv (Name.lower first_name));
             ("nz", code_varenv (Name.lower surname));
             ("ocz", string_of_int p.occ)]
          else [("iz", string_of_int (Adef.int_of_iper p.cle_index))]
        in
        conf.henv := conf.henv @ x
    | None -> () ];
    match p_getenv conf.env "dsrc" with
    [ Some "" | None -> ()
    | Some s -> conf.henv := conf.henv @ [("dsrc", code_varenv s)] ];
    match p_getenv conf.env "templ" with
    [ None -> ()
    | Some s -> conf.henv := conf.henv @ [("templ", code_varenv s)] ];
    match p_getenv conf.env "escache" with
    [ Some _ ->
        let v = escache_value conf in
        conf.henv := conf.henv @ [("escache", v)]
    | None -> () ];
  }
;

value set_owner conf =
  ifdef UNIX then
    let s = Unix.stat (Util.base_path [] (conf.bname ^ ".gwb")) in
    try do { Unix.setgid s.Unix.st_gid; Unix.setuid s.Unix.st_uid; } with
    [ Unix.Unix_error _ _ _ -> () ]
  else ()
;

value thousand oc x = Num.print (output_string oc) "," (Num.of_int x);

value log_count conf (log_file, log_oc, flush_log) r =
  if conf.cgi && log_file = "" then ()
  else
    match r with
    [ Some (welcome_cnt, request_cnt, start_date) ->
        let oc = log_oc () in
        do {
          Printf.fprintf oc "  #accesses %a (#welcome %a) since %s\n"
            thousand (welcome_cnt + request_cnt) thousand welcome_cnt
            start_date;
          flush_log oc;
        }
    | None -> () ]
;

value print_moved conf base s =
  match Util.open_etc_file "moved" with
  [ Some ic ->
      let env = [('b', fun () -> conf.bname); ('t', fun () -> s)] in
      do {
        Util.html conf;
        Util.nl ();
        Util.copy_from_etc env conf.lang conf.indep_command ic;
        Util.trailer conf;
      }
  | None ->
      let title _ = Wserver.wprint "%s -&gt; %s" conf.bname s in
      do {
        Util.header_no_page_title conf title;
        Wserver.wprint "The database %s has moved to:\n<dl><dt><dd>\n"
          conf.bname;
        stag "a" "href=\"%s\"" s begin Wserver.wprint "%s" s; end;
        Wserver.wprint "\n</dd></dt></dl>\n";
        Util.trailer conf;
      } ]
;

value family conf base log =
  do {
    match p_getenv conf.base_env "moved" with
    [ Some s ->
        print_moved conf base s
    | None ->
        match (p_getenv conf.env "opt", p_getenv conf.env "m") with
        [ (Some "no_index", _) -> print_no_index conf base
        | (_, Some "IM") -> Image.print conf base
        | _ ->
    
            do {
              set_owner conf;
              extract_henv conf base;
              make_senv conf base;
              if only_special_env conf.env then do {
                let r = Srcfile.incr_welcome_counter conf in
                log_count conf log r;
                Srcfile.print_start conf base;
              }
              else do {
                let r = Srcfile.incr_request_counter conf in
                log_count conf log r;
                family_m conf base;
              };
            } ] ];
    Wserver.wflush ();
  }
;
