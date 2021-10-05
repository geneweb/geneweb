open Geneweb
open Config
open Gwdb

module UI = struct

  let enabled conf s = List.assoc_opt s conf.env = Some "on"

  let print_arg conf (name, kind, doc) =
    match kind with
    | `Arg_Set ->
      Output.print_string conf {|<p><label><input type="checkbox" name="|} ;
      Output.print_string conf name ;
      Output.print_string conf {|" value="on"> |} ;
      Output.print_string conf doc ;
      Output.print_string conf {|</label></p>|} ;
    | _ -> assert false

  let form conf m submit args =
    Output.print_string conf {|<form action="|} ;
    Output.print_string conf (Util.commd conf) ;
    Output.print_string conf {|" method="GET">|} ;
    Output.print_string conf {|<input type="hidden" name="m" value="|} ;
    Output.print_string conf m ;
    Output.print_string conf {|">|} ;
    List.iter (print_arg conf) args ;
    Output.print_string conf {|<input type="submit" value="|} ;
    Output.print_string conf submit ;
    Output.print_string conf {|">|} ;
    Output.print_string conf {|</form>|}

end

let arg_f_parents = "f_parents"
let arg_f_children = "f_children"
let arg_p_parents = "p_parents"
let arg_p_NBDS = "p_NBDS"
let arg_p_families = "p_families"
let arg_pevents_witnesses = "pevents_witnesses"
let arg_fevents_witnesses = "fevents_witnesses"
let arg_marriage_divorce = "marriage_divorce"
let arg_missing_spouses = "missing_spouses"
let arg_invalid_utf8 = "invalid_utf8"
let arg_p_key = "p_key"
let arg_tstab = "tstab"

let fixbase conf _base =
  let title = Util.transl conf "plugin_fixbase_FIXBASE" in
  !GWPARAM.wrap_output conf title @@ fun () ->
  Output.print_string conf {|<p>|} ;
  Output.print_string conf (Util.transl conf "plugin_fixbase_description") ;
  Output.print_string conf {|</p>|} ;
  let args =
    let input name txt = (name, `Arg_Set, Util.transl conf txt) in
    [ input arg_f_parents "plugin_fixbase_f_parents"
    ; input arg_f_children "plugin_fixbase_f_children"
    ; input arg_p_parents "plugin_fixbase_p_parents"
    ; input arg_p_NBDS "plugin_fixbase_p_NBDS"
    ; input arg_p_families "plugin_fixbase_p_families"
    ; input arg_pevents_witnesses "plugin_fixbase_pevents_witnesses"
    ; input arg_fevents_witnesses "plugin_fixbase_fevents_witnesses"
    ; input arg_marriage_divorce "plugin_fixbase_marriage_divorce"
    ; input arg_missing_spouses "plugin_fixbase_missing_spouses"
    ; input arg_invalid_utf8 "plugin_fixbase_invalid_utf8"
    ; input arg_p_key "plugin_fixbase_p_key"
    ; input arg_tstab "plugin_fixbase_tstab"
    ]
  in
  UI.form conf "FIXBASE_OK" (Util.transl conf "plugin_fixbase_submit") args

let fixbase_ok conf base =
  let dry_run = Util.p_getenv conf.env "dry_run" <> Some "off" in
  let process () =
    ignore @@ Unix.alarm 0 ;  (* cancel timeout *)
    let base' = Gwdb.open_base @@ Util.base_path [] (bname base ^ ".gwb") in
    let ipers = ref [] in
    let ifams = ref [] in
    let istrs = ref [] in
    let report = function
      | Fixbase.Fix_NBDS ip
      | Fix_AddedUnion ip
      | Fix_AddedParents ip
      | Fix_ParentDeleted ip
      | Fix_AddedRelatedFromPevent (ip, _)
      | Fix_AddedRelatedFromFevent (ip, _)
      | Fix_UpdatedOcc (ip, _, _)
        -> ipers := ip :: !ipers
      | Fix_RemovedUnion (iper, ifam)
      | Fix_RemovedDuplicateUnion (iper, ifam)
      | Fix_MissingSpouse (ifam, iper)
        -> ifams := ifam :: !ifams
         ; ipers := iper :: !ipers ;
      | Fix_MarriageDivorce ifam
      | Fix_AddedChild ifam
        -> ifams := ifam :: !ifams
      | Fix_WrongUTF8Encoding (ifam, iper, istr)
        -> istrs := (ifam, iper, istr) :: !istrs
    in
    let progress (_:int) (_:int) = () in
    let enabled = List.exists (UI.enabled conf) in
    if enabled [ "marriage_divorce" ; "f_parents" ; "f_children" ; "fevents_witnesses" ; "missing_spouses" ; "invalid_utf8" ]
    then Gwdb.load_families_array base ;
    if enabled [ "invalid_utf8" ; "p_key" ]
    then Gwdb.load_strings_array base ;
    if enabled [ "f_parents" ; "p_families" ] then Gwdb.load_unions_array base ;
    if enabled [ "f_children" ; "p_parents" ] then begin
      Gwdb.load_descends_array base ;
      Gwdb.load_ascends_array base
    end ;
    load_persons_array base ;
    let opt s (fn : ?report:_ -> _ -> _ -> _) = if UI.enabled conf s then fn ~report progress base in
    let title = Util.transl conf "plugin_fixbase_FIXBASE_OK" in
    !GWPARAM.wrap_output conf title @@ fun () ->
    opt "f_parents" Fixbase.check_families_parents ;
    opt "f_children" Fixbase.check_families_children ;
    opt "p_parents" Fixbase.check_persons_parents ;
    opt "p_NBDS" Fixbase.check_NBDS ;
    opt "p_families" Fixbase.check_persons_families ;
    opt "pevents_witnesses" Fixbase.check_pevents_witnesses ;
    opt "fevents_witnesses" Fixbase.check_fevents_witnesses ;
    opt "marriage_divorce" Fixbase.fix_marriage_divorce ;
    opt "missing_spouses" Fixbase.fix_missing_spouses ;
    opt "invalid_utf8" Fixbase.fix_utf8_sequence ;
    opt "p_key" Fixbase.fix_key ;
    opt "p_key" Fixbase.fix_key ;
    clear_persons_array base ;
    clear_strings_array base ;
    clear_families_array base ;
    clear_unions_array base ;
    clear_descends_array base ;
    clear_ascends_array base ;
    let ifneq x1 x2 label s =
      if x1 <> x2 then begin
        Output.print_string conf {|<tr><td><b>|} ;
        Output.print_string conf label ;
        Output.print_string conf {|</b></td><td>|} ;
        Output.print_string conf (s x1) ;
        Output.print_string conf {|</td><td>|} ;
        Output.print_string conf (s x2) ;
        Output.print_string conf {|</td></tr>|}
      end
    in
    let dump_p p p' =
      let mka p =
        let a = gen_ascend_of_person p in
        { a with parents = Option.map string_of_ifam a.parents }
      in
      let mku p =
        { Def.family = Array.map string_of_ifam (gen_union_of_person p).family }
      in
      let mkp p =
        let p = gen_person_of_person p in
        let p = Futil.map_person_ps string_of_iper (sou base) p in
        { p with key_index = string_of_iper p.key_index }
      in
      let a1 = mka p in
      let u1 = mku p in
      let p1 = mkp p in
      let a2 = mka p' in
      let u2 = mku p' in
      let p2 = mkp p' in
      ifneq p1.first_name p2.first_name "first_name" (fun s -> s) ;
      ifneq p1.surname p2.surname "surname" (fun s -> s) ;
      ifneq p1.occ p2.occ "occ" string_of_int ;
      ifneq p1.image p2.image "image" (fun s -> s) ;
      ifneq p1.public_name p2.public_name "public_name" (fun s -> s) ;
      ifneq p1.qualifiers p2.qualifiers "qualifiers" [%show: string list] ;
      ifneq p1.aliases p2.aliases "aliases" [%show: string list] ;
      ifneq p1.first_names_aliases p2.first_names_aliases "first_names_aliases" [%show: string list] ;
      ifneq p1.surnames_aliases p2.surnames_aliases "surnames_aliases" [%show: string list] ;
      ifneq p1.titles p2.titles "titles" [%show: string Def_show.gen_title list] ;
      ifneq p1.rparents p2.rparents "rparents" [%show: (string, string) Def_show.gen_relation list] ;
      ifneq p1.related p2.related "related" [%show: string list] ;
      ifneq p1.occupation p2.occupation "occupation" (fun s -> s) ;
      ifneq p1.sex p2.sex "sex" [%show: Def_show.sex] ;
      ifneq p1.access p2.access "access" [%show: Def_show.access] ;
      ifneq p1.birth p2.birth "birth" [%show: Def_show.cdate] ;
      ifneq p1.birth_place p2.birth_place "birth_place" (fun s -> s) ;
      ifneq p1.birth_note p2.birth_note "birth_note" (fun s -> s) ;
      ifneq p1.birth_src p2.birth_src "birth_src" (fun s -> s) ;
      ifneq p1.baptism p2.baptism "baptism"  [%show: Def_show.cdate] ;
      ifneq p1.baptism_place p2.baptism_place "baptism_place" (fun s -> s) ;
      ifneq p1.baptism_note p2.baptism_note "baptism_note" (fun s -> s) ;
      ifneq p1.baptism_src p2.baptism_src "baptism_src" (fun s -> s) ;
      ifneq p1.death p2.death "death"  [%show: Def_show.death] ;
      ifneq p1.death_place p2.death_place "death_place" (fun s -> s) ;
      ifneq p1.death_note p2.death_note "death_note" (fun s -> s) ;
      ifneq p1.death_src p2.death_src "death_src" (fun s -> s) ;
      ifneq p1.burial p2.burial "burial"  [%show: Def_show.burial] ;
      ifneq p1.burial_place p2.burial_place "burial_place" (fun s -> s) ;
      ifneq p1.burial_note p2.burial_note "burial_note" (fun s -> s) ;
      ifneq p1.burial_src p2.burial_src "burial_src" (fun s -> s) ;
      ifneq p1.pevents p2.pevents "pevents" [%show: (string, string) Def_show.gen_pers_event list] ;
      ifneq p1.notes p2.notes "notes" (fun s -> s) ;
      ifneq p1.psources p2.psources "psources" (fun s -> s) ;
      ifneq p1.key_index p2.key_index "key_index" (fun s -> s) ;
      ifneq a1.parents a2.parents "parents" [%show: string option] ;
      ifneq a1.consang a2.consang "consang" [%show: Def_show.fix] ;
      ifneq u1.family u2.family "family" [%show: string array] ;
    in
    let dump_f f f' =
      let mkf f =
        Futil.map_family_ps string_of_iper string_of_ifam (sou base) (gen_family_of_family f)
      in
      let mkc f =
        Futil.map_couple_p false string_of_iper (gen_couple_of_family f)
      in
      let mkd f =
        Futil.map_descend_p string_of_iper (gen_descend_of_family f)
      in
      let f1 = mkf f in
      let c1 = mkc f in
      let d1 = mkd f in
      let f2 = mkf f' in
      let c2 = mkc f' in
      let d2 = mkd f' in
      ifneq f1.marriage f2.marriage "marriage"  [%show: Def_show.cdate] ;
      ifneq f1.marriage_place f2.marriage_place "marriage_place" (fun s -> s) ;
      ifneq f1.marriage_note f2.marriage_note "marriage_note" (fun s -> s) ;
      ifneq f1.marriage_src f2.marriage_src "marriage_src" (fun s -> s) ;
      ifneq f1.witnesses f2.witnesses "witnesses" [%show: string array] ;
      ifneq f1.relation f2.relation "relation" [%show: Def_show.relation_kind] ;
      ifneq f1.divorce f2.divorce "divorce" [%show: Def_show.divorce] ;
      ifneq f1.fevents f2.fevents "fevents" [%show: (string, string) Def_show.gen_fam_event list] ;
      ifneq f1.comment f2.comment "comment" (fun s -> s) ;
      ifneq f1.origin_file f2.origin_file "origin_file" (fun s -> s) ;
      ifneq f1.fsources f2.fsources "fsources" (fun s -> s) ;
      ifneq f1.fam_index f2.fam_index "fam_index" (fun s -> s) ;
      ifneq (Adef.father c1) (Adef.father c2) "father" (fun s -> s) ;
      ifneq (Adef.mother c1) (Adef.mother c2) "mother" (fun s -> s) ;
      ifneq d1.children d2.children "children" [%show: string array] ;
    in
    let string_of_p i =
      Printf.sprintf {|<a href="%s&i=%s">%s</a>|}
        (Util.commd conf)
        (string_of_iper i)
        (Gutil.designation base (poi base i))
    in
    let string_of_f i =
      let fam = foi base i in
      Printf.sprintf "[%s & %s]"
        (string_of_p @@ get_father fam)
        (string_of_p @@ get_mother fam);
    in
    let dump string_of dump get data =
      List.iter begin fun i ->
        Output.print_string conf "<b>" ;
        Output.print_string conf (string_of i) ;
        Output.print_string conf "</b>" ;
        Output.print_string conf "<table>" ;
        dump (get base' i) (get base i) ;
        Output.print_string conf "</table>" ;
      end data
    in
    dump string_of_p dump_p poi !ipers ;
    dump string_of_f dump_f foi !ifams ;
    List.iter begin fun (ifam_opt, iper_opt, opt) ->
      let aux, sou =
        match opt with
        | Some (i, i') -> ifneq i i', sou base
        | None -> ifneq empty_string quest_string, fun _ -> "Dtext"
      in
      Output.print_string conf "<table>" ;
      aux
        (match ifam_opt with
         | Some i -> string_of_f i
         | None -> match iper_opt with
           | Some i -> string_of_p i
           | None -> assert false)
        sou ;
      Output.print_string conf "</table>" ;
    end !istrs ;
    let repost dry txt =
      Output.print_string conf {|<form action="|} ;
      Output.print_string conf (Util.commd conf) ;
      Output.print_string conf {|" method="GET">|} ;
      Output.print_string conf {|<input type="hidden" name="m" value="FIXBASE_OK">|} ;
      if not dry then Output.print_string conf {|<input type="hidden" name="dry_run" value="off">|} ;
      Output.print_string conf {|<input type="hidden" name="date_of_last_change" value="|} ;
      Output.print_string conf (Gwdb.date_of_last_change base |> string_of_float) ;
      Output.print_string conf {|">|} ;
      let opt s =
        if UI.enabled conf s
        then begin
          Output.print_string conf {|<input type="hidden" name="|} ;
          Output.print_string conf s ;
          Output.print_string conf {|" value="on">|}
        end
      in
      opt "f_parents" ;
      opt "f_children" ;
      opt "p_parents" ;
      opt "p_NBDS" ;
      opt "p_families" ;
      opt "pevents_witnesses" ;
      opt "fevents_witnesses" ;
      opt "marriage_divorce" ;
      opt "missing_spouses" ;
      opt "invalid_utf8" ;
      opt "p_key" ;
      opt "tstab" ;
      Output.print_string conf {|<p>|} ;
      Output.print_string conf {|<input type="submit" value="|} ;
      Output.print_string conf txt ;
      Output.print_string conf {|">|} ;
      Output.print_string conf {|</p>|} ;
      Output.print_string conf {|</form>|} ;
    in
    let tstab () =
      if UI.enabled conf "tstab" then begin
        let bname = Util.base_path [] (bname base ^ ".gwb") in
        Mutil.rm (Filename.concat bname "tstab_visitor") ;
        Mutil.rm (Filename.concat bname "tstab") ;
        Output.print_string conf {|<p>|} ;
        Output.print_string conf (Util.transl conf "plugin_fixbase_ok_tstab") ;
        Output.print_string conf {|</p>|} ;
      end
    in
    if not dry_run then begin
      if Util.p_getenv conf.env "date_of_last_change"
         = Some (Gwdb.date_of_last_change base |> string_of_float)
      then begin
        Gwdb.commit_patches base ;
        Output.print_string conf {|<p>|} ;
        Output.print_string conf (Util.transl conf "plugin_fixbase_ok_commit_patches") ;
        Output.print_string conf {|</p>|} ;
        tstab ()
      end else if !ipers <> [] || !ifams <> [] || !istrs <> [] then begin
        Output.print_string conf {|<p>|} ;
        Output.print_string conf (Util.transl conf "plugin_fixbase_ok_base_changed") ;
        Output.print_string conf {|</p>|} ;
        repost true (Util.transl conf "plugin_fixbase_ok_refresh")
      end else tstab ()
    end else if !ipers <> [] || !ifams <> [] || !istrs <> [] then begin
      repost false (Util.transl conf "plugin_fixbase_ok_apply")
    end else begin
      Output.print_string conf {|<p>|} ;
      Output.print_string conf (Util.transl conf "plugin_fixbase_ok_nothing") ;
      Output.print_string conf {|</p>|}
    end ;
    Output.print_string conf {|<p><a href="|} ;
    Output.print_string conf (Util.commd conf) ;
    Output.print_string conf {|&m=FIXBASE">|} ;
    Output.print_string conf (Util.transl conf "plugin_fixbase_ok_return") ;
    Output.print_string conf {|</a></p>|} ;
  in
  if dry_run then process ()
  else
    Lock.control (Mutil.lock_file @@ Util.base_path [] (conf.bname ^ ".gwb")) false
      ~onerror:(fun () -> !GWPARAM.output_error conf Def.Service_Unavailable)
      process

let ns = "fixbase"

let opt_password =
  match Sys.getenv_opt "GW_PLUGIN_FIXBASE_PASSWORD" with
  | Some "" | None -> None
  | x -> x

let opt_manitou =
  match Sys.getenv_opt "GW_PLUGIN_FIXBASE_ONLY_MANITOU" with
  | Some ("on"|"ON"|"y"|"Y"|"1") -> true
  | _ -> false

let _ =
  let aux fn _assets conf = function
    | Some base ->
      if if opt_manitou then conf.manitou else conf.wizard
      then
        if opt_password = List.assoc_opt "password" conf.env
        then (fn conf base ; true)
        else false
      else false
    | None -> false
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "FIXBASE", aux fixbase
    ; "FIXBASE_OK", aux fixbase_ok
    ]
