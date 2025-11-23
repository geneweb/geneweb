let arg_f_parents = "f_parents"
let arg_f_children = "f_children"
let arg_p_parents = "p_parents"
let arg_p_NBDS = "p_NBDS"
let arg_p_families = "p_families"
let arg_pevents_witnesses = "pevents_witnesses"
let arg_fevents_witnesses = "fevents_witnesses"
let arg_marriage_divorce = "marriage_divorce"
let arg_missing_spouses = "missing_spouses"
let arg_invalid_strings = "invalid_strings"
let arg_p_key = "p_key"
let arg_tstab = "tstab"
let arg_password = "password"

module UI = struct
  let enabled conf s =
    (List.assoc_opt s conf.Geneweb.Config.env :> string option) = Some "on"

  let print_arg conf
      ((name, kind, doc) :
        Adef.encoded_string * [> `Arg_Set | `Arg_String ] * Adef.safe_string) =
    match kind with
    | `Arg_Set ->
        Geneweb.Output.print_sstring conf
          {|<p><label><input type="checkbox" name="|};
        Geneweb.Output.print_string conf name;
        Geneweb.Output.print_sstring conf {|" value="on"> |};
        Geneweb.Output.print_string conf doc;
        Geneweb.Output.print_sstring conf {|</label></p>|}
    | `Arg_String ->
        Geneweb.Output.print_sstring conf
          {|<p><label><input type="type" name="|};
        Geneweb.Output.print_string conf name;
        Geneweb.Output.print_sstring conf {|"> |};
        Geneweb.Output.print_string conf doc;
        Geneweb.Output.print_sstring conf {|</label></p>|}

  let form conf (m : Adef.encoded_string) (submit : Adef.safe_string) args =
    Geneweb.Output.print_sstring conf {|<form action="|};
    Geneweb.Output.print_string conf (Geneweb.Util.commd conf);
    Geneweb.Output.print_sstring conf {|" method="GET">|};
    Geneweb.Output.print_sstring conf {|<input type="hidden" name="m" value="|};
    Geneweb.Output.print_string conf m;
    Geneweb.Output.print_sstring conf {|">|};
    (match List.assoc_opt arg_password conf.Geneweb.Config.env with
    | Some x ->
        Geneweb.Output.print_sstring conf
          {|<input type="hidden" name="password" value="|};
        Geneweb.Output.print_string conf x;
        Geneweb.Output.print_sstring conf {|">|}
    | None -> ());
    List.iter (print_arg conf) args;
    Geneweb.Output.print_sstring conf {|<input type="submit" value="|};
    Geneweb.Output.print_string conf submit;
    Geneweb.Output.print_sstring conf {|">|};
    Geneweb.Output.print_sstring conf {|</form>|}
end

let opt_password =
  match Sys.getenv_opt "GW_PLUGIN_FIXBASE_PASSWORD" with
  | Some "" | None -> None
  | Some x -> Some (Mutil.encode x)

let opt_manitou =
  match Sys.getenv_opt "GW_PLUGIN_FIXBASE_ONLY_MANITOU" with
  | Some ("on" | "ON" | "y" | "Y" | "1") -> true
  | _ -> false

let missing_password conf =
  let args =
    [
      ( Mutil.encode arg_password,
        `Arg_String,
        Geneweb.Util.transl conf "plugin_fixbase_password_missing" |> Adef.safe
      );
    ]
  in
  UI.form conf
    (List.assoc "m" conf.Geneweb.Config.env)
    (Geneweb.Util.transl conf "plugin_fixbase_password_submit" |> Adef.safe)
    args

let wrap conf title fn =
  Geneweb.GWPARAM.wrap_output conf title @@ fun () ->
  if opt_password = List.assoc_opt arg_password conf.Geneweb.Config.env then
    fn ()
  else missing_password conf

let fixbase conf _base =
  wrap conf (Geneweb.Util.transl conf "plugin_fixbase_FIXBASE" |> Adef.safe)
  @@ fun () ->
  Geneweb.Output.print_sstring conf {|<p>|};
  Geneweb.Output.print_sstring conf
    (Geneweb.Util.transl conf "plugin_fixbase_description");
  Geneweb.Output.print_sstring conf {|</p>|};
  let args =
    let input name txt =
      (Mutil.encode name, `Arg_Set, Geneweb.Util.transl conf txt |> Adef.safe)
    in
    [
      input arg_f_parents "plugin_fixbase_f_parents";
      input arg_f_children "plugin_fixbase_f_children";
      input arg_p_parents "plugin_fixbase_p_parents";
      input arg_p_NBDS "plugin_fixbase_p_NBDS";
      input arg_p_families "plugin_fixbase_p_families";
      input arg_pevents_witnesses "plugin_fixbase_pevents_witnesses";
      input arg_fevents_witnesses "plugin_fixbase_fevents_witnesses";
      input arg_marriage_divorce "plugin_fixbase_marriage_divorce";
      input arg_missing_spouses "plugin_fixbase_missing_spouses";
      input arg_invalid_strings "plugin_fixbase_invalid_strings";
      input arg_p_key "plugin_fixbase_p_key";
      input arg_tstab "plugin_fixbase_tstab";
    ]
  in
  UI.form conf
    (Adef.encoded "FIXBASE_OK")
    (Geneweb.Util.transl conf "plugin_fixbase_submit" |> Adef.safe)
    args

let fixbase_ok conf base =
  let dry_run =
    Geneweb.Util.p_getenv conf.Geneweb.Config.env "dry_run" <> Some "off"
  in
  let process () =
    ignore @@ Unix.alarm 0;
    (* cancel timeout *)
    let base' =
      Gwdb.open_base @@ Geneweb.GWPARAM.base_path [] (Gwdb.bname base ^ ".gwb")
    in
    let ipers = ref [] in
    let ifams = ref [] in
    let istrs = ref [] in
    let report = function
      | Geneweb.Fixbase.Fix_NBDS ip
      | Geneweb.Fixbase.Fix_AddedUnion ip
      | Geneweb.Fixbase.Fix_AddedParents ip
      | Geneweb.Fixbase.Fix_ParentDeleted ip
      | Geneweb.Fixbase.Fix_AddedRelatedFromPevent (ip, _)
      | Geneweb.Fixbase.Fix_AddedRelatedFromFevent (ip, _)
      | Geneweb.Fixbase.Fix_UpdatedOcc (ip, _, _) ->
          ipers := ip :: !ipers
      | Geneweb.Fixbase.Fix_RemovedUnion (iper, ifam)
      | Geneweb.Fixbase.Fix_RemovedDuplicateUnion (iper, ifam)
      | Geneweb.Fixbase.Fix_MissingSpouse (ifam, iper) ->
          ifams := ifam :: !ifams;
          ipers := iper :: !ipers
      | Geneweb.Fixbase.Fix_MarriageDivorce ifam
      | Geneweb.Fixbase.Fix_AddedChild ifam ->
          ifams := ifam :: !ifams
      | Geneweb.Fixbase.Fix_WrongString (ifam, iper, istr) ->
          istrs := (ifam, iper, istr) :: !istrs
      | Geneweb.Fixbase.Fix_WrongStringFailure (_, _, _) -> ()
    in
    let progress (_ : int) (_ : int) = () in
    let enabled = List.exists (UI.enabled conf) in
    if
      enabled
        [
          "marriage_divorce";
          "f_parents";
          "f_children";
          "fevents_witnesses";
          "missing_spouses";
          "invalid_strings";
        ]
    then Gwdb.load_families_array base;
    if enabled [ "invalid_strings"; "p_key" ] then Gwdb.load_strings_array base;
    if enabled [ "f_parents"; "p_families" ] then Gwdb.load_unions_array base;
    if enabled [ "f_children"; "p_parents" ] then (
      Gwdb.load_descends_array base;
      Gwdb.load_ascends_array base);
    Gwdb.load_persons_array base;
    let opt s fn fixes = if UI.enabled conf s then fn :: fixes else fixes in
    wrap conf (Geneweb.Util.transl conf "plugin_fixbase_FIXBASE_OK" |> Adef.safe)
    @@ fun () ->
    let family_fixes = [] in
    let person_fixes = [] in
    let family_fixes =
      opt "f_parents" Geneweb.Fixbase.fix_family_parents family_fixes
    in
    let family_fixes =
      opt "f_children" Geneweb.Fixbase.fix_family_children family_fixes
    in
    let person_fixes =
      opt "p_parents" Geneweb.Fixbase.fix_person_parents person_fixes
    in
    let person_fixes = opt "p_NBDS" Geneweb.Fixbase.fix_nbds person_fixes in
    let person_fixes =
      opt "p_families" Geneweb.Fixbase.fix_person_unions person_fixes
    in
    let person_fixes =
      opt "pevents_witnesses" Geneweb.Fixbase.fix_person_events_witnesses
        person_fixes
    in
    let family_fixes =
      opt "fevents_witnesses" Geneweb.Fixbase.fix_family_events_witnesses
        family_fixes
    in
    let family_fixes =
      opt "marriage_divorce" Geneweb.Fixbase.fix_family_divorce family_fixes
    in
    let family_fixes =
      opt "missing_spouses" Geneweb.Fixbase.fix_family_spouses family_fixes
    in
    let person_fixes =
      opt "invalid_strings" Geneweb.Fixbase.fix_person_strings person_fixes
    in
    let family_fixes =
      opt "invalid_strings" Geneweb.Fixbase.fix_family_strings family_fixes
    in
    let person_fixes =
      opt "p_key" (Geneweb.Fixbase.fix_person_key base) person_fixes
    in
    let person_fixes = List.rev person_fixes in
    let family_fixes = List.rev family_fixes in
    ignore
      (Geneweb.Fixbase.perform_fixes ~report:(Some report) ~progress ~base
         ~person_fixes ~family_fixes);
    Gwdb.clear_persons_array base;
    Gwdb.clear_strings_array base;
    Gwdb.clear_families_array base;
    Gwdb.clear_unions_array base;
    Gwdb.clear_descends_array base;
    Gwdb.clear_ascends_array base;
    let ifneq x1 x2 label s =
      if x1 <> x2 then (
        Geneweb.Output.print_sstring conf {|<tr><td><b>|};
        Geneweb.Output.print_string conf label;
        Geneweb.Output.print_sstring conf {|</b></td><td>|};
        Geneweb.Output.print_string conf (s x1 |> Geneweb.Util.escape_html);
        Geneweb.Output.print_sstring conf {|</td><td>|};
        Geneweb.Output.print_string conf (s x2 |> Geneweb.Util.escape_html);
        Geneweb.Output.print_sstring conf {|</td></tr>|})
    in
    let dump_p p p' =
      let mka p =
        let a = Gwdb.gen_ascend_of_person p in
        { a with parents = Option.map Gwdb.string_of_ifam a.parents }
      in
      let mku p =
        {
          Def.family =
            Array.map Gwdb.string_of_ifam (Gwdb.gen_union_of_person p).family;
        }
      in
      let mkp p =
        let p = Gwdb.gen_person_of_person p in
        let p =
          Futil.map_person_ps Gwdb.string_of_iper
            (fun ?format:_ -> Gwdb.sou base)
            p
        in
        { p with key_index = Gwdb.string_of_iper p.key_index }
      in
      let a1 = mka p in
      let u1 = mku p in
      let p1 = mkp p in
      let a2 = mka p' in
      let u2 = mku p' in
      let p2 = mkp p' in
      let ifneq x1 x2 label s =
        ifneq x1 x2 (Geneweb.Util.escape_html label) s
      in
      ifneq p1.first_name p2.first_name "first_name" Fun.id;
      ifneq p1.surname p2.surname "surname" Fun.id;
      ifneq p1.occ p2.occ "occ" string_of_int;
      ifneq p1.image p2.image "image" Fun.id;
      ifneq p1.public_name p2.public_name "public_name" Fun.id;
      ifneq p1.qualifiers p2.qualifiers "qualifiers" [%show: string list];
      ifneq p1.aliases p2.aliases "aliases" [%show: string list];
      ifneq p1.first_names_aliases p2.first_names_aliases "first_names_aliases"
        [%show: string list];
      ifneq p1.surnames_aliases p2.surnames_aliases "surnames_aliases"
        [%show: string list];
      ifneq p1.titles p2.titles "titles" [%show: string Def_show.gen_title list];
      ifneq p1.rparents p2.rparents "rparents"
        [%show: (string, string) Def_show.gen_relation list];
      ifneq p1.related p2.related "related" [%show: string list];
      ifneq p1.occupation p2.occupation "occupation" Fun.id;
      ifneq p1.sex p2.sex "sex" [%show: Def_show.sex];
      ifneq p1.access p2.access "access" [%show: Def_show.access];
      ifneq p1.birth p2.birth "birth" [%show: Def_show.cdate];
      ifneq p1.birth_place p2.birth_place "birth_place" Fun.id;
      ifneq p1.birth_note p2.birth_note "birth_note" Fun.id;
      ifneq p1.birth_src p2.birth_src "birth_src" Fun.id;
      ifneq p1.baptism p2.baptism "baptism" [%show: Def_show.cdate];
      ifneq p1.baptism_place p2.baptism_place "baptism_place" Fun.id;
      ifneq p1.baptism_note p2.baptism_note "baptism_note" Fun.id;
      ifneq p1.baptism_src p2.baptism_src "baptism_src" Fun.id;
      ifneq p1.death p2.death "death" [%show: Def_show.death];
      ifneq p1.death_place p2.death_place "death_place" Fun.id;
      ifneq p1.death_note p2.death_note "death_note" Fun.id;
      ifneq p1.death_src p2.death_src "death_src" Fun.id;
      ifneq p1.burial p2.burial "burial" [%show: Def_show.burial];
      ifneq p1.burial_place p2.burial_place "burial_place" Fun.id;
      ifneq p1.burial_note p2.burial_note "burial_note" Fun.id;
      ifneq p1.burial_src p2.burial_src "burial_src" Fun.id;
      ifneq p1.pevents p2.pevents "pevents"
        [%show: (string, string) Def_show.gen_pers_event list];
      ifneq p1.notes p2.notes "notes" Fun.id;
      ifneq p1.psources p2.psources "psources" Fun.id;
      ifneq p1.key_index p2.key_index "key_index" Fun.id;
      ifneq a1.parents a2.parents "parents" [%show: string option];
      ifneq a1.consang a2.consang "consang" [%show: Def_show.fix];
      ifneq u1.family u2.family "family" [%show: string array]
    in
    let dump_f f f' =
      let mkf f =
        Futil.map_family_ps Gwdb.string_of_iper Gwdb.string_of_ifam
          (fun ?format:_ -> Gwdb.sou base)
          (Gwdb.gen_family_of_family f)
      in
      let mkc f =
        Futil.map_couple_p Gwdb.string_of_iper (Gwdb.gen_couple_of_family f)
      in
      let mkd f =
        Futil.map_descend_p Gwdb.string_of_iper (Gwdb.gen_descend_of_family f)
      in
      let f1 = mkf f in
      let c1 = mkc f in
      let d1 = mkd f in
      let f2 = mkf f' in
      let c2 = mkc f' in
      let d2 = mkd f' in
      let ifneq x1 x2 label s =
        ifneq x1 x2 (Geneweb.Util.escape_html label) s
      in
      ifneq f1.marriage f2.marriage "marriage" [%show: Def_show.cdate];
      ifneq f1.marriage_place f2.marriage_place "marriage_place" Fun.id;
      ifneq f1.marriage_note f2.marriage_note "marriage_note" Fun.id;
      ifneq f1.marriage_src f2.marriage_src "marriage_src" Fun.id;
      ifneq f1.witnesses f2.witnesses "witnesses" [%show: string array];
      ifneq f1.relation f2.relation "relation" [%show: Def_show.relation_kind];
      ifneq f1.divorce f2.divorce "divorce" [%show: Def_show.divorce];
      ifneq f1.fevents f2.fevents "fevents"
        [%show: (string, string) Def_show.gen_fam_event list];
      ifneq f1.comment f2.comment "comment" Fun.id;
      ifneq f1.origin_file f2.origin_file "origin_file" Fun.id;
      ifneq f1.fsources f2.fsources "fsources" Fun.id;
      ifneq f1.fam_index f2.fam_index "fam_index" Fun.id;
      ifneq (Adef.father c1) (Adef.father c2) "father" Fun.id;
      ifneq (Adef.mother c1) (Adef.mother c2) "mother" Fun.id;
      ifneq d1.children d2.children "children" [%show: string array]
    in
    let string_of_p i =
      Printf.sprintf {|<a href="%s&i=%s">%s</a>|}
        (Geneweb.Util.commd conf :> string)
        (Gwdb.string_of_iper i |> Mutil.encode :> string)
        (Geneweb.Util.designation base (Gwdb.poi base i)
          : Adef.escaped_string
          :> string)
      |> Adef.safe
    in
    let string_of_f i =
      let fam = Gwdb.foi base i in
      Printf.sprintf "[%s & %s]"
        (string_of_p @@ Gwdb.get_father fam : Adef.safe_string :> string)
        (string_of_p @@ Gwdb.get_mother fam : Adef.safe_string :> string)
      |> Adef.safe
    in
    let dump string_of dump get data =
      List.iter
        (fun i ->
          Geneweb.Output.print_sstring conf "<b>";
          Geneweb.Output.print_string conf (string_of i);
          Geneweb.Output.print_sstring conf "</b>";
          Geneweb.Output.print_sstring conf "<table>";
          dump (get base' i) (get base i);
          Geneweb.Output.print_sstring conf "</table>")
        data
    in
    dump string_of_p dump_p Gwdb.poi !ipers;
    dump string_of_f dump_f Gwdb.foi !ifams;
    List.iter
      (fun (ifam_opt, iper_opt, opt) ->
        let aux, sou =
          match opt with
          | Some (i, i') -> (ifneq i i', Gwdb.sou base)
          | None -> (ifneq Gwdb.empty_string Gwdb.quest_string, fun _ -> "Dtext")
        in
        Geneweb.Output.print_sstring conf "<table>";
        aux
          (match ifam_opt with
          | Some i -> string_of_f i
          | None -> (
              match iper_opt with
              | Some i -> string_of_p i
              | None -> assert false))
          sou;
        Geneweb.Output.print_sstring conf "</table>")
      !istrs;
    let repost dry txt =
      Geneweb.Output.print_sstring conf {|<form action="|};
      Geneweb.Output.print_string conf (Geneweb.Util.commd conf);
      Geneweb.Output.print_sstring conf {|" method="GET">|};
      Geneweb.Output.print_sstring conf
        {|<input type="hidden" name="m" value="FIXBASE_OK">|};
      if not dry then
        Geneweb.Output.print_sstring conf
          {|<input type="hidden" name="dry_run" value="off">|};
      Geneweb.Output.print_sstring conf
        {|<input type="hidden" name="date_of_last_change" value="|};
      Geneweb.Output.print_sstring conf
        (Gwdb.date_of_last_change base |> string_of_float);
      Geneweb.Output.print_sstring conf {|">|};
      let opt s =
        if UI.enabled conf s then (
          Geneweb.Output.print_sstring conf {|<input type="hidden" name="|};
          Geneweb.Output.print_string conf (Mutil.encode s);
          Geneweb.Output.print_sstring conf {|" value="on">|})
      in
      opt "f_parents";
      opt "f_children";
      opt "p_parents";
      opt "p_NBDS";
      opt "p_families";
      opt "pevents_witnesses";
      opt "fevents_witnesses";
      opt "marriage_divorce";
      opt "missing_spouses";
      opt "invalid_strings";
      opt "p_key";
      opt "tstab";
      Geneweb.Output.print_sstring conf {|<p>|};
      Geneweb.Output.print_sstring conf {|<input type="submit" value="|};
      Geneweb.Output.print_string conf txt;
      Geneweb.Output.print_sstring conf {|">|};
      Geneweb.Output.print_sstring conf {|</p>|};
      Geneweb.Output.print_sstring conf {|</form>|}
    in
    let tstab () =
      if UI.enabled conf "tstab" then (
        let bname = Geneweb.GWPARAM.base_path [] (Gwdb.bname base ^ ".gwb") in
        Files.rm (Filename.concat bname "tstab_visitor");
        Files.rm (Filename.concat bname "tstab");
        Geneweb.Output.print_sstring conf {|<p>|};
        Geneweb.Output.print_sstring conf
          (Geneweb.Util.transl conf "plugin_fixbase_ok_tstab");
        Geneweb.Output.print_sstring conf {|</p>|})
    in
    if not dry_run then
      if
        Geneweb.Util.p_getenv conf.Geneweb.Config.env "date_of_last_change"
        = Some (Gwdb.date_of_last_change base |> string_of_float)
      then (
        Gwdb.commit_patches base;
        Geneweb.Output.print_sstring conf {|<p>|};
        Geneweb.Output.print_sstring conf
          (Geneweb.Util.transl conf "plugin_fixbase_ok_commit_patches");
        Geneweb.Output.print_sstring conf {|</p>|};
        tstab ())
      else if !ipers <> [] || !ifams <> [] || !istrs <> [] then (
        Geneweb.Output.print_sstring conf {|<p>|};
        Geneweb.Output.print_sstring conf
          (Geneweb.Util.transl conf "plugin_fixbase_ok_base_changed");
        Geneweb.Output.print_sstring conf {|</p>|};
        repost true
          (Geneweb.Util.transl conf "plugin_fixbase_ok_refresh" |> Adef.safe))
      else tstab ()
    else if !ipers <> [] || !ifams <> [] || !istrs <> [] then
      repost false
        (Geneweb.Util.transl conf "plugin_fixbase_ok_apply" |> Adef.safe)
    else (
      Geneweb.Output.print_sstring conf {|<p>|};
      Geneweb.Output.print_sstring conf
        (Geneweb.Util.transl conf "plugin_fixbase_ok_nothing");
      Geneweb.Output.print_sstring conf {|</p>|});
    Geneweb.Output.print_sstring conf {|<p><a href="|};
    Geneweb.Output.print_string conf
      (Geneweb.Util.commd conf : Adef.escaped_string);
    Geneweb.Output.print_sstring conf {|&m=FIXBASE">|};
    Geneweb.Output.print_sstring conf
      (Geneweb.Util.transl conf "plugin_fixbase_ok_return");
    Geneweb.Output.print_sstring conf {|</a></p>|}
  in
  if dry_run then process ()
  else
    Lock.control
      (Files.lock_file
      @@ Geneweb.GWPARAM.base_path [] (conf.Geneweb.Config.bname ^ ".gwb"))
      false
      ~onerror:(fun () ->
        Geneweb.GWPARAM.output_error conf Def.Service_Unavailable)
      process

let ns = "fixbase"

let () =
  let aux fn _assets conf base =
    if
      if opt_manitou then conf.Geneweb.Config.manitou
      else conf.Geneweb.Config.wizard
    then (
      fn conf base;
      true)
    else false
  in
  let w_base = Gwd_lib.Request.w_base ~none:(fun _ -> false) in
  Gwd_lib.GwdPlugin.register ~ns
    [
      ("FIXBASE", fun assets -> w_base @@ aux fixbase assets);
      ("FIXBASE_OK", fun assets -> w_base @@ aux fixbase_ok assets);
    ]
