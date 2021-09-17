module Mwrite = Api_saisie_write_piqi
module Mext_write = Api_saisie_write_piqi_ext

open Geneweb
open Gwdb
open Def
open Util
open Api_update_util

let reconstitute_person_aux conf fn_occ fn_rparents fn_pevt_witnesses mod_p =
  let no_html_tags_only_printable s = no_html_tags (only_printable s) in
  let key_index = Gwdb.iper_of_string @@ Int32.to_string mod_p.Mwrite.Person.index in
  let first_name = no_html_tags_only_printable mod_p.Mwrite.Person.firstname in
  let surname = no_html_tags_only_printable mod_p.Mwrite.Person.lastname in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if (List.exists contain_fn Name.forbidden_char)
    || (List.exists contain_sn Name.forbidden_char)
    then (Name.purge first_name, Name.purge surname)
    else (first_name, surname)
  in
  let occ = fn_occ mod_p in
  let image = Opt.map_default "" only_printable mod_p.Mwrite.Person.image in
  let strings_aux = List.map no_html_tags_only_printable in
  let first_names_aliases = strings_aux mod_p.Mwrite.Person.firstname_aliases in
  let surnames_aliases = strings_aux mod_p.Mwrite.Person.surname_aliases in
  let public_name = Opt.to_string mod_p.Mwrite.Person.public_name |> no_html_tags_only_printable in
  let qualifiers = strings_aux mod_p.Mwrite.Person.qualifiers in
  let aliases = strings_aux mod_p.Mwrite.Person.aliases in
  let titles =
    List.map begin fun t ->
      { t_name =
          begin match t.Mwrite.Title.name with
            | Some s -> if s = "" then Tnone else Tname s
            | None -> Tnone
          end
      ; t_ident = begin match t.Mwrite.Title.title with
          | Some s -> s
          | None -> ""
        end
      ; t_place = begin match t.Mwrite.Title.fief with
          | Some s -> s
          | None -> ""
        end
      ; t_date_start = begin match t.Mwrite.Title.date_begin with
          | Some date -> Api_update_util.date_of_piqi_date conf date |> Adef.cdate_of_od
          | None -> Adef.cdate_None
        end
      ; t_date_end = begin match t.Mwrite.Title.date_end with
          | Some date -> Api_update_util.date_of_piqi_date conf date |> Adef.cdate_of_od
          | None -> Adef.cdate_None
        end
      ; t_nth = begin match t.Mwrite.Title.nth with
          | Some i -> Int32.to_int i
          | None -> 0
        end
      }
    end mod_p.Mwrite.Person.titles
  in
  let rparents = fn_rparents mod_p in
  let access = Api_piqi_util.piqi_access_to_access mod_p.Mwrite.Person.access in
  let occupation = Opt.map_default "" only_printable mod_p.Mwrite.Person.occupation in
  let sex =
    match mod_p.Mwrite.Person.sex with
    | `male -> Male
    | `female -> Female
    | `unknown -> Neuter
  in
  let death =
    match mod_p.Mwrite.Person.death_type with
    | `not_dead -> NotDead
    | `dead -> DeadDontKnowWhen
    | `dead_young -> DeadYoung
    | `dead_dont_know_when -> DeadDontKnowWhen
    | `dont_know_if_dead -> DontKnowIfDead
    | `of_course_dead -> OfCourseDead
  in
  let psources = Opt.map_default "" only_printable mod_p.Mwrite.Person.psources in
  let notes =
    Opt.map_default ""
      (fun s -> only_printable_or_nl (Mutil.strip_all_trailing_spaces s))
      mod_p.Mwrite.Person.notes
  in
  let original_pevents =
    (* GeneWeb used to strip empty death event, but we need to do it after conflicts check. *)
    List.map begin fun evt ->
      let name =
        match evt.Mwrite.Pevent.event_perso with
        | Some n -> Epers_Name (no_html_tags (only_printable n))
        | _ ->
          match evt.Mwrite.Pevent.pevent_type with
          | Some x -> Api_piqi_util.pevent_name_of_piqi_pevent_name x
          | _ -> Epers_Name ""
      in
      let date =
        match evt.Mwrite.Pevent.date with
        | Some date -> Api_update_util.date_of_piqi_date conf date
        | None -> None
      in
      let place = Opt.map_default "" (fun p -> no_html_tags (only_printable p)) evt.Mwrite.Pevent.place in
      let reason = Opt.map_default "" (fun r -> no_html_tags (only_printable r)) evt.Mwrite.Pevent.reason in
      let note =
        Opt.map_default
          "" (fun n -> only_printable_or_nl (Mutil.strip_all_trailing_spaces n))
          evt.Mwrite.Pevent.note
      in
      let src = Opt.map_default "" only_printable evt.Mwrite.Pevent.src in
      let witnesses = fn_pevt_witnesses evt in
      { epers_name = name; epers_date = Adef.cdate_of_od date;
        epers_place = place; epers_reason = reason; epers_note = note;
        epers_src = src; epers_witnesses = Array.of_list witnesses }
    end mod_p.Mwrite.Person.pevents
  in
  let (bi, bp, de, bu, pevents) =
    (* [reconstitute_from_pevents] sorts pevents.
       We need to keep the original pevents list in case of error.  *)
    UpdateIndOk.reconstitute_from_pevents original_pevents false
      (Adef.cdate_None, "", "", "")
      (Adef.cdate_None, "", "", "")
      (death, "", "", "")
      (UnknownBurial, "", "", "")
  in
  let (birth, birth_place, birth_note, birth_src) = bi in
  let (baptism, baptism_place, baptism_note, baptism_src) = bp in
  let (death, death_place, death_note, death_src) = de in
  let (burial, burial_place, burial_note, burial_src) = bu in
  (* Maintenant qu'on a propagé les évènements, on a *)
  (* peut-être besoin de refaire un infer_death.     *)
  (* FIXME: do no use the _bb version *)
  let death =
    match death with
    | DontKnowIfDead ->
      Update.infer_death_bb conf (Adef.od_of_cdate birth) (Adef.od_of_cdate baptism)
    | _ -> death
  in
  ( original_pevents
  , { first_name ; surname ; occ ; sex ; access
    ; image
    ; first_names_aliases ; surnames_aliases
    ; public_name ; qualifiers ; aliases
    ; titles
    ; rparents
    ; occupation
    ; related = []
    ; birth ; birth_place ; birth_note ; birth_src
    ; baptism ; baptism_place ; baptism_note ; baptism_src
    ; death ; death_place ; death_note ; death_src
    ; burial ; burial_place ; burial_note ; burial_src
    ; notes
    ; pevents
    ; psources
    ; key_index
    }
  )

let reconstitute_person conf base mod_p
  : ('a, string * string * int * Update.create * string, string) gen_person =
  let fn_occ mod_p =
    match mod_p.Mwrite.Person.create_link with
    | `create ->
      let fn = mod_p.Mwrite.Person.firstname in
      let sn = mod_p.Mwrite.Person.lastname in
      Api_update_util.api_find_free_occ base fn sn
    | _ ->
      (* Cas par défaut, i.e. modifier personne sans changer le occ. *)
      Opt.map_default 0 Int32.to_int mod_p.Mwrite.Person.occ
  in
  let fn_rparents mod_p =
    List.fold_right begin fun r accu ->
      match r.Mwrite.Relation_parent.person with
      | Some person ->
        let r_type =
          match r.Mwrite.Relation_parent.rpt_type with
          | `rpt_adoption_father | `rpt_adoption_mother -> Adoption
          | `rpt_recognition_father | `rpt_recognition_mother -> Recognition
          | `rpt_candidate_parent_father | `rpt_candidate_parent_mother -> CandidateParent
          | `rpt_god_parent_father | `rpt_god_parent_mother -> GodParent
          | `rpt_foster_parent_father | `rpt_foster_parent_mother -> FosterParent
        in
        let (r_fath, r_moth) =
          match person.Mwrite.Person_link.sex with
          | `female -> (None, Some (reconstitute_somebody base person))
          | _ -> (Some (reconstitute_somebody base person), None)
        in
        let r_sources =
          match r.Mwrite.Relation_parent.source with
          | Some s -> s
          | None -> ""
        in
        let r =
          { r_type = r_type; r_fath = r_fath;
            r_moth = r_moth; r_sources = r_sources }
        in
        r :: accu
      | None -> accu
    end mod_p.Mwrite.Person.rparents []
  in
  let fn_pevt_witnesses evt =
    List.fold_right begin fun witness accu ->
      match witness.Mwrite.Witness.person with
      | Some person ->
        let wk =
          match witness.Mwrite.Witness.witness_type with
          | `witness -> Witness
          | `witness_godparent -> Witness_GodParent
          | `witness_officer -> Witness_Officer
        in
        let wit = (reconstitute_somebody base person, wk) in
        wit :: accu
      | None -> accu
    end evt.Mwrite.Pevent.witnesses []
  in
  let original_pevents, p = reconstitute_person_aux conf fn_occ fn_rparents fn_pevt_witnesses mod_p in
  ignore @@ Api_update_util.check_person_conflict base original_pevents p ;
  (* Now, trim and format events *)
  let pevents =
    Mutil.filter_map begin function
      | { epers_name = Epers_Death
        ; epers_place = ""
        ; epers_reason = ""
        ; epers_note = ""
        ; epers_src = ""
        ; epers_witnesses = [||]
        ; epers_date
        }
        when epers_date = Adef.cdate_None && p.death = DontKnowIfDead -> None
      | e ->
        Some { e
               with epers_witnesses =
                      Array.map begin fun ((f, s, o, create, var, _), wk) ->
                        ((f, s, o, create, var), wk)
                      end e.epers_witnesses
             }
    end p.pevents
  in
  let rparents =
    List.map begin fun r ->
      let (r_fath, r_moth) =
        match (r.r_fath, r.r_moth) with
        | (Some (f, s, o, create, var, _), None) ->
          (Some (f, s, o, create, var), None)
        | (None, Some (f, s, o, create, var, _)) ->
          (None, Some (f, s, o, create, var))
        | _ -> failwith "rparents_gw"
      in
      { r  with r_fath ; r_moth }
    end p.rparents
  in
  { p with rparents ; pevents ; related = [] }

(**/**)


let print_add conf base mod_p =
  try
    let sp : ('a, string * string * int * Update.create * string, string) gen_person = reconstitute_person conf base mod_p in
    let sp = {(sp) with key_index = Gwdb.dummy_iper} in
    (* On met à jour les occ. *)
    if sp.occ <> 0 then mod_p.Mwrite.Person.occ <- Some (Int32.of_int sp.occ);
    let sp : ('a, string * string * int * Update.create * string, string) gen_person = UpdateIndOk.strip_person sp in
    match UpdateIndOk.check_person conf (sp : ('a, string * string * int * Update.create * string, string) gen_person) with
    | Some err ->
        (* Correspond au cas ou fn/sn = ""/"?" *)
        (* => ne devrait pas se produire       *)
        Api_update_util.UpdateError err
    | None ->
        let (p, a) = UpdateIndOk.effective_add conf base (sp : ('a, string * string * int * Update.create * string, string) gen_person) in
        let u = {family = get_family (poi base p.key_index)} in
        let wl = UpdateIndOk.all_checks_person base p a u in
        let changed = U_Add_person (Util.string_gen_person base p) in
        let hr = [(fun () -> History.record conf base changed "ap")] in
        Api_update_util.UpdateSuccess (wl, [], hr)
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c

let print_mod_aux conf base ncn mod_p callback =
  try
    let p : ('a, string * string * int * Update.create * string, string) gen_person =
      reconstitute_person conf base mod_p
    in
    let p = UpdateIndOk.strip_person p in
    let ini_ps = UpdateInd.string_person_of base (poi base p.key_index) in
    let digest = Update.digest_person ini_ps in
    if digest = mod_p.Mwrite.Person.digest then
      match match if ncn then None else Update.check_missing_name conf p with
        | Some _ as err -> err
        | None -> Update.check_missing_witnesses_names conf (fun e -> e.epers_witnesses) p.pevents
      with
      | Some err -> Api_update_util.UpdateError err
      | None -> callback p
    else
      let _ = Update.error_digest conf in
      Api_update_util.UpdateError "BaseChanged"
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c

let print_mod ?(no_check_name = false) ?(fexclude = []) conf base mod_p =
  let ip = Gwdb.iper_of_string @@ Int32.to_string mod_p.Mwrite.Person.index in
  let o_p =
    Util.string_gen_person base (gen_person_of_person (poi base ip))
  in
  let callback p =
    begin
      let p = UpdateIndOk.effective_mod conf base p in
      let op = poi base p.key_index in
      let u = {family = get_family op} in
      patch_person base p.key_index p;
      let s =
        let sl =
          [p.notes; p.occupation; p.birth_note; p.birth_src; p.baptism_note;
           p.baptism_src; p.death_note; p.death_src; p.burial_note;
           p.burial_src; p.psources]
        in
        let sl =
          let rec loop l accu =
            match l with
            | [] -> accu
            | evt :: l -> loop l (evt.epers_note :: evt.epers_src :: accu)
          in
          loop (p.pevents) sl
        in
        String.concat " " (List.map (sou base) sl)
      in
      Notes.update_notes_links_db base (NLDB.PgInd p.key_index) s;
      let wl =
        let a = poi base p.key_index in
        let a = {parents = get_parents a; consang = get_consang a} in
        let family =
          Array.of_list @@
          Array.fold_right begin fun ifam acc ->
            if List.mem ifam fexclude then acc else ifam :: acc
          end u.family []
        in
        let u = { family } in
        UpdateIndOk.all_checks_person base p a u
      in
      let changed = U_Modify_person (o_p, (Util.string_gen_person base p)) in
      let hr =
        [(fun () -> History.record conf base changed "mp");
         (fun () ->
           if not (is_quest_string p.surname) &&
              not (is_quest_string p.first_name) &&
              not (is_old_person conf p)
           then
             Update.delete_topological_sort_v conf base
           else ())]
      in
      Api_update_util.UpdateSuccess (wl, [], hr)
    end
  in
  print_mod_aux conf base no_check_name mod_p callback


(**/**) (* Fonctions pour la première saisie, i.e. on n'a pas de base ! *)


(* Comme on n'a pas de base, on va garder une hashtbl des occurrences. *)
let ht_occ = Hashtbl.create 7 ;;

let find_free_occ_nobase fn sn =
  let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
  try
    let occ = Hashtbl.find ht_occ key in
    Hashtbl.replace ht_occ key (succ occ);
    occ
  with Not_found ->
    begin
      let occ = 0 in
      Hashtbl.add ht_occ key (succ occ);
      occ
    end

let reconstitute_person_nobase conf mod_p =
  let fn_occ mod_p =
    match mod_p.Mwrite.Person.create_link with
    | `create_default_occ ->
      let fn = mod_p.Mwrite.Person.firstname in
      let sn = mod_p.Mwrite.Person.lastname in
      find_free_occ_nobase fn sn
    | `create ->
      begin match mod_p.Mwrite.Person.occ with
        | Some occ -> Int32.to_int occ
        | None -> 0
      end
    | `link ->
      failwith "ErrorAddPersonNoBase"
  in
  let fn_rparents _ = [] in
  let fn_pevt_witnesses _ = [] in
  let _, p = reconstitute_person_aux conf fn_occ fn_rparents fn_pevt_witnesses mod_p in
  { p with pevents = List.filter begin fun e ->
        e.epers_name <> Epers_Death
        || e.epers_place <> ""
        || e.epers_reason <> ""
        || e.epers_note <> ""
        || e.epers_src <> ""
        || e.epers_witnesses <> [||]
        || e.epers_date <> Adef.cdate_None
        || p.death = DontKnowIfDead
      end p.pevents }

let print_add_nobase conf mod_p =
  try
    let sp = reconstitute_person_nobase conf mod_p in
    let sp = {(sp) with key_index = Gwdb.dummy_iper} in
    (* On met à jour les occ. *)
    if sp.occ <> 0 then mod_p.Mwrite.Person.occ <- Some (Int32.of_int sp.occ);
    let _sp = UpdateIndOk.strip_person sp in
    (* On ne vérifie pas ici si le prénom de la personne est vide, mais *)
    (* on le fait plus haut, pour savoir si c'est un oubli ou si l'on   *)
    (* ne connait pas la personne.                                      *)
    (* On n'appelle pas CheckItem car ils ne sont pas révélateurs *)
    Api_update_util.UpdateSuccess ([], [], [])
  with
  | Update.ModErr s -> Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
