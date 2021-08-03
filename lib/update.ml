(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

exception ModErr of string

type create_info =
  { ci_birth_date : date option;
    ci_birth_place : string;
    ci_death : death;
    ci_death_date : date option;
    ci_death_place : string;
    ci_occupation : string;
    ci_public : bool }
type create =
    Create of sex * create_info option
  | Link
type key = string * string * int * create * string

let infer_death_from_age a =
  if a > 120 then OfCourseDead (*TODO private_years??? *)
  else DontKnowIfDead

let infer_death_from_date conf d =
  infer_death_from_age (Date.time_elapsed d conf.today).year

let infer_death_from_odate conf = function
  | Some (Dgreg (d, _)) -> infer_death_from_date conf d
  | _ -> DontKnowIfDead

let infer_death_bb conf birth bapt =
  match infer_death_from_odate conf birth with
  | DontKnowIfDead -> infer_death_from_odate conf bapt
  | death -> death

let infer_death_from_parents conf base fam =
  let fath = poi base @@ get_father fam in
  let moth = poi base @@ get_mother fam in
  let aux = function
    | (Death (_, d),_) | (_, Death (_, d)) ->
      infer_death_from_odate conf (Adef.od_of_cdate d)
    | _ ->
      match
        ( Adef.od_of_cdate @@ get_birth moth
        , Adef.od_of_cdate @@ get_birth fath )
      with
      | (Some (Dgreg (d, _)), _) | (_, Some (Dgreg (d, _))) ->
        infer_death_from_age @@ (Date.time_elapsed d conf.today).year - 120
      | _ -> DontKnowIfDead
  in
  match get_death fath, get_death moth with
  | (Death (_, d1) as a), (Death (_, d2) as b) ->
    begin match Adef.od_of_cdate d1, Adef.od_of_cdate d2 with
      | Some (Dgreg (d1, _) as d1'), Some (Dgreg (d2, _)as d2') ->
        if Date.compare_date d1' d2' > 0
        then infer_death_from_date conf d2
        else infer_death_from_date conf d1
      | Some (Dgreg (d, _)), _ | _, Some (Dgreg (d, _)) -> infer_death_from_date conf d
      | _ -> aux (a, b)
    end
  | a, b -> aux (a, b)

let rec infer_death conf base p =
  let death =
    infer_death_bb conf
      (Adef.od_of_cdate (get_birth p))
      (Adef.od_of_cdate (get_baptism p))
  in
  if death <> DontKnowIfDead then death
  else
    let death =
      let families = get_family p in
      let len = Array.length families in
      let rec loop_families i =
        if i = len then DontKnowIfDead
        else
          let fam = foi base families.(i) in
          match Adef.od_of_cdate (get_marriage fam) with
          (* TODO this 120 should be related to private_years_marriage *)
          | Some (Dgreg (d, _)) when (Date.time_elapsed d conf.today).year > 120 ->
            OfCourseDead
          | _ ->
            let death =
              let children = get_children fam in
              let len = Array.length children in
              let rec loop_children j =
                if j = len then DontKnowIfDead
                else
                  let death = infer_death conf base (poi base children.(j)) in
                  if death = OfCourseDead then OfCourseDead
                  else loop_children (j + 1)
              in
              loop_children 0
            in
            if death = OfCourseDead then OfCourseDead
            else loop_families (i + 1)
      in
      loop_families 0
    in
    if death <> DontKnowIfDead then death
    else
      match get_parents p with
      | None -> DontKnowIfDead
      | Some ifam -> infer_death_from_parents conf base (foi base ifam)

(*let restrict_to_small_list el =
  let rec begin_list n rl el =
     TODO suppress this limit
    if n > 25 then
      let rec end_list n sl el =
        if n > 25 then List.rev_append rl (None :: sl)
        else
          match el with
            e :: el -> end_list (n + 1) (Some e :: sl) el
          | [] -> List.rev_append rl sl
      in
      end_list 0 [] (List.rev el)
    else
      match el with
        e :: el -> begin_list (n + 1) (Some e :: rl) el
      | [] -> List.rev rl
  in
  begin_list 0 [] el*)

(* ************************************************************************** *)
(*  [Fonc] print_person_parents_and_spouses :
             config -> base -> person -> unit                                 *)
(** [Description] : Print several information to distinguish homonyms. The
      information includes name of the person, name of the parents,
      name of the spouse.
    [Args] :
      - conf : configuration of the base
      - base : base
      - p    : person
    [Retour] : unit
    [Rem] : Not visible.                                                      *)
(* ************************************************************************** *)
let print_person_parents_and_spouse conf base p =
  Output.printf conf "<a href=\"%s%s\">" (commd conf) (acces conf base p);
  Output.printf conf "%s.%d %s" (p_first_name base p) (get_occ p)
    (p_surname base p);
  Output.print_string conf "</a>";
  Output.print_string conf (DateDisplay.short_dates_text conf base p);
  let cop = Util.child_of_parent conf base p in
  if (String.length cop) > 0 then Output.printf conf ", %s" cop;
  let hbw = Util.husband_wife conf base p true in
  if (String.length hbw) > 0 then Output.printf conf ", %s" hbw;
  Output.print_string conf ".\n"

let print_same_name conf base p =
  match Gutil.find_same_name base p with
    [_] -> ()
  | pl ->
      Output.print_string conf "<p>\n";
      Output.printf conf "%s%s\n"
        (Utf8.capitalize_fst (transl conf "persons having the same name"))
        (transl conf ":");
      Output.print_string conf "<ul>\n";
      List.iter
        (fun p ->
           Output.print_string conf "<li>";
           print_person_parents_and_spouse conf base p;
           Output.print_string conf "</li>")
        pl;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</p>\n"


(* ************************************************************************* *)
(*  [Fonc] is_label_note : string -> bool                                    *)
(** [Description] : Test si le label contient le mot 'note' pour savoir si
      dans les évènement secondaires, il faut traiter la note comme un
      textarea.
    [Args] :
      - lbl : le label
    [Retour] :
      - bool
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let is_label_note lbl =
  let rec loop i =
    if i = String.length lbl then false
    else if lbl.[i] = 'n' then
      let note = "note" in
      if String.length note + i <= String.length lbl then
        let sub_x = String.sub lbl i (String.length note) in
        if sub_x = note then true else loop (i + String.length note)
      else false
    else loop (i + 1)
  in
  loop 0

let print_aux conf param value submit =
  Output.printf conf {|<p><form method="post" action="%s">|} conf.command;
  List.iter begin fun (x, v) ->
    (* Only textarea can contain newline. *)
    Output.printf conf {|<textarea style="display:none;" name="%s">|} x ;
    Output.print_string conf (Util.escape_html (Mutil.decode v)) ;
    Output.print_string conf "</textarea>"
  end (conf.henv @ conf.env) ;
  Output.printf conf {|<input type="hidden" name="%s" value="%s">|} param value ;
  Output.printf conf {|<input type="submit" value="%s">|} submit ;
  Output.print_string conf {|</form></p>|}

let print_return conf =
  print_aux conf "return" "ok" (Utf8.capitalize_fst (transl conf "back"))

let print_continue
    conf ?(continue = Utf8.capitalize_fst (transl conf "continue")) param value =
  print_aux conf param value continue

let prerr conf _err fn =
#ifdef API
  if not conf.api_mode then begin
#endif
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title ;
  fn () ;
  Hutil.trailer conf;
  Output.flush conf ;
#ifdef API
  end ;
#endif
  raise @@ ModErr _err

let print_err_unknown conf _base (f, s, o) =
  let err =
    Printf.sprintf "%s%s <strong>%s.%d %s</strong>\n"
      (Utf8.capitalize_fst (transl conf "unknown person")) (transl conf ":") f o s
  in
  prerr conf err @@ fun () ->
  Output.print_string conf err;
  print_return conf

let delete_topological_sort_v conf _base =
  let bfile = Util.bpath (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab_visitor" in
  Mutil.rm tstab_file ;
  let tstab_file = Filename.concat bfile "restrict" in
  Mutil.rm tstab_file

let delete_topological_sort conf base =
  let _ = delete_topological_sort_v conf base in
  let bfile = Util.bpath (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab" in
  Mutil.rm tstab_file

let print_someone conf base p =
  Output.printf conf "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print_first_name conf base p =
  Output.printf conf "%s%s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))

let print_someone_strong _conf base p =
  Printf.sprintf "<strong>%s%s %s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print_first_name_strong conf base p =
  Output.printf conf "<strong>%s%s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))

let string_of_error conf base = function
  | AlreadyDefined p ->
    Printf.sprintf
      (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
      ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
       p_surname base p ^ "\"")
      (fun _ ->
         Printf.sprintf "<a href=\"%s%s\">" (commd conf) (acces conf base p))
      (fun _ -> "</a>.")
  | OwnAncestor p ->
    Printf.sprintf "%s\n%s" (print_someone_strong conf base p)
      (transl conf "would be his/her own ancestor")
  | BadSexOfMarriedPerson _ ->
    Printf.sprintf "%s."
      (Utf8.capitalize_fst (transl conf "cannot change sex of a married person"))

let print_error conf base e =
  Output.print_string conf @@ string_of_error conf base e

let someone_ref_text conf base p =
  "<a href=\"" ^ commd conf ^ acces conf base p ^ "\">\n" ^
  p_first_name base p ^
  (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p)) ^ " " ^
  p_surname base p ^ "</a>"

let print_list_aux conf base title list printer =
  if list <> [] then begin
    Output.printf conf "%s\n<ul>" (Utf8.capitalize_fst (transl conf title)) ;
    printer conf base list ;
    Output.print_string conf "</ul>";
  end

let print_warning conf base =
  function
  | BigAgeBetweenSpouses (p1, p2, a) ->
      Output.printf conf
        (fcapitale
           (ftransl conf
              "the difference of age between %t and %t is quite important"))
        (fun _ -> print_someone_strong conf base p1)
        (fun _ -> print_someone_strong conf base p2);
      Output.printf conf ": %s" (DateDisplay.string_of_age conf a)
  | BirthAfterDeath p ->
      Output.printf conf (ftransl conf "%t died before his/her birth")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
  | ChangedOrderOfChildren (ifam, _, before, after) ->
      let cpl = foi base ifam in
      let fath = poi base (get_father cpl) in
      let moth = poi base (get_mother cpl) in
      Output.printf conf "%s\n"
        (Utf8.capitalize_fst (transl conf "changed order of children"));
      Output.print_string conf "-&gt;\n";
      Output.print_string conf
        (someone_ref_text conf base fath ^ "\n" ^ transl_nth conf "and" 0 ^
         " " ^ someone_ref_text conf base moth ^ "\n");
      let print_list arr diff_arr =
        Array.iteri
          (fun i (* i *)p ->
             let p = poi base p in
             Output.printf conf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             if eq_istr (get_surname p) (get_surname fath) then
               print_first_name conf base p
             else print_someone conf base p;
             Output.print_string conf (DateDisplay.short_dates_text conf base p);
             Output.print_string conf "\n";
             Output.print_string conf "</li>\n")
          arr
      in
      let (bef_d, aft_d) = Difference.f before after in
      Output.print_string conf "<table style=\"margin:1em\">\n";
      Output.print_string conf "<tr>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "</tr>\n";
      Output.print_string conf "</table>\n"
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      let cpl = foi base ifam in
      Output.printf conf
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are not in order"))
        (fun _ -> print_someone_strong conf base (poi base (get_father cpl)))
        (fun _ -> print_someone_strong conf base (poi base (get_mother cpl)));
      Output.print_string conf ":\n";
      Output.print_string conf "<ul>\n";
      Output.print_string conf "<li>";
      print_first_name_strong conf base elder;
      Output.print_string conf (DateDisplay.short_dates_text conf base elder);
      Output.print_string conf "</li>";
      Output.print_string conf "<li>";
      print_first_name_strong conf base x;
      Output.print_string conf (DateDisplay.short_dates_text conf base x);
      Output.print_string conf "</li>";
      Output.print_string conf "</ul>\n"
  | ChangedOrderOfMarriages (p, before, after) ->
      Output.printf conf "%s\n"
        (Utf8.capitalize_fst (transl conf "changed order of marriages"));
      Output.print_string conf "-&gt;\n";
      let print_list arr diff_arr =
        Array.iteri
          (fun i ifam ->
             let fam = foi base ifam in
             let sp = Gutil.spouse (get_iper p) fam in
             let sp = poi base sp in
             Output.printf conf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             print_first_name conf base p;
             Output.print_string conf "  &amp;";
             Output.printf conf "%s\n"
               (DateDisplay.short_marriage_date_text conf base fam p sp);
             print_someone conf base sp;
             Output.print_string conf "\n";
             Output.print_string conf "</li>\n")
          arr
      in
      let (bef_d, aft_d) = Difference.f before after in
      Output.print_string conf "<table style=\"margin:1em\">\n";
      Output.print_string conf "<tr>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "</tr>\n";
      Output.print_string conf "</table>\n"
  | ChangedOrderOfFamilyEvents (_, before, after) ->
      Output.printf conf "%s\n"
        (Utf8.capitalize_fst (transl conf "changed order of family's events"));
      Output.print_string conf "-&gt;\n";
      let print_list arr diff_arr =
        Array.iteri
          (fun i evt ->
             let name = Util.string_of_fevent_name conf base evt.efam_name in
             Output.printf conf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             Output.printf conf "%s\n" name;
             Output.print_string conf "</li>\n")
          arr
      in
      let before = Array.of_list before in
      let after = Array.of_list after in
      let (bef_d, aft_d) = Difference.f before after in
      Output.print_string conf "<table style=\"margin:1em\">\n";
      Output.print_string conf "<tr>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "</tr>\n";
      Output.print_string conf "</table>\n"
  | ChangedOrderOfPersonEvents (_, before, after) ->
      Output.printf conf "%s\n"
        (Utf8.capitalize_fst (transl conf "changed order of person's events"));
      Output.print_string conf "-&gt;\n";
      let print_list arr diff_arr =
        Array.iteri
          (fun i evt ->
             let name = Util.string_of_pevent_name conf base evt.epers_name in
             Output.printf conf "<li %s>\n"
               (if diff_arr.(i) then "style=\"background:pink\"" else "");
             Output.printf conf "%s\n" name;
             Output.print_string conf "</li>\n")
          arr
      in
      let before = Array.of_list before in
      let after = Array.of_list after in
      let (bef_d, aft_d) = Difference.f before after in
      Output.print_string conf "<table style=\"margin:1em\">\n";
      Output.print_string conf "<tr>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list before bef_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "<td>\n";
      Output.print_string conf "<ul style=\"list-style-type:none\">\n";
      print_list after aft_d;
      Output.print_string conf "</ul>\n";
      Output.print_string conf "</td>\n";
      Output.print_string conf "</tr>\n";
      Output.print_string conf "</table>\n"
  | CloseChildren (ifam, c1, c2) ->
      let cpl = foi base ifam in
      Output.printf conf
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are born very close"))
        (fun _ -> print_someone_strong conf base (poi base (get_father cpl)))
        (fun _ -> print_someone_strong conf base (poi base (get_mother cpl)));
      Output.print_string conf ":\n";
      Output.print_string conf "<ul>\n";
      Output.print_string conf "<li>";
      print_first_name_strong conf base c1;
      Output.print_string conf (DateDisplay.short_dates_text conf base c1);
      Output.print_string conf "</li>";
      Output.print_string conf "<li>";
      print_first_name_strong conf base c2;
      Output.print_string conf (DateDisplay.short_dates_text conf base c2);
      Output.print_string conf "</li>";
      Output.print_string conf "</ul>\n"
  | DistantChildren (ifam, p1, p2) ->
      let cpl = foi base ifam in
      Output.printf conf
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are born very distant"))
        (fun _ -> print_someone_strong conf base (poi base (get_father cpl)))
        (fun _ -> print_someone_strong conf base (poi base (get_mother cpl)));
      Output.print_string conf ":\n";
      Output.print_string conf "<ul>\n";
      Output.print_string conf "<li>";
      print_first_name_strong conf base p1;
      Output.print_string conf (DateDisplay.short_dates_text conf base p1);
      Output.print_string conf "</li>";
      Output.print_string conf "<li>";
      print_first_name_strong conf base p2;
      Output.print_string conf (DateDisplay.short_dates_text conf base p2);
      Output.print_string conf "</li>";
      Output.print_string conf "</ul>\n"
  | DeadOld (p, a) ->
      Output.printf conf "%s\n%s\n" (print_someone_strong conf base p)
        (transl_nth conf "died at an advanced age"
           (index_of_sex (get_sex p)));
      Output.printf conf "(%s)" (DateDisplay.string_of_age conf a)
  | DeadTooEarlyToBeFather (father, child) ->
      Output.printf conf
        (ftransl conf "%t is born more than 2 years after the death of his/her father %t")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base child)
             (DateDisplay.short_dates_text conf base child))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base father)
             (DateDisplay.short_dates_text conf base father))
  | FEventOrder (p, e1, e2) ->
      Output.printf conf (fcapitale (ftransl conf "%t's %s before his/her %s"))
        (fun _ -> print_someone_strong conf base p)
        (Util.string_of_fevent_name conf base e1.efam_name)
        (Util.string_of_fevent_name conf base e2.efam_name)
  | FWitnessEventAfterDeath (p, e, _origin) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s after his/her death"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
        (Util.string_of_fevent_name conf base e.efam_name)
  | FWitnessEventBeforeBirth (p, e, _origin) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s before his/her birth"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
        (Util.string_of_fevent_name conf base e.efam_name)
  | IncoherentSex (p, _, _) ->
      Output.printf conf
        (fcapitale
           (ftransl conf "%t's sex is not coherent with his/her relations"))
        (fun _ -> print_someone_strong conf base p)
  | IncoherentAncestorDate (anc, p) ->
      Output.printf conf "%s has a younger ancestor %s"
        (print_someone_strong conf base p)
        (print_someone_strong conf base anc)
  | MarriageDateAfterDeath p ->
      Output.printf conf
        (fcapitale
           (ftransl conf "marriage had occurred after the death of %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
  | MarriageDateBeforeBirth p ->
      Output.printf conf
        (fcapitale
           (ftransl conf "marriage had occurred before the birth of %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
  | MotherDeadBeforeChildBirth (mother, child) ->
      Output.printf conf
        (ftransl conf "%t is born after the death of his/her mother %t")
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base child)
             (DateDisplay.short_dates_text conf base child))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base mother)
             (DateDisplay.short_dates_text conf base mother))
  | ParentBornAfterChild (p, c) ->
      Output.printf conf "%s\n%s\n%s" (print_someone_strong conf base p)
        (transl conf "is born after his/her child")
        (print_someone_strong conf base c)
  | ParentTooYoung (p, a, _) ->
      Output.printf conf "%s\n%s\n" (print_someone_strong conf base p)
        (transl conf "is a very young parent");
      Output.printf conf "(%s)" (DateDisplay.string_of_age conf a)
  | ParentTooOld (p, a, _) ->
      Output.printf conf "%s\n%s\n" (print_someone_strong conf base p)
        (transl conf "is a very old parent");
      Output.printf conf "(%s)" (DateDisplay.string_of_age conf a)
  | PossibleDuplicateFam (f1, _) ->
    let f = foi base f1 in
    Output.printf conf
      (fcapitale (ftransl conf "%s and %s have several unions"))
      (print_someone_strong conf base @@ poi base @@ get_father f)
      (print_someone_strong conf base @@ poi base @@ get_mother f)
  | PEventOrder (p, e1, e2) ->
      Output.printf conf (fcapitale (ftransl conf "%t's %s before his/her %s"))
        (fun _ -> print_someone_strong conf base p)
        (Util.string_of_pevent_name conf base e1.epers_name)
        (Util.string_of_pevent_name conf base e2.epers_name)
  | PWitnessEventAfterDeath (p, e, _origin) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s after his/her death"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
        (Util.string_of_pevent_name conf base e.epers_name)
  | PWitnessEventBeforeBirth (p, e, _origin) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s before his/her birth"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
        (Util.string_of_pevent_name conf base e.epers_name)
  | TitleDatesError (p, t) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t has incorrect title dates: %t"))
        (fun _ ->
           Printf.sprintf "%s%s" (print_someone_strong conf base p)
             (DateDisplay.short_dates_text conf base p))
        (fun _ ->
           Printf.sprintf "<strong>%s %s</strong> <em>%s-%s</em>"
             (sou base t.t_ident) (sou base t.t_place)
             (match Adef.od_of_cdate t.t_date_start with
                Some d -> DateDisplay.string_of_date conf d
              | _ -> "")
             (match Adef.od_of_cdate t.t_date_end with
                Some d -> DateDisplay.string_of_date conf d
              | _ -> ""))
  | UndefinedSex p ->
      Output.printf conf (fcapitale (ftransl conf "undefined sex for %t"))
        (fun _ -> print_someone_strong conf base p)
  | YoungForMarriage (p, a, _)
  | OldForMarriage (p, a, _) ->
      Output.printf conf "%s\n" (print_someone_strong conf base p);
      Output.printf conf (ftransl conf "married at age %t")
        (fun _ -> DateDisplay.string_of_age conf a)

let print_warnings conf base wl =
  print_list_aux conf base "warnings" wl @@ fun conf base wl ->
  (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
  (* levé par plusieurs fonctions différents selon le context.       *)
  let wl = List.sort_uniq compare wl in
  List.iter
    (fun w ->
       Output.print_string conf "<li>" ;
       print_warning conf base w ;
       Output.print_string conf "</li>" )
    wl

(* ************************************************************************* *)
(*  [Fonc] print_misc : config -> base -> Def.misc -> unit                   *)
(** [Description] : Fonction d'impression des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - fun  : Def.misc (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let print_misc conf _base =
  function
    MissingSources ->
      Output.print_string conf "<em>";
      Output.printf conf "%s\n" (Utf8.capitalize_fst (transl conf "missing sources"));
      Output.print_string conf "</em>"

(* ************************************************************************* *)
(*  [Fonc] print_miscs : config -> base -> Def.misc list -> unit             *)
(** [Description] : Affiche la liste des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - ml   : Def.misc list (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
let print_miscs conf base ml =
  print_list_aux conf base "miscellaneous informations" ml @@ fun conf base ->
  List.iter
    (fun m -> Output.print_string conf "<li>" ; print_misc conf base m ; Output.print_string conf "</li>")

(* ************************************************************************* *)
(*  [Fonc] print_miscs :
      config -> base -> (Def.warning list * Def.misc list) -> unit           *)
(** [Description] : Affiche sous la même rubrique, la liste des warnings
                    et la liste des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - wl   : Def.warning list
      - ml   : Def.misc list (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                              *)
(* ************************************************************************* *)
let print_warnings_and_miscs conf base wl ml =
  if wl <> [] || ml <> [] then begin
    Output.printf conf "%s\n" (Utf8.capitalize_fst (transl conf "warnings"));
    Output.print_string conf "<ul>\n";
    List.iter
      (fun w ->
         Output.print_string conf "<li>" ;
         print_warning conf base w ;
         Output.print_string conf "</li>")
      wl ;
    List.iter
      (fun m ->
         Output.print_string conf "<li>" ;
         print_misc conf base m ;
         Output.print_string conf "</li>")
      ml;
    Output.print_string conf "</ul>\n"
  end

let error conf base x =
  let err = string_of_error conf base x in
  prerr conf err @@ fun () ->
  Output.print_string conf err;
  Output.print_string conf "\n";
  print_return conf

let error_locked conf =
  let title _ = Output.print_string conf (Utf8.capitalize_fst (transl conf "error")) in
  Hutil.rheader conf title;
  Output.print_string conf "<p>\n";
  Output.printf conf
    (fcapitale
       (ftransl conf "the file is temporarily locked: please try again"));
  Output.print_string conf ".\n";
  Output.print_string conf "</p>\n";
  Output.print_string conf "<table>\n";
  Output.print_string conf "<tr>\n";
  Output.print_string conf "<td>\n";
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       if x = "retry" then ()
       else if x = "notes" || is_label_note x then
         begin
           Output.printf conf "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Output.print_string conf (Util.escape_html (Mutil.decode v));
           Output.print_string conf "</textarea>\n"
         end
       else
         Output.printf conf "<input type=\"hidden\" name=\"%s\" value=\"%s\">\n"
           x (Util.escape_html (Mutil.decode v)))
    (conf.henv @ conf.env);
  (* just to see in the traces... *)
  Output.printf conf "<input type=\"hidden\" name=\"retry\" value=\"%s\">\n"
    (Util.escape_html conf.user);
  Output.printf conf "<input type=\"submit\" value=\"%s\">\n"
    (Utf8.capitalize_fst (transl conf "try again"));
  Output.print_string conf "</form>\n";
  Output.print_string conf "</td>\n";
  Output.print_string conf "<td>\n";
  Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       Output.printf conf "<input type=\"hidden\" name=\"%s\" value=\"%s\">\n" x
         (Util.escape_html (Mutil.decode v)))
    conf.henv;
  begin let ip =
    match p_getenv conf.env "ip" with
      Some ip -> Some ip
    | None -> p_getenv conf.env "i"
  in
    match ip with
      Some n ->
        Output.printf conf "<input type=\"hidden\" name=\"i\" value=\"%s\">\n" n
    | None -> ()
  end;
  Output.printf conf "<input type=\"submit\" value=\"%s\">\n"
    (Utf8.capitalize_fst (transl_nth conf "user/password/cancel" 2));
  Output.print_string conf "</form>\n";
  Output.print_string conf "</td>\n";
  Output.print_string conf "</tr>\n";
  Output.print_string conf "</table>\n";
  Hutil.trailer conf

let error_digest conf =
  let err =
    Printf.sprintf @@
    fcapitale (ftransl conf "the base has changed; do \"back\", \"reload\", and refill the form")
  in
  prerr conf err @@ fun () ->
  Hutil.print_link_to_welcome conf true;
  Output.printf conf "<p>%s.\n</p>\n" err

let digest_person p = Iovalue.digest p
let digest_family (fam, cpl, des) = Iovalue.digest (fam, cpl, des)

let get var key env =
  match p_getenv env (var ^ "_" ^ key) with
    Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let get_number var key env =
  match p_getint env (var ^ "_" ^ key) with
  | Some x when x <> 0 -> Some x
  | _ -> None

let bad_date conf d =
  let err =
    Printf.sprintf
      "%s%s%a\n"
      (Utf8.capitalize_fst (transl conf "incorrect date"))
      (transl conf ":")
      (fun _ -> function
         | {day = 0; month = 0; year = a} -> Printf.sprintf "%d" a
         | {day = 0; month = m; year = a} -> Printf.sprintf "%d/%d" m a
         | {day = j; month = m; year = a} -> Printf.sprintf "%d/%d/%d" j m a)
      d
  in
  prerr conf err @@ fun () ->
  Output.print_string conf err

let int_of_field s =
  match int_of_string (String.trim s) with
  | exception Failure _ -> None
  | x -> if x <> 0 then Some x else None

let reconstitute_date_dmy2 conf var =
  let m =
    let m = get var "ormonth" conf.env in
    match String.uppercase_ascii m with
      "VD" -> Some 1
    | "BR" -> Some 2
    | "FM" -> Some 3
    | "NI" -> Some 4
    | "PL" -> Some 5
    | "VT" -> Some 6
    | "GE" -> Some 7
    | "FL" -> Some 8
    | "PR" -> Some 9
    | "ME" -> Some 10
    | "TH" -> Some 11
    | "FT" -> Some 12
    | "JC" -> Some 13
    | _ -> int_of_field m
  in
  match get_number var "oryear" conf.env with
    Some y ->
      begin match m with
        Some m ->
          begin match get_number var "orday" conf.env with
            Some d ->
              let dmy2 = {day2 = d; month2 = m; year2 = y; delta2 = 0} in
              if dmy2.day2 >= 1 && dmy2.day2 <= 31 && dmy2.month2 >= 1 &&
                 dmy2.month2 <= 13
              then
                dmy2
              else let d = Date.dmy_of_dmy2 dmy2 in bad_date conf d
          | None ->
              let dmy2 = {day2 = 0; month2 = m; year2 = y; delta2 = 0} in
              if dmy2.month2 >= 1 && dmy2.month2 <= 13 then dmy2
              else let d = Date.dmy_of_dmy2 dmy2 in bad_date conf d
          end
      | None -> {day2 = 0; month2 = 0; year2 = y; delta2 = 0}
      end
  | None -> raise @@ ModErr (__FILE__ ^ " " ^ string_of_int __LINE__)

let reconstitute_date_dmy conf var =
  let (prec, y) =
    let y = get var "yyyy" conf.env in
    let prec = p_getenv conf.env (var ^ "_prec") in
    let len = String.length y in
    if len > 1 then
      match y.[0], y.[len-1] with
        '?', _ -> Some "maybe", String.sub y 1 (len - 1)
      | '~', _ -> Some "about", String.sub y 1 (len - 1)
      | '/', '/' -> Some "about", String.sub y 1 (len - 2)
      | '<', _ | '/', _ -> Some "before", String.sub y 1 (len - 1)
      | '>', _ -> Some "after", String.sub y 1 (len - 1)
      | _, '/' -> Some "after", String.sub y 0 (len - 1)
      | _ -> prec, y
    else prec, y
  in
  let (force_f_cal, m) =
    let m = get var "mm" conf.env in
    match String.uppercase_ascii m with
      "VD" -> true, Some 1
    | "BR" -> true, Some 2
    | "FM" -> true, Some 3
    | "NI" -> true, Some 4
    | "PL" -> true, Some 5
    | "VT" -> true, Some 6
    | "GE" -> true, Some 7
    | "FL" -> true, Some 8
    | "PR" -> true, Some 9
    | "ME" -> true, Some 10
    | "TH" -> true, Some 11
    | "FT" -> true, Some 12
    | "JC" -> true, Some 13
    | _ -> false, int_of_field m
  in
  let d =
    match int_of_field y with
      Some y ->
        let prec =
          match prec with
            Some "about" -> About
          | Some "maybe" -> Maybe
          | Some "before" -> Before
          | Some "after" -> After
          | Some "oryear" ->
              begin match get_number var "oryear" conf.env with
                Some _ ->
                  let dmy2 = reconstitute_date_dmy2 conf var in OrYear dmy2
              | None -> Sure
              end
          | Some "yearint" ->
              begin match get_number var "oryear" conf.env with
                Some _ ->
                  let dmy2 = reconstitute_date_dmy2 conf var in YearInt dmy2
              | None -> Sure
              end
          | _ -> Sure
        in
        begin match m with
          Some m ->
            begin match get_number var "dd" conf.env with
              Some d ->
                let d =
                  {day = d; month = m; year = y; prec = prec; delta = 0}
                in
                if d.day >= 1 && d.day <= 31 && d.month >= 1 && d.month <= 13
                then
                  Some d
                else bad_date conf d
            | None ->
                let d =
                  {day = 0; month = m; year = y; prec = prec; delta = 0}
                in
                if d.month >= 1 && d.month <= 13 then Some d
                else bad_date conf d
            end
        | None -> Some {day = 0; month = 0; year = y; prec = prec; delta = 0}
        end
    | None -> None
  in
  d, force_f_cal

let check_missing_name conf p =
  if p.first_name = "" || p.first_name = "?"
  then Some (transl conf "first name missing")
  else if p.surname = "" || p.surname = "?"
  then Some (transl conf "surname missing")
  else None

let check_missing_witnesses_names conf get list =
  let aux witnesses =
    let len = Array.length witnesses in
    let rec loop i =
      if i = len then None
      else begin
        let ((fn, sn, _, _, _), _) = Array.get witnesses i in
        if fn = "" && sn = "" then loop (i + 1)
        else if fn = "" || fn = "?" then
          Some
            (transl_nth conf "witness/witnesses" 0 ^ " : " ^
             transl conf "first name missing")
        else if sn = "" || sn = "?" then
          Some
            (transl_nth conf "witness/witnesses" 0 ^ " : " ^
             transl conf "surname missing")
        else loop (i + 1)
      end
    in
    loop 0
  in
  let rec loop = function
    | [] -> None
    | hd :: tl ->
      match aux (get hd) with
      | Some _ as err -> err
      | None -> loop tl
  in
  loop list

let check_greg_day conf d =
  if d.day > Date.nb_days_in_month d.month d.year then bad_date conf d

let reconstitute_date conf var =
  match reconstitute_date_dmy conf var with
    Some d, false ->
      let (d, cal) =
        match p_getenv conf.env (var ^ "_cal") with
          Some "G" | None -> check_greg_day conf d; d, Dgregorian
        | Some "J" -> Calendar.gregorian_of_julian d, Djulian
        | Some "F" -> Calendar.gregorian_of_french d, Dfrench
        | Some "H" -> Calendar.gregorian_of_hebrew d, Dhebrew
        | _ -> d, Dgregorian
      in
      Some (Dgreg (d, cal))
  | Some d, true -> Some (Dgreg (Calendar.gregorian_of_french d, Dfrench))
  | None, _ ->
      match p_getenv conf.env (var ^ "_text") with
        Some _ ->
          let txt = only_printable (get var "text" conf.env) in
          if txt = "" then None else Some (Dtext txt)
      | _ -> None

let parse_int s i =
  let j =
    let rec loop j =
      if j = String.length s
      || match String.unsafe_get s j with '0'..'9' -> false | _ -> true
      then j
      else loop (j + 1)
    in
    loop i
  in
  (int_of_string @@ String.sub s i (j - i), j)

let text_of_var conf =
  function
  | "pa1" -> transl_nth conf "him/her" 0
  | "pa2" -> transl_nth conf "him/her" 1
  | var -> match String.get var 0 with
    | 'r' ->
      let (pos, i) = parse_int var 1 in
      assert (String.get var i = '_') ;
      let pn = match String.get var (i + 1) with 'f' -> 0 | 'm' -> 1 | _ -> assert false in
      transl_nth conf "relation/relations" 0 ^ " " ^ string_of_int pos
      ^ " - " ^ transl_nth conf "father/mother" pn
    | 'e' ->
      let (epos, i) = parse_int var 1 in
      assert (String.get var i = '_') ;
      assert (String.get var (i + 1) = 'w') ;
      assert (String.get var (i + 2) = 'i') ;
      assert (String.get var (i + 3) = 't') ;
      assert (String.get var (i + 4) = 'n') ;
      let (wpos, _) = parse_int var (i + 5) in
      let a = transl_nth conf "witness/witnesses" 0 ^ " " ^ string_of_int wpos in
      let b = transl_nth conf "event/events" 0 ^ " " ^ string_of_int epos in
      transl_a_of_b conf a b b
    | 'c' when String.length var >= 3 && String.unsafe_get var 1 = 'h' ->
      let (pos, _) = parse_int var 2 in
      Util.translate_eval (transl_nth conf "child/children" 0)
      ^ " " ^ string_of_int pos
    | _ -> var

let print_create_conflict conf base p var =
  let err =
    Printf.sprintf
      (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
      ("\"" ^ p_first_name base p ^ "." ^ string_of_int (get_occ p) ^ " " ^
       p_surname base p ^ "\" (" ^ text_of_var conf var ^ ")")
      (fun _ ->
         Printf.sprintf "<a href=\"%s%s\">" (commd conf) (acces conf base p))
      (fun _ -> "</a>.");
  in
  prerr conf err @@ fun () ->
  Output.print_string conf err ;
  let free_n =
    Gutil.find_free_occ base (p_first_name base p) (p_surname base p) 0
  in
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  List.iter
    (fun (x, v) ->
       (* Seul un textarea peut contenir des sauts de ligne. *)
       (* On remplace donc l'input par un textarea.          *)
       if x = "notes" || is_label_note x then
         begin
           Output.printf conf "<textarea style=\"display:none;\" name=\"%s\">\n"
             x;
           Output.print_string conf (Util.escape_html (Mutil.decode v));
           Output.print_string conf "</textarea>\n"
         end
       else
         Output.printf conf "<input type=\"hidden\" name=\"%s\" value=\"%s\">\n"
           x (Util.escape_html (Mutil.decode v)))
    (conf.henv @ conf.env);
  Output.printf conf "<input type=\"hidden\" name=\"field\" value=\"%s\">\n" var;
  Output.printf conf "<input type=\"hidden\" name=\"free_occ\" value=\"%d\">\n"
    free_n;
  Output.print_string conf "<ul>\n";
  Output.print_string conf "<li>";
  Output.printf conf "%s%s %d. \n" (Utf8.capitalize_fst (transl conf "first free number"))
    (transl conf ":") free_n;
  Output.printf conf (fcapitale (ftransl conf "click on \"%s\""))
    (transl conf "create");
  Output.printf conf " %s." (transl conf "to try again with this number");
  Output.print_string conf "</li>";
  Output.print_string conf "<li>";
  Output.printf conf "%s " (Utf8.capitalize_fst (transl conf "or"));
  Output.printf conf (ftransl conf "click on \"%s\"") (transl conf "back");
  Output.printf conf " %s %s." (transl_nth conf "and" 0)
    (transl conf "change it (the number) yourself");
  Output.print_string conf "</li>";
  Output.print_string conf "<li>";
  Output.printf conf "%s " (Utf8.capitalize_fst (transl conf "or"));
  Output.printf conf (ftransl conf "click on \"%s\"") (transl conf "back");
  Output.printf conf " %s %s." (transl_nth conf "and" 0)
    (transl conf "use \"link\" instead of \"create\"");
  Output.print_string conf "</li>";
  Output.print_string conf "</ul>\n";
  Output.printf conf "<input type=\"submit\" name=\"create\" value=\"%s\">\n"
    (Utf8.capitalize_fst (transl conf "create"));
  Output.printf conf "<input type=\"submit\" name=\"return\" value=\"%s\">\n"
    (Utf8.capitalize_fst (transl conf "back"));
  Output.print_string conf "</form>\n";
  print_same_name conf base p

let insert_person conf base src new_persons (f, s, o, create, var) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
    Create (sex, info) ->
      begin try
        if f = "?" || s = "?" then
          if o <= 0 || o >= nb_of_persons base then raise Not_found
          else
            (* FIXME: this would fail if internal repr of iper is not int *)
            let ip = Gwdb.iper_of_string @@ string_of_int o in
            let p = poi base ip in
            if p_first_name base p = f && p_surname base p = s then ip
            else raise Not_found
        else
          match person_of_key base f s o with
            Some ip -> print_create_conflict conf base (poi base ip) var
          | None -> raise Not_found
      with Not_found ->
        let o = if f = "?" || s = "?" then 0 else o in
        let empty_string = Gwdb.empty_string in
        let (birth, birth_place, baptism, baptism_place) =
          match info with
            Some {ci_birth_date = b; ci_birth_place = bpl} ->
              if String.length bpl >= 2 && String.sub bpl 0 2 = "b/" then
                None, "", b, String.sub bpl 2 (String.length bpl - 2)
              else b, bpl, None, ""
          | None -> None, "", None, ""
        in
        let (death, death_place) =
          match info with
            Some {ci_death_date = Some d; ci_death_place = dpl} ->
              Death (Unspecified, Adef.cdate_of_date d), dpl
          | Some {ci_death_date = None; ci_death_place = dpl}
            when dpl <> "" ->
              DeadDontKnowWhen, dpl
          | Some
              {ci_death = DeadDontKnowWhen | NotDead as dead;
               ci_death_date = None; ci_death_place = dpl} ->
              dead, dpl
          | _ -> infer_death_bb conf birth baptism, ""
        in
        let occupation =
          match info with
            Some {ci_occupation = occupation} -> occupation
          | _ -> ""
        in
        let access =
          match info with
            Some {ci_public = p} -> if p then Public else IfTitles
          | None -> IfTitles
        in
        let p =
          {first_name = Gwdb.insert_string base f;
           surname = Gwdb.insert_string base s; occ = o; image = empty_string;
           first_names_aliases = []; surnames_aliases = [];
           public_name = empty_string; qualifiers = []; aliases = [];
           titles = []; rparents = []; related = [];
           occupation = Gwdb.insert_string base occupation; sex = sex;
           access = access; birth = Adef.cdate_of_od birth;
           birth_place = Gwdb.insert_string base birth_place;
           birth_note = empty_string; birth_src = empty_string;
           baptism = Adef.cdate_of_od baptism;
           baptism_place = Gwdb.insert_string base baptism_place;
           baptism_note = empty_string; baptism_src = empty_string;
           death = death; death_place = Gwdb.insert_string base death_place;
           death_note = empty_string; death_src = empty_string;
           burial = UnknownBurial; burial_place = empty_string;
           burial_note = empty_string; burial_src = empty_string;
           pevents = []; notes = empty_string;
           psources = Gwdb.insert_string base (only_printable src);
           key_index = Gwdb.dummy_iper}
        in
        let a = no_ascend in
        let u = no_union in
        let ip = insert_person base p a u in
        if f <> "?" && s <> "?" then new_persons := { p with key_index = ip } :: !new_persons ;
        ip
      end
  | Link ->
    if f = "?" || s = "?" then
      if o < 0 || o >= nb_of_persons base then
        print_err_unknown conf base (f, s, o)
      else
        (* FIXME: this would fail if internal repr of iper is not int *)
        let ip = Gwdb.iper_of_string @@ string_of_int o in
        let p = poi base ip in
        if p_first_name base p = f && p_surname base p = s then ip
        else print_err_unknown conf base (f, s, o)
    else
      match person_of_key base f s o with
        Some ip -> ip
      | None -> print_err_unknown conf base (f, s, o)

let rec update_conf_env field p occ o_env n_env =
  match o_env with
    [] -> n_env
  | ((name, _) as head) :: rest ->
      if name = field ^ "p" then
        update_conf_env field p occ rest ((name, p) :: n_env)
      else if name = field ^ "occ" then
        update_conf_env field p occ rest ((name, occ) :: n_env)
      else if
        name = "link" || name = "create" || name = "free_occ" ||
        name = "field" || name = "link_occ"
      then
        update_conf_env field p occ rest n_env
      else update_conf_env field p occ rest (head :: n_env)

let update_conf_create conf =
  let field =
    match p_getenv conf.env "field" with
      Some f -> f ^ "_"
    | _ -> ""
  in
  let occ =
    match p_getenv conf.env "free_occ" with
      Some occ -> occ
    | _ -> ""
  in
  {conf with env = update_conf_env field "create" occ conf.env []}

let update_conf_link conf =
  let field =
    match p_getenv conf.env "field" with
      Some f -> f ^ "_"
    | _ -> ""
  in
  let occ =
    match p_getenv conf.env "link_occ" with
      Some occ -> occ
    | _ -> ""
  in
  {conf with env = update_conf_env field "link" occ conf.env []}

let update_conf conf =
  match p_getenv conf.env "link" with
    Some _ -> update_conf_link conf
  | None ->
      match p_getenv conf.env "create" with
        Some _ -> update_conf_create conf
      | None -> conf

let rec list_except x =
  function
  | y :: l -> if x = y then l else y :: list_except x l
  | [] -> invalid_arg "list_except"

let update_related_pointers base pi ol nl =
  let ol = List.sort compare ol in
  let nl = List.sort compare nl in
  let (added_rel, removed_rel) =
    let rec loop (added_rel, removed_rel) ol nl =
      match ol, nl with
        oip :: orl, nip :: nrl ->
          if oip < nip then loop (added_rel, oip :: removed_rel) orl nl
          else if oip > nip then loop (nip :: added_rel, removed_rel) ol nrl
          else loop (added_rel, removed_rel) orl nrl
      | [], _ -> nl @ added_rel, removed_rel
      | _, [] -> added_rel, ol @ removed_rel
    in
    loop ([], []) ol nl
  in
  List.iter
    (fun ip ->
       let p = gen_person_of_person (poi base ip) in
       patch_person base ip {p with related = pi :: p.related})
    added_rel;
  List.iter
    (fun ip ->
       let p = gen_person_of_person (poi base ip) in
       let related =
         if List.mem pi p.related then list_except pi p.related
         else
           begin
             Printf.eprintf "Warning: related pointer was missing\n";
             flush stderr;
             p.related
           end
       in
       patch_person base ip {p with related = related})
    removed_rel
