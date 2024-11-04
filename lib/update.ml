(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

type update_error =
  | UERR of Adef.safe_string
  | UERR_sex_married of person
  | UERR_sex_incoherent of base * person
  | UERR_sex_undefined of string * string * int
  | UERR_unknow_person of string * string * int
  | UERR_already_defined of base * person * string
  | UERR_own_ancestor of base * person
  | UERR_digest
  | UERR_bad_date of Date.dmy
  | UERR_missing_field of Adef.safe_string
  | UERR_already_has_parents of base * person
  | UERR_missing_surname of Adef.safe_string
  | UERR_missing_first_name of Adef.safe_string
  | UERR_locked_base
  | UERR_illegal_access_update of Def.access * Def.access

exception ModErr of update_error

(* TODO duplicate *)
type create_info = {
  ci_birth_date : Date.date option;
  ci_birth_place : string;
  ci_death : death;
  ci_death_date : Date.date option;
  ci_death_place : string;
  ci_occupation : string;
  ci_public : bool;
}

type create = Create of sex * create_info option | Link
type key = string * string * int * create * string

let maximum_lifespan = 125

let infer_death_from_dmy conf ?(max_age = maximum_lifespan) d =
  (* TODO this max_age should be related to private_years_marriage *)
  let age = (Date.time_elapsed d conf.today).year in
  if age > max_age then OfCourseDead else DontKnowIfDead

let infer_death_from_cdate conf ?(max_age = maximum_lifespan) cdate =
  match Date.cdate_to_dmy_opt cdate with
  | None -> DontKnowIfDead
  | Some dmy -> infer_death_from_dmy conf ~max_age dmy

let infer_death_bb conf birth bapt =
  let infer_death_from_odate conf = function
    | Some (Date.Dgreg (d, _)) -> infer_death_from_dmy conf d
    | Some (Dtext _) | None -> DontKnowIfDead
  in
  match infer_death_from_odate conf birth with
  | DontKnowIfDead -> infer_death_from_odate conf bapt
  | (NotDead | Death _ | DeadYoung | DeadDontKnowWhen | OfCourseDead) as
    death_status ->
      death_status

let infer_death_from_parents conf base fam =
  let infer parent =
    (* child is considered OfCourseDead if one parent is
       dead more than maximum_lifespan years ago *)
    let from_death =
      match Date.dmy_of_death (get_death parent) with
      | Some dmy -> infer_death_from_dmy conf dmy
      | None -> DontKnowIfDead
    in
    if from_death = OfCourseDead then OfCourseDead
    else
      (* child is considered OfCourseDead if one parent was
         born more than maximum_lifespan + 25 years ago *)
      infer_death_from_cdate conf ~max_age:(maximum_lifespan + 25)
        (get_birth parent)
  in
  let from_father = infer @@ poi base @@ get_father fam in
  let from_mother = infer @@ poi base @@ get_mother fam in
  let from_marriage = infer_death_from_cdate conf (get_marriage fam) in
  if
    Array.exists (( = ) OfCourseDead)
      [| from_father; from_mother; from_marriage |]
  then OfCourseDead
  else DontKnowIfDead

let rec infer_death conf base p =
  let death =
    infer_death_bb conf
      (Date.od_of_cdate (get_birth p))
      (Date.od_of_cdate (get_baptism p))
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
          match Date.cdate_to_dmy_opt (get_marriage fam) with
          | Some d -> infer_death_from_dmy conf d
          | None ->
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
(* [Fonc] print_person_parents_and_spouses :
            config -> base -> person -> unit *)

(* ************************************************************************** *)

(** [Description] : Print several information to distinguish homonyms. The
      information includes name of the person, name of the parents,
      name of the spouse.
    [Args] :
      - conf : configuration of the base
      - base : base
      - p    : person
    [Retour] : unit
    [Rem] : Not visible.                                                      *)
let print_person_parents_and_spouse conf base p =
  Output.print_sstring conf {|<a href="|};
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p);
  Output.print_sstring conf {|">|};
  Output.print_string conf (escape_html @@ p_first_name base p);
  Output.print_sstring conf ".";
  Output.print_sstring conf (string_of_int @@ get_occ p);
  Output.print_sstring conf " ";
  Output.print_string conf (escape_html @@ p_surname base p);
  Output.print_sstring conf "</a>";
  Output.print_string conf (DateDisplay.short_dates_text conf base p);
  let cop = NameDisplay.child_of_parent conf base p in
  if String.length (cop :> string) > 0 then (
    Output.print_sstring conf ", ";
    Output.print_string conf cop);
  let hbw = NameDisplay.husband_wife conf base p true in
  if String.length (hbw :> string) > 0 then (
    Output.print_sstring conf ", ";
    Output.print_string conf hbw);
  Output.print_sstring conf ". "

let print_same_name conf base p =
  match Gutil.find_same_name base p with
  | [ _ ] -> ()
  | pl ->
      Output.print_sstring conf "<p>";
      Output.print_sstring conf
      @@ Utf8.capitalize_fst (transl conf "persons having the same name");
      Output.print_sstring conf (transl conf ":");
      Output.print_sstring conf "<ul>";
      List.iter
        (fun p ->
          Output.print_sstring conf "<li>";
          print_person_parents_and_spouse conf base p;
          Output.print_sstring conf "</li>")
        pl;
      Output.print_sstring conf "</ul></p>"

(* ************************************************************************* *)
(*  [Fonc] is_label_note : string -> bool                                    *)

(* ************************************************************************* *)

(** [Description] : Test si le label contient le mot 'note' pour savoir si
      dans les évènement secondaires, il faut traiter la note comme un
      textarea.
    [Args] :
      - lbl : le label
    [Retour] :
      - bool
    [Rem] : Non exporté en clair hors de ce module.                          *)
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

let print_aux conf param (value : Adef.encoded_string)
    (submit : Adef.encoded_string) =
  Output.printf conf {|<p><form method="post" action="%s">|} conf.command;
  List.iter (fun (x, v) -> Util.hidden_textarea conf x v) conf.henv;
  List.iter (fun (x, v) -> Util.hidden_textarea conf x v) conf.env;
  Util.hidden_input conf param value;
  Output.print_sstring conf {|<input type="submit" value="|};
  Output.print_string conf submit;
  Output.print_sstring conf {|"></form></p>|}

let print_return conf =
  print_aux conf "return" (Adef.encoded "ok")
    (Adef.encoded @@ Utf8.capitalize_fst @@ transl conf "back")

let print_continue conf
    ?(continue = Adef.encoded @@ Utf8.capitalize_fst @@ transl conf "continue")
    param value =
  print_aux conf param value continue

let prerr conf _err fn =
  if not conf.api_mode then (
    let title _ =
      Output.print_sstring conf (Utf8.capitalize_fst (transl conf "error"))
    in
    Hutil.rheader conf title;
    fn ();
    Hutil.trailer conf;
    Output.flush conf);
  raise @@ ModErr _err

let string_of_error conf =
  let fso f s o : Adef.escaped_string =
    Util.escape_html f ^^^ "." ^<^ string_of_int o ^<^ " "
    ^<^ Util.escape_html s
  in
  let fso_p base p =
    let f = Gwdb.get_first_name p |> Gwdb.sou base |> Name.lower in
    let s = Gwdb.get_surname p |> Gwdb.sou base |> Name.lower in
    let o = get_occ p in
    fso f s o
  in
  let strong s = ("<strong>" ^<^ s ^>^ "</strong>" :> Adef.safe_string) in
  function
  | UERR s -> s
  | UERR_sex_married _ ->
      Utf8.capitalize_fst (transl conf "cannot change sex of a married person")
      |> Adef.safe
  | UERR_sex_incoherent (base, p) ->
      (Utf8.capitalize_fst (fso_p base p :> string)
      ^ " "
      ^
      if get_sex p = Female then transl conf "should be male"
      else transl conf "should be female")
      |> Adef.safe
  | UERR_sex_undefined (f, s, o) ->
      Printf.sprintf
        (fcapitale (ftransl conf "undefined sex for %t"))
        (fun _ -> (fso f s o :> string))
      |> Adef.safe
  | UERR_unknow_person (f, s, o) ->
      Utf8.capitalize_fst (transl conf "unknown person")
      ^<^ transl conf ":" ^<^ " "
      ^<^ strong (fso f s o)
  | UERR_already_defined (base, p, var) ->
      (Printf.sprintf
         (fcapitale (ftransl conf "name %s already used by %tthis person%t"))
         ("\"" ^ (fso_p base p :> string) ^ "\"")
         (fun _ ->
           Printf.sprintf "<a href=\"%s%s\">"
             (commd conf : Adef.escaped_string :> string)
             (acces conf base p : Adef.escaped_string :> string))
         (fun _ -> "</a>")
      ^
      if var = "" then "."
      else
        "<span class=\"UERR_already_defined_var\">("
        ^ (Util.escape_html var : Adef.escaped_string :> string)
        ^ ")</span>.")
      |> Adef.safe
  | UERR_own_ancestor (base, p) ->
      strong (fso_p base p)
      ^>^ " "
      ^ transl conf "would be his/her own ancestor"
  | UERR_digest ->
      transl conf
        {|the base has changed; do "back", "reload", and refill the form|}
      |> Utf8.capitalize_fst |> Adef.safe
  | UERR_bad_date d ->
      (Utf8.capitalize_fst (transl conf "incorrect date")
      ^ transl conf ":" ^ " "
      ^
      match d with
      | { day = 0; month = 0; year = a } -> Printf.sprintf "%d" a
      | { day = 0; month = m; year = a } -> Printf.sprintf "%d/%d" m a
      | { day = j; month = m; year = a } -> Printf.sprintf "%d/%d/%d" j m a)
      |> Adef.safe
  | UERR_missing_field s -> "missing field: " ^<^ s
  | UERR_already_has_parents (base, p) ->
      Printf.sprintf
        (fcapitale (ftransl conf "%t already has parents"))
        (fun _ ->
          (NameDisplay.referenced_person_text conf base p
            : Adef.safe_string
            :> string))
      |> Adef.safe
  | UERR_missing_first_name s when s = Adef.safe "" ->
      transl conf "first name missing" |> Utf8.capitalize_fst |> Adef.safe
  | UERR_missing_first_name x ->
      (transl conf "first name missing" |> Utf8.capitalize_fst)
      ^<^ transl conf ":" ^<^ " "
      ^<^ (x :> Adef.safe_string)
  | UERR_missing_surname x ->
      (transl conf "surname missing" |> Utf8.capitalize_fst)
      ^<^ transl conf ":" ^<^ " "
      ^<^ (x :> Adef.safe_string)
  | UERR_locked_base ->
      transl conf "the file is temporarily locked: please try again"
      |> Utf8.capitalize_fst |> Adef.safe
  | UERR_illegal_access_update (old_access, new_access) ->
      Utf8.capitalize_fst (transl conf "illegal access update")
      ^<^ transl conf ":" ^<^ " "
      ^<^ (Util.string_of_access conf old_access :> string)
      ^<^ " "
      ^<^ (Util.string_of_access conf new_access :> Adef.safe_string)

let print_err_unknown conf (f, s, o) =
  let err = UERR_unknow_person (f, s, o) in
  prerr conf err @@ fun () ->
  Output.print_string conf (string_of_error conf err);
  print_return conf

let delete_topological_sort_v conf _base =
  let bfile = Util.bpath (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab_visitor" in
  Files.rm tstab_file;
  let tstab_file = Filename.concat bfile "restrict" in
  Files.rm tstab_file

let delete_topological_sort conf base =
  let _ = delete_topological_sort_v conf base in
  let bfile = Util.bpath (conf.bname ^ ".gwb") in
  let tstab_file = Filename.concat bfile "tstab" in
  Files.rm tstab_file

let print_someone conf base p =
  Output.printf conf "%s%s %s" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))
    (p_surname base p)

let print_first_name conf base p =
  Output.print_string conf (Util.escape_html @@ p_first_name base p);
  if get_occ p <> 0 then (
    Output.print_sstring conf ".";
    Output.print_sstring conf @@ string_of_int (get_occ p))

let someone_strong base p =
  "<strong>"
  ^<^ escape_html (p_first_name base p)
  ^^^ (if get_occ p = 0 then Adef.escaped ""
      else Adef.escaped @@ "." ^ string_of_int (get_occ p))
  ^^^ " "
  ^<^ escape_html (p_surname base p)
  ^>^ "</strong>"

let print_first_name_strong conf base p =
  Output.printf conf "<strong>%s%s</strong>" (p_first_name base p)
    (if get_occ p = 0 then "" else "." ^ string_of_int (get_occ p))

let print_error conf e = Output.print_string conf @@ string_of_error conf e

let print_someone_ref_text conf base p =
  Output.print_sstring conf {|<a href="|};
  Output.print_string conf (commd conf);
  Output.print_string conf (acces conf base p);
  Output.print_sstring conf {|">|};
  Output.print_string conf (escape_html @@ p_first_name base p);
  if get_occ p <> 0 then (
    Output.print_sstring conf ".";
    Output.print_sstring conf (string_of_int (get_occ p)));
  Output.print_sstring conf " ";
  Output.print_string conf (escape_html @@ p_surname base p);
  Output.print_sstring conf "</a>"

let print_list_aux conf base title list printer =
  if list <> [] then (
    Output.printf conf "%s\n<ul>" (Utf8.capitalize_fst (transl conf title));
    printer conf base list;
    Output.print_sstring conf "</ul>")

let print_order_changed conf print_list (before : 'a array) (after : 'a array) =
  let bef_d, aft_d = Difference.f before after in
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf
    {|<table style="margin:1em"><tr><td><ul style="list-style-type:none">|};
  print_list before bef_d;
  Output.print_sstring conf {|</ul></td><td><ul style="list-style-type:none">|};
  print_list after aft_d;
  Output.print_sstring conf {|</ul></td></tr></table>|}

let someone_strong_n_short_dates conf base p =
  (someone_strong base p :> Adef.safe_string)
  ^^^ DateDisplay.short_dates_text conf base p

let print_warning conf base (w : CheckItem.base_warning) =
  match w with
  | Warning.BigAgeBetweenSpouses (p1, p2, a) ->
      Output.printf conf
        (fcapitale
           (ftransl conf
              "the difference of age between %t and %t is quite important"))
        (fun _ -> (someone_strong base p1 :> string))
        (fun _ -> (someone_strong base p2 :> string));
      Output.print_sstring conf (transl conf ":");
      Output.print_sstring conf " ";
      Output.print_string conf (DateDisplay.string_of_age conf a)
  | BirthAfterDeath p ->
      Output.printf conf (ftransl conf "%t died before his/her birth") (fun _ ->
          (someone_strong_n_short_dates conf base p :> string))
  | ChangedOrderOfChildren (ifam, _, before, after) ->
      let cpl = foi base ifam in
      let fath = poi base (get_father cpl) in
      let moth = poi base (get_mother cpl) in
      let print_list arr diff_arr =
        Array.iteri
          (fun i p ->
            let p = poi base p in
            Output.print_sstring conf "<li";
            if diff_arr.(i) then
              Output.print_sstring conf {| style="background:pink"|};
            Output.print_sstring conf ">";
            Output.print_sstring conf "\n";
            if eq_istr (get_surname p) (get_surname fath) then
              print_first_name conf base p
            else print_someone conf base p;
            Output.print_string conf (DateDisplay.short_dates_text conf base p);
            Output.print_sstring conf "\n";
            Output.print_sstring conf "</li>\n")
          arr
      in
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "changed order of children"));
      Output.print_sstring conf "\n";
      print_someone_ref_text conf base fath;
      Output.print_sstring conf " ";
      Output.print_sstring conf (transl_nth conf "and" 0);
      Output.print_sstring conf " ";
      print_someone_ref_text conf base moth;
      print_order_changed conf print_list before after
  | ChildrenNotInOrder (ifam, _, elder, x) ->
      let cpl = foi base ifam in
      Output.printf conf
        (fcapitale
           (ftransl conf "the following children of %t and %t are not in order"))
        (fun _ -> (someone_strong base (poi base (get_father cpl)) :> string))
        (fun _ -> (someone_strong base (poi base (get_mother cpl)) :> string));
      Output.print_sstring conf ":\n<ul><li>";
      print_first_name_strong conf base elder;
      Output.print_string conf (DateDisplay.short_dates_text conf base elder);
      Output.print_sstring conf "</li><li>";
      print_first_name_strong conf base x;
      Output.print_string conf (DateDisplay.short_dates_text conf base x);
      Output.print_sstring conf "</li></ul>\n"
  | ChangedOrderOfMarriages (p, before, after) ->
      let print_list arr diff_arr =
        Array.iteri
          (fun i ifam ->
            let fam = foi base ifam in
            let sp = Gutil.spouse (get_iper p) fam in
            let sp = poi base sp in
            Output.print_sstring conf "<li";
            if diff_arr.(i) then
              Output.print_sstring conf {| style="background:pink"|};
            Output.print_sstring conf ">\n";
            print_first_name conf base p;
            Output.print_sstring conf "  &amp;";
            Output.print_string conf
              (DateDisplay.short_marriage_date_text conf base fam p sp);
            Output.print_sstring conf "\n";
            print_someone conf base sp;
            Output.print_sstring conf "\n</li>\n")
          arr
      in
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "changed order of marriages"));
      print_order_changed conf print_list before after
  | ChangedOrderOfFamilyEvents (_, before, after) ->
      let print_list arr diff_arr =
        Array.iteri
          (fun i evt ->
            let name = Util.string_of_fevent_name conf base evt.efam_name in
            Output.print_sstring conf "<li";
            if diff_arr.(i) then
              Output.print_sstring conf {| style="background:pink"|};
            Output.print_sstring conf ">";
            Output.print_string conf name;
            Output.print_sstring conf "</li>")
          arr
      in
      let before = Array.of_list before in
      let after = Array.of_list after in
      Output.printf conf "%s\n"
        (Utf8.capitalize_fst (transl conf "changed order of family's events"));
      print_order_changed conf print_list before after
  | ChangedOrderOfPersonEvents (_, before, after) ->
      let print_list arr diff_arr =
        Array.iteri
          (fun i evt ->
            let name = Util.string_of_pevent_name conf base evt.epers_name in
            Output.print_sstring conf "<li";
            if diff_arr.(i) then
              Output.print_sstring conf {| style="background:pink"|};
            Output.print_sstring conf ">";
            Output.print_string conf name;
            Output.print_sstring conf "</li>")
          arr
      in
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "changed order of person's events"));
      Output.print_sstring conf " -&gt; ";
      let before = Array.of_list before in
      let after = Array.of_list after in
      print_order_changed conf print_list before after
  | CloseChildren (ifam, c1, c2) ->
      let cpl = foi base ifam in
      Output.printf conf
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are born very close"))
        (fun _ -> (someone_strong base (poi base (get_father cpl)) :> string))
        (fun _ -> (someone_strong base (poi base (get_mother cpl)) :> string));
      Output.print_sstring conf ":\n<ul><li>";
      print_first_name_strong conf base c1;
      Output.print_string conf (DateDisplay.short_dates_text conf base c1);
      Output.print_sstring conf "</li><li>";
      print_first_name_strong conf base c2;
      Output.print_string conf (DateDisplay.short_dates_text conf base c2);
      Output.print_sstring conf "</li></ul>\n"
  | DistantChildren (ifam, p1, p2) ->
      let cpl = foi base ifam in
      Output.printf conf
        (fcapitale
           (ftransl conf
              "the following children of %t and %t are born very distant"))
        (fun _ -> (someone_strong base (poi base (get_father cpl)) :> string))
        (fun _ -> (someone_strong base (poi base (get_mother cpl)) :> string));
      Output.print_sstring conf ":<ul><li>";
      print_first_name_strong conf base p1;
      Output.print_string conf (DateDisplay.short_dates_text conf base p1);
      Output.print_sstring conf "</li><li>";
      print_first_name_strong conf base p2;
      Output.print_string conf (DateDisplay.short_dates_text conf base p2);
      Output.print_sstring conf "</li></ul>"
  | DeadOld (p, a) ->
      Output.print_string conf (someone_strong base p);
      Output.print_sstring conf " ";
      Output.print_sstring conf
        (transl_nth conf "died at an advanced age" @@ index_of_sex @@ get_sex p);
      Output.print_sstring conf "(";
      Output.print_string conf (DateDisplay.string_of_age conf a);
      Output.print_sstring conf ")"
  | DeadTooEarlyToBeFather (father, child) ->
      Output.printf conf
        (ftransl conf
           "%t is born more than 2 years after the death of his/her father %t")
        (fun _ -> (someone_strong_n_short_dates conf base child :> string))
        (fun _ -> (someone_strong_n_short_dates conf base father :> string))
  | FEventOrder (p, e1, e2) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t's %s before his/her %s"))
        (fun _ -> (someone_strong base p :> string))
        (Util.string_of_fevent_name conf base e1.efam_name :> string)
        (Util.string_of_fevent_name conf base e2.efam_name :> string)
  | FWitnessEventAfterDeath (p, e, _) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s after his/her death"))
        (fun _ -> (someone_strong_n_short_dates conf base p :> string))
        (Util.string_of_fevent_name conf base e.efam_name :> string)
  | FWitnessEventBeforeBirth (p, e, _) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s before his/her birth"))
        (fun _ -> (someone_strong_n_short_dates conf base p :> string))
        (Util.string_of_fevent_name conf base e.efam_name :> string)
  | IncoherentSex (p, _, _) ->
      Output.printf conf
        (fcapitale
           (ftransl conf "%t's sex is not coherent with his/her relations"))
        (fun _ -> (someone_strong base p :> string))
  | IncoherentAncestorDate (anc, p) ->
      Output.printf conf "%s has a younger ancestor %s"
        (someone_strong base p :> string)
        (someone_strong base anc :> string)
  | MarriageDateAfterDeath p ->
      Output.printf conf
        (fcapitale (ftransl conf "marriage had occurred after the death of %t"))
        (fun _ -> (someone_strong_n_short_dates conf base p :> string))
  | MarriageDateBeforeBirth p ->
      Output.printf conf
        (fcapitale
           (ftransl conf "marriage had occurred before the birth of %t"))
        (fun _ -> (someone_strong_n_short_dates conf base p :> string))
  | MotherDeadBeforeChildBirth (mother, child) ->
      Output.printf conf
        (ftransl conf "%t is born after the death of his/her mother %t")
        (fun _ -> (someone_strong_n_short_dates conf base child :> string))
        (fun _ -> (someone_strong_n_short_dates conf base mother :> string))
  | ParentBornAfterChild (p, c) ->
      Output.print_string conf (someone_strong base p);
      Output.print_sstring conf " ";
      Output.print_sstring conf (transl conf "is born after his/her child");
      Output.print_sstring conf " ";
      Output.print_string conf (someone_strong base c)
  | ParentTooYoung (p, a, _) ->
      Output.print_string conf (someone_strong base p);
      Output.print_sstring conf " ";
      Output.print_sstring conf (transl conf "is a very young parent");
      Output.print_sstring conf " (";
      Output.print_string conf (DateDisplay.string_of_age conf a);
      Output.print_sstring conf ")"
  | ParentTooOld (p, a, _) ->
      Output.print_string conf (someone_strong base p);
      Output.print_sstring conf " ";
      Output.print_sstring conf (transl conf "is a very old parent");
      Output.print_sstring conf " (";
      Output.print_string conf (DateDisplay.string_of_age conf a);
      Output.print_sstring conf ")"
  | PossibleDuplicateFam (f1, _) ->
      let f = foi base f1 in
      Output.printf conf
        (fcapitale (ftransl conf "%s and %s have several unions"))
        (someone_strong base @@ poi base @@ get_father f :> string)
        (someone_strong base @@ poi base @@ get_mother f :> string)
  | PossibleDuplicateFamHomonymous (f1, _, p) ->
      let f = foi base f1 in
      let fath = get_father f in
      let moth = get_mother f in
      let curr, hom =
        if eq_iper fath (get_iper p) then (moth, fath) else (fath, moth)
      in
      Output.printf conf
        (fcapitale (ftransl conf "%s has unions with several persons named %s"))
        (someone_strong base @@ poi base @@ curr :> string)
        (someone_strong base @@ poi base @@ hom :> string)
  | PEventOrder (p, e1, e2) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t's %s before his/her %s"))
        (fun _ -> (someone_strong base p :> string))
        (Util.string_of_pevent_name conf base e1.epers_name :> string)
        (Util.string_of_pevent_name conf base e2.epers_name :> string)
  | PWitnessEventAfterDeath (p, e, _origin) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s after his/her death"))
        (fun _ -> (someone_strong_n_short_dates conf base p :> string))
        (Util.string_of_pevent_name conf base e.epers_name :> string)
  | PWitnessEventBeforeBirth (p, e, _origin) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t witnessed the %s before his/her birth"))
        (fun _ -> (someone_strong_n_short_dates conf base p :> string))
        (Util.string_of_pevent_name conf base e.epers_name :> string)
  | TitleDatesError (p, t) ->
      Output.printf conf
        (fcapitale (ftransl conf "%t has incorrect title dates: %t"))
        (fun _ -> (someone_strong_n_short_dates conf base p :> string))
        (fun _ ->
          ("<strong>"
           ^<^ (safe_html @@ sou base t.t_ident)
           ^^^ " "
           ^<^ (safe_html @@ sou base t.t_place)
           ^^^ "</strong> <em>"
           ^<^ (match Date.od_of_cdate t.t_date_start with
               | Some d -> DateDisplay.string_of_date conf d
               | None -> Adef.safe "")
           ^^^ "-"
           ^<^ (match Date.od_of_cdate t.t_date_end with
               | Some d -> DateDisplay.string_of_date conf d
               | None -> Adef.safe "")
           ^>^ "</em>"
            :> string))
  | UndefinedSex p ->
      Output.printf conf
        (fcapitale (ftransl conf "undefined sex for %t"))
        (fun _ -> (someone_strong base p :> string))
  | YoungForMarriage (p, a, _) | OldForMarriage (p, a, _) ->
      Output.print_string conf (someone_strong base p);
      Output.print_sstring conf " ";
      Output.printf conf (ftransl conf "married at age %t") (fun _ ->
          (DateDisplay.string_of_age conf a :> string))

let print_warnings conf base (wl : CheckItem.base_warning list) =
  print_list_aux conf base "warnings" wl @@ fun conf base wl ->
  (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
  (* levé par plusieurs fonctions différents selon le context.       *)
  let wl = List.sort_uniq compare wl in
  List.iter
    (fun (w : CheckItem.base_warning) ->
      Output.print_sstring conf "<li>";
      print_warning conf base w;
      Output.print_sstring conf "</li>")
    wl

(* ************************************************************************* *)
(*  [Fonc] print_misc : config -> base -> Def.misc -> unit                   *)

(* ************************************************************************* *)

(** [Description] : Fonction d'impression des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - fun  : Def.misc (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                          *)
let print_misc conf _base = function
  | Warning.MissingSources ->
      Output.print_sstring conf "<em>";
      Output.printf conf "%s\n"
        (Utf8.capitalize_fst (transl conf "missing sources"));
      Output.print_sstring conf "</em>"

(* ************************************************************************* *)
(*  [Fonc] print_miscs : config -> base -> Def.misc list -> unit             *)

(* ************************************************************************* *)

(** [Description] : Affiche la liste des 'informations diverses'.
    [Args] :
      - conf : configuration
      - base : base
      - ml   : Def.misc list (miscellaneous)
    [Retour] :
      - unit
    [Rem] : Exporté en clair hors de ce module.                          *)
let print_miscs conf base ml =
  print_list_aux conf base "miscellaneous informations" ml @@ fun conf base ->
  List.iter (fun m ->
      Output.print_sstring conf "<li>";
      print_misc conf base m;
      Output.print_sstring conf "</li>")

(* ************************************************************************* *)
(* [Fonc] print_miscs :
     config -> base -> (Def.warning list * Def.misc list) -> unit *)

(* ************************************************************************* *)

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
let print_warnings_and_miscs conf base wl ml =
  if wl <> [] || ml <> [] then (
    Output.printf conf "%s\n" (Utf8.capitalize_fst (transl conf "warnings"));
    Output.print_sstring conf "<ul>\n";
    List.iter
      (fun w ->
        Output.print_sstring conf "<li>";
        print_warning conf base w;
        Output.print_sstring conf "</li>")
      wl;
    List.iter
      (fun m ->
        Output.print_sstring conf "<li>";
        print_misc conf base m;
        Output.print_sstring conf "</li>")
      ml;
    Output.print_sstring conf "</ul>\n")

let error conf err =
  prerr conf err @@ fun () ->
  Output.print_string conf (string_of_error conf err);
  Output.print_sstring conf "\n";
  print_return conf

let def_error conf base x =
  error conf
  @@
  match x with
  | AlreadyDefined p -> UERR_already_defined (base, p, "")
  | OwnAncestor p -> UERR_own_ancestor (base, p)
  | BadSexOfMarriedPerson p -> UERR_sex_married p

let error_locked conf =
  let err = UERR_locked_base in
  prerr conf err @@ fun () ->
  Output.print_sstring conf "<p>\n";
  transl conf "the file is temporarily locked: please try again"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|.</p><table><tr><td><form method="post" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|};
  let aux env =
    List.iter
      (fun (x, v) ->
        if x = "retry" then ()
        else if x = "notes" || is_label_note x then
          Util.hidden_textarea conf x v
        else Util.hidden_input conf x v)
      env
  in
  aux conf.henv;
  aux conf.env;
  (* just to see in the traces... *)
  Util.hidden_input conf "retry" (Mutil.encode conf.user);
  Util.hidden_input conf "submit"
    (transl conf "try again" |> Utf8.capitalize_fst |> Mutil.encode);
  Output.print_sstring conf {|</form></td><td><form method="get" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|};
  Util.hidden_env_aux conf conf.henv;
  (match
     match p_getenv conf.env "ip" with
     | Some ip -> Some ip
     | None -> p_getenv conf.env "i"
   with
  | Some n -> Util.hidden_input conf "i" (Mutil.encode n)
  | None -> ());
  transl_nth conf "user/password/cancel" 2
  |> Utf8.capitalize_fst |> Mutil.encode
  |> Util.hidden_input conf "submit";
  Output.print_sstring conf "</form></td></tr></table>"

let error_digest conf =
  let err = UERR_digest in
  prerr conf err @@ fun () ->
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf "<p>";
  Output.print_string conf (string_of_error conf err);
  Output.print_sstring conf "</p>"

let digest_person p = Marshal.to_string p [] |> Ext_string.digest
let digest_family f = Marshal.to_string f [] |> Ext_string.digest

let get var key env =
  match p_getenv env (var ^ "_" ^ key) with
  | Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound")

let get_number var key env =
  match p_getint env (var ^ "_" ^ key) with
  | Some x when x <> 0 -> Some x
  | Some _ | None -> None

let bad_date conf d =
  let err = UERR_bad_date d in
  prerr conf err @@ fun () ->
  Output.print_string conf (string_of_error conf err)

let int_of_field s =
  match int_of_string (String.trim s) with
  | exception Failure _ -> None
  | x -> if x <> 0 then Some x else None

let reconstitute_date_dmy2 conf var =
  let m =
    let m = get var "ormonth" conf.env in
    match String.uppercase_ascii m with
    | "VD" -> Some 1
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
  | Some y -> (
      match m with
      | None -> Date.{ day2 = 0; month2 = 0; year2 = y; delta2 = 0 }
      | Some m -> (
          match get_number var "orday" conf.env with
          | Some d ->
              let dmy2 = Date.{ day2 = d; month2 = m; year2 = y; delta2 = 0 } in
              if
                dmy2.day2 >= 1 && dmy2.day2 <= 31 && dmy2.month2 >= 1
                && dmy2.month2 <= 13
              then dmy2
              else
                let d = Date.dmy_of_dmy2 dmy2 in
                bad_date conf d
          | None ->
              let dmy2 = Date.{ day2 = 0; month2 = m; year2 = y; delta2 = 0 } in
              if dmy2.month2 >= 1 && dmy2.month2 <= 13 then dmy2
              else
                let d = Date.dmy_of_dmy2 dmy2 in
                bad_date conf d))
  | None -> raise @@ ModErr (UERR_missing_field (Adef.safe "oryear"))

let reconstitute_date_dmy conf var =
  let prec, y =
    let y = get var "yyyy" conf.env in
    let prec = p_getenv conf.env (var ^ "_prec") in
    let len = String.length y in
    if len > 1 then
      match (y.[0], y.[len - 1]) with
      | '?', _ -> (Some "maybe", String.sub y 1 (len - 1))
      | '~', _ -> (Some "about", String.sub y 1 (len - 1))
      | '/', '/' -> (Some "about", String.sub y 1 (len - 2))
      | '<', _ | '/', _ -> (Some "before", String.sub y 1 (len - 1))
      | '>', _ -> (Some "after", String.sub y 1 (len - 1))
      | _, '/' -> (Some "after", String.sub y 0 (len - 1))
      | _ -> (prec, y)
    else (prec, y)
  in
  let force_f_cal, m =
    let m = get var "mm" conf.env in
    match String.uppercase_ascii m with
    | "VD" -> (true, Some 1)
    | "BR" -> (true, Some 2)
    | "FM" -> (true, Some 3)
    | "NI" -> (true, Some 4)
    | "PL" -> (true, Some 5)
    | "VT" -> (true, Some 6)
    | "GE" -> (true, Some 7)
    | "FL" -> (true, Some 8)
    | "PR" -> (true, Some 9)
    | "ME" -> (true, Some 10)
    | "TH" -> (true, Some 11)
    | "FT" -> (true, Some 12)
    | "JC" -> (true, Some 13)
    | _ -> (false, int_of_field m)
  in
  let d =
    match int_of_field y with
    | None -> None
    | Some y -> (
        let prec =
          match prec with
          | Some "about" -> Date.About
          | Some "maybe" -> Maybe
          | Some "before" -> Before
          | Some "after" -> After
          | Some "oryear" -> (
              match get_number var "oryear" conf.env with
              | Some _ ->
                  let dmy2 = reconstitute_date_dmy2 conf var in
                  OrYear dmy2
              | None -> Sure)
          | Some "yearint" -> (
              match get_number var "oryear" conf.env with
              | Some _ ->
                  let dmy2 = reconstitute_date_dmy2 conf var in
                  YearInt dmy2
              | None -> Sure)
          | Some _ | None -> Sure
        in
        match m with
        | None -> Some Date.{ day = 0; month = 0; year = y; prec; delta = 0 }
        | Some m -> (
            match get_number var "dd" conf.env with
            | Some day ->
                let d = Date.{ day; month = m; year = y; prec; delta = 0 } in
                if d.day >= 1 && d.day <= 31 && d.month >= 1 && d.month <= 13
                then Some d
                else bad_date conf d
            | None ->
                let d =
                  Date.{ day = 0; month = m; year = y; prec; delta = 0 }
                in
                if d.month >= 1 && d.month <= 13 then Some d
                else bad_date conf d))
  in
  (d, force_f_cal)

let is_illegal_access_update ~previous_access ~new_access =
  match (previous_access, new_access) with
  | IfTitles, Public | IfTitles, Private | Public, Private | Private, Public ->
      true
  | Public, IfTitles
  | Private, IfTitles
  | Public, Public
  | Private, Private
  | IfTitles, IfTitles ->
      false

let check_illegal_access_update base person =
  let previous_access_opt =
    let iper = person.key_index in
    if Gwdb.iper_exists base iper then
      let old_person = Gwdb.poi base iper in
      if is_empty_person old_person then None
      else Some (Gwdb.get_access old_person)
    else None
  in
  match previous_access_opt with
  | Some previous_access ->
      if is_illegal_access_update ~previous_access ~new_access:person.access
      then Some (UERR_illegal_access_update (previous_access, person.access))
      else None
  | None -> None

let check_missing_name base p =
  let quest f g =
    (* only raise error if `?` is not already recorded in the database *)
    f = "?" && p.key_index <> dummy_iper
    && poi base p.key_index |> g |> sou base |> ( <> ) "?"
  in
  if p.first_name = "" || quest p.first_name get_first_name then
    Some (UERR_missing_first_name (Adef.safe ""))
  else if p.surname = "" || quest p.surname get_surname then
    Some (UERR_missing_surname (Adef.safe ""))
  else None

let check_missing_witnesses_names conf get list =
  let aux witnesses =
    let len = Array.length witnesses in
    let rec loop i =
      if i = len then None
      else
        let (fn, sn, _, _, _), _, _ = Array.get witnesses i in
        if fn = "" && sn = "" then loop (i + 1)
        else if fn = "" || fn = "?" then
          Some
            (UERR_missing_first_name
               (transl_nth conf "witness/witnesses" 0 |> Adef.safe))
        else if sn = "" || sn = "?" then
          Some
            (UERR_missing_surname
               (transl_nth conf "witness/witnesses" 0 |> Adef.safe))
        else loop (i + 1)
    in
    loop 0
  in
  let rec loop = function
    | [] -> None
    | hd :: tl -> (
        match aux (get hd) with Some err -> Some err | None -> loop tl)
  in
  loop list

let check_greg_day conf d =
  if d.Date.day > Date.nb_days_in_month d.month d.year then bad_date conf d

let reconstitute_date conf var =
  match reconstitute_date_dmy conf var with
  | Some d, false ->
      let calendar =
        match p_getenv conf.env (var ^ "_cal") with
        | Some "G" | None ->
            check_greg_day conf d;
            Date.Dgregorian
        | Some "J" -> Djulian
        | Some "F" -> Dfrench
        | Some "H" -> Dhebrew
        | _ -> Dgregorian
      in
      let date = Date.convert ~from:calendar ~to_:Dgregorian d in
      Some (Date.Dgreg (date, calendar))
  | Some d, true ->
      Some (Dgreg (Date.convert ~from:Dfrench ~to_:Dgregorian d, Dfrench))
  | None, _ -> (
      match p_getenv conf.env (var ^ "_text") with
      | Some _ ->
          let txt = Ext_string.only_printable (get var "text" conf.env) in
          if txt = "" then None else Some (Dtext txt)
      | None -> None)

let print_create_conflict conf base p var =
  let err = UERR_already_defined (base, p, var) in
  prerr conf err @@ fun () ->
  print_error conf err;
  let free_n =
    Gutil.find_free_occ base (p_first_name base p) (p_surname base p)
  in
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|">|};
  let aux =
    List.iter (fun (x, v) ->
        if x = "notes" || is_label_note x then Util.hidden_textarea conf x v
        else Util.hidden_input conf x v)
  in
  aux conf.henv;
  aux conf.env;
  if var <> "" then Util.hidden_input conf "field" (Mutil.encode var);
  Util.hidden_input conf "free_occ" (Mutil.encode @@ string_of_int free_n);
  Output.print_sstring conf "<ul><li>";
  transl conf "first free number"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf (transl conf ":");
  Output.print_sstring conf " ";
  Output.print_sstring conf (string_of_int free_n);
  Output.print_sstring conf ".\n";
  Output.printf conf
    (fcapitale (ftransl conf {|click on "%s"|}))
    (transl conf "create");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "to try again with this number");
  Output.print_sstring conf ".</li><li>";
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "or"));
  Output.print_sstring conf " ";
  Output.printf conf (ftransl conf {|click on "%s"|}) (transl conf "back");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_nth conf "and" 0);
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "change it (the number) yourself");
  Output.print_sstring conf ".</li><li>";
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf "or"));
  Output.print_sstring conf " ";
  Output.printf conf (ftransl conf {|click on "%s"|}) (transl conf "back");
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_nth conf "and" 0);
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf {|use "link" instead of "create"|});
  Output.print_sstring conf ".</li></ul>";
  transl conf "create" |> Utf8.capitalize_fst |> Adef.encoded
  |> Util.submit_input conf "create";
  transl conf "back" |> Utf8.capitalize_fst |> Adef.encoded
  |> Util.submit_input conf "return";
  Output.print_sstring conf {|</form>|};
  print_same_name conf base p

let insert_person conf base src new_persons (f, s, o, create, var) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
  | Create (sex, info) -> (
      try
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
          | Some ip -> print_create_conflict conf base (poi base ip) var
          | None -> raise Not_found
      with Not_found ->
        let o = if f = "?" || s = "?" then 0 else o in
        let empty_string = Gwdb.empty_string in
        let birth, birth_place, baptism, baptism_place =
          match info with
          | Some { ci_birth_date = b; ci_birth_place = bpl } ->
              if String.length bpl >= 2 && String.sub bpl 0 2 = "b/" then
                (None, "", b, String.sub bpl 2 (String.length bpl - 2))
              else (b, bpl, None, "")
          | None -> (None, "", None, "")
        in
        let death, death_place =
          match info with
          | Some { ci_death_date = Some d; ci_death_place = dpl } ->
              (Death (Unspecified, Date.cdate_of_date d), dpl)
          | Some { ci_death_date = None; ci_death_place = dpl } when dpl <> ""
            ->
              (DeadDontKnowWhen, dpl)
          | Some
              {
                ci_death = (DeadDontKnowWhen | NotDead) as dead;
                ci_death_date = None;
                ci_death_place = dpl;
              } ->
              (dead, dpl)
          | Some _ | None -> (infer_death_bb conf birth baptism, "")
        in
        let occupation =
          match info with
          | Some { ci_occupation = occupation } -> occupation
          | None -> ""
        in
        let access =
          match info with
          | Some { ci_public = p } -> if p then Public else IfTitles
          | None -> IfTitles
        in
        let p =
          {
            first_name = Gwdb.insert_string base f;
            surname = Gwdb.insert_string base s;
            occ = o;
            image = empty_string;
            first_names_aliases = [];
            surnames_aliases = [];
            public_name = empty_string;
            qualifiers = [];
            aliases = [];
            titles = [];
            rparents = [];
            related = [];
            occupation = Gwdb.insert_string base occupation;
            sex;
            access;
            birth = Date.cdate_of_od birth;
            birth_place = Gwdb.insert_string base birth_place;
            birth_note = empty_string;
            birth_src = empty_string;
            baptism = Date.cdate_of_od baptism;
            baptism_place = Gwdb.insert_string base baptism_place;
            baptism_note = empty_string;
            baptism_src = empty_string;
            death;
            death_place = Gwdb.insert_string base death_place;
            death_note = empty_string;
            death_src = empty_string;
            burial = UnknownBurial;
            burial_place = empty_string;
            burial_note = empty_string;
            burial_src = empty_string;
            pevents = [];
            notes = empty_string;
            psources = Gwdb.insert_string base (Ext_string.only_printable src);
            key_index = Gwdb.dummy_iper;
          }
        in
        let a = no_ascend in
        let u = no_union in
        let ip = insert_person base p a u in
        if f <> "?" && s <> "?" then
          new_persons := { p with key_index = ip } :: !new_persons;
        ip)
  | Link -> (
      if f = "?" || s = "?" then
        if o < 0 || o >= nb_of_persons base then print_err_unknown conf (f, s, o)
        else
          (* FIXME: this would fail if internal repr of iper is not int *)
          let ip = Gwdb.iper_of_string @@ string_of_int o in
          let p = poi base ip in
          if p_first_name base p = f && p_surname base p = s then ip
          else print_err_unknown conf (f, s, o)
      else
        match person_of_key base f s o with
        | Some ip -> ip
        | None -> print_err_unknown conf (f, s, o))

let rec update_conf_env field (p : Adef.encoded_string)
    (occ : Adef.encoded_string) o_env n_env =
  match o_env with
  | [] -> n_env
  | ((name, _) as head) :: rest ->
      if name = field ^ "p" then
        update_conf_env field p occ rest ((name, p) :: n_env)
      else if name = field ^ "occ" then
        update_conf_env field p occ rest ((name, occ) :: n_env)
      else if
        name = "link" || name = "create" || name = "free_occ" || name = "field"
        || name = "link_occ"
      then update_conf_env field p occ rest n_env
      else update_conf_env field p occ rest (head :: n_env)

let update_conf_aux _create _occ conf =
  let field =
    match p_getenv conf.env "field" with Some f -> f ^ "_" | None -> ""
  in
  let occ =
    match p_getenv conf.env _occ with
    | Some occ -> Mutil.encode occ
    | None -> Adef.encoded ""
  in
  { conf with env = update_conf_env field _create occ conf.env [] }

let update_conf_create = update_conf_aux (Adef.encoded "create") "free_occ"
let update_conf_link = update_conf_aux (Adef.encoded "link") "link_occ"

let update_conf conf =
  match p_getenv conf.env "link" with
  | Some _ -> update_conf_link conf
  | None -> (
      match p_getenv conf.env "create" with
      | Some _ -> update_conf_create conf
      | None -> conf)

let rec list_except x = function
  | y :: l -> if x = y then l else y :: list_except x l
  | [] -> invalid_arg "list_except"

let update_related_pointers base pi ol nl =
  let ol = List.sort_uniq compare ol in
  let nl = List.sort_uniq compare nl in
  let added_rel, removed_rel =
    let rec loop (added_rel, removed_rel) ol nl =
      match (ol, nl) with
      | oip :: orl, nip :: nrl ->
          if oip < nip then loop (added_rel, oip :: removed_rel) orl nl
          else if oip > nip then loop (nip :: added_rel, removed_rel) ol nrl
          else loop (added_rel, removed_rel) orl nrl
      | [], _ -> (nl @ added_rel, removed_rel)
      | _, [] -> (added_rel, ol @ removed_rel)
    in
    loop ([], []) ol nl
  in
  List.iter
    (fun ip ->
      let p = gen_person_of_person (poi base ip) in
      patch_person base ip { p with related = pi :: p.related })
    added_rel;
  List.iter
    (fun ip ->
      let p = gen_person_of_person (poi base ip) in
      let related =
        if List.mem pi p.related then list_except pi p.related
        else (
          Printf.eprintf "Warning: related pointer was missing\n";
          flush stderr;
          p.related)
      in
      patch_person base ip { p with related })
    removed_rel
