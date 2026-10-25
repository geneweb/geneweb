open Config
open Def
open Anomaly
module Driver = Geneweb_db.Driver
module Code = Geneweb_http.Code

let page_size = 100

(* ---- context ------------------------------------------------------------- *)

let tr conf key = Util.transl conf key
let ctr conf key = Utf8.capitalize_fst (Util.transl conf key)
let kind_label conf k = ctr conf ("ANOMALY " ^ kind_name k)
let esc s = (Util.escape_html s :> string)
let config_dir conf = GWPARAM.base_config_dir conf.bname

(* Kinds shown apart unless the base says otherwise: mostly data-entry checks
   (order of children and events, sex, duplicates) rather than dates. *)
let default_hidden =
  [
    K_ChangedOrderOfChildren;
    K_ChangedOrderOfFamilyEvents;
    K_ChangedOrderOfMarriages;
    K_ChangedOrderOfPersonEvents;
    K_ChildrenNotInOrder;
    K_DistantChildren;
    K_FEventOrder;
    K_PEventOrder;
    K_PossibleDuplicateFam;
    K_PossibleDuplicateFamHomonymous;
    K_PWitnessEventAfterDeath;
    K_PWitnessEventBeforeBirth;
    K_TitleDatesError;
    K_UndefinedSex;
  ]

let hidden_kinds conf =
  match List.assoc_opt "anomalies_hidden" conf.base_env with
  | Some s ->
      String.split_on_char ',' s
      |> List.filter_map (fun n -> kind_of_name (String.trim n))
  | None -> default_hidden

let url conf params =
  (Util.commd conf :> string)
  ^ String.concat "&"
      (List.map (fun (k, v) -> k ^ "=" ^ (Mutil.encode v :> string)) params)

(* ---- persons on the open base -------------------------------------------- *)

(* Anomalies name persons by key; the page shows them as they are now. A key
   that no longer exists (person renamed or deleted since the analysis) is
   shown as text. *)
let resolver base =
  let cache = Hashtbl.create 256 in
  fun (p : person) ->
    let k = (p.first_name, p.surname, p.occ) in
    match Hashtbl.find_opt cache k with
    | Some r -> r
    | None ->
        let r =
          if is_unnamed p then None
          else
            Option.map (Driver.poi base)
              (Driver.person_of_key base p.first_name p.surname p.occ)
        in
        Hashtbl.add cache k r;
        r

let who conf base resolve p =
  match resolve p with
  | Some dp ->
      "<strong>"
      ^ (Util.referenced_person_text conf base dp :> string)
      ^ "</strong>"
      ^ (DateDisplay.short_dates_text conf base dp :> string)
  | None -> "<strong>" ^ esc (person_key p) ^ "</strong>"

let sex_index resolve p =
  match resolve p with
  | Some dp -> Util.index_of_sex (Driver.get_sex dp)
  | None -> 2

let age_text conf n =
  (DateDisplay.string_of_age conf
     { Adef.day = 0; month = 0; year = n; prec = Adef.Sure; delta = 0 }
    :> string)

let pevent conf base (e : pevent) =
  match e with
  | Epers_Name s -> esc s
  | ( Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
    | Epers_Cremation | Epers_Accomplishment | Epers_Acquisition
    | Epers_Adhesion | Epers_BaptismLDS | Epers_BarMitzvah | Epers_BatMitzvah
    | Epers_Benediction | Epers_ChangeName | Epers_Circumcision
    | Epers_Confirmation | Epers_ConfirmationLDS | Epers_Decoration
    | Epers_DemobilisationMilitaire | Epers_Diploma | Epers_Distinction
    | Epers_Dotation | Epers_DotationLDS | Epers_Education | Epers_Election
    | Epers_Emigration | Epers_Excommunication | Epers_FamilyLinkLDS
    | Epers_FirstCommunion | Epers_Funeral | Epers_Graduate
    | Epers_Hospitalisation | Epers_Illness | Epers_Immigration
    | Epers_ListePassenger | Epers_MilitaryDistinction
    | Epers_MilitaryPromotion | Epers_MilitaryService
    | Epers_MobilisationMilitaire | Epers_Naturalisation | Epers_Occupation
    | Epers_Ordination | Epers_Property | Epers_Recensement | Epers_Residence
    | Epers_Retired | Epers_ScellentChildLDS | Epers_ScellentParentLDS
    | Epers_ScellentSpouseLDS | Epers_VenteBien | Epers_Will ) as e ->
      (Util.string_of_pevent_name conf base e :> string)

let fevent conf base (e : fevent) =
  match e with
  | Efam_Name s -> esc s
  | ( Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage
    | Efam_Divorce | Efam_Separated | Efam_Annulation | Efam_MarriageBann
    | Efam_MarriageContract | Efam_MarriageLicense | Efam_PACS
    | Efam_Residence ) as e ->
      (Util.string_of_fevent_name conf base e :> string)

(* ---- messages ------------------------------------------------------------ *)

(* The messages of Update.print_warning (same lexicon entries), rebuilt from
   the keys stored in the report. *)
let print_message conf base resolve a =
  let who = who conf base resolve in
  let out = Output.print_sstring conf in
  let fmt f = Util.fcapitale (Util.ftransl conf f) in
  let children msg c p1 p2 =
    Output.printf conf (fmt msg) (fun _ -> who c.father) (fun _ -> who c.mother);
    out ":<ul class=\"mb-0\"><li>";
    out (who p1);
    out "</li><li>";
    out (who p2);
    out "</li></ul>"
  in
  let parent_age p key n =
    out (who p);
    out " ";
    out (tr conf key);
    out " (";
    out (age_text conf n);
    out ")"
  in
  let label_and key subject =
    out (ctr conf key);
    out (tr conf ":");
    out " ";
    out subject
  in
  match a with
  | BigAgeBetweenSpouses (p1, p2, n) ->
      Output.printf conf
        (fmt "the difference of age between %t and %t is quite important")
        (fun _ -> who p1)
        (fun _ -> who p2);
      out (tr conf ":");
      out " ";
      out (age_text conf n)
  | BirthAfterDeath p ->
      Output.printf conf (fmt "%t died before his/her birth") (fun _ -> who p)
  | ChangedOrderOfChildren c ->
      let arg p = (who p, p.first_name) in
      out
        (Utf8.capitalize_fst
           (Util.transl_args conf "changed order of children of %1 and %2"
              [ arg c.father; arg c.mother ]))
  | ChangedOrderOfFamilyEvents c ->
      label_and "changed order of family's events"
        (who c.father ^ " &amp; " ^ who c.mother)
  | ChangedOrderOfMarriages p -> label_and "changed order of marriages" (who p)
  | ChangedOrderOfPersonEvents p ->
      label_and "changed order of person's events" (who p)
  | ChildrenNotInOrder (c, p1, p2) ->
      children "the following children of %t and %t are not in order" c p1 p2
  | CloseChildren (c, p1, p2) ->
      children "the following children of %t and %t are born very close" c p1
        p2
  | DistantChildren (c, p1, p2) ->
      children "the following children of %t and %t are born very distant" c p1
        p2
  | DeadOld (p, n) ->
      out (who p);
      out " ";
      out (Util.transl_nth conf "died at an advanced age" (sex_index resolve p));
      out " (";
      out (age_text conf n);
      out ")"
  | DeadTooEarlyToBeFather (father, child) ->
      Output.printf conf
        (Util.ftransl conf
           "%t is born more than 2 years after the death of his/her father %t")
        (fun _ -> who child)
        (fun _ -> who father)
  | FEventOrder (p, e1, e2) ->
      Output.printf conf
        (fmt "%t's %s before his/her %s")
        (fun _ -> who p)
        (fevent conf base e1) (fevent conf base e2)
  | FWitnessEventAfterDeath (p, e, _) ->
      Output.printf conf
        (fmt "%t witnessed the %s after his/her death")
        (fun _ -> who p)
        (fevent conf base e)
  | FWitnessEventBeforeBirth (p, e, _) ->
      Output.printf conf
        (fmt "%t witnessed the %s before his/her birth")
        (fun _ -> who p)
        (fevent conf base e)
  | IncoherentAncestorDate (anc, p) ->
      Output.printf conf
        (fmt "%t has a younger ancestor %t")
        (fun _ -> who p)
        (fun _ -> who anc)
  | IncoherentSex p ->
      Output.printf conf
        (fmt "%t's sex is not coherent with his/her relations")
        (fun _ -> who p)
  | MarriageDateAfterDeath p ->
      Output.printf conf
        (fmt "marriage had occurred after the death of %t")
        (fun _ -> who p)
  | MarriageDateBeforeBirth p ->
      Output.printf conf
        (fmt "marriage had occurred before the birth of %t")
        (fun _ -> who p)
  | MotherDeadBeforeChildBirth (mother, child) ->
      Output.printf conf
        (Util.ftransl conf "%t is born after the death of his/her mother %t")
        (fun _ -> who child)
        (fun _ -> who mother)
  | ParentBornAfterChild (p, c) ->
      out (who p);
      out " ";
      out (tr conf "is born after his/her child");
      out " ";
      out (who c)
  | ParentTooYoung (p, n, _) ->
      parent_age p "is a very young parent" n
  | ParentTooOld (p, n, _) -> parent_age p "is a very old parent" n
  | PEventOrder (p, e1, e2) ->
      Output.printf conf
        (fmt "%t's %s before his/her %s")
        (fun _ -> who p)
        (pevent conf base e1) (pevent conf base e2)
  | PossibleDuplicateFam c ->
      Output.printf conf
        (fmt "%s and %s have several unions")
        (who c.father) (who c.mother)
  | PossibleDuplicateFamHomonymous (c, p) ->
      let curr, hom =
        if person_key p = person_key c.father then (c.mother, c.father)
        else (c.father, c.mother)
      in
      Output.printf conf
        (fmt "%s has unions with several persons named %s")
        (who curr) (who hom)
  | PWitnessEventAfterDeath (p, e, _) ->
      Output.printf conf
        (fmt "%t witnessed the %s after his/her death")
        (fun _ -> who p)
        (pevent conf base e)
  | PWitnessEventBeforeBirth (p, e, _) ->
      Output.printf conf
        (fmt "%t witnessed the %s before his/her birth")
        (fun _ -> who p)
        (pevent conf base e)
  | TitleDatesError (p, title) ->
      Output.printf conf
        (fmt "%t has incorrect title dates: %t")
        (fun _ -> who p)
        (fun _ -> "<strong>" ^ esc title ^ "</strong>")
  | UndefinedSex p ->
      Output.printf conf (fmt "undefined sex for %t") (fun _ -> who p)
  | YoungForMarriage (p, n, _) | OldForMarriage (p, n, _) ->
      out (who p);
      out " ";
      Output.printf conf
        (Util.ftransl conf "married at age %t")
        (fun _ -> age_text conf n)

(* ---- edit links ---------------------------------------------------------- *)

let wrench = {|<i class="fa fa-wrench fa-xs" aria-hidden="true"></i>|}

let edit_person conf resolve p =
  match resolve p with
  | Some dp ->
      Printf.sprintf {|<a class="ms-2" href="%s" title="%s">%s</a>|}
        (url conf
           [ ("m", "MOD_IND"); ("i", Driver.Iper.to_string (Driver.get_iper dp)) ])
        (esc (ctr conf "modify" ^ " " ^ person_key p))
        wrench
  | None -> ""

(* the unions of the couple, as they are now *)
let edit_couple conf base resolve c =
  match (resolve c.father, resolve c.mother) with
  | Some f, Some m ->
      let ip_f = Driver.get_iper f and ip_m = Driver.get_iper m in
      Driver.get_family f |> Array.to_list
      |> List.filter_map (fun ifam ->
          let fam = Driver.foi base ifam in
          let a = Driver.get_father fam and b = Driver.get_mother fam in
          if
            (Driver.Iper.equal a ip_f && Driver.Iper.equal b ip_m)
            || (Driver.Iper.equal a ip_m && Driver.Iper.equal b ip_f)
          then
            Some
              (Printf.sprintf {|<a class="ms-2" href="%s" title="%s">%s</a>|}
                 (url conf
                    [
                      ("m", "MOD_FAM");
                      ("i", Driver.Ifam.to_string ifam);
                      ("ip", Driver.Iper.to_string ip_f);
                    ])
                 (esc
                    (ctr conf "modify" ^ " " ^ person_key c.father ^ " & "
                   ^ person_key c.mother))
                 wrench)
          else None)
      |> String.concat ""
  | Some _, None | None, Some _ | None, None -> ""

(* ---- report state -------------------------------------------------------- *)

type state = {
  report : report;
  whitelist : AnomalyWhitelist.t;
  changes : (string, change) Hashtbl.t;
}

let last_change st a =
  List.find_map (fun p -> Hashtbl.find_opt st.changes (person_key p)) (persons a)

let verified st a = AnomalyWhitelist.mem st.whitelist a

let page conf ~title body =
  let title_str = ctr conf title in
  Hutil.header conf (fun _ -> Output.print_sstring conf (esc title_str));
  body ();
  Hutil.trailer conf

let print_no_report conf msg =
  Output.status conf Code.Not_Found;
  page conf ~title:"anomalies" (fun () ->
      Output.printf conf {|<div class="alert alert-info">%s</div>|}
        (esc (ctr conf "ANOMALY no report"));
      if conf.debug then Output.printf conf "<pre>%s</pre>" (esc msg))

let print_report_info conf st =
  let r = st.report in
  Output.printf conf {|<p class="text-muted">%s|}
    (esc
       (Printf.sprintf
          (Util.ftransl conf "ANOMALY analysis of %s by %s")
          r.date r.producer));
  let pending = AnomalyWhitelist.pending (config_dir conf) in
  Output.printf conf {| · <a href="%s">%s</a> (%d)|}
    (url conf [ ("m", "OK_FILE") ])
    (esc (tr conf "ANOMALY verified anomalies"))
    (AnomalyWhitelist.size st.whitelist);
  if pending <> [] then
    Output.printf conf {| · <a href="%s">%s</a> (%d)|}
      (url conf [ ("m", "OK_FOLD") ])
      (esc (tr conf "ANOMALY consolidate"))
      (List.length pending);
  Output.print_sstring conf "</p>\n"

(* ---- summary ------------------------------------------------------------- *)

type counts = { mutable found : int; mutable ok : int }

let print_counts_table conf kinds counts =
  Output.printf conf
    {|<table class="table table-sm table-hover w-auto"><thead><tr><th>%s</th><th class="text-end">%s</th><th class="text-end">%s</th><th class="text-end">%s</th></tr></thead><tbody>|}
    (esc (ctr conf "type"))
    (esc (ctr conf "ANOMALY found"))
    (esc (Utf8.capitalize_fst (Util.transl_nth conf "ANOMALY verified" 1)))
    (esc (ctr conf "ANOMALY to check"));
  List.iter
    (fun k ->
      let c = counts k in
      if c.found > 0 then
        Output.printf conf
          {|<tr><td><a href="%s">%s</a></td><td class="text-end">%d</td><td class="text-end">%d</td><td class="text-end fw-bold">%d</td></tr>|}
          (url conf [ ("m", "ANOMA"); ("k", kind_name k) ])
          (esc (kind_label conf k))
          c.found c.ok (c.found - c.ok))
    kinds;
  Output.print_sstring conf "</tbody></table>\n"

(* "extreme" ages, among the anomalies left to check *)
let extremes =
  [
    ("ANOMALY married at 11 or younger", K_YoungForMarriage, None, Some 11);
    ("ANOMALY married at 12", K_YoungForMarriage, Some 12, Some 12);
    ("ANOMALY died aged 100 to 109", K_DeadOld, Some 100, Some 109);
    ("ANOMALY died aged 110 to 119", K_DeadOld, Some 110, Some 119);
    ("ANOMALY died aged 120 or more", K_DeadOld, Some 120, None);
  ]

let in_range amin amax a =
  match age a with
  | Some n ->
      Option.fold ~none:true ~some:(fun m -> n >= m) amin
      && Option.fold ~none:true ~some:(fun m -> n <= m) amax
  | None -> amin = None && amax = None

let print_extremes conf todo =
  let rows =
    List.filter_map
      (fun (label, k, amin, amax) ->
        let n =
          List.fold_left
            (fun n a -> if kind a = k && in_range amin amax a then n + 1 else n)
            0 todo
        in
        if n = 0 then None
        else
          let range =
            List.filter_map Fun.id
              [
                Option.map (fun m -> ("amin", string_of_int m)) amin;
                Option.map (fun m -> ("amax", string_of_int m)) amax;
              ]
          in
          Some
            (Printf.sprintf
               {|<li><a href="%s">%s</a> <span class="badge bg-secondary">%d</span></li>|}
               (url conf (("m", "ANOMA") :: ("k", kind_name k) :: range))
               (esc (ctr conf label))
               n))
      extremes
  in
  if rows <> [] then (
    Output.printf conf "<h2>%s</h2>\n<ul>"
      (esc (ctr conf "ANOMALY extreme ages"));
    List.iter (Output.print_sstring conf) rows;
    Output.print_sstring conf "</ul>\n")

let print_several conf base resolve todo =
  let tbl = Hashtbl.create 1024 in
  List.iter
    (fun a ->
      let k = kind a in
      List.iter
        (fun s ->
          let ps =
            match s with Person p -> [ p ] | Couple c -> [ c.father; c.mother ]
          in
          List.iter
            (fun p ->
              if not (is_unnamed p) then
                let key = person_key p in
                let _, kinds =
                  Option.value ~default:(p, []) (Hashtbl.find_opt tbl key)
                in
                if not (List.mem k kinds) then
                  Hashtbl.replace tbl key (p, k :: kinds))
            ps)
        (subjects a))
    todo;
  let several =
    Hashtbl.fold
      (fun _ (p, kinds) acc ->
        if List.compare_length_with kinds 2 >= 0 then (p, kinds) :: acc else acc)
      tbl []
    |> List.sort (fun (p1, k1) (p2, k2) ->
        match compare (List.length k2) (List.length k1) with
        | 0 -> compare (person_key p1) (person_key p2)
        | c -> c)
  in
  if several <> [] then (
    Output.printf conf "<h2>%s</h2>\n<ul>"
      (esc (ctr conf "ANOMALY persons with several anomalies"));
    List.iteri
      (fun i (p, kinds) ->
        if i < page_size then
          Output.printf conf "<li>%s%s — %s</li>\n" (who conf base resolve p)
            (edit_person conf resolve p)
            (String.concat ", "
               (List.map
                  (fun k ->
                    Printf.sprintf {|<a href="%s">%s</a>|}
                      (url conf [ ("m", "ANOMA"); ("k", kind_name k) ])
                      (esc (kind_label conf k)))
                  (List.rev kinds))))
      several;
    Output.print_sstring conf "</ul>\n")

let print_summary conf base st anomalies =
  let resolve = resolver base in
  let counts = Hashtbl.create 32 in
  let get k =
    match Hashtbl.find_opt counts k with
    | Some c -> c
    | None ->
        let c = { found = 0; ok = 0 } in
        Hashtbl.add counts k c;
        c
  in
  let todo =
    List.filter
      (fun a ->
        let c = get (kind a) in
        c.found <- c.found + 1;
        if verified st a then (
          c.ok <- c.ok + 1;
          false)
        else true)
      anomalies
  in
  let hidden = hidden_kinds conf in
  let shown, others =
    List.partition (fun k -> not (List.mem k hidden)) all_kinds
  in
  page conf ~title:"anomalies" (fun () ->
      print_report_info conf st;
      if anomalies = [] then
        Output.printf conf {|<div class="alert alert-success">%s</div>|}
          (esc (ctr conf "ANOMALY none found"))
      else (
        print_counts_table conf shown get;
        if List.exists (fun k -> (get k).found > 0) others then (
          Output.printf conf {|<details class="mb-3"><summary>%s</summary>|}
            (esc (ctr conf "ANOMALY other checks"));
          print_counts_table conf others get;
          Output.print_sstring conf "</details>\n");
        let todo_shown = List.filter (fun a -> List.mem (kind a) shown) todo in
        print_extremes conf todo_shown;
        print_several conf base resolve todo_shown))

(* ---- list of one kind ---------------------------------------------------- *)

(* The filters of a list page, and the URL showing them. *)
type filters = {
  k : kind;
  all : bool;
  wiz : string option;
  amin : int option;
  amax : int option;
  pg : int;
}

let filters_url conf f =
  url conf
    (List.filter_map Fun.id
       [
         Some ("m", "ANOMA");
         Some ("k", kind_name f.k);
         (if f.all then Some ("all", "1") else None);
         Option.map (fun w -> ("wiz", w)) f.wiz;
         Option.map (fun m -> ("amin", string_of_int m)) f.amin;
         Option.map (fun m -> ("amax", string_of_int m)) f.amax;
         (if f.pg > 1 then Some ("pg", string_of_int f.pg) else None);
       ])

let filter_links conf f wizards =
  let f = { f with pg = 1 } in
  let link f' label active =
    Printf.sprintf {|<a class="btn btn-sm %s me-1 mb-1" href="%s">%s</a>|}
      (if active then "btn-primary" else "btn-outline-secondary")
      (filters_url conf f') label
  in
  Output.print_sstring conf {|<div class="mb-2">|};
  if wizards <> [] then (
    Output.printf conf {|<span class="me-2">%s%s</span>|}
      (esc (ctr conf "ANOMALY last modified by"))
      (esc (tr conf ":"));
    Output.print_sstring conf
      (link { f with wiz = None } (esc (ctr conf "all")) (f.wiz = None));
    List.iter
      (fun (w, n) ->
        Output.print_sstring conf
          (link { f with wiz = Some w }
             (Printf.sprintf
                "%s <span class=\"badge bg-light text-dark\">%d</span>" (esc w)
                n)
             (f.wiz = Some w)))
      wizards;
    Output.print_sstring conf "<br>");
  if AnomalyWhitelist.can_verify f.k then
    Output.print_sstring conf
      (link { f with all = not f.all }
         (esc (ctr conf "ANOMALY show verified anomalies"))
         f.all);
  if f.amin <> None || f.amax <> None then
    Output.print_sstring conf
      (link
         { f with amin = None; amax = None }
         (esc
            (ctr conf "ANOMALY all ages"
            ^ Printf.sprintf " (%s–%s)"
                (Option.fold ~none:"" ~some:string_of_int f.amin)
                (Option.fold ~none:"" ~some:string_of_int f.amax)))
         false);
  Output.print_sstring conf "</div>\n"

(* The list of a page: what the "verified" buttons (anomalies.js) need. *)
let list_open conf =
  Output.printf conf
    {|<ul class="list-group" id="anomaly-list" data-submit="%s" data-mark="%s" data-cancel="%s" data-verified="%s" data-failed="%s">|}
    (esc (Util.commd conf :> string))
    (esc (tr conf "ANOMALY mark as verified"))
    (esc (tr conf "ANOMALY cancel verification"))
    (esc (Util.transl_nth conf "ANOMALY verified" 0))
    (esc (tr conf "ANOMALY failed"))

let day date = if String.length date >= 10 then String.sub date 0 10 else date

let print_item ?(with_kind = false) conf base resolve ~entry ~change a =
  let verified = entry <> None in
  Output.printf conf {|<li class="list-group-item%s">|}
    (if verified then " list-group-item-success" else "");
  if with_kind then
    Output.printf conf {|<div class="small text-muted">%s</div>|}
      (esc (kind_label conf (kind a)));
  print_message conf base resolve a;
  Output.print_sstring conf {|<div class="small mt-1">|};
  List.iter
    (fun s ->
      Output.print_sstring conf
        (match s with
        | Person p -> edit_person conf resolve p
        | Couple c -> edit_couple conf base resolve c))
    (subjects a);
  (match entry with
  | Some (e : AnomalyWhitelist.entry) ->
      Output.printf conf
        {|<span class="badge bg-success ms-2 anomaly-badge">%s</span>|}
        (esc
           (if e.wizard = "" then
              Printf.sprintf
                (Util.ftransl conf "ANOMALY verified on %s")
                (day e.date)
            else
              Printf.sprintf
                (Util.ftransl conf "ANOMALY verified by %s on %s")
                e.wizard (day e.date)))
  | None -> ());
  if
    AnomalyWhitelist.can_verify (kind a)
    && not (List.exists is_unnamed (persons a))
  then
    Output.printf conf
      {|<button type="button" class="btn btn-sm %s py-0 ms-2 anomaly-verify" data-anomaly="%s" data-verified="%s">%s</button>|}
      (if verified then "btn-outline-secondary" else "btn-outline-success")
      (esc (Yojson.Safe.to_string (Anomaly.to_json a)))
      (if verified then "1" else "0")
      (esc
         (tr conf
            (if verified then "ANOMALY cancel verification"
             else "ANOMALY mark as verified")));
  (match change with
  | Some c ->
      Output.printf conf {|<span class="text-muted ms-2">· %s %s</span>|}
        (esc c.wizard) (esc (day c.time))
  | None -> ());
  Output.print_sstring conf "</div></li>\n"

let print_kind conf base st anomalies k =
  let resolve = resolver base in
  let f =
    {
      k;
      all = Util.p_getenv conf.env "all" = Some "1";
      wiz =
        (match Util.p_getenv conf.env "wiz" with
        | Some "" | None -> None
        | Some w -> Some w);
      amin = Util.p_getint conf.env "amin";
      amax = Util.p_getint conf.env "amax";
      pg = 1;
    }
  in
  let of_kind =
    List.filter
      (fun a ->
        kind a = k && in_range f.amin f.amax a && (f.all || not (verified st a)))
      anomalies
  in
  let wizards =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun a ->
        match last_change st a with
        | Some c ->
            Hashtbl.replace tbl c.wizard
              (1 + Option.value ~default:0 (Hashtbl.find_opt tbl c.wizard))
        | None -> ())
      of_kind;
    Hashtbl.fold (fun w n acc -> (w, n) :: acc) tbl [] |> List.sort compare
  in
  let items =
    match f.wiz with
    | None -> of_kind
    | Some w ->
        List.filter
          (fun a ->
            match last_change st a with
            | Some c -> c.wizard = w
            | None -> false)
          of_kind
  in
  let total = List.length items in
  let pages = max 1 ((total + page_size - 1) / page_size) in
  let pg =
    match Util.p_getint conf.env "pg" with
    | Some n when n >= 1 && n <= pages -> n
    | Some _ | None -> 1
  in
  page conf ~title:"anomalies" (fun () ->
      Output.printf conf {|<p><a href="%s">%s</a></p>|}
        (url conf [ ("m", "ANOMA") ])
        (esc (ctr conf "ANOMALY all anomalies"));
      print_report_info conf st;
      Output.printf conf
        "<h2>%s <span class=\"badge bg-secondary\">%d</span></h2>\n"
        (esc (kind_label conf k))
        total;
      filter_links conf f wizards;
      list_open conf;
      List.iteri
        (fun i a ->
          if i >= (pg - 1) * page_size && i < pg * page_size then
            print_item conf base resolve
              ~entry:(AnomalyWhitelist.find st.whitelist a)
              ~change:(last_change st a) a)
        items;
      Output.print_sstring conf "</ul>\n";
      if pages > 1 then (
        Output.print_sstring conf
          {|<nav class="mt-2"><ul class="pagination pagination-sm flex-wrap">|};
        for n = 1 to pages do
          Output.printf conf
            {|<li class="page-item%s"><a class="page-link" href="%s">%d</a></li>|}
            (if n = pg then " active" else "")
            (filters_url conf { f with pg = n })
            n
        done;
        Output.print_sstring conf "</ul></nav>\n"))

(* ---- m=ANOMA ------------------------------------------------------------- *)

let print conf base =
  let dir = config_dir conf in
  let kind = Option.bind (Util.p_getenv conf.env "k") kind_of_name in
  (* a list page reads only the lines of its kind *)
  match Anomaly.fold ?kind dir (fun acc a -> a :: acc) [] with
  | Error msg -> print_no_report conf msg
  | Ok (report, rev) -> (
      let anomalies = List.rev rev in
      let changes = Hashtbl.create 1024 in
      List.iter (fun (k, c) -> Hashtbl.replace changes k c) report.changes;
      let st = { report; whitelist = AnomalyWhitelist.load dir; changes } in
      match kind with
      | Some k -> print_kind conf base st anomalies k
      | None -> print_summary conf base st anomalies)

(* ---- m=OK_ADD ------------------------------------------------------------ *)

let plain conf status msg =
  Output.status conf status;
  Util.html ~content_type:"text/plain" conf;
  Output.print_sstring conf msg;
  Output.print_sstring conf "\n"

let record ~verified conf _base =
  let anomaly =
    Option.bind (Util.p_getenv conf.env "a") (fun s ->
        match Yojson.Safe.from_string s with
        | j -> Anomaly.of_json j
        | exception Yojson.Json_error _ -> None)
  in
  match anomaly with
  | None -> plain conf Code.Bad_Request "missing or malformed anomaly"
  | Some a -> (
      match
        AnomalyWhitelist.record (config_dir conf) ~wizard:conf.user ~verified a
      with
      | Ok () -> plain conf Code.OK "OK"
      | Error msg -> plain conf Code.Bad_Request msg)

let print_ok_add = record ~verified:true
let print_ok_del = record ~verified:false

(* ---- m=OK_FILE ----------------------------------------------------------- *)

let print_ok_file conf base =
  let dir = config_dir conf in
  let entries = AnomalyWhitelist.entries (AnomalyWhitelist.load dir) in
  let resolve = resolver base in
  let total = List.length entries in
  let pages = max 1 ((total + page_size - 1) / page_size) in
  let pg =
    match Util.p_getint conf.env "pg" with
    | Some n when n >= 1 && n <= pages -> n
    | Some _ | None -> 1
  in
  page conf ~title:"ANOMALY verified anomalies" (fun () ->
      Output.printf conf {|<p><a href="%s">%s</a></p>|}
        (url conf [ ("m", "ANOMA") ])
        (esc (ctr conf "ANOMALY all anomalies"));
      (match AnomalyWhitelist.pending dir with
      | [] -> ()
      | frags ->
          Output.printf conf
            {|<p>%s (%d) <a class="btn btn-sm btn-outline-primary ms-2" href="%s">%s</a></p>|}
            (esc (ctr conf "ANOMALY pending contributions"))
            (List.length frags)
            (url conf [ ("m", "OK_FOLD") ])
            (esc (ctr conf "ANOMALY consolidate")));
      if entries = [] then
        Output.printf conf {|<p class="text-muted">%s</p>|}
          (esc (ctr conf "ANOMALY no verified anomaly yet"))
      else (
        list_open conf;
        List.iteri
          (fun i (e : AnomalyWhitelist.entry) ->
            if i >= (pg - 1) * page_size && i < pg * page_size then
              print_item ~with_kind:true conf base resolve ~entry:(Some e)
                ~change:None e.anomaly)
          entries;
        Output.print_sstring conf "</ul>\n";
        if pages > 1 then (
          Output.print_sstring conf
            {|<nav class="mt-2"><ul class="pagination pagination-sm flex-wrap">|};
          for n = 1 to pages do
            Output.printf conf
              {|<li class="page-item%s"><a class="page-link" href="%s">%d</a></li>|}
              (if n = pg then " active" else "")
              (url conf [ ("m", "OK_FILE"); ("pg", string_of_int n) ])
              n
          done;
          Output.print_sstring conf "</ul></nav>\n")))

(* ---- m=OK_FOLD ----------------------------------------------------------- *)

let print_ok_fold conf _base =
  let dir = config_dir conf in
  page conf ~title:"ANOMALY consolidate" (fun () ->
      (match AnomalyWhitelist.fold dir with
      | r ->
          Output.printf conf {|<div class="alert alert-success">%s</div>|}
            (esc
               (Printf.sprintf
                  (Util.ftransl conf
                     "ANOMALY %d contributions merged, %d verified anomalies")
                  r.fragments r.entries))
      | exception (Sys_error _ as e) | exception (Unix.Unix_error _ as e) ->
          Output.printf conf {|<div class="alert alert-danger">%s</div>|}
            (esc (Printexc.to_string e)));
      Output.printf conf {|<p><a href="%s">%s</a> · <a href="%s">%s</a></p>|}
        (url conf [ ("m", "OK_FILE") ])
        (esc (ctr conf "ANOMALY verified anomalies"))
        (url conf [ ("m", "ANOMA") ])
        (esc (ctr conf "ANOMALY all anomalies")))
