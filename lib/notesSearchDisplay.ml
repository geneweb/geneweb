open Config
open Util
module Driver = Geneweb_db.Driver

(* Groups of searched notes, also the scopes of the form, in display order. *)
type group = Misc | Wiz | Corpus of NotesCorpus.kind

let groups =
  [
    Misc;
    Wiz;
    Corpus NotesCorpus.Person;
    Corpus NotesCorpus.Family;
    Corpus NotesCorpus.Person_event;
    Corpus NotesCorpus.Family_event;
  ]

let group_index g =
  match g with
  | Misc -> 0
  | Wiz -> 1
  | Corpus NotesCorpus.Person -> 2
  | Corpus NotesCorpus.Family -> 3
  | Corpus NotesCorpus.Person_event -> 4
  | Corpus NotesCorpus.Family_event -> 5

let group_param g =
  match g with
  | Misc -> "sc_m"
  | Wiz -> "sc_w"
  | Corpus NotesCorpus.Person -> "sc_p"
  | Corpus NotesCorpus.Family -> "sc_f"
  | Corpus NotesCorpus.Person_event -> "sc_pe"
  | Corpus NotesCorpus.Family_event -> "sc_fe"

let group_label conf g =
  transl_nth conf "notes search scopes" (group_index g) |> Utf8.capitalize_fst

(* Who may search what: misc notes, everybody (each note keeps its RESTRICT
   rule); wizard notes, friends and wizards when the base has them (as the
   welcome page shows them); person, family and event notes, friends and
   wizards. *)
let allowed conf g =
  match g with
  | Misc -> true
  | Wiz ->
      conf.authorized_wizards_notes
      && (conf.wizard || conf.friend)
      && WiznotesDisplay.wizard_auth_file_name conf <> ""
  | Corpus _ -> conf.wizard || conf.friend

type query = {
  text : string;  (** as typed *)
  case_sens : bool;
  full_word : bool;
  terms : string list * string list;  (** required, excluded *)
  scope : group list;  (** searched groups, all allowed *)
}

(* the ticked scopes the user may search; all of those when none is ticked *)
let query_of_env conf =
  let text =
    match p_getenv conf.env "s" with Some s -> String.trim s | None -> ""
  in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let allowed_groups = List.filter (allowed conf) groups in
  let ticked =
    List.filter
      (fun g -> p_getenv conf.env (group_param g) = Some "on")
      allowed_groups
  in
  {
    text;
    case_sens;
    full_word = p_getenv conf.env "word" = Some "on";
    terms = NotesSearch.query case_sens text;
    scope = (match ticked with [] -> allowed_groups | _ :: _ -> ticked);
  }

let in_scope q g = List.mem g q.scope

let max_results conf =
  match List.assoc_opt "notes_search_max_results" conf.base_env with
  | Some s -> (
      match int_of_string_opt (String.trim s) with
      | Some n when n > 0 -> n
      | Some _ | None -> 500)
  | None -> 500

type subject =
  | Of_page of { href : string; label : string; icon : string; file : string }
      (** misc or wizard note; [label] is HTML *)
  | Of_person of Driver.person
  | Of_family of Driver.ifam * Driver.family * Driver.person * Driver.person
      (** family, its father and mother *)

type hit = {
  group : group;
  subject : subject;
  event : Adef.safe_string;  (** event name, empty for plain notes *)
  rank : int;  (** misc notes: terms found in the title, listed first *)
  count : int;
  snippet : string;  (** escaped HTML *)
}

let matches q text =
  NotesSearch.matches ~full_word:q.full_word q.case_sens q.terms text

let count q text =
  NotesSearch.count_occurrences ~full_word:q.full_word q.case_sens
    (fst q.terms) text

let snippet q text =
  NotesSearch.snippet ~full_word:q.full_word q.case_sens (fst q.terms) text

(* parameters reproducing the query in a link *)
let query_params q =
  "s="
  ^ (Mutil.encode q.text :> string)
  ^ (if q.case_sens then "&c=on" else "")
  ^ if q.full_word then "&word=on" else ""

(* Misc notes (notes_d, base notes), read live: they are few. A note is a
   hit when every required term occurs in its title or text and no excluded
   one does. Without word match, Util.in_text on the source is also tried. *)
let misc_hits conf base q =
  let pages =
    ""
    :: List.filter_map
         (fun (fnotes, _) ->
           if fnotes = "" || Notes.note_file base fnotes = None then None
           else Some fnotes)
         (Notes.notes_links_db conf base true)
  in
  List.filter_map
    (fun fnotes ->
      let nenv, nt = Notes.read_notes base fnotes in
      if Notes.note_is_restricted conf base nenv then None
      else
        let tit = Option.value ~default:"" (List.assoc_opt "TITLE" nenv) in
        let plain = NotesSearch.plain_text nt in
        let plain_tit = NotesSearch.plain_text tit in
        let in_tit t =
          NotesSearch.mem_all ~full_word:q.full_word q.case_sens [ t ] plain_tit
        in
        let present t =
          in_tit t
          || ((not q.full_word) && in_text q.case_sens (NotesSearch.core t) nt)
          || NotesSearch.mem_all ~full_word:q.full_word q.case_sens [ t ] plain
        in
        let required, excluded = q.terms in
        if List.for_all present required && not (List.exists present excluded)
        then
          let gallery = Notes.is_gallery nenv in
          let href =
            if gallery then
              Printf.sprintf "%sm=GALLERY&f=%s"
                (commd conf :> string)
                (Mutil.encode fnotes :> string)
            else
              Printf.sprintf "%sm=MISC_NOTES_SEARCH&%s&f=%s#first-found"
                (commd conf :> string)
                (query_params q)
                (Mutil.encode fnotes :> string)
          in
          let label =
            if plain_tit <> "" then
              NotesSearch.highlight ~full_word:q.full_word q.case_sens required
                plain_tit
            else if fnotes = "" then
              (Util.escape_html (Utf8.capitalize_fst (transl conf "base notes"))
                :> string)
            else (Util.escape_html fnotes :> string)
          in
          Some
            {
              group = Misc;
              subject =
                Of_page
                  {
                    href;
                    label;
                    icon = (if gallery then "image" else "file-lines");
                    file =
                      Option.value ~default:"" (Notes.note_file base fnotes);
                  };
              event = Adef.safe "";
              rank = List.length (List.filter in_tit required);
              count = count q plain;
              snippet = snippet q plain;
            }
        else None)
    pages

(* wizard notes, read live as well *)
let wiz_hits conf base q =
  List.filter_map
    (fun wz ->
      let nt = WiznotesDisplay.wizard_note conf base wz in
      let plain = NotesSearch.plain_text nt in
      let present t =
        ((not q.full_word) && in_text q.case_sens (NotesSearch.core t) nt)
        || NotesSearch.mem_all ~full_word:q.full_word q.case_sens [ t ] plain
      in
      let required, excluded = q.terms in
      if List.for_all present required && not (List.exists present excluded)
      then
        Some
          {
            group = Wiz;
            subject =
              Of_page
                {
                  href =
                    Printf.sprintf "%sm=WIZNOTES_SEARCH&%s&f=%s"
                      (commd conf :> string)
                      (query_params q)
                      (Mutil.encode wz :> string);
                  label =
                    (Util.escape_html (WiznotesDisplay.wizard_display_name conf wz)
                      :> string);
                  icon = "file-lines";
                  file =
                    Driver.base_wiznotes_dir base ^ "/" ^ wz ^ ".txt";
                };
            event = Adef.safe "";
            rank = 0;
            count = count q plain;
            snippet = snippet q plain;
          }
      else None)
    (WiznotesDisplay.wizards conf base)

let hit_of_entry q subject event (e : NotesCorpus.entry) =
  if in_scope q (Corpus e.kind) && matches q e.text then
    Some
      {
        group = Corpus e.kind;
        subject;
        event;
        rank = 0;
        count = count q e.text;
        snippet = snippet q e.text;
      }
  else None

(* Live hits of a person: re-read, access-checked, matched on the current
   text. *)
let person_hits conf base q iper =
  let p = pget conf base iper in
  if conf.no_note || not (authorized_age conf base p) then []
  else
    let pevents = Array.of_list (Driver.get_pevents p) in
    List.filter_map
      (fun (e : NotesCorpus.entry) ->
        let event =
          if e.evt >= 0 && e.evt < Array.length pevents then
            string_of_pevent_name conf base pevents.(e.evt).Def.epers_name
          else Adef.safe ""
        in
        hit_of_entry q (Of_person p) event e)
      (NotesCorpus.person_entries base p)

(* same for a family, visible when both spouses are, as on person pages *)
let family_hits conf base q ifam =
  let fam = Driver.foi base ifam in
  let ifath = Driver.get_father fam and imoth = Driver.get_mother fam in
  if Driver.Iper.is_dummy ifath || Driver.Iper.is_dummy imoth then []
  else
    let fath = pget conf base ifath and moth = pget conf base imoth in
    if
      conf.no_note
      || not (authorized_age conf base fath && authorized_age conf base moth)
    then []
    else
      let fevents = Array.of_list (Driver.get_fevents fam) in
      List.filter_map
        (fun (e : NotesCorpus.entry) ->
          let event =
            if e.evt >= 0 && e.evt < Array.length fevents then
              string_of_fevent_name conf base fevents.(e.evt).Def.efam_name
            else Adef.safe ""
          in
          hit_of_entry q (Of_family (ifam, fam, fath, moth)) event e)
        (NotesCorpus.family_entries base ifam fam)

(* Corpus hits, at most [max], and [true] if the search stopped there. The
   corpus is streamed: an entry whose folded text may match is a candidate,
   checked at once on the live base (access, exact match); persons and
   families changed since the build are skipped there and checked
   afterwards, as their corpus text is stale. Stopping at [max] hits bounds
   the work for common words. *)
let corpus_hits conf base q bname (ch_p, ch_f) max =
  let seen_p = Driver.Iper.Table.create 256 in
  let seen_f = Driver.Ifam.Table.create 256 in
  let may_match = NotesSearch.candidate q.terms in
  let exception Enough of hit list in
  let check (n, acc) hits =
    let n = n + List.length hits in
    let acc = List.rev_append hits acc in
    if n >= max then raise (Enough acc) else (n, acc)
  in
  let from_corpus kind id folded acc =
    if in_scope q (Corpus kind) && may_match folded then
      match kind with
      | NotesCorpus.Person | NotesCorpus.Person_event ->
          let ip = Driver.Iper.of_string id in
          if Driver.Iper.Set.mem ip ch_p || Driver.Iper.Table.mem seen_p ip
          then acc
          else (
            Driver.Iper.Table.add seen_p ip ();
            check acc (person_hits conf base q ip))
      | NotesCorpus.Family | NotesCorpus.Family_event ->
          let ifam = Driver.Ifam.of_string id in
          if Driver.Ifam.Set.mem ifam ch_f || Driver.Ifam.Table.mem seen_f ifam
          then acc
          else (
            Driver.Ifam.Table.add seen_f ifam ();
            check acc (family_hits conf base q ifam))
    else acc
  in
  match
    let acc = NotesCorpus.fold_entries bname from_corpus (0, []) in
    let acc =
      Driver.Iper.Set.fold
        (fun ip acc -> check acc (person_hits conf base q ip))
        ch_p acc
    in
    Driver.Ifam.Set.fold
      (fun ifam acc -> check acc (family_hits conf base q ifam))
      ch_f acc
  with
  | _, acc -> (List.rev acc, false)
  | exception Enough acc -> (List.rev acc, true)

(* "?" button and the panel it opens (Bootstrap collapse, no script). The
   examples are the same in every language; their explanations come from
   the lexicon. *)
let help_id = "notes-search-help"

let print_help_button conf =
  Output.printf conf
    {|<button type="button" class="btn btn-outline-secondary" data-bs-toggle="collapse" data-bs-target="#%s" aria-expanded="false" aria-controls="%s" title="%s"><i class="far fa-circle-question"></i></button>|}
    help_id help_id
    (transl conf "notes search help" |> Utf8.capitalize_fst)

let print_help_panel conf =
  let rows =
    [
      ("moulin dupont", "notes search help and");
      ({|"saint pair"|}, "notes search help phrase");
      ("moulin -dupont", "notes search help exclude");
      ("dupon?", "notes search help joker");
      ("henri* *ette", "notes search help star");
      ("l'a", "notes search help word");
      ("Église", "notes search help case");
    ]
  in
  Output.printf conf
    {|<div class="collapse mt-2" id="%s"><div class="card card-body small"><div class="fw-semibold mb-1">%s</div><dl class="row mb-0">|}
    help_id
    (transl conf "notes search help" |> Utf8.capitalize_fst);
  List.iter
    (fun (example, key) ->
      Output.printf conf
        {|<dt class="col-sm-3"><code>%s</code></dt><dd class="col-sm-9">%s</dd>|}
        (Util.escape_html example :> string)
        (transl conf key |> Utf8.capitalize_fst))
    rows;
  Output.print_sstring conf "</dl></div></div>\n"

(* The form; [suffix] keeps the element ids unique when it is printed twice
   (above and below the results), the help being only in the first one. *)
let print_form ?(suffix = "") ?(help = true) conf q =
  Output.printf conf {|<form class="mb-3" method="get" action="%s">|}
    conf.command;
  hidden_env conf;
  Output.print_sstring conf
    {|<input type="hidden" name="m" value="NOTES_SEARCH">|};
  Output.print_sstring conf
    {|<div class="d-flex flex-wrap align-items-center gap-2 mb-2">|};
  Output.printf conf
    {|<input type="text" name="s" size="40" class="form-control w-auto" value="%s"%s>|}
    (Util.escape_html q.text :> string)
    (if suffix = "" then " autofocus" else "");
  let check name label on =
    let id = name ^ suffix in
    Output.printf conf
      {|<div class="form-check form-check-inline mb-0"><input type="checkbox" name="%s" value="on" class="form-check-input" id="%s"%s><label class="form-check-label text-nowrap" for="%s">%s</label></div>|}
      name id
      (if on then " checked" else "")
      id label
  in
  check "c" (transl_nth conf "search/case sensitive" 1) q.case_sens;
  check "word" (transl conf "word match") q.full_word;
  (* the buttons stay together when the row wraps *)
  Output.printf conf
    {|<div class="d-flex gap-2"><button type="submit" class="btn btn-primary text-nowrap">%s</button>|}
    (transl_nth conf "search/case sensitive" 0 |> Utf8.capitalize_fst);
  if help then print_help_button conf;
  Output.print_sstring conf "</div></div>";
  (* scopes, when there is a choice *)
  (match List.filter (allowed conf) groups with
  | [] | [ _ ] -> ()
  | gs ->
      Output.print_sstring conf {|<div class="d-flex flex-wrap gap-2">|};
      List.iter
        (fun g -> check (group_param g) (group_label conf g) (in_scope q g))
        gs;
      Output.print_sstring conf "</div>");
  Output.print_sstring conf "</form>\n";
  if help then print_help_panel conf

(* Highlighting on the pages opened from the results: the query is passed
   as hl (with hl_c / hl_w for the options), and the notes rendered by
   Perso go through [highlight_note]. *)
let highlight_params q =
  "&hl="
  ^ (Mutil.encode q.text :> string)
  ^ (if q.case_sens then "&hl_c=on" else "")
  ^ if q.full_word then "&hl_w=on" else ""

let highlight_note conf html =
  match p_getenv conf.env "hl" with
  | None | Some "" -> html
  | Some s ->
      let case_sens = p_getenv conf.env "hl_c" = Some "on" in
      let full_word = p_getenv conf.env "hl_w" = Some "on" in
      NotesSearch.highlight_html ~full_word ~anchor:false case_sens
        (NotesSearch.terms case_sens s)
        html

(* Gregorian year of a date, also for partial non-Gregorian dates *)
let year cdate =
  match Date.cdate_to_gregorian_dmy_opt cdate with
  | Some d when d.Adef.year > 0 -> Some d.Adef.year
  | Some _ | None -> None

let death_year p =
  match Driver.get_death p with
  | Def.Death (_, cd) -> year cd
  | Def.NotDead | Def.DeadYoung | Def.DeadDontKnowWhen | Def.DontKnowIfDead
  | Def.OfCourseDead ->
      None

let birth_year p =
  match year (Driver.get_birth p) with
  | Some y -> Some y
  | None -> year (Driver.get_baptism p)

(* "(1813-1880)", "(1813-)", "(-1880)", or nothing *)
let years_text b d =
  let y = Option.fold ~none:"" ~some:string_of_int in
  match (b, d) with
  | None, None -> ""
  | Some _, _ | None, Some _ -> Printf.sprintf "(%s-%s)" (y b) (y d)

(* links without id (a person can appear in several hits), carrying the
   query so that the person page highlights it in the notes *)
let person_link conf base hl p =
  Printf.sprintf {|<a href="%s%s%s">%s</a>|}
    (commd conf :> string)
    (acces conf base p :> string)
    hl
    (person_title_text conf base p :> string)

(* "Jean.1 Dupont", as in the "modify" titles of the templates *)
let key_text base p =
  Driver.sou base (Driver.get_first_name p)
  ^ (match Driver.get_occ p with 0 -> "" | n -> "." ^ string_of_int n)
  ^ " "
  ^ Driver.sou base (Driver.get_surname p)

(* As elsewhere in GeneWeb, the dates are, for wizards, a link to modify the
   person or the family, with a title saying so. *)
let edit_dates conf href title text =
  if conf.wizard && text <> "" then
    Printf.sprintf {|<a href="%s%s" title="%s">%s</a>|}
      (commd conf :> string)
      href
      (Util.escape_html title :> string)
      text
  else text

let subject_html conf base hl subject =
  match subject with
  | Of_page { href; label; icon; file } ->
      Printf.sprintf
        {|<i class="far fa-%s fa-fw me-2"></i><a href="%s">%s</a><code class="small ms-2">%s</code>|}
        icon href label
        (Util.escape_html file :> string)
  | Of_person p ->
      let dates = years_text (birth_year p) (death_year p) in
      person_link conf base hl p
      ^ (if dates = "" then "" else " ")
      ^ edit_dates conf
          ("m=MOD_IND&i=" ^ Driver.Iper.to_string (Driver.get_iper p))
          (Utf8.capitalize_fst (transl conf "modify") ^ " " ^ key_text base p)
          dates
  | Of_family (ifam, fam, fath, moth) ->
      let amp =
        match year (Driver.get_marriage fam) with
        | Some y -> Printf.sprintf "&amp; %d" y
        | None -> "&amp;"
      in
      person_link conf base hl fath
      ^ " "
      ^ edit_dates conf
          (Printf.sprintf "m=MOD_FAM&i=%s&ip=%s" (Driver.Ifam.to_string ifam)
             (Driver.Iper.to_string (Driver.get_iper fath)))
          (Printf.sprintf "%s %s %s %s"
             (Utf8.capitalize_fst
                (transl_decline conf "modify"
                   (transl_nth conf "family/families" 0)))
             (key_text base fath) (transl conf "and") (key_text base moth))
          amp
      ^ " "
      ^ person_link conf base hl moth

let print_hits conf base q hits =
  let hl = highlight_params q in
  List.iter
    (fun g ->
      let l = List.filter (fun h -> h.group = g) hits in
      let l =
        List.stable_sort
          (fun h1 h2 ->
            match Int.compare h2.rank h1.rank with
            | 0 -> Int.compare h2.count h1.count
            | c -> c)
          l
      in
      if l <> [] then (
        Output.printf conf
          {|<h2 class="h4 mt-4">%s <span class="badge text-bg-secondary">%d</span></h2>|}
          (group_label conf g) (List.length l);
        Output.print_sstring conf {|<ul class="list-group mb-3">|};
        List.iter
          (fun h ->
            let event =
              if (h.event :> string) = "" then ""
              else
                Printf.sprintf {|<span class="text-muted ms-2">%s</span>|}
                  (h.event :> string)
            in
            let count =
              if h.count = 0 then ""
              else
                Printf.sprintf
                  {|<span class="badge text-bg-secondary ms-2">%d</span>|}
                  h.count
            in
            Output.printf conf
              {|<li class="list-group-item"><div>%s%s%s</div><div class="small text-muted">%s</div></li>
|}
              (subject_html conf base hl h.subject)
              event count h.snippet)
          l;
        Output.print_sstring conf "</ul>\n"))
    groups

let date_text t =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min

let print_message conf msg =
  Output.printf conf {|<div class="alert alert-warning">%s</div>|} msg

(* hits of the corpus groups in scope, after the corpus status *)
let search_corpus conf base q =
  if not (List.exists (function Corpus _ -> true | Misc | Wiz -> false) q.scope)
  then []
  else
    let bname = Filename.remove_extension conf.bname in
    match NotesCorpus.read bname with
    | NotesCorpus.Missing ->
        print_message conf
          (Printf.sprintf
             (ftransl conf "notes search index missing %s")
             (Util.escape_html bname :> string));
        []
    | NotesCorpus.Outdated ->
        print_message conf
          (Printf.sprintf
             (ftransl conf "notes search index outdated %s")
             (Util.escape_html bname :> string));
        []
    | NotesCorpus.Available corpus ->
        let ((ch_p, ch_f) as changes) = NotesCorpus.changes bname corpus in
        Output.printf conf {|<p class="small text-muted">%s</p>|}
          (Printf.sprintf
             (ftransl conf "notes search index date %s %d")
             (date_text corpus.built)
             (Driver.Iper.Set.cardinal ch_p + Driver.Ifam.Set.cardinal ch_f));
        let max = max_results conf in
        let hits, truncated = corpus_hits conf base q bname changes max in
        if truncated then
          print_message conf
            (Printf.sprintf (ftransl conf "notes search first results %d") max);
        hits

let print conf base =
  let q = query_of_env conf in
  let title _ =
    transl conf "notes search" |> Utf8.capitalize_fst
    |> Output.print_sstring conf
  in
  Hutil.header conf title;
  print_form conf q;
  (match q.terms with
  | [], [] -> ()
  | _ :: _, _ | [], _ :: _ -> (
      let misc = if in_scope q Misc then misc_hits conf base q else [] in
      let wiz = if in_scope q Wiz then wiz_hits conf base q else [] in
      let corpus = search_corpus conf base q in
      (* groups are printed in order whatever the order of the hits *)
      match List.rev_append misc (List.rev_append wiz corpus) with
      | [] ->
          Output.printf conf "<p>%s</p>\n"
            (Utf8.capitalize_fst (transl conf "no match"))
      | hits ->
          print_hits conf base q hits;
          print_form ~suffix:"-bottom" ~help:false conf q));
  Hutil.trailer conf
