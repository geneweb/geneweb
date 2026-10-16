open Config
open Util
module Driver = Geneweb_db.Driver
module Code = Geneweb_http.Code

type scope = { sc_p : bool; sc_f : bool; sc_pe : bool; sc_fe : bool }

(* scopes from the form; all of them when none is ticked *)
let scope_of_env conf =
  let on k = p_getenv conf.env k = Some "on" in
  let sc =
    {
      sc_p = on "sc_p";
      sc_f = on "sc_f";
      sc_pe = on "sc_pe";
      sc_fe = on "sc_fe";
    }
  in
  if sc.sc_p || sc.sc_f || sc.sc_pe || sc.sc_fe then sc
  else { sc_p = true; sc_f = true; sc_pe = true; sc_fe = true }

let in_scope sc kind =
  match kind with
  | NotesCorpus.Person -> sc.sc_p
  | NotesCorpus.Family -> sc.sc_f
  | NotesCorpus.Person_event -> sc.sc_pe
  | NotesCorpus.Family_event -> sc.sc_fe

let kinds = NotesCorpus.[ Person; Family; Person_event; Family_event ]

let scope_index kind =
  match kind with
  | NotesCorpus.Person -> 0
  | NotesCorpus.Family -> 1
  | NotesCorpus.Person_event -> 2
  | NotesCorpus.Family_event -> 3

let scope_param kind =
  match kind with
  | NotesCorpus.Person -> "sc_p"
  | NotesCorpus.Family -> "sc_f"
  | NotesCorpus.Person_event -> "sc_pe"
  | NotesCorpus.Family_event -> "sc_fe"

let scope_label conf kind =
  transl_nth conf "notes search scopes" (scope_index kind)
  |> Utf8.capitalize_fst

let max_results conf =
  match List.assoc_opt "notes_search_max_results" conf.base_env with
  | Some s -> (
      match int_of_string_opt (String.trim s) with
      | Some n when n > 0 -> n
      | Some _ | None -> 500)
  | None -> 500

type subject =
  | Of_person of Driver.person
  | Of_family of Driver.person * Driver.person  (** father, mother *)

type hit = {
  kind : NotesCorpus.kind;
  subject : subject;
  event : Adef.safe_string;  (** event name, empty for plain notes *)
  count : int;
  snippet : string;  (** escaped HTML *)
}

type query = {
  case_sens : bool;
  full_word : bool;
  terms : string list * string list;  (** required, excluded *)
  scope : scope;
}

let hit_of_entry q subject event (e : NotesCorpus.entry) =
  if
    in_scope q.scope e.kind
    && NotesSearch.matches ~full_word:q.full_word q.case_sens q.terms e.text
  then
    let required = fst q.terms in
    Some
      {
        kind = e.kind;
        subject;
        event;
        count =
          NotesSearch.count_occurrences ~full_word:q.full_word q.case_sens
            required e.text;
        snippet =
          NotesSearch.snippet ~full_word:q.full_word q.case_sens required e.text;
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
          hit_of_entry q (Of_family (fath, moth)) event e)
        (NotesCorpus.family_entries base ifam fam)

(* Hits, at most [max], and [true] if the search stopped there. The corpus
   is streamed: an entry whose folded text may match is a candidate, checked
   at once on the live base (access, exact match); persons and families
   changed since the build are skipped there and checked afterwards, as
   their corpus text is stale. Stopping at [max] hits bounds the work for
   common words. *)
let collect conf base q bname (ch_p, ch_f) max =
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
    if in_scope q.scope kind && may_match folded then
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

(* "?" button and the panel it opens (Bootstrap collapse, no script),
   shared with the misc notes search form. The examples are the same in
   every language; their explanations come from the lexicon. *)
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

let print_form conf s q =
  Output.printf conf {|<form class="mb-3" method="get" action="%s">|}
    conf.command;
  hidden_env conf;
  Output.print_sstring conf
    {|<input type="hidden" name="m" value="NOTES_SEARCH">|};
  Output.print_sstring conf
    {|<div class="d-flex flex-wrap align-items-center gap-2 mb-2">|};
  Output.printf conf
    {|<input type="text" name="s" size="40" class="form-control w-auto" value="%s" autofocus>|}
    (Util.escape_html s :> string);
  let check name id label on =
    Output.printf conf
      {|<div class="form-check form-check-inline mb-0"><input type="checkbox" name="%s" value="on" class="form-check-input" id="%s"%s><label class="form-check-label text-nowrap" for="%s">%s</label></div>|}
      name id
      (if on then " checked" else "")
      id label
  in
  check "c" "case" (transl_nth conf "search/case sensitive" 1) q.case_sens;
  check "word" "word" (transl conf "word match") q.full_word;
  (* the buttons stay together when the row wraps *)
  Output.printf conf
    {|<div class="d-flex gap-2"><button type="submit" class="btn btn-primary text-nowrap">%s</button>|}
    (transl_nth conf "search/case sensitive" 0 |> Utf8.capitalize_fst);
  print_help_button conf;
  Output.print_sstring conf "</div></div>";
  Output.print_sstring conf {|<div class="d-flex flex-wrap gap-2">|};
  List.iter
    (fun kind ->
      let p = scope_param kind in
      check p p (scope_label conf kind) (in_scope q.scope kind))
    kinds;
  Output.print_sstring conf "</div></form>\n";
  print_help_panel conf

(* links without id: a person can appear in several hits *)
let person_link conf base p =
  (gen_person_title_text reference_noid conf base p :> string)

let subject_html conf base subject =
  match subject with
  | Of_person p ->
      person_link conf base p ^ " "
      ^ (DateDisplay.short_dates_text conf base p :> string)
  | Of_family (fath, moth) ->
      person_link conf base fath ^ " &amp; " ^ person_link conf base moth

let print_hits conf base hits =
  List.iter
    (fun kind ->
      let l = List.filter (fun h -> h.kind = kind) hits in
      let l = List.stable_sort (fun h1 h2 -> Int.compare h2.count h1.count) l in
      if l <> [] then (
        Output.printf conf
          {|<h2 class="h4 mt-4">%s <span class="badge text-bg-secondary">%d</span></h2>|}
          (scope_label conf kind) (List.length l);
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
              (subject_html conf base h.subject)
              event count h.snippet)
          l;
        Output.print_sstring conf "</ul>\n"))
    kinds

let date_text t =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min

let print_message conf msg =
  Output.printf conf {|<div class="alert alert-warning">%s</div>|} msg

let search conf base q s =
  let bname = Filename.remove_extension conf.bname in
  match NotesCorpus.read bname with
  | NotesCorpus.Missing ->
      print_message conf
        (Printf.sprintf
           (ftransl conf "notes search index missing %s")
           (Util.escape_html bname :> string))
  | NotesCorpus.Outdated ->
      print_message conf
        (Printf.sprintf
           (ftransl conf "notes search index outdated %s")
           (Util.escape_html bname :> string))
  | NotesCorpus.Available corpus ->
      let ((ch_p, ch_f) as changes) = NotesCorpus.changes bname corpus in
      Output.printf conf {|<p class="small text-muted">%s</p>|}
        (Printf.sprintf
           (ftransl conf "notes search index date %s %d")
           (date_text corpus.built)
           (Driver.Iper.Set.cardinal ch_p + Driver.Ifam.Set.cardinal ch_f));
      let max = max_results conf in
      let hits, truncated =
        collect conf base q bname changes max
      in
      if hits = [] then
        Output.printf conf "<p>%s</p>\n"
          (Utf8.capitalize_fst (transl conf "no match"))
      else (
        if truncated then
          print_message conf
            (Printf.sprintf (ftransl conf "notes search first results %d") max);
        print_hits conf base hits);
      Output.printf conf
        {|<p class="mt-3"><a href="%sm=MISC_NOTES_SEARCH&s=%s%s%s">%s</a></p>|}
        (commd conf :> string)
        (Mutil.encode s :> string)
        (if q.case_sens then "&c=on" else "")
        (if q.full_word then "&word=on" else "")
        (transl conf "miscellaneous notes" |> Utf8.capitalize_fst)

(* Not GWPARAM.output_error: it shows a bare "Incorrect request" whatever
   the status, which does not tell a visitor that logging in is needed. *)
let print_reserved conf =
  Output.status conf Code.Unauthorized;
  let title _ =
    transl conf "notes search reserved to friends or wizards"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.rheader conf title;
  Hutil.trailer conf

let print conf base =
  if not (conf.wizard || conf.friend) then print_reserved conf
  else
    let s =
      match p_getenv conf.env "s" with Some s -> String.trim s | None -> ""
    in
    let case_sens = p_getenv conf.env "c" = Some "on" in
    let q =
      {
        case_sens;
        full_word = p_getenv conf.env "word" = Some "on";
        terms = NotesSearch.query case_sens s;
        scope = scope_of_env conf;
      }
    in
    let title _ =
      transl conf "notes search" |> Utf8.capitalize_fst
      |> Output.print_sstring conf
    in
    Hutil.header conf title;
    print_form conf s q;
    (match q.terms with
    | [], [] -> ()
    | _ :: _, _ | [], _ :: _ -> search conf base q s);
    Hutil.trailer conf
