open Config
module Driver = Geneweb_db.Driver

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
  Util.transl_nth conf "notes search scopes" (group_index g)
  |> Utf8.capitalize_fst

let allowed conf g =
  match g with
  | Misc -> true
  | Wiz ->
      conf.authorized_wizards_notes
      && (conf.wizard || conf.friend)
      && WiznotesDisplay.wizard_auth_file_name conf <> ""
  | Corpus _ -> conf.wizard || conf.friend

let max_results conf =
  let positive s =
    match int_of_string_opt (String.trim s) with
    | Some n when n > 0 -> Some n
    | Some _ | None -> None
  in
  match Option.bind (Util.p_getenv conf.env "k") positive with
  | Some n -> n
  | None ->
      Option.bind
        (List.assoc_opt "notes_search_max_results" conf.base_env)
        positive
      |> Option.value ~default:500

type query = {
  text : string;
  case_sens : bool;
  full_word : bool;
  terms : string list * string list;
  scope : group list;
  max_hits : int;  (** corpus hits only *)
}

let query_of_env conf =
  let text =
    match Util.p_getenv conf.env "s" with Some s -> String.trim s | None -> ""
  in
  let case_sens = Util.p_getenv conf.env "c" = Some "on" in
  let allowed_groups = List.filter (allowed conf) groups in
  let ticked =
    List.filter
      (fun g -> Util.p_getenv conf.env (group_param g) = Some "on")
      allowed_groups
  in
  {
    text;
    case_sens;
    full_word = Util.p_getenv conf.env "word" = Some "on";
    terms = NotesSearch.query case_sens text;
    scope = (match ticked with [] -> allowed_groups | _ :: _ -> ticked);
    max_hits = max_results conf;
  }

let in_scope q g = List.mem g q.scope

type subject =
  | Of_page of { href : string; label : string; icon : string; file : string }
      (** misc or wizard note; [label] is HTML *)
  | Of_person of Driver.person
  | Of_family of Driver.ifam * Driver.family * Driver.person * Driver.person
      (** family, father, mother *)

type hit = {
  group : group;
  subject : subject;
  event : Adef.safe_string;
  rank : int;  (** misc notes: terms found in the title, listed first *)
  count : int;
  snippet : string;  (** escaped HTML *)
}

let matches q text =
  NotesSearch.matches ~full_word:q.full_word q.case_sens q.terms text

let count q text =
  NotesSearch.count_occurrences ~full_word:q.full_word q.case_sens (fst q.terms)
    text

let snippet q text =
  NotesSearch.snippet ~full_word:q.full_word q.case_sens (fst q.terms) text

(* read back by [NotesSearch.highlight_note] *)
let highlight_params q =
  "&hl="
  ^ (Mutil.encode q.text :> string)
  ^ (if q.case_sens then "&hl_c=on" else "")
  ^ if q.full_word then "&hl_w=on" else ""

let gallery_text json =
  let text_keys = [ "title"; "chronicle"; "desc"; "alt" ] in
  let rec strings acc j =
    match j with
    | `Assoc l ->
        List.fold_left
          (fun acc (k, v) ->
            match v with
            | `String s when List.mem k text_keys -> s :: acc
            | `String _ | `Null | `Bool _ | `Int _ | `Float _ -> acc
            | `List _ | `Assoc _ -> strings acc v)
          acc l
    | `List l -> List.fold_left strings acc l
    | `String _ | `Null | `Bool _ | `Int _ | `Float _ -> acc
  in
  match Yojson.Basic.from_string json with
  | j -> String.concat "\n" (List.rev (strings [] j))
  | exception Yojson.Json_error _ -> json

(* read live: misc notes are few *)
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
        let gallery = Notes.is_gallery nenv in
        let tit = Option.value ~default:"" (List.assoc_opt "TITLE" nenv) in
        let plain =
          NotesSearch.plain_text (if gallery then gallery_text nt else nt)
        in
        let plain_tit = NotesSearch.plain_text tit in
        let in_tit t =
          NotesSearch.mem_all ~full_word:q.full_word q.case_sens [ t ] plain_tit
        in
        let required = fst q.terms in
        if matches q (plain_tit ^ " " ^ plain) then
          let href =
            Printf.sprintf "%sm=%s&f=%s%s"
              (Util.commd conf :> string)
              (if gallery then "GALLERY" else "NOTES")
              (Mutil.encode fnotes :> string)
              (if gallery then "" else highlight_params q ^ "#first-found")
          in
          let label =
            if plain_tit <> "" then
              NotesSearch.highlight ~full_word:q.full_word q.case_sens required
                plain_tit
            else if fnotes = "" then
              (Util.escape_html
                 (Utf8.capitalize_fst (Util.transl conf "base notes"))
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

let wiz_hits conf base q =
  List.filter_map
    (fun wz ->
      let nt = WiznotesDisplay.wizard_note conf base wz in
      let plain = NotesSearch.plain_text nt in
      if matches q plain then
        Some
          {
            group = Wiz;
            subject =
              Of_page
                {
                  href =
                    Printf.sprintf "%sm=WIZNOTES&f=%s%s#first-found"
                      (Util.commd conf :> string)
                      (Mutil.encode wz :> string)
                      (highlight_params q);
                  label =
                    (Util.escape_html
                       (WiznotesDisplay.wizard_display_name conf wz)
                      :> string);
                  icon = "file-lines";
                  file = Driver.base_wiznotes_dir base ^ "/" ^ wz ^ ".txt";
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

let person_hits conf base q iper =
  let p = Util.pget conf base iper in
  if conf.no_note || not (Util.authorized_age conf base p) then []
  else
    let pevents = Array.of_list (Driver.get_pevents p) in
    List.filter_map
      (fun (e : NotesCorpus.entry) ->
        let event =
          if e.evt >= 0 && e.evt < Array.length pevents then
            Util.string_of_pevent_name conf base pevents.(e.evt).Def.epers_name
          else Adef.safe ""
        in
        hit_of_entry q (Of_person p) event e)
      (NotesCorpus.person_entries base p)

(* visible when both spouses are, as on person pages *)
let family_hits conf base q ifam =
  let fam = Driver.foi base ifam in
  let ifath = Driver.get_father fam and imoth = Driver.get_mother fam in
  if Driver.Iper.is_dummy ifath || Driver.Iper.is_dummy imoth then []
  else
    let fath = Util.pget conf base ifath and moth = Util.pget conf base imoth in
    if
      conf.no_note
      || not
           (Util.authorized_age conf base fath
           && Util.authorized_age conf base moth)
    then []
    else
      let fevents = Array.of_list (Driver.get_fevents fam) in
      List.filter_map
        (fun (e : NotesCorpus.entry) ->
          let event =
            if e.evt >= 0 && e.evt < Array.length fevents then
              Util.string_of_fevent_name conf base fevents.(e.evt).Def.efam_name
            else Adef.safe ""
          in
          hit_of_entry q (Of_family (ifam, fam, fath, moth)) event e)
        (NotesCorpus.family_entries base ifam fam)

(* the corpus entries of persons and families changed since the build are
   stale: skipped while streaming, read live afterwards *)
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
          if Driver.Iper.Set.mem ip ch_p || Driver.Iper.Table.mem seen_p ip then
            acc
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

(* the examples are the same in every language, only their explanations are
   translated *)
let help_id = "notes-search-help"

let print_help_button conf =
  let label = Util.transl conf "notes search help" |> Utf8.capitalize_fst in
  Output.printf conf
    {|<button type="button" class="btn btn-outline-secondary" data-bs-toggle="collapse" data-bs-target="#%s" aria-expanded="false" aria-controls="%s" title="%s"><i class="far fa-circle-question" aria-hidden="true"></i><span class="visually-hidden">%s</span></button>|}
    help_id help_id label label

(* the examples and their results are the same in every language *)
let print_help_panel conf =
  let rows =
    [
      ("moulin dupont", "", "notes search help and");
      ({|"saint pair"|}, "Saint Pair", "notes search help phrase");
      ("moulin -dupont", "", "notes search help exclude");
      ("jean?pierre", "Jean-Pierre, Jean Pierre", "notes search help joker");
      ("henri*", "Henri, Henriette", "notes search help star");
      ("jean", "Jean, Jean-Pierre", "notes search help word");
      ("l'eglise", "L’Église, l'église", "notes search help case");
    ]
  in
  Output.printf conf
    {|<div class="collapse mt-2" id="%s"><div class="card card-body small"><div class="fw-semibold">%s</div>
<table class="table table-sm table-borderless w-auto mt-1 mb-0"><tbody>|}
    help_id
    (Util.transl conf "notes search help" |> Utf8.capitalize_fst);
  List.iter
    (fun (example, results, key) ->
      Output.printf conf
        {|<tr><td class="text-nowrap"><code>%s</code></td><td class="text-nowrap">%s</td><td>%s</td></tr>|}
        (Util.escape_html example :> string)
        (Util.escape_html results :> string)
        (Util.transl conf key |> Utf8.capitalize_fst))
    rows;
  Output.print_sstring conf "</tbody></table></div></div>"

let print_form conf q =
  Output.printf conf {|<form class="mt-3" method="get" action="%s">|}
    conf.command;
  Util.hidden_env conf;
  Output.printf conf
    {|<input type="hidden" name="m" value="NOTES_SEARCH">
<div class="d-flex flex-wrap align-items-center gap-2">
<input type="text" name="s" size="40" class="form-control w-auto" value="%s">|}
    (Util.escape_html q.text :> string);
  let check name label on =
    Output.printf conf
      {|<div class="form-check form-check-inline mb-0"><input type="checkbox" name="%s" value="on" class="form-check-input" id="%s"%s><label class="form-check-label text-nowrap" for="%s">%s</label></div>|}
      name name
      (if on then " checked" else "")
      name label
  in
  check "c" (Util.transl_nth conf "search/case sensitive" 1) q.case_sens;
  check "word" (Util.transl conf "word match") q.full_word;
  if allowed conf (Corpus NotesCorpus.Person) then
    Output.printf conf
      {|<label for="k">%s</label>
<input type="number" class="form-control w-auto" id="k" name="k" min="1" value="%d">|}
      (Util.transl conf "maximum" |> Utf8.capitalize_fst)
      q.max_hits;
  Output.printf conf
    {|<div class="d-flex gap-2"><button type="submit" class="btn btn-primary text-nowrap">%s</button>|}
    (Util.transl_nth conf "search/case sensitive" 0 |> Utf8.capitalize_fst);
  print_help_button conf;
  Output.print_sstring conf "</div></div>";
  (match List.filter (allowed conf) groups with
  | [] | [ _ ] -> ()
  | gs ->
      Output.print_sstring conf {|<div class="d-flex flex-wrap gap-2 mt-2">|};
      List.iter
        (fun g -> check (group_param g) (group_label conf g) (in_scope q g))
        gs;
      Output.print_sstring conf "</div>");
  Output.print_sstring conf "</form>";
  print_help_panel conf

(* no id: a person can appear in several hits *)
let person_link conf base hl p =
  Printf.sprintf {|<a href="%s%s%s">%s</a>|}
    (Util.commd conf :> string)
    (Util.acces conf base p :> string)
    hl
    (Util.person_title_text conf base p :> string)

(* as in the "modify" titles of the templates *)
let key_text base p =
  Driver.sou base (Driver.get_first_name p)
  ^ (match Driver.get_occ p with 0 -> "" | n -> "." ^ string_of_int n)
  ^ " "
  ^ Driver.sou base (Driver.get_surname p)

let edit_dates conf href title text =
  if conf.wizard && text <> "" then
    Printf.sprintf {|<a href="%s%s" title="%s">%s</a>|}
      (Util.commd conf :> string)
      href
      (Util.escape_html title :> string)
      text
  else text

let subject_html conf base hl subject =
  match subject with
  | Of_page { href; label; icon; file } ->
      Printf.sprintf
        {|<i class="far fa-%s fa-fw me-2" aria-hidden="true"></i><a href="%s">%s</a><code class="small ms-2">%s</code>|}
        icon href label
        (Util.escape_html file :> string)
  | Of_person p ->
      let dates = DateDisplay.short_dates_text_notag conf base p in
      person_link conf base hl p
      ^ (if dates = "" then "" else " ")
      ^ edit_dates conf
          ("m=MOD_IND&i=" ^ Driver.Iper.to_string (Driver.get_iper p))
          (Utf8.capitalize_fst (Util.transl conf "modify")
          ^ " " ^ key_text base p)
          dates
  | Of_family (ifam, fam, fath, moth) ->
      let amp =
        match
          (DateDisplay.short_marriage_date_text conf base fam fath moth
            :> string)
        with
        | "" -> "&amp;"
        | y -> "&amp; " ^ y
      in
      person_link conf base hl fath
      ^ " "
      ^ edit_dates conf
          (Printf.sprintf "m=MOD_FAM&i=%s&ip=%s"
             (Driver.Ifam.to_string ifam)
             (Driver.Iper.to_string (Driver.get_iper fath)))
          (Printf.sprintf "%s %s %s %s"
             (Utf8.capitalize_fst
                (Util.transl_decline conf "modify"
                   (Util.transl_nth conf "family/families" 0)))
             (key_text base fath) (Util.transl conf "and") (key_text base moth))
          amp
      ^ " "
      ^ person_link conf base hl moth

let print_hits conf base q hits =
  let hl = highlight_params q in
  let sections =
    List.filter_map
      (fun g ->
        match List.filter (fun h -> h.group = g) hits with
        | [] -> None
        | l ->
            Some
              ( g,
                List.stable_sort
                  (fun h1 h2 ->
                    match Int.compare h2.rank h1.rank with
                    | 0 -> Int.compare h2.count h1.count
                    | c -> c)
                  l ))
      groups
  in
  let id g = group_param g ^ "-hits" in
  (match sections with
  | [] | [ _ ] -> ()
  | _ :: _ :: _ ->
      Output.print_sstring conf {|<div class="mt-2">|};
      List.iter
        (fun (g, l) ->
          Output.printf conf
            {|<a href="#%s" class="btn btn-sm btn-outline-primary me-2 mt-2">
<i class="fa fa-arrow-down me-1" aria-hidden="true"></i>%s
<span class="badge text-bg-light ms-1">%d</span></a>|}
            (id g) (group_label conf g) (List.length l))
        sections;
      Output.print_sstring conf "</div>");
  List.iter
    (fun (g, l) ->
      Output.printf conf
        {|<h2 class="h4 mt-4" id="%s">%s <span class="badge text-bg-light">%d</span></h2>
<ul class="list-group">|}
        (id g) (group_label conf g) (List.length l);
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
                {|<span class="badge text-bg-light ms-2">%d</span>|} h.count
          in
          Output.printf conf
            {|<li class="list-group-item"><div>%s%s%s</div><div class="small text-muted">%s</div></li>|}
            (subject_html conf base hl h.subject)
            event count h.snippet)
        l;
      Output.print_sstring conf "</ul>")
    sections

let date_text t =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min

let print_message conf msg =
  Output.printf conf {|<div class="alert alert-warning mt-3">%s</div>|}
    (Utf8.capitalize_fst msg)

let search_corpus conf base q =
  if
    not
      (List.exists (function Corpus _ -> true | Misc | Wiz -> false) q.scope)
  then []
  else
    let bname = Filename.remove_extension conf.bname in
    match NotesCorpus.read bname with
    | NotesCorpus.Missing ->
        print_message conf
          (Printf.sprintf
             (Util.ftransl conf "notes search index missing %s")
             (Util.escape_html bname :> string));
        []
    | NotesCorpus.Outdated ->
        print_message conf
          (Printf.sprintf
             (Util.ftransl conf "notes search index outdated %s")
             (Util.escape_html bname :> string));
        []
    | NotesCorpus.Available corpus ->
        let ((ch_p, ch_f) as changes) = NotesCorpus.changes bname corpus in
        let n = Driver.Iper.Set.cardinal ch_p + Driver.Ifam.Set.cardinal ch_f in
        Output.printf conf {|<p class="small text-muted mt-3">%s%s</p>|}
          (Printf.sprintf
             (Util.ftransl conf "notes search index date %s")
             (date_text corpus.built)
          |> Utf8.capitalize_fst)
          (if n = 0 then ""
           else
             "<br>"
             ^ (Printf.sprintf
                  (Util.ftransl conf "notes search index changes %d")
                  n
               |> Utf8.capitalize_fst));
        let hits, truncated =
          corpus_hits conf base q bname changes q.max_hits
        in
        if truncated then
          print_message conf
            (Printf.sprintf
               (Util.ftransl conf "notes search first results %d")
               q.max_hits);
        hits

let print conf base =
  let q = query_of_env conf in
  let title _ =
    Util.transl conf "notes search"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  print_form conf q;
  (match fst q.terms with
  | [] -> ()
  | _ :: _ -> (
      let misc = if in_scope q Misc then misc_hits conf base q else [] in
      let wiz = if in_scope q Wiz then wiz_hits conf base q else [] in
      let corpus = search_corpus conf base q in
      match List.rev_append misc (List.rev_append wiz corpus) with
      | [] ->
          Output.printf conf {|<p class="mt-3">%s</p>|}
            (Utf8.capitalize_fst (Util.transl conf "no match"))
      | hits -> print_hits conf base q hits));
  Hutil.trailer conf
