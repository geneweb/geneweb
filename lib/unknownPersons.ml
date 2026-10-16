open Config
open Def
module Collection = Geneweb_db.Collection
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let src = Logs.Src.create ~doc:"UnknownPersons" "UNKN"

module Log = (val Logs.src_log src : Logs.LOG)

type lists = {
  to_delete : Driver.iper list;
  to_review : Driver.iper list;
  couples_to_delete : Driver.ifam list;
  couples_to_review : Driver.ifam list;
}

let empty =
  {
    to_delete = [];
    to_review = [];
    couples_to_delete = [];
    couples_to_review = [];
  }

let default_placeholders = "NN, N.N., Ne"

let placeholders conf =
  (match Util.p_getenv conf.env "ph" with
    | Some s when String.trim s <> "" -> s
    | Some _ | None -> default_placeholders)
  |> String.split_on_char ','
  |> List.filter_map (fun s ->
      match String.trim s with "" -> None | s -> Some s)

let max_shown conf =
  Option.fold ~none:100
    ~some:(fun n -> max 0 (min 500 n))
    (Util.p_getint conf.env "max")

let unknown_flags base names =
  let strings s =
    List.rev_append
      (Driver.base_strings_of_first_name base s)
      (Driver.base_strings_of_surname base s)
  in
  let add set s =
    let k = Name.lower s in
    List.fold_left
      (fun set istr ->
        if Name.lower (Driver.sou base istr) = k then
          Driver.Istr.Set.add istr set
        else set)
      set (strings s)
  in
  let istrs =
    List.fold_left add
      (add (Driver.Istr.Set.singleton Driver.Istr.quest) "")
      (List.filter (fun s -> Name.lower s <> "") names)
  in
  let flags = Bytes.make (Driver.nb_of_persons base) '\000' in
  let mark spi f =
    Driver.Istr.Set.iter
      (fun istr -> List.iter f (Driver.spi_find spi istr))
      istrs
  in
  mark (Driver.persons_of_surname base) (fun ip ->
      Bytes.set flags (Driver.Iper.hash ip) '\001');
  mark (Driver.persons_of_first_name base) (fun ip ->
      let k = Driver.Iper.hash ip in
      if Bytes.get flags k = '\001' then Bytes.set flags k '\002');
  flags

let is_unknown flags ip = Bytes.get flags (Driver.Iper.hash ip) = '\002'
let filled istr = not (Driver.Istr.is_empty istr)

let event_has_data date place reason note src witnesses =
  Date.od_of_cdate date <> None
  || filled place || filled reason || filled note || filled src
  || Array.length witnesses > 0

let family_has_data fam =
  filled (Driver.get_comment fam)
  || filled (Driver.get_fsources fam)
  || List.exists
       (fun e ->
         event_has_data e.efam_date e.efam_place e.efam_reason e.efam_note
           e.efam_src e.efam_witnesses)
       (Driver.get_fevents fam)

let has_images conf base p =
  Option.is_some (Image.get_portrait conf base p)
  || Option.is_some (Image.get_old_portrait conf base p)
  || Image.has_blason conf base p true
  || Image.get_carrousel_imgs conf base p <> []
  || Image.get_carrousel_old_imgs conf base p <> []

let has_data conf base p =
  let psources = Driver.get_psources p in
  let family_source ifam =
    Driver.Istr.equal psources (Driver.get_fsources (Driver.foi base ifam))
  in
  filled (Driver.get_image p)
  || filled (Driver.get_public_name p)
  || filled (Driver.get_occupation p)
  || filled (Driver.get_notes p)
  || Driver.get_qualifiers p <> []
  || Driver.get_aliases p <> []
  || Driver.get_first_names_aliases p <> []
  || Driver.get_surnames_aliases p <> []
  || Driver.get_titles p <> []
  || Driver.get_rparents p <> []
  || Driver.get_related p <> []
  || List.exists
       (fun e ->
         event_has_data e.epers_date e.epers_place e.epers_reason e.epers_note
           e.epers_src e.epers_witnesses)
       (Driver.get_pevents p)
  || (filled psources && not (Array.exists family_source (Driver.get_family p)))
  || has_images conf base p

let blank conf base p =
  Option.is_none (Driver.get_parents p) && not (has_data conf base p)

let small fam = Array.length (Driver.get_children fam) <= 1

let couple_deletable conf base fam =
  let alone ip =
    let p = Driver.poi base ip in
    Array.length (Driver.get_family p) = 1 && blank conf base p
  in
  small fam
  && (not (family_has_data fam))
  && alone (Driver.get_father fam)
  && alone (Driver.get_mother fam)

let unknown_couples base flags p =
  let ip = Driver.get_iper p in
  Array.fold_left
    (fun l ifam ->
      let fam = Driver.foi base ifam in
      if is_unknown flags (Gutil.spouse ip fam) then fam :: l else l)
    [] (Driver.get_family p)

let scan conf base flags =
  Collection.fold
    (fun l ip ->
      if not (is_unknown flags ip) then l
      else
        let p = Driver.poi base ip in
        let couples = unknown_couples base flags p in
        let l =
          List.fold_left
            (fun l fam ->
              if not (Driver.Iper.equal ip (Driver.get_father fam)) then l
              else if couple_deletable conf base fam then
                {
                  l with
                  couples_to_delete = Driver.get_ifam fam :: l.couples_to_delete;
                }
              else
                {
                  l with
                  couples_to_review = Driver.get_ifam fam :: l.couples_to_review;
                })
            l couples
        in
        let canonical = Util.is_empty_name p in
        if couples <> [] || (canonical && not (has_data conf base p)) then l
        else if (not canonical) && blank conf base p then
          { l with to_delete = ip :: l.to_delete }
        else { l with to_review = ip :: l.to_review })
    empty (Driver.ipers base)

let sort conf base names = scan conf base (unknown_flags base names)

let title conf _ =
  Output.print_sstring conf
    (Utf8.capitalize_fst (Util.transl conf "unknown persons"))

let link conf base ip =
  (Util.referenced_person_text conf base (Driver.poi base ip) :> string)

let links conf base ipl = String.concat ", " (List.map (link conf base) ipl)

let person_item conf base ip =
  let spouse ifam = Gutil.spouse ip (Driver.foi base ifam) in
  match Array.to_list (Driver.get_family (Driver.poi base ip)) with
  | [] -> link conf base ip
  | ifams ->
      link conf base ip ^ " &amp; " ^ links conf base (List.map spouse ifams)

let couple_item conf base ifam =
  let fam = Driver.foi base ifam in
  let couple =
    link conf base (Driver.get_father fam)
    ^ " &amp; "
    ^ link conf base (Driver.get_mother fam)
  in
  match Array.to_list (Driver.get_children fam) with
  | [] -> couple
  | children -> couple ^ " (" ^ links conf base children ^ ")"

let shown n l = List.filteri (fun i _ -> i < n) (List.rev l)

let print_delete_form conf names del =
  Output.printf conf {|<form method="post" action="%s" class="ms-auto">|}
    conf.command;
  Util.hidden_env ~senv:false conf;
  Util.hidden_input_s conf "m" "UNKNOWN_OK";
  Util.hidden_input_s conf "ph" (String.concat ", " names);
  Util.hidden_input_s conf "max" (string_of_int (max_shown conf));
  Util.hidden_input_s conf "del" del;
  Output.printf conf
    {|<button type="submit" class="btn btn-sm btn-danger">%s</button></form>|}
    (Utf8.capitalize_fst (Util.transl_nth conf "validate/delete" 1))

let print_section conf names (id, key, del, total, items) =
  Output.printf conf
    {|<div class="d-flex align-items-center mt-4" id="%s">
<h2 class="h4 mb-0">%s <span class="badge text-bg-secondary">%d</span></h2>|}
    id
    (Utf8.capitalize_fst (Util.transl_nth conf key 0))
    total;
  Option.iter (print_delete_form conf names) del;
  Output.printf conf {|</div>
<p class="text-body-secondary mt-1">%s</p>|}
    (Utf8.capitalize_fst (Util.transl_nth conf key 1));
  if items <> [] then (
    Output.print_sstring conf "<ul>";
    List.iter (Output.printf conf "<li>%s</li>") items;
    if List.length items < total then Output.print_sstring conf "<li>…</li>";
    Output.print_sstring conf "</ul>")

let blank_names base =
  let found data spi strings =
    List.filter_map
      (fun istr ->
        if Driver.Istr.is_quest istr || Name.lower (Driver.sou base istr) <> ""
        then None
        else
          match List.length (Driver.spi_find spi istr) with
          | 0 -> None
          | n -> Some (data, istr, n))
      strings
  in
  List.rev_append
    (found "fn"
       (Driver.persons_of_first_name base)
       (Driver.base_strings_of_first_name base ""))
    (found "sn"
       (Driver.persons_of_surname base)
       (Driver.base_strings_of_surname base ""))

let blank_item conf base (data, istr, n) =
  let s = Driver.sou base istr in
  Printf.sprintf
    {|<a href="%sm=MOD_DATA&data=%s&key=%s&s=%s">%s</a>
(%s, %d %s)|}
    (Util.commd conf :> string)
    data
    (Driver.Istr.to_string istr)
    (Mutil.encode s :> string)
    (Util.escape_html s :> string)
    (Util.transl_nth conf
       (if data = "fn" then "first name/first names" else "surname/surnames")
       0)
    n
    (Util.transl_nth conf "person/persons" (if n > 1 then 1 else 0))

let print_lists conf base names l =
  let n = max_shown conf in
  let section id key del item l =
    (id, key, del, List.length l, List.map item (shown n l))
  in
  let sections =
    List.filter
      (fun (_, _, _, total, _) -> total > 0)
      [
        section "blank-names" "unknown blank names" None (blank_item conf base)
          (blank_names base);
        section "persons-without-data" "unknown persons without data"
          (Some "persons") (person_item conf base) l.to_delete;
        section "persons-with-data" "unknown persons with data" None
          (person_item conf base) l.to_review;
        section "couples-without-data" "unknown couples without data"
          (Some "couples") (couple_item conf base) l.couples_to_delete;
        section "couples-with-data" "unknown couples with data" None
          (couple_item conf base) l.couples_to_review;
      ]
  in
  if sections = [] then
    Output.printf conf {|<p class="mt-3">%s</p>|}
      (Utf8.capitalize_fst (Util.transl conf "no match"))
  else (
    Output.print_sstring conf {|<div class="mt-2">|};
    List.iter
      (fun (id, key, _, total, _) ->
        Output.printf conf
          {|<a href="#%s" class="btn btn-sm btn-primary me-2 mt-2">
<i class="fa fa-arrow-down me-1" aria-hidden="true"></i>%s
<span class="badge text-bg-light ms-1">%d</span></a>|}
          id
          (Utf8.capitalize_fst (Util.transl_nth conf key 0))
          total)
      sections;
    Output.print_sstring conf "</div>";
    List.iter (print_section conf names) sections)

let print conf base =
  let names = placeholders conf in
  Hutil.header conf (title conf);
  Output.printf conf {|<div class="alert alert-info mt-3">%s</div>|}
    (Utf8.capitalize_fst (Util.transl conf "unknown persons initials"));
  Output.printf conf {|<form method="get" action="%s" class="mt-3">|}
    conf.command;
  Util.hidden_env ~senv:false conf;
  Util.hidden_input_s conf "m" "UNKNOWN";
  Output.printf conf
    {|<label for="ph" class="form-label">%s</label>
<input type="text" class="form-control" id="ph" name="ph" value="%s"
  placeholder="%s">
<div class="d-flex align-items-center mt-2">
  <label for="max" class="me-2">%s</label>
  <input type="number" class="form-control w-auto" id="max" name="max"
    min="0" max="500" step="10" value="%d">
  <button type="submit" class="btn btn-primary ms-2">%s</button>
</div>
</form>|}
    (Utf8.capitalize_fst (Util.transl conf "unknown persons names"))
    (Util.escape_html (String.concat ", " names) :> string)
    default_placeholders
    (Utf8.capitalize_fst (Util.transl conf "unknown persons max shown"))
    (max_shown conf)
    (Utf8.capitalize_fst (Util.transl_nth conf "search/case sensitive" 0));
  if Option.is_some (Util.p_getenv conf.env "ph") then
    print_lists conf base names (sort conf base names);
  Hutil.trailer conf

let print_ok conf base =
  let names = placeholders conf in
  let l = sort conf base names in
  let ipers =
    match Util.p_getenv conf.env "del" with
    | Some "persons" -> l.to_delete
    | Some "couples" ->
        List.fold_left
          (fun acc ifam ->
            let fam = Driver.foi base ifam in
            Driver.get_father fam :: Driver.get_mother fam :: acc)
          [] l.couples_to_delete
    | Some _ | None -> []
  in
  let deleted =
    List.rev_map
      (fun ip ->
        let op =
          Util.string_gen_person base
            (Driver.gen_person_of_person (Driver.poi base ip))
        in
        UpdateIndOk.effective_del_no_commit base op;
        if Notes.has_links op.psources then
          Notes.update_notes_links_db conf base (NLDB.PgInd ip) "";
        op)
      ipers
  in
  if deleted <> [] then (
    Util.commit_patches conf base;
    History.notify conf base "dp";
    List.iter
      (fun op ->
        HistoryDiff.archive_deleted conf op;
        Log.info (fun k ->
            k "%s: %s.%d %s deleted (i=%s)" conf.bname op.first_name op.occ
              op.surname
              (Driver.Iper.to_string op.key_index)))
      deleted;
    Log.info (fun k ->
        k "%s [%s]: %d unknown persons deleted (%s)" conf.bname conf.user
          (List.length deleted) (String.concat ", " names)));
  Hutil.header conf (title conf);
  Output.printf conf
    {|<p class="mt-3">%s <span class="badge text-bg-secondary">%d</span></p>
<a href="%sm=UNKNOWN&ph=%s&max=%d">%s</a>|}
    (Utf8.capitalize_fst (Util.transl conf "unknown persons deleted"))
    (List.length deleted)
    (Util.commd conf :> string)
    (Mutil.encode (String.concat ", " names) :> string)
    (max_shown conf)
    (Utf8.capitalize_fst (Util.transl conf "unknown persons"));
  Hutil.trailer conf
