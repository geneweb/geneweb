open Config
open Util
module Driver = Geneweb_db.Driver

let esc s = (Util.escape_html s :> string)
let enc s = (Mutil.encode s :> string)
let prefix conf = (commd conf :> string)

(* the three families of pages: same layout, their own strings *)
type section = Places | Sources | Occupations

let nb_max conf =
  match List.assoc_opt "book_max_results" conf.base_env with
  | Some n -> (
      match int_of_string_opt n with Some n when n > 0 -> n | _ -> 1000)
  | None -> 1000

(* [s] escaped, its invisible characters and the spaces at both ends marked as
   in m=CHK_DATA, so that "Canada", " Canada" and "Canada " can be told apart *)
let vis conf s =
  "<span class=\"inv-hl\">" ^ CheckData.invisible_html conf s ^ "</span>"

let utf8_prefix n s = if Utf8.length s <= n then s else Utf8.sub s 0 n

let term_link conf t =
  Printf.sprintf "<a href=\"%sm=PLACE_TERMS&t=%s\">%s</a>" (prefix conf) (enc t)
    (vis conf t)

let place_edit_link conf raw =
  Printf.sprintf
    "<a href=\"%sm=MOD_DATA&data=place&s=%s\" class=\"ms-2\" title=\"%s\" \
     data-bs-toggle=\"tooltip\"><i class=\"fa fa-pen-to-square \
     fa-sm\"></i></a>"
    (prefix conf)
    (enc (Place.without_suburb raw))
    (esc (Utf8.capitalize_fst (transl conf "modify")))

(* [term] rendered inside its context, child first: "Term, Parent, Root" or
   "[Term] – Parent, Root" for a suburb. *)
let context_html conf (e : PlaceTerms.entry) (c : PlaceTerms.context) =
  let parents = String.concat ", " (List.rev_map (vis conf) c.parents) in
  let t = "<strong>" ^ vis conf e.term ^ "</strong>" in
  if c.suburb then
    Printf.sprintf "[%s]%s" t (if parents = "" then "" else " – " ^ parents)
  else if parents = "" then t
  else t ^ ", " ^ parents

(* the raw place with the occurrence of the term in this context marked *)
let mark_html conf inverted (e : PlaceTerms.entry) (c : PlaceTerms.context) raw
    =
  let comps = Place.components inverted raw in
  let target =
    if c.suburb then List.find_opt (fun (x : Place.component) -> x.suburb) comps
    else
      List.filteri (fun _ (x : Place.component) -> not x.suburb) comps
      |> fun l -> List.nth_opt l (List.length c.parents)
  in
  match target with
  | Some x when x.text = e.term ->
      let n = String.length raw in
      vis conf (String.sub raw 0 x.pos)
      ^ "<mark>" ^ vis conf x.text ^ "</mark>"
      ^ vis conf (String.sub raw (x.pos + x.len) (n - x.pos - x.len))
  | Some _ | None -> vis conf raw

let variants_of entries =
  let h = Hashtbl.create 1024 in
  List.iter
    (fun (e : PlaceTerms.entry) ->
      Hashtbl.replace h e.cluster
        (e.term :: Option.value ~default:[] (Hashtbl.find_opt h e.cluster)))
    entries;
  fun (e : PlaceTerms.entry) ->
    Option.value ~default:[] (Hashtbl.find_opt h e.cluster)
    |> List.filter (( <> ) e.term)
    |> List.sort Geneweb_db.Gutil.alphabetic_order

let print_initials conf s inis =
  Output.print_sstring conf "<div class=\"d-flex flex-wrap my-2\">\n";
  List.iter
    (fun (ini, n) ->
      Output.printf conf
        "<a class=\"btn btn-sm btn-outline-primary font-monospace me-1 mb-1\" \
         href=\"%sm=PLACE_TERMS&s=%s\" title=\"%d\">%s</a>\n"
        (prefix conf) (enc ini) n
        (esc (if s = "" then Utf8.capitalize_fst ini else ini)))
    inis;
  Output.print_sstring conf "</div>\n"

let print_table conf variants entries =
  Output.printf conf
    "<table class=\"table table-sm table-hover w-auto\">\n\
     <thead><tr><th>%s</th><th>%s</th><th class=\"text-end\">%s</th><th \
     class=\"text-end\">%s</th><th class=\"text-end\">%s</th></tr></thead>\n\
     <tbody>\n"
    (Utf8.capitalize_fst (transl_nth conf "place term/place terms" 0))
    (Utf8.capitalize_fst (transl conf "spellings"))
    (Utf8.capitalize_fst (transl conf "contexts"))
    (Utf8.capitalize_fst (transl_nth conf "full place/full places" 1))
    (Utf8.capitalize_fst (transl conf "uses"));
  List.iter
    (fun (e : PlaceTerms.entry) ->
      Output.printf conf
        "<tr><td>%s</td><td>%s</td><td class=\"text-end\">%d</td><td \
         class=\"text-end\">%d</td><td class=\"text-end\">%d</td></tr>\n"
        (term_link conf e.term)
        (String.concat ", " (List.map (term_link conf) (variants e)))
        (List.length e.contexts) e.nb_places e.uses)
    entries;
  Output.print_sstring conf "</tbody></table>\n"

let print_list conf entries s =
  let key = Name.lower s in
  let selected =
    if key = "" then entries
    else
      List.filter
        (fun (e : PlaceTerms.entry) -> String.starts_with ~prefix:key e.key)
        entries
  in
  Output.printf conf "<p>%d %s" (List.length selected)
    (transl_nth conf "place term/place terms"
       (if List.length selected > 1 then 1 else 0));
  if s <> "" then Output.printf conf " – <code>%s</code>" (esc s);
  Output.print_sstring conf "</p>\n";
  let inis =
    let n = if key = "" then 1 else Utf8.length key + 1 in
    let h = Hashtbl.create 64 in
    List.iter
      (fun (e : PlaceTerms.entry) ->
        let i = utf8_prefix n e.key in
        if i <> key then
          Hashtbl.replace h i
            (1 + Option.value ~default:0 (Hashtbl.find_opt h i)))
      selected;
    Hashtbl.fold (fun i n acc -> (i, n) :: acc) h [] |> List.sort compare
  in
  print_initials conf s inis;
  if key <> "" then
    if List.length selected <= nb_max conf then
      print_table conf (variants_of entries) selected
    else
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "v7 list too long"))

(* A checkbox of class [ctx-all] and [data-ctx="x"] checks or unchecks every
   checkbox of class [ctxx]. *)
let toggle_script =
  {|<script>
document.querySelectorAll('.ctx-all').forEach(function (b) {
  b.addEventListener('change', function () {
    document.querySelectorAll('.ctx' + b.dataset.ctx).forEach(function (x) {
      x.checked = b.checked;
    });
  });
});
</script>
|}

(* On a cluster page, the row of the spelling [i] to keep (two cells of the
   [keep-grid] opened by [keep_rows]), with a checkbox
   leaving that spelling unchanged ("Loiré" beside "Loire"): it is the
   inverse of the checkbox [spt<i>] in front of the occurrences of the
   spelling, kept in sync by [exclude_script]. *)
let keep_row conf i ~value ~checked label =
  let dnm = esc (transl conf "do not modify") in
  Printf.sprintf
    "<div class=\"form-check\"><input class=\"form-check-input\" \
     type=\"radio\" name=\"ntc\" id=\"ntc%d\" value=\"%s\"%s><label \
     class=\"form-check-label\" for=\"ntc%d\">%s</label></div><div \
     class=\"form-check\"><input class=\"form-check-input sp-excl\" \
     type=\"checkbox\" id=\"spx%d\" data-sp=\"spt%d\" \
     aria-label=\"%s\">%s</div>\n"
    i (esc value)
    (if checked then " checked" else "")
    i label i i dnm
    (* the label only on the first row: the column is enough below *)
    (if i = 0 then
       Printf.sprintf
         "<label class=\"form-check-label small text-muted\" \
          for=\"spx%d\">%s</label>"
         i dnm
     else "")

(* the rows of the spellings to keep, in a grid so that the checkboxes are in
   a column *)
let keep_rows conf rows =
  Output.print_sstring conf "<div class=\"keep-grid\">\n";
  List.iter (Output.print_sstring conf) rows;
  Output.print_sstring conf "</div>\n"

let exclude_script =
  {|<script>
document.querySelectorAll('.sp-excl').forEach(function (x) {
  var h = document.getElementById(x.dataset.sp);
  if (!h) return;
  x.addEventListener('change', function () {
    h.checked = !x.checked;
    h.dispatchEvent(new Event('change'));
  });
  h.addEventListener('change', function () { x.checked = !h.checked; });
});
</script>
|}

let cluster_link conf key =
  Printf.sprintf "%sm=PLACE_TERMS&c=%s" (prefix conf) (enc key)

let counts_html conf (e : PlaceTerms.entry) =
  Printf.sprintf "%d %s · %d %s · %d %s" (List.length e.contexts)
    (transl conf "contexts") e.nb_places
    (transl_nth conf "full place/full places"
       (if e.nb_places > 1 then 1 else 0))
    e.uses (transl conf "uses")

(* The contexts of [e], each with a checkbox checking its places, the places
   with one checkbox per occurrence. Context checkboxes are numbered from
   [first]; [cls] is added to every checkbox (to check a whole term). Returns
   the next number. *)
let print_contexts conf (e : PlaceTerms.entry) ~first ~cls =
  let inverted = Place.places_inverted conf in
  List.iteri
    (fun ci (c : PlaceTerms.context) ->
      let ci = first + ci in
      let depth = if c.suburb then None else Some (List.length c.parents) in
      Output.printf conf
        "<div class=\"h6 mt-3\"><input type=\"checkbox\" \
         class=\"form-check-input ctx-all%s me-2\" data-ctx=\"%d\" checked>%s \
         <span class=\"badge bg-secondary\">%d · %d</span>%s</div>\n\
         <ul class=\"list-unstyled ms-3\">\n"
        cls ci (context_html conf e c) (List.length c.places) c.c_uses
        (if c.suburb then
           " <span class=\"badge bg-info\">"
           ^ esc (transl conf "subdivision")
           ^ "</span>"
         else "");
      List.iter
        (fun (p : PlaceTerms.place) ->
          Output.printf conf
            "<li><input type=\"checkbox\" class=\"form-check-input ctx%d%s \
             me-2\" name=\"%s\" value=\"on\" checked>%s <span \
             class=\"text-muted\">(%d)</span>%s</li>\n"
            ci cls
            (PlaceRename.occurrence_key (p.istr, depth))
            (mark_html conf inverted e c p.raw)
            p.uses
            (place_edit_link conf p.raw))
        c.places;
      Output.print_sstring conf "</ul>\n")
    e.contexts;
  first + List.length e.contexts

let open_form ?(m = "PLACE_TERMS") conf =
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded m)

let print_term conf entries t =
  match List.find_opt (fun (e : PlaceTerms.entry) -> e.term = t) entries with
  | None ->
      Output.printf conf "<p>%s: <code>%s</code></p>\n"
        (Utf8.capitalize_fst (transl conf "not found"))
        (esc t)
  | Some e ->
      Output.printf conf "<h2 class=\"h3\">%s</h2>\n<p>%s</p>\n"
        (vis conf e.term) (counts_html conf e);
      (match variants_of entries e with
      | [] -> ()
      | l ->
          Output.printf conf "<p>%s: %s · <a href=\"%s\">%s</a></p>\n"
            (Utf8.capitalize_fst (transl conf "spellings"))
            (String.concat ", " (List.map (term_link conf) l))
            (cluster_link conf e.cluster)
            (Utf8.capitalize_fst (transl conf "merge spellings")));
      (* the occurrences to rename are chosen with the checkboxes, all
         checked by default; the form leads to the preview *)
      open_form conf;
      Util.hidden_input_s conf "t" e.term;
      Output.printf conf
        "<div class=\"d-flex align-items-center gap-2 my-3\">\n\
         <label for=\"nt\" class=\"text-nowrap\">%s</label>\n\
         <input type=\"text\" class=\"form-control w-auto\" size=\"40\" \
         id=\"nt\" name=\"nt\" value=\"%s\" required>\n\
         <button type=\"submit\" class=\"btn btn-primary\">%s</button>\n\
         </div>\n"
        (Utf8.capitalize_fst (transl conf "rename this term"))
        (esc e.term)
        (Utf8.capitalize_fst (transl conf "preview of the changes"));
      ignore (print_contexts conf e ~first:0 ~cls:"" : int);
      Output.print_sstring conf "</form>\n";
      Output.print_sstring conf toggle_script

(* ---- clusters of spellings ---- *)

let print_clusters conf entries =
  let clusters = PlaceTerms.clusters entries in
  Output.printf conf "<p>%d</p>\n" (List.length clusters);
  if clusters <> [] then (
    Output.printf conf
      "<table class=\"table table-sm table-hover w-auto\">\n\
       <thead><tr><th>%s</th><th class=\"text-end\">%s</th><th \
       class=\"text-end\">%s</th><th></th></tr></thead>\n\
       <tbody>\n"
      (Utf8.capitalize_fst (transl conf "spellings"))
      (Utf8.capitalize_fst (transl_nth conf "full place/full places" 1))
      (Utf8.capitalize_fst (transl conf "uses"));
    List.iter
      (fun (key, (l : PlaceTerms.entry list)) ->
        let sum f = List.fold_left (fun n e -> n + f e) 0 l in
        Output.printf conf
          "<tr><td>%s</td><td class=\"text-end\">%d</td><td \
           class=\"text-end\">%d</td><td><a href=\"%s\">%s</a></td></tr>\n"
          (String.concat ", "
             (List.map
                (fun (e : PlaceTerms.entry) ->
                  Printf.sprintf "%s <span class=\"text-muted\">(%d)</span>"
                    (term_link conf e.term) e.uses)
                l))
          (sum (fun (e : PlaceTerms.entry) -> e.nb_places))
          (sum (fun (e : PlaceTerms.entry) -> e.uses))
          (cluster_link conf key)
          (esc (Utf8.capitalize_fst (transl conf "merge spellings"))))
      clusters;
    Output.print_sstring conf "</tbody></table>\n")

(* value of the "other spelling" radio button: never a valid term *)
let other_choice = "[other]"

let print_cluster conf entries key =
  match List.assoc_opt key (PlaceTerms.clusters entries) with
  | None ->
      Output.printf conf "<p>%s: <code>%s</code></p>\n"
        (Utf8.capitalize_fst (transl conf "not found"))
        (esc key)
  | Some l ->
      Output.printf conf "<h2 class=\"h3\">%s</h2>\n"
        (Utf8.capitalize_fst (transl conf "merge spellings"));
      open_form conf;
      List.iteri
        (fun i (e : PlaceTerms.entry) ->
          Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) e.term)
        l;
      (* the spelling to keep: one of the cluster (the most used by default)
         or another one *)
      Output.printf conf
        "<fieldset class=\"my-3\"><legend class=\"h5\">%s</legend>\n"
        (Utf8.capitalize_fst (transl conf "keep the spelling"));
      keep_rows conf
        (List.mapi
           (fun i (e : PlaceTerms.entry) ->
             keep_row conf i ~value:e.term ~checked:(i = 0)
               (Printf.sprintf
                  "<code>%s</code> <span class=\"text-muted\">(%d)</span>"
                  (vis conf e.term) e.uses))
           l);
      Output.printf conf
        "<div class=\"form-check d-flex align-items-center gap-2\"><input \
         class=\"form-check-input\" type=\"radio\" name=\"ntc\" id=\"ntco\" \
         value=\"%s\"><label class=\"form-check-label\" \
         for=\"ntco\">%s</label> <input type=\"text\" class=\"form-control \
         form-control-sm w-auto\" size=\"30\" name=\"nto\" \
         onfocus=\"document.getElementById('ntco').checked=true\"></div>\n\
         </fieldset>\n\
         <button type=\"submit\" class=\"btn btn-primary mb-3\">%s</button>\n"
        (esc other_choice)
        (Utf8.capitalize_fst (transl conf "other spelling"))
        (Utf8.capitalize_fst (transl conf "preview of the changes"));
      (* the spellings to merge: every occurrence of each spelling *)
      ignore
        (List.fold_left
           (fun (i, first) (e : PlaceTerms.entry) ->
             Output.printf conf
               "<h3 class=\"h5 mt-4\"><input type=\"checkbox\" \
                class=\"form-check-input ctx-all me-2\" id=\"spt%d\" \
                data-ctx=\"t%d\" checked>%s <span class=\"text-muted \
                small\">%s</span></h3>\n"
               i i (term_link conf e.term) (counts_html conf e);
             ( i + 1,
               print_contexts conf e ~first ~cls:(Printf.sprintf " ctxt%d" i) ))
           (0, 0) l
          : int * int);
      Output.print_sstring conf "</form>\n";
      Output.print_sstring conf toggle_script;
      Output.print_sstring conf exclude_script

(* the new term: [nt] on a term page, the [ntc] radio button (or [nto] for
   another spelling) on a cluster page *)
let new_term_of_env conf =
  match p_getenv_notrim conf.env "nt" with
  | Some nt -> Some nt
  | None -> (
      match p_getenv_notrim conf.env "ntc" with
      | Some c when c = other_choice ->
          Some (Option.value ~default:"" (p_getenv_notrim conf.env "nto"))
      | x -> x)

(* ---- rename: preview and apply ---- *)

(* [s] rendered with [vis], the parts between \001 and \002 marked; the
   markers are cut before [vis], which would show them as invisible
   characters *)
let marked conf s =
  match String.split_on_char '\001' s with
  | [] -> ""
  | first :: rest ->
      vis conf first
      ^ String.concat ""
          (List.map
             (fun part ->
               match String.index_opt part '\002' with
               | Some j ->
                   "<mark>"
                   ^ vis conf (String.sub part 0 j)
                   ^ "</mark>"
                   ^ vis conf
                       (String.sub part (j + 1) (String.length part - j - 1))
               | None -> vis conf part)
             rest)

let mark_terms conf terms depths raw =
  let inverted = Place.places_inverted conf in
  match
    PlaceRename.rewrite_with inverted ~terms
      (fun t -> "\001" ^ t ^ "\002")
      raw depths
  with
  | Some s -> marked conf s
  | None -> vis conf raw

let kind_html section conf base (k : PlaceRename.kind) =
  let badge cls txt =
    Printf.sprintf "<span class=\"badge %s\">%s</span>" cls
      (esc (Utf8.capitalize_fst txt))
  in
  match k with
  | New_place ->
      badge "bg-success"
        (transl conf
           (match section with
           | Places -> "new place"
           | Sources -> "new source"
           | Occupations -> "new occupation"))
  | Merge_existing j ->
      badge "bg-warning text-dark"
        (transl conf
           (match section with
           | Places -> "merged into an existing place"
           | Sources -> "merged into an existing source"
           | Occupations -> "merged into an existing occupation"))
      ^ " "
      ^ esc (Driver.sou base j)
  | Merge_batch ->
      badge "bg-warning text-dark"
        (transl conf
           (match section with
           | Places -> "merged with another renamed place"
           | Sources -> "merged with another renamed source"
           | Occupations -> "merged with another renamed occupation"))

(* back to the term page, or to the cluster page for several terms *)
let back_href conf terms =
  match terms with
  | [ t ] -> Printf.sprintf "%sm=PLACE_TERMS&t=%s" (prefix conf) (enc t)
  | t :: _ -> cluster_link conf (PlaceTerms.cluster_key t)
  | [] -> Printf.sprintf "%sm=PLACE_TERMS" (prefix conf)

let terms_html terms =
  String.concat " / " (List.map (fun t -> "<code>" ^ esc t ^ "</code>") terms)

let invalid_new_term conf terms nt =
  Output.printf conf
    "<div class=\"alert alert-danger\">%s: <code>%s</code></div>\n\
     <p><a href=\"%s\">%s</a></p>\n"
    (Utf8.capitalize_fst (transl conf "invalid new term"))
    (esc nt) (back_href conf terms) (terms_html terms)

(* the field of the new place of [istr] in a preview, editable *)
let edit_key istr = "ne" ^ Driver.Istr.to_string istr

(* The new places as edited in the preview: an edited place replaces the
   computed one (cleaned as check_data does), a place edited back to its old
   text is left out, an emptied field keeps the computed place; then the kinds
   are computed again (an edited place may exist already). *)
let apply_edits conf base items =
  List.filter_map
    (fun (it : PlaceRename.item) ->
      match p_getenv_notrim conf.env (edit_key it.istr) with
      | Some v -> (
          match Mutil.normalize_utf_8 (Util.only_printable v) with
          | "" -> Some it
          | v when v = it.old_s -> None
          | v -> Some { it with new_s = v; kind = PlaceRename.New_place })
      | None -> Some it)
    items
  |> List.map (fun (it : PlaceRename.item) ->
      { it with kind = PlaceRename.New_place })
  |> PlaceRename.classify base

(* The preview of a plan: one checkbox per place, all checked; only the
   checked places are changed by [m=PLACE_TERMS_OK]. The new place of each
   item is an editable field, for a last correction ([apply_edits]). [hidden]
   emits the fields describing the change, [old_html] renders the old place
   of an item. *)
let print_plan conf base u items ~hidden ~back ~old_html =
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS_OK");
  hidden ();
  Output.print_sstring conf
    "<table class=\"table table-sm\">\n\
     <thead><tr><th><input type=\"checkbox\" class=\"form-check-input \
     ctx-all\" data-ctx=\"0\" checked></th><th colspan=\"4\"></th></tr></thead>\n\
     <tbody>\n";
  List.iter
    (fun (it : PlaceRename.item) ->
      List.iter
        (fun d ->
          Util.hidden_input conf
            (PlaceRename.occurrence_key (it.istr, d))
            (Adef.encoded "on"))
        it.depths;
      Output.printf conf
        "<tr><td><input type=\"checkbox\" class=\"form-check-input ctx0\" \
         name=\"%s\" value=\"on\" checked></td><td>%s</td><td>→</td><td><input \
         type=\"text\" class=\"form-control form-control-sm\" size=\"40\" \
         name=\"%s\" value=\"%s\"></td><td class=\"text-end \
         text-muted\">%d</td><td>%s</td></tr>\n"
        (PlaceRename.place_key it.istr)
        (old_html it) (edit_key it.istr) (esc it.new_s)
        (PlaceRename.uses u it.istr)
        (kind_html Places conf base it.kind))
    items;
  Output.print_sstring conf "</tbody></table>\n";
  Output.printf conf
    "<button type=\"submit\" class=\"btn btn-primary\">%s</button> <a \
     class=\"btn btn-outline-secondary\" href=\"%s\">%s</a>\n\
     </form>\n"
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0))
    back
    (Utf8.capitalize_fst (transl conf "cancel"));
  Output.print_sstring conf toggle_script

let print_preview conf base terms nt =
  let nt = PlaceTerms.clean_term nt in
  if not (PlaceRename.valid_new_term nt) then invalid_new_term conf terms nt
  else
    let u = PlaceRename.usage base in
    let items =
      PlaceRename.plan conf base ~terms ~new_term:nt
        (PlaceRename.occurrences_of_env conf)
    in
    Output.printf conf "<h2 class=\"h3\">%s</h2>\n<p>%s → <code>%s</code></p>\n"
      (Utf8.capitalize_fst (transl conf "preview of the changes"))
      (terms_html terms) (esc nt);
    if items = [] then
      Output.printf conf "<p>%s</p>\n<p><a href=\"%s\">%s</a></p>\n"
        (Utf8.capitalize_fst (transl conf "no modification"))
        (back_href conf terms) (terms_html terms)
    else
      print_plan conf base u items
        ~hidden:(fun () ->
          List.iteri
            (fun i t ->
              Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) t)
            terms;
          Util.hidden_input_s conf "nt" nt)
        ~back:(back_href conf terms)
        ~old_html:(fun (it : PlaceRename.item) ->
          mark_terms conf terms it.depths it.old_s)

(* ---- terms to split ---- *)

let split_href conf = Printf.sprintf "%sm=PLACE_TERMS&split=on" (prefix conf)

let split_min_uses conf =
  match p_getint conf.env "n" with
  | Some n when n > 0 -> n
  | Some _ | None -> PlaceSplit.default_min_uses

(* the terms checked on the list of proposals: fields [st1], [st2]... *)
let split_terms_of_env conf =
  List.filter_map
    (fun (k, _) ->
      let n = String.length k in
      if
        n > 2
        && String.starts_with ~prefix:"st" k
        && String.for_all
             (fun c -> c >= '0' && c <= '9')
             (String.sub k 2 (n - 2))
      then
        match p_getenv_notrim conf.env k with
        | Some t when t <> "" -> Some t
        | Some _ | None -> None
      else None)
    conf.env
  |> List.sort_uniq compare

let split_selected conf base =
  let terms = split_terms_of_env conf in
  PlaceSplit.proposals ~min_uses:(split_min_uses conf)
    (PlaceTerms.build conf base)
  |> List.filter (fun (p : PlaceSplit.proposal) -> List.mem p.entry.term terms)

let split_hidden conf terms =
  Util.hidden_input conf "split" (Adef.encoded "on");
  Util.hidden_input conf "n"
    (Adef.encoded (string_of_int (split_min_uses conf)));
  List.iteri
    (fun i t -> Util.hidden_input_s conf (Printf.sprintf "st%d" (i + 1)) t)
    terms

let print_split_list conf base =
  let min_uses = split_min_uses conf in
  let proposals = PlaceSplit.proposals ~min_uses (PlaceTerms.build conf base) in
  Output.printf conf "<p>%s</p>\n"
    (Utf8.capitalize_fst (transl conf "terms to split help"));
  (* the threshold of the frequent terms *)
  Output.printf conf
    "<form method=\"get\" action=\"%s\" class=\"d-flex align-items-center \
     gap-2 my-2\">\n"
    conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS");
  Util.hidden_input conf "split" (Adef.encoded "on");
  Output.printf conf
    "<label for=\"n\" class=\"text-nowrap\">%s</label>\n\
     <input type=\"number\" class=\"form-control form-control-sm w-auto\" \
     min=\"1\" id=\"n\" name=\"n\" value=\"%d\">\n\
     <button type=\"submit\" class=\"btn btn-sm btn-outline-secondary\" \
     title=\"%s\"><i class=\"fa fa-arrows-rotate\"></i></button>\n\
     </form>\n\
     <p>%d</p>\n"
    (Utf8.capitalize_fst (transl conf "frequent terms: minimum uses"))
    min_uses
    (esc (Utf8.capitalize_fst (transl conf "frequent terms: minimum uses")))
    (List.length proposals);
  if proposals <> [] then (
    let max = nb_max conf in
    if List.length proposals > max then
      Output.printf conf "<p>%s (%d)</p>\n"
        (Utf8.capitalize_fst (transl conf "v7 list too long"))
        max;
    Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
    Util.hidden_env conf;
    Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS");
    Util.hidden_input conf "split" (Adef.encoded "on");
    Util.hidden_input conf "n" (Adef.encoded (string_of_int min_uses));
    Output.printf conf
      "<button type=\"submit\" class=\"btn btn-primary my-2\">%s</button>\n"
      (Utf8.capitalize_fst (transl conf "preview of the changes"));
    (* one group per frequent term, with a checkbox checking the group; a
       proposal is checked when its first part is a known term *)
    ignore
      (List.fold_left
         (fun (i, group) (p : PlaceSplit.proposal) ->
           if i >= max then (i + 1, group)
           else
             (* [g]: the number of the group, the index of its first
                proposal *)
             let g =
               match group with
               | Some (right, g) when right = p.right -> g
               | Some _ | None ->
                   if group <> None then Output.print_sstring conf "</ul>\n";
                   Output.printf conf
                     "<h3 class=\"h5 mt-3\"><input type=\"checkbox\" \
                      class=\"form-check-input ctx-all me-2\" \
                      data-ctx=\"g%d\">%s</h3>\n\
                      <ul class=\"list-unstyled ms-3\">\n"
                     i (vis conf p.right);
                   i
             in
             Output.printf conf
               "<li><input type=\"checkbox\" class=\"form-check-input ctxg%d \
                me-2\" name=\"st%d\" value=\"%s\"%s>%s <span \
                class=\"text-muted\">(%d)</span> → <code>%s</code>%s</li>\n"
               g (i + 1) (esc p.entry.term)
               (if p.known then " checked" else "")
               (term_link conf p.entry.term)
               p.entry.uses
               (vis conf (PlaceSplit.new_term p))
               (if p.known then
                  " <span class=\"badge bg-success\">"
                  ^ esc (transl conf "known term")
                  ^ "</span>"
                else "");
             (i + 1, Some (p.right, g)))
         (0, None) proposals
        : int * (string * int) option);
    Output.printf conf
      "</ul>\n\
       <button type=\"submit\" class=\"btn btn-primary my-2\">%s</button>\n\
       </form>\n"
      (Utf8.capitalize_fst (transl conf "preview of the changes"));
    Output.print_sstring conf toggle_script)

let print_split_preview conf base =
  let selected = split_selected conf base in
  let items = PlaceSplit.plan conf base selected in
  let terms =
    List.map (fun (p : PlaceSplit.proposal) -> p.entry.term) selected
  in
  Output.printf conf "<h2 class=\"h3\">%s</h2>\n<p>%d %s</p>\n"
    (Utf8.capitalize_fst (transl conf "preview of the changes"))
    (List.length items)
    (transl_nth conf "place/places" (if List.length items > 1 then 1 else 0));
  if items = [] then
    Output.printf conf "<p>%s</p>\n<p><a href=\"%s\">%s</a></p>\n"
      (Utf8.capitalize_fst (transl conf "no modification"))
      (split_href conf)
      (Utf8.capitalize_fst (transl conf "terms to split"))
  else
    print_plan conf base (PlaceRename.usage base) items
      ~hidden:(fun () -> split_hidden conf terms)
      ~back:(split_href conf)
      ~old_html:(fun (it : PlaceRename.item) ->
        mark_terms conf terms it.depths it.old_s)

(* ---- terms to join ---- *)

let join_href conf = Printf.sprintf "%sm=PLACE_TERMS&join=on" (prefix conf)

let join_min_uses conf =
  match p_getint conf.env "n" with
  | Some n when n > 0 -> n
  | Some _ | None -> PlaceJoin.default_min_uses

(* the values of the fields [<prefix>1], [<prefix>2]... *)
let numbered_values conf prefix =
  let lp = String.length prefix in
  List.filter_map
    (fun (k, _) ->
      let n = String.length k in
      if
        n > lp
        && String.starts_with ~prefix k
        && String.for_all
             (fun c -> c >= '0' && c <= '9')
             (String.sub k lp (n - lp))
      then
        match p_getenv_notrim conf.env k with
        | Some t when t <> "" -> Some t
        | Some _ | None -> None
      else None)
    conf.env
  |> List.sort_uniq compare

let join_all conf base =
  PlaceJoin.proposals ~min_uses:(join_min_uses conf)
    (Place.places_inverted conf)
    (PlaceTerms.build conf base)
    (PlaceTerms.place_uses conf base
    |> List.map (fun (istr, uses) -> (istr, Driver.sou base istr, uses)))

let join_selected conf base =
  let keys = numbered_values conf "jt" in
  join_all conf base
  |> List.filter (fun (p : PlaceJoin.proposal) -> List.mem p.pkey keys)

let print_join_list conf base =
  let min_uses = join_min_uses conf in
  let proposals = join_all conf base in
  Output.printf conf "<p>%s</p>\n"
    (Utf8.capitalize_fst (transl conf "terms to join help"));
  Output.printf conf
    "<form method=\"get\" action=\"%s\" class=\"d-flex align-items-center \
     gap-2 my-2\">\n"
    conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS");
  Util.hidden_input conf "join" (Adef.encoded "on");
  Output.printf conf
    "<label for=\"n\" class=\"text-nowrap\">%s</label>\n\
     <input type=\"number\" class=\"form-control form-control-sm w-auto\" \
     min=\"1\" id=\"n\" name=\"n\" value=\"%d\">\n\
     <button type=\"submit\" class=\"btn btn-sm btn-outline-secondary\" \
     title=\"%s\"><i class=\"fa fa-arrows-rotate\"></i></button>\n\
     </form>\n\
     <p>%d</p>\n"
    (Utf8.capitalize_fst (transl conf "frequent terms: minimum uses"))
    min_uses
    (esc (Utf8.capitalize_fst (transl conf "frequent terms: minimum uses")))
    (List.length proposals);
  if proposals <> [] then (
    let max = nb_max conf in
    if List.length proposals > max then
      Output.printf conf "<p>%s (%d)</p>\n"
        (Utf8.capitalize_fst (transl conf "v7 list too long"))
        max;
    Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
    Util.hidden_env conf;
    Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS");
    Util.hidden_input conf "join" (Adef.encoded "on");
    Util.hidden_input conf "n" (Adef.encoded (string_of_int min_uses));
    let button () =
      Output.printf conf
        "<button type=\"submit\" class=\"btn btn-primary my-2\">%s</button>\n"
        (Utf8.capitalize_fst (transl conf "preview of the changes"))
    in
    button ();
    (* one group per frequent term, with a checkbox checking the group *)
    ignore
      (List.fold_left
         (fun (i, group) (p : PlaceJoin.proposal) ->
           if i >= max then (i + 1, group)
           else
             let g =
               match group with
               | Some (target, g) when target = p.target -> g
               | Some _ | None ->
                   if group <> None then Output.print_sstring conf "</ul>\n";
                   Output.printf conf
                     "<h3 class=\"h5 mt-3\"><input type=\"checkbox\" \
                      class=\"form-check-input ctx-all me-2\" data-ctx=\"j%d\" \
                      checked>%s</h3>\n\
                      <ul class=\"list-unstyled ms-3\">\n"
                     i (vis conf p.target);
                   i
             in
             Output.printf conf
               "<li><input type=\"checkbox\" class=\"form-check-input ctxj%d \
                me-2\" name=\"jt%d\" value=\"%s\" checked><code>%s</code> \
                <span class=\"text-muted\">(%d · %d)</span> → \
                <code>%s</code></li>\n"
               g (i + 1) (esc p.pkey) (vis conf p.written)
               (List.length p.places) p.uses (vis conf p.target);
             (i + 1, Some (p.target, g)))
         (0, None) proposals
        : int * (string * int) option);
    Output.print_sstring conf "</ul>\n";
    button ();
    Output.print_sstring conf "</form>\n";
    Output.print_sstring conf toggle_script)

let print_join_preview conf base =
  let selected = join_selected conf base in
  let items = PlaceJoin.plan base selected in
  Output.printf conf "<h2 class=\"h3\">%s</h2>\n<p>%d %s</p>\n"
    (Utf8.capitalize_fst (transl conf "preview of the changes"))
    (List.length items)
    (transl_nth conf "place/places" (if List.length items > 1 then 1 else 0));
  if items = [] then
    Output.printf conf "<p>%s</p>\n<p><a href=\"%s\">%s</a></p>\n"
      (Utf8.capitalize_fst (transl conf "no modification"))
      (join_href conf)
      (Utf8.capitalize_fst (transl conf "terms to join"))
  else
    print_plan conf base (PlaceRename.usage base) items
      ~hidden:(fun () ->
        Util.hidden_input conf "join" (Adef.encoded "on");
        Util.hidden_input conf "n"
          (Adef.encoded (string_of_int (join_min_uses conf)));
        List.iteri
          (fun i (p : PlaceJoin.proposal) ->
            Util.hidden_input_s conf (Printf.sprintf "jt%d" (i + 1)) p.pkey)
          selected)
      ~back:(join_href conf)
      ~old_html:(fun (it : PlaceRename.item) -> vis conf it.old_s)

(* ---- places to complete ---- *)

let groups_href conf = Printf.sprintf "%sm=PLACE_TERMS&groups=on" (prefix conf)

let group_href conf g =
  Printf.sprintf "%sm=PLACE_TERMS&g=%s" (prefix conf)
    (Driver.Istr.to_string (PlaceGroups.key g))

let place_html (p : PlaceTerms.place) =
  Printf.sprintf "%s <span class=\"text-muted\">(%d)</span>" (esc p.raw) p.uses

let print_groups conf groups =
  Output.printf conf "<p>%d</p>\n" (List.length groups);
  if groups <> [] then (
    Output.printf conf
      "<table class=\"table table-sm table-hover w-auto\">\n\
       <thead><tr><th>%s</th><th>%s</th><th \
       class=\"text-end\">%s</th><th></th></tr></thead>\n\
       <tbody>\n"
      (Utf8.capitalize_fst (transl_nth conf "full place/full places" 0))
      (Utf8.capitalize_fst (transl conf "places to complete"))
      (Utf8.capitalize_fst (transl conf "uses"));
    List.iter
      (fun (g : PlaceGroups.group) ->
        Output.printf conf
          "<tr><td>%s</td><td>%s</td><td class=\"text-end\">%d</td><td><a \
           href=\"%s\">%s</a></td></tr>\n"
          (String.concat "<br>" (List.map place_html g.targets))
          (String.concat "<br>" (List.map place_html g.members))
          g.uses (group_href conf g)
          (esc (Utf8.capitalize_fst (transl conf "complete the places"))))
      groups;
    Output.print_sstring conf "</tbody></table>\n")

(* the complete place to keep (one of the targets, the most used by default)
   and the places to replace by it; a place in several groups is ambiguous,
   left unchecked *)
let print_group conf groups key =
  match PlaceGroups.find groups key with
  | None ->
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "not found"))
  | Some (g : PlaceGroups.group) ->
      Output.printf conf "<h2 class=\"h3\">%s</h2>\n"
        (Utf8.capitalize_fst (transl conf "complete the places"));
      open_form conf;
      Util.hidden_input conf "g"
        (Adef.encoded (Driver.Istr.to_string (PlaceGroups.key g)));
      Output.printf conf
        "<fieldset class=\"my-3\"><legend class=\"h5\">%s</legend>\n"
        (Utf8.capitalize_fst (transl_nth conf "full place/full places" 0));
      List.iteri
        (fun i (p : PlaceTerms.place) ->
          Output.printf conf
            "<div class=\"form-check\"><input class=\"form-check-input\" \
             type=\"radio\" name=\"gt\" id=\"gt%d\" value=\"%s\"%s><label \
             class=\"form-check-label\" for=\"gt%d\">%s</label></div>\n"
            i
            (Driver.Istr.to_string p.istr)
            (if i = 0 then " checked" else "")
            i (place_html p))
        g.targets;
      Output.printf conf
        "</fieldset>\n\
         <h3 class=\"h5\"><input type=\"checkbox\" class=\"form-check-input \
         ctx-all me-2\" data-ctx=\"0\" checked>%s</h3>\n\
         <ul class=\"list-unstyled ms-3\">\n"
        (Utf8.capitalize_fst (transl conf "places to complete"));
      (* every place of the group; the one chosen as complete place is left
         as it is *)
      List.iter
        (fun (p : PlaceTerms.place) ->
          let n = PlaceGroups.count_groups groups p.istr in
          let ambiguous = n > 1 in
          Output.printf conf
            "<li><input type=\"checkbox\" class=\"form-check-input ctx0 me-2\" \
             name=\"%s\" value=\"on\"%s>%s%s%s</li>\n"
            (PlaceRename.place_key p.istr)
            (if ambiguous then "" else " checked")
            (place_html p)
            (if ambiguous then
               Printf.sprintf
                 " <span class=\"badge bg-warning text-dark\">%s</span>"
                 (esc (Printf.sprintf (ftransl conf "in %d groups") n))
             else "")
            (place_edit_link conf p.raw))
        (g.targets @ g.members);
      Output.printf conf
        "</ul>\n\
         <button type=\"submit\" class=\"btn btn-primary\">%s</button>\n\
         </form>\n"
        (Utf8.capitalize_fst (transl conf "preview of the changes"));
      Output.print_sstring conf toggle_script

(* the places of the submitted group to replace by its chosen target; only
   places still in the group are kept *)
let group_items conf base =
  match (p_getenv conf.env "g", p_getenv conf.env "gt") with
  | Some g, Some gt -> (
      let groups = PlaceGroups.build conf base in
      match PlaceGroups.find groups (Driver.Istr.of_string g) with
      | None -> None
      | Some (grp : PlaceGroups.group) ->
          let target = Driver.Istr.of_string gt in
          let in_group i =
            List.exists
              (fun (p : PlaceTerms.place) -> Driver.Istr.equal p.istr i)
              (grp.targets @ grp.members)
          in
          if not (in_group target) then None
          else
            let places =
              List.filter in_group (PlaceRename.places_of_env conf)
            in
            Some (target, PlaceRename.plan_places base ~target places))
  | _ -> None

let print_group_preview conf base =
  match group_items conf base with
  | None ->
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "not found"))
  | Some (target, items) ->
      Output.printf conf "<h2 class=\"h3\">%s</h2>\n<p>→ <code>%s</code></p>\n"
        (Utf8.capitalize_fst (transl conf "preview of the changes"))
        (esc (Driver.sou base target));
      if items = [] then
        Output.printf conf "<p>%s</p>\n"
          (Utf8.capitalize_fst (transl conf "no modification"))
      else
        print_plan conf base (PlaceRename.usage base) items
          ~hidden:(fun () ->
            List.iter
              (fun k ->
                Util.hidden_input conf k
                  (Option.value ~default:(Adef.encoded "")
                     (List.assoc_opt k conf.env)))
              [ "g"; "gt" ])
          ~back:(groups_href conf)
          ~old_html:(fun (it : PlaceRename.item) -> esc it.old_s)

(* ---- sources to group ---- *)

let src_href conf part =
  Printf.sprintf "%sm=SOURCE_TERMS&part=%s" (prefix conf)
    (SourceTerms.string_of_part part)

let src_part conf =
  Option.value ~default:SourceTerms.Wizard
    (Option.bind (p_getenv conf.env "part") SourceTerms.part_of_string)

(* [raw] escaped, with [spans] marked *)
let mark_spans conf raw (spans : SourceTerms.span list) =
  let spans =
    List.sort (fun (a : SourceTerms.span) b -> compare a.pos b.pos) spans
  in
  let buf = Buffer.create (String.length raw + 32) in
  let last =
    List.fold_left
      (fun i (sp : SourceTerms.span) ->
        if sp.pos < i then i
        else (
          Buffer.add_string buf (vis conf (String.sub raw i (sp.pos - i)));
          Buffer.add_string buf ("<mark>" ^ vis conf sp.text ^ "</mark>");
          sp.pos + sp.len))
      0 spans
  in
  Buffer.add_string buf
    (vis conf (String.sub raw last (String.length raw - last)));
  Buffer.contents buf

let spelling_html conf (sp : SourceTerms.spelling) =
  Printf.sprintf "<code>%s</code> <span class=\"text-muted\">(%d · %d)</span>"
    (vis conf sp.text) sp.strings sp.uses

let src_cluster_href conf (c : SourceTerms.cluster) =
  Printf.sprintf "%s&c=%s" (src_href conf c.part) (enc c.ckey)

let print_src_clusters conf base =
  let part = src_part conf in
  let d = SourceTerms.load conf base in
  let clusters = SourceTerms.clusters d part in
  let tab p label =
    Printf.sprintf "<a class=\"nav-link%s\" href=\"%s\">%s</a>"
      (if p = part then " active" else "")
      (src_href conf p)
      (esc (Utf8.capitalize_fst label))
  in
  Output.printf conf "<nav class=\"nav nav-tabs mb-2\">%s%s</nav>\n"
    (tab SourceTerms.Wizard
       (transl_nth conf "wizard/wizards/friend/friends/exterior" 1))
    (tab SourceTerms.Source (transl_nth conf "source/sources" 1));
  (* the sources are many: chosen by the beginning of their key *)
  let s = Option.value ~default:"" (p_getenv conf.env "s") in
  let selected =
    List.filter
      (fun (c : SourceTerms.cluster) -> String.starts_with ~prefix:s c.ckey)
      clusters
  in
  Output.printf conf "<p>%d</p>\n" (List.length selected);
  if part = SourceTerms.Source then (
    let inis =
      let n = String.length s + 1 in
      List.filter_map
        (fun (c : SourceTerms.cluster) ->
          if String.length c.ckey >= n then Some (String.sub c.ckey 0 n)
          else None)
        selected
      |> List.sort_uniq compare
    in
    Output.print_sstring conf "<div class=\"d-flex flex-wrap my-2\">\n";
    List.iter
      (fun ini ->
        Output.printf conf
          "<a class=\"btn btn-sm btn-outline-primary font-monospace me-1 \
           mb-1\" href=\"%s&s=%s\">%s</a>\n"
          (src_href conf part) (enc ini) (esc ini))
      inis;
    Output.print_sstring conf "</div>\n");
  if List.length selected > nb_max conf then
    Output.printf conf "<p>%s</p>\n"
      (Utf8.capitalize_fst (transl conf "v7 list too long"))
  else if selected <> [] then (
    Output.printf conf
      "<table class=\"table table-sm table-hover w-auto\">\n\
       <thead><tr><th>%s</th><th \
       class=\"text-end\">%s</th><th></th></tr></thead>\n\
       <tbody>\n"
      (Utf8.capitalize_fst (transl conf "spellings"))
      (Utf8.capitalize_fst (transl conf "uses"));
    List.iter
      (fun (c : SourceTerms.cluster) ->
        Output.printf conf
          "<tr><td>%s</td><td class=\"text-end\">%d</td><td><a \
           href=\"%s\">%s</a></td></tr>\n"
          (String.concat "<br>" (List.map (spelling_html conf) c.spellings))
          c.total (src_cluster_href conf c)
          (esc (Utf8.capitalize_fst (transl conf "merge spellings"))))
      selected;
    Output.print_sstring conf "</tbody></table>\n")

(* The spelling to keep and the spellings to replace by it, each with a few
   examples: the form of a cluster of sources or of occupations. [hidden]
   emits the fields of the section. *)
let print_spellings_form conf ~m ~hidden (spellings : SourceTerms.spelling list)
    samples =
  Output.printf conf "<h2 class=\"h3\">%s</h2>\n"
    (Utf8.capitalize_fst (transl conf "merge spellings"));
  open_form ~m conf;
  hidden ();
  Output.printf conf
    "<fieldset class=\"my-3\"><legend class=\"h5\">%s</legend>\n"
    (Utf8.capitalize_fst (transl conf "keep the spelling"));
  keep_rows conf
    (List.mapi
       (fun i (sp : SourceTerms.spelling) ->
         keep_row conf i ~value:sp.text ~checked:(i = 0) (spelling_html conf sp))
       spellings);
  Output.printf conf
    "<div class=\"form-check d-flex align-items-center gap-2\"><input \
     class=\"form-check-input\" type=\"radio\" name=\"ntc\" id=\"ntco\" \
     value=\"%s\"><label class=\"form-check-label\" for=\"ntco\">%s</label> \
     <input type=\"text\" class=\"form-control form-control-sm w-auto\" \
     size=\"30\" name=\"nto\" \
     onfocus=\"document.getElementById('ntco').checked=true\"></div>\n\
     </fieldset>\n"
    (esc other_choice)
    (Utf8.capitalize_fst (transl conf "other spelling"));
  (* every spelling, with all its occurrences; the one kept is left as it is *)
  List.iteri
    (fun i (sp : SourceTerms.spelling) ->
      Output.printf conf
        "<h3 class=\"h6 mt-3\"><input type=\"checkbox\" \
         class=\"form-check-input me-2\" id=\"spt%d\" name=\"t%d\" \
         value=\"%s\" checked>%s</h3>\n\
         <ul class=\"small ms-3\">\n"
        i (i + 1) (esc sp.text) (spelling_html conf sp);
      List.iter
        (fun (raw, spans) ->
          Output.printf conf "<li>%s</li>\n" (mark_spans conf raw spans))
        (Option.value ~default:[] (List.assoc_opt sp.text samples));
      Output.print_sstring conf "</ul>\n")
    spellings;
  Output.printf conf
    "<button type=\"submit\" class=\"btn btn-primary\">%s</button>\n</form>\n"
    (Utf8.capitalize_fst (transl conf "preview of the changes"));
  Output.print_sstring conf exclude_script

let print_src_cluster conf base key =
  let part = src_part conf in
  let d = SourceTerms.load conf base in
  match
    List.find_opt
      (fun (c : SourceTerms.cluster) -> c.ckey = key)
      (SourceTerms.clusters d part)
  with
  | None ->
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "not found"))
  | Some c ->
      let samples =
        SourceTerms.samples d part
          (List.map (fun (sp : SourceTerms.spelling) -> sp.text) c.spellings)
          5
      in
      print_spellings_form conf ~m:"SOURCE_TERMS"
        ~hidden:(fun () ->
          Util.hidden_input conf "part"
            (Adef.encoded (SourceTerms.string_of_part part)))
        c.spellings samples

let invalid_src_term conf =
  Output.printf conf
    "<div class=\"alert alert-danger\">%s</div>\n<p><a href=\"%s\">%s</a></p>\n"
    (Utf8.capitalize_fst (transl conf "invalid new term"))
    (src_href conf (src_part conf))
    (Utf8.capitalize_fst (transl conf "sources to group"))

let src_items conf base terms nt =
  let d = SourceTerms.load conf base in
  SourceTerms.plan base d (src_part conf) ~terms ~new_term:nt

(* The preview of a renaming of sources or occupations: they are many, so the
   whole selection is confirmed at once, the first ones shown. [hidden] emits
   the fields of the section; [plural] is the lexicon entry of the strings. *)
let print_spans_preview conf base section ~m ~hidden ~back ~plural u terms nt
    items =
  Output.printf conf
    "<h2 class=\"h3\">%s</h2>\n<p>%s → <code>%s</code></p>\n<p>%d %s</p>\n"
    (Utf8.capitalize_fst (transl conf "preview of the changes"))
    (terms_html terms) (esc nt) (List.length items)
    (transl_nth conf plural (if List.length items > 1 then 1 else 0));
  if items = [] then
    Output.printf conf "<p>%s</p>\n"
      (Utf8.capitalize_fst (transl conf "no modification"))
  else (
    Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
    Util.hidden_env conf;
    Util.hidden_input conf "m" (Adef.encoded m);
    hidden ();
    List.iteri
      (fun i t -> Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) t)
      terms;
    Util.hidden_input_s conf "nt" nt;
    Output.printf conf
      "<button type=\"submit\" class=\"btn btn-primary\">%s</button> <a \
       class=\"btn btn-outline-secondary\" href=\"%s\">%s</a>\n\
       </form>\n"
      (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0))
      back
      (Utf8.capitalize_fst (transl conf "cancel"));
    Output.print_sstring conf
      "<table class=\"table table-sm small mt-3\"><tbody>\n";
    List.iteri
      (fun i ((it : PlaceRename.item), spans) ->
        if i < 200 then
          Output.printf conf
            "<tr><td>%s</td><td>→</td><td>%s</td><td class=\"text-end \
             text-muted\">%d</td><td>%s</td></tr>\n"
            (mark_spans conf it.old_s spans)
            (esc it.new_s)
            (PlaceRename.uses u it.istr)
            (kind_html section conf base it.kind))
      items;
    Output.print_sstring conf "</tbody></table>\n";
    if List.length items > 200 then Output.print_sstring conf "<p>…</p>\n")

let print_src_preview conf base terms nt =
  let nt = PlaceTerms.clean_term nt in
  if nt = "" then invalid_src_term conf
  else
    let part = src_part conf in
    print_spans_preview conf base Sources ~m:"SOURCE_TERMS_OK"
      ~hidden:(fun () ->
        Util.hidden_input conf "part"
          (Adef.encoded (SourceTerms.string_of_part part)))
      ~back:(src_href conf part) ~plural:"source/sources"
      (PlaceRename.usage ~field:PlaceRename.Source base)
      terms nt
      (src_items conf base terms nt)

let print_src conf base =
  let nonempty k =
    match p_getenv_notrim conf.env k with
    | Some s when s <> "" -> Some s
    | _ -> None
  in
  match (PlaceRename.terms_of_env conf, new_term_of_env conf) with
  | (_ :: _ as terms), Some nt -> print_src_preview conf base terms nt
  | _ -> (
      match nonempty "c" with
      | Some key -> print_src_cluster conf base key
      | None -> print_src_clusters conf base)

(* ---- occupations ---- *)

let occu_href conf = prefix conf ^ "m=OCCU_TERMS"

(* the clusters of spellings, chosen by the beginning of their key when they
   are too many *)
let print_occu_clusters conf base =
  let clusters = OccuTerms.clusters (OccuTerms.load base) in
  let s = Option.value ~default:"" (p_getenv conf.env "s") in
  let selected =
    List.filter
      (fun (c : OccuTerms.cluster) -> String.starts_with ~prefix:s c.ckey)
      clusters
  in
  Output.printf conf "<p>%d</p>\n" (List.length selected);
  let too_long = List.length selected > nb_max conf in
  if too_long || s <> "" then (
    let inis =
      let n = String.length s + 1 in
      List.filter_map
        (fun (c : OccuTerms.cluster) ->
          if String.length c.ckey >= n then Some (String.sub c.ckey 0 n)
          else None)
        selected
      |> List.sort_uniq compare
    in
    Output.print_sstring conf "<div class=\"d-flex flex-wrap my-2\">\n";
    List.iter
      (fun ini ->
        Output.printf conf
          "<a class=\"btn btn-sm btn-outline-primary font-monospace me-1 \
           mb-1\" href=\"%s&s=%s\">%s</a>\n"
          (occu_href conf) (enc ini) (esc ini))
      inis;
    Output.print_sstring conf "</div>\n");
  if too_long then
    Output.printf conf "<p>%s</p>\n"
      (Utf8.capitalize_fst (transl conf "v7 list too long"))
  else if selected <> [] then (
    Output.printf conf
      "<table class=\"table table-sm table-hover w-auto\">\n\
       <thead><tr><th>%s</th><th \
       class=\"text-end\">%s</th><th></th></tr></thead>\n\
       <tbody>\n"
      (Utf8.capitalize_fst (transl conf "spellings"))
      (Utf8.capitalize_fst (transl conf "uses"));
    List.iter
      (fun (c : OccuTerms.cluster) ->
        Output.printf conf
          "<tr><td>%s</td><td class=\"text-end\">%d</td><td><a \
           href=\"%s&c=%s\">%s</a></td></tr>\n"
          (String.concat "<br>" (List.map (spelling_html conf) c.spellings))
          c.total (occu_href conf) (enc c.ckey)
          (esc (Utf8.capitalize_fst (transl conf "merge spellings"))))
      selected;
    Output.print_sstring conf "</tbody></table>\n")

let print_occu_cluster conf base key =
  let d = OccuTerms.load base in
  match
    List.find_opt
      (fun (c : OccuTerms.cluster) -> c.ckey = key)
      (OccuTerms.clusters d)
  with
  | None ->
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "not found"))
  | Some c ->
      let samples =
        OccuTerms.samples d
          (List.map (fun (sp : SourceTerms.spelling) -> sp.text) c.spellings)
          5
      in
      print_spellings_form conf ~m:"OCCU_TERMS"
        ~hidden:(fun () -> ())
        c.spellings samples

let invalid_occu_term conf =
  Output.printf conf
    "<div class=\"alert alert-danger\">%s</div>\n<p><a href=\"%s\">%s</a></p>\n"
    (Utf8.capitalize_fst (transl conf "invalid new term"))
    (occu_href conf)
    (Utf8.capitalize_fst (transl conf "occupations to group"))

let occu_items base terms nt =
  OccuTerms.plan base (OccuTerms.load base) ~terms ~new_term:nt

let print_occu conf base =
  let nonempty k =
    match p_getenv_notrim conf.env k with
    | Some s when s <> "" -> Some s
    | _ -> None
  in
  match (PlaceRename.terms_of_env conf, new_term_of_env conf) with
  | (_ :: _ as terms), Some nt ->
      let nt = PlaceTerms.clean_term nt in
      if nt = "" then invalid_occu_term conf
      else
        print_spans_preview conf base Occupations ~m:"OCCU_TERMS_OK"
          ~hidden:(fun () -> ())
          ~back:(occu_href conf) ~plural:"occupation/occupations"
          (PlaceRename.usage ~field:PlaceRename.Occupation base)
          terms nt (occu_items base terms nt)
  | _ -> (
      match nonempty "c" with
      | Some key -> print_occu_cluster conf base key
      | None -> print_occu_clusters conf base)

(* the places, the sources and the occupations have their own pages, with the
   same layout: the book, then the pages of grouping *)
let header section conf =
  let title _ =
    Output.print_sstring conf
      (Utf8.capitalize_fst
         (transl conf
            (match section with
            | Places -> "spelling clusters"
            | Sources -> "sources to group"
            | Occupations -> "occupations to group")))
  in
  Hutil.header conf title;
  let book data icon plural =
    Printf.sprintf
      "<a href=\"%sm=MOD_DATA&data=%s\"><i class=\"fa fa-%s me-1\"></i>%s</a>"
      (prefix conf) data icon
      (Utf8.capitalize_fst
         (Printf.sprintf (ftransl conf "book of %s") (transl_nth conf plural 1)))
  in
  let link href icon label =
    Printf.sprintf "<a href=\"%s\"><i class=\"fa fa-%s me-1\"></i>%s</a>" href
      icon
      (Utf8.capitalize_fst (transl conf label))
  in
  Output.printf conf "<p>%s</p>\n"
    (String.concat " · "
       (match section with
       | Sources ->
           [
             book "src" "box-archive" "source/sources";
             link
               (prefix conf ^ "m=SOURCE_TERMS")
               "object-group" "sources to group";
           ]
       | Occupations ->
           [
             book "occu" "briefcase" "occupation/occupations";
             link (occu_href conf) "object-group" "occupations to group";
           ]
       | Places ->
           [
             book "place" "location-dot" "place/places";
             link
               (prefix conf ^ "m=PLACE_TERMS")
               "object-group" "spelling clusters";
             link (groups_href conf) "up-right-and-down-left-from-center"
               "places to complete";
             link (split_href conf) "scissors" "terms to split";
             link (join_href conf) "link" "terms to join";
           ]))

let print conf base =
  header Places conf;
  let nonempty k =
    match p_getenv_notrim conf.env k with
    | Some s when s <> "" -> Some s
    | _ -> None
  in
  (match (PlaceRename.terms_of_env conf, new_term_of_env conf) with
  | (_ :: _ as terms), Some nt -> print_preview conf base terms nt
  | _ when p_getenv conf.env "gt" <> None -> print_group_preview conf base
  | _ when p_getenv conf.env "split" = Some "on" ->
      if split_terms_of_env conf <> [] then print_split_preview conf base
      else print_split_list conf base
  | _ when p_getenv conf.env "join" = Some "on" ->
      if numbered_values conf "jt" <> [] then print_join_preview conf base
      else print_join_list conf base
  | _ when nonempty "g" <> None ->
      print_group conf
        (PlaceGroups.build conf base)
        (Driver.Istr.of_string (Option.get (nonempty "g")))
  | _ when p_getenv conf.env "groups" = Some "on" ->
      print_groups conf (PlaceGroups.build conf base)
  | _ -> (
      (* the clusters of spellings by default; the index of the terms
         ([s=]) is not linked any more *)
      match (nonempty "c", nonempty "t", p_getenv_notrim conf.env "s") with
      | Some key, _, _ -> print_cluster conf (PlaceTerms.build conf base) key
      | None, Some t, _ -> print_term conf (PlaceTerms.build conf base) t
      | None, None, Some s -> print_list conf (PlaceTerms.build conf base) s
      | None, None, None -> print_clusters conf (PlaceTerms.build conf base)));
  Hutil.trailer conf

(* the fields of the confirmed form, to apply it again *)
let continue_form ~m conf =
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded m);
  List.iter
    (fun (k, v) ->
      if k <> "m" then Util.hidden_input_s conf k (Mutil.gen_decode false v))
    (List.filter
       (fun (k, _) ->
         k = "nt" || k = "t" || k = "g" || k = "gt" || k = "part" || k = "split"
         || k = "join" || k = "n"
         || String.length k > 2
            && String.starts_with ~prefix:"jt" k
            && String.for_all
                 (fun c -> c >= '0' && c <= '9')
                 (String.sub k 2 (String.length k - 2))
         || String.length k > 2
            && String.starts_with ~prefix:"ne" k
            && String.for_all
                 (fun c -> c >= '0' && c <= '9')
                 (String.sub k 2 (String.length k - 2))
         || PlaceRename.occurrence_of_key k <> None
         || String.length k > 2
            && String.starts_with ~prefix:"st" k
            && String.for_all
                 (fun c -> c >= '0' && c <= '9')
                 (String.sub k 2 (String.length k - 2))
         || String.length k > 1
            && (k.[0] = 't' || k.[0] = 'p')
            && String.for_all
                 (fun c -> c >= '0' && c <= '9')
                 (String.sub k 1 (String.length k - 1)))
       conf.env);
  Output.printf conf
    "<p>%s <button type=\"submit\" class=\"btn btn-primary\">%s</button></p>\n\
     </form>\n"
    (Utf8.capitalize_fst (transl conf "continue correcting"))
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0))

let print_result section conf (r : PlaceRename.result) =
  Output.printf conf "<div class=\"alert alert-success\">%s: %s</div>\n"
    (Utf8.capitalize_fst (transl conf "modification successful"))
    (Printf.sprintf
       (match section with
       | Places ->
           ftransl conf "%d places renamed (%d merged), %d records updated"
       | Sources ->
           ftransl conf "%d sources renamed (%d merged), %d records updated"
       | Occupations ->
           ftransl conf "%d occupations renamed (%d merged), %d records updated")
       r.places r.merged (r.persons + r.families));
  if not r.complete then
    continue_form conf
      ~m:
        (match section with
        | Places -> "PLACE_TERMS_OK"
        | Sources -> "SOURCE_TERMS_OK"
        | Occupations -> "OCCU_TERMS_OK");
  if List.assoc_opt "history" conf.base_env = Some "yes" then
    Output.printf conf "<p><a href=\"%sm=HIST&k=20\">%s</a></p>\n" (prefix conf)
      (Utf8.capitalize_fst (transl_nth conf "modification/modifications" 1))

let print_ok conf base =
  header Places conf;
  (match (PlaceRename.terms_of_env conf, p_getenv_notrim conf.env "nt") with
  | _ when p_getenv conf.env "gt" <> None -> (
      (* recomputed: the base may have changed since the preview *)
      match group_items conf base with
      | None ->
          Output.printf conf "<p>%s</p>\n"
            (Utf8.capitalize_fst (transl conf "not found"))
      | Some (_, items) ->
          print_result Places conf
            (PlaceRename.apply conf base (apply_edits conf base items));
          Output.printf conf "<p><a href=\"%s\">%s</a></p>\n" (groups_href conf)
            (Utf8.capitalize_fst (transl conf "places to complete")))
  | _ when p_getenv conf.env "split" = Some "on" ->
      (* recomputed: the base may have changed since the preview *)
      let items =
        PlaceSplit.plan conf base (split_selected conf base)
        |> List.filter (fun (it : PlaceRename.item) ->
            p_getenv conf.env (PlaceRename.place_key it.istr) = Some "on")
        |> apply_edits conf base
      in
      print_result Places conf (PlaceRename.apply conf base items);
      Output.printf conf "<p><a href=\"%s&n=%d\">%s</a></p>\n" (split_href conf)
        (split_min_uses conf)
        (Utf8.capitalize_fst (transl conf "terms to split"))
  | _ when p_getenv conf.env "join" = Some "on" ->
      (* recomputed: the base may have changed since the preview *)
      let items =
        PlaceJoin.plan base (join_selected conf base)
        |> List.filter (fun (it : PlaceRename.item) ->
            p_getenv conf.env (PlaceRename.place_key it.istr) = Some "on")
        |> apply_edits conf base
      in
      print_result Places conf (PlaceRename.apply conf base items);
      Output.printf conf "<p><a href=\"%s&n=%d\">%s</a></p>\n" (join_href conf)
        (join_min_uses conf)
        (Utf8.capitalize_fst (transl conf "terms to join"))
  | (_ :: _ as terms), Some nt ->
      let nt = PlaceTerms.clean_term nt in
      if not (PlaceRename.valid_new_term nt) then invalid_new_term conf terms nt
      else
        (* recomputed: the base may have changed since the preview *)
        let items =
          PlaceRename.plan conf base ~terms ~new_term:nt
            (PlaceRename.occurrences_of_env conf
            |> List.filter (fun (istr, _) ->
                p_getenv conf.env (PlaceRename.place_key istr) = Some "on"))
          |> apply_edits conf base
        in
        print_result Places conf (PlaceRename.apply conf base items);
        Output.printf conf "<p><a href=\"%sm=PLACE_TERMS&t=%s\">%s</a></p>\n"
          (prefix conf) (enc nt) (esc nt)
  | _ -> ());
  Hutil.trailer conf

let print_sources conf base =
  header Sources conf;
  print_src conf base;
  Hutil.trailer conf

let print_sources_ok conf base =
  header Sources conf;
  (match (PlaceRename.terms_of_env conf, p_getenv_notrim conf.env "nt") with
  | (_ :: _ as terms), Some nt ->
      let nt = PlaceTerms.clean_term nt in
      if nt = "" then invalid_src_term conf
      else
        (* recomputed: the base may have changed since the preview *)
        let items = List.map fst (src_items conf base terms nt) in
        print_result Sources conf
          (PlaceRename.apply ~field:PlaceRename.Source conf base items);
        Output.printf conf "<p><a href=\"%s\">%s</a></p>\n"
          (src_href conf (src_part conf))
          (Utf8.capitalize_fst (transl conf "sources to group"))
  | _ -> ());
  Hutil.trailer conf

let print_occupations conf base =
  header Occupations conf;
  print_occu conf base;
  Hutil.trailer conf

let print_occupations_ok conf base =
  header Occupations conf;
  (match (PlaceRename.terms_of_env conf, p_getenv_notrim conf.env "nt") with
  | (_ :: _ as terms), Some nt ->
      let nt = PlaceTerms.clean_term nt in
      if nt = "" then invalid_occu_term conf
      else
        (* recomputed: the base may have changed since the preview *)
        let items = List.map fst (occu_items base terms nt) in
        print_result Occupations conf
          (PlaceRename.apply ~field:PlaceRename.Occupation conf base items);
        Output.printf conf "<p><a href=\"%s\">%s</a></p>\n" (occu_href conf)
          (Utf8.capitalize_fst (transl conf "occupations to group"))
  | _ -> ());
  Hutil.trailer conf
