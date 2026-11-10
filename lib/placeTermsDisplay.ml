open Config
open Util
module Driver = Geneweb_db.Driver

let esc s = (Util.escape_html s :> string)
let enc s = (Mutil.encode s :> string)
let prefix conf = (commd conf :> string)

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

let ok_button href =
  Printf.sprintf "<a class=\"btn btn-sm btn-primary\" href=\"%s\">OK</a>" href

(* the header of the column of these buttons: "Merge", "Merge spellings" in
   its tooltip (the title of the page says it already) *)
let merge_th conf =
  Printf.sprintf "<th class=\"ps-4\" title=\"%s\">%s</th>"
    (esc (Utf8.capitalize_fst (transl conf "merge spellings")))
    (Utf8.capitalize_fst (transl_decline conf "merge" ""))

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
      Output.print_sstring conf "</form>\n"

(* ---- clusters of spellings ---- *)

(* ---- long lists: a slice of [n] rows from [from], filtered by [bw] ---- *)

let page_sizes = [ 25; 50; 100; 200; 500; 1000 ]

(* the number of rows of a slice: [n] chosen on the page, else
   [terms_page_size] of the .gwf, else 100 *)
let page_size conf =
  let ok n = n >= 1 && n <= 5000 in
  match p_getint conf.env "n" with
  | Some n when ok n -> n
  | Some _ | None -> (
      match
        Option.bind
          (List.assoc_opt "terms_page_size" conf.base_env)
          int_of_string_opt
      with
      | Some n when ok n -> n
      | Some _ | None -> 100)

let slice conf total =
  let from =
    match p_getint conf.env "from" with
    | Some f when f >= 1 && f <= total -> f
    | Some _ | None -> 1
  in
  (from, min total (from + page_size conf - 1))

let take (from, upto) l = List.filteri (fun i _ -> i >= from - 1 && i < upto) l

(* [bw]: the rows whose first spelling begins with it, case and accents
   ignored, its spaces kept ("33 " is not "33,"); not [b], the name of the
   base in some URLs *)
let begins_with conf =
  match p_getenv_notrim conf.env "bw" with
  | Some b when b <> "" -> Some b
  | Some _ | None -> None

(* [s] lowercased and unaccented, its spaces and punctuation kept (unlike
   [Name.lower]): "33 " and "33," are different beginnings *)
let fold_case s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec loop i =
    if i < n then (
      if Char.code s.[i] < 0x80 then (
        Buffer.add_char b (Char.lowercase_ascii s.[i]);
        loop (i + 1))
      else
        let t, j = Name.unaccent_utf_8 true s i in
        Buffer.add_string b t;
        loop j)
  in
  loop 0;
  Buffer.contents b

let begins_ok conf =
  match begins_with conf with
  | None -> fun _ -> true
  | Some b ->
      let b = fold_case b in
      fun s -> String.starts_with ~prefix:b (fold_case s)

(* The bar of a long list: the field [bw], the size of the slices, the
   previous and next slices. [href] is the page without [bw], [n] and
   [from]; [hidden] emits its fields. Changing [bw] or [n] goes back to the
   first row. [counts] is the number of rows and the slice shown; without it
   (nothing computed yet), a button shows all the rows ([all=on]). *)
let print_list_bar conf ~href ~hidden counts =
  let n = page_size conf in
  let bw = begins_with conf in
  let url ?(bw = bw) from =
    Printf.sprintf "%s%s&n=%d&from=%d" href
      (match bw with Some b -> "&bw=" ^ enc b | None -> "")
      n from
  in
  let button label href =
    Printf.sprintf
      "<a class=\"btn btn-sm btn-outline-primary\" href=\"%s\">%s</a>" href
      label
  in
  Output.printf conf
    "<form method=\"get\" action=\"%s\" class=\"d-flex flex-wrap \
     align-items-center gap-2 my-2\">\n"
    conf.command;
  Util.hidden_env conf;
  hidden ();
  Output.printf conf
    "<label for=\"bw\" class=\"text-nowrap\">%s</label>\n\
     <input type=\"text\" class=\"form-control form-control-sm w-auto\" \
     id=\"bw\" name=\"bw\" value=\"%s\" size=\"20\">\n\
     <button type=\"submit\" class=\"btn btn-sm btn-outline-secondary\"><i \
     class=\"fa fa-magnifying-glass\"></i></button>\n"
    (esc (Utf8.capitalize_fst (transl conf "starting with")))
    (esc (Option.value ~default:"" bw));
  if bw <> None then
    Output.print_sstring conf
      (button "<i class=\"fa fa-xmark\"></i>" (url ~bw:None 1));
  match counts with
  | None ->
      Output.print_sstring conf
        (button
           (esc (Utf8.capitalize_fst (transl conf "all")))
           (Printf.sprintf "%s&all=on&n=%d" href n));
      Output.print_sstring conf "</form>\n"
  | Some (total, (from, upto)) ->
      Output.printf conf
        "<select class=\"form-select form-select-sm w-auto ms-3 auto-submit\" \
         name=\"n\" aria-label=\"%s\" title=\"%s\">\n"
        (esc (Utf8.capitalize_fst (transl conf "rows per page")))
        (esc (Utf8.capitalize_fst (transl conf "rows per page")));
      List.iter
        (fun k ->
          Output.printf conf "<option value=\"%d\"%s>%d</option>\n" k
            (if k = n then " selected" else "")
            k)
        (List.sort_uniq compare (n :: page_sizes));
      Output.print_sstring conf "</select>\n";
      if from > 1 then
        Output.print_sstring conf
          (button
             (esc (Utf8.capitalize_fst (transl conf "previous")))
             (url (max 1 (from - n))));
      Output.printf conf "<span class=\"text-nowrap\">%s</span>\n"
        (if total = 0 then "0" else Printf.sprintf "%d–%d / %d" from upto total);
      if upto < total then
        Output.print_sstring conf
          (button
             (esc (Utf8.capitalize_fst (transl conf "next")))
             (url (upto + 1)));
      Output.print_sstring conf "</form>\n"

(* The clusters are many on a large base (Roglo), and long to compute: the
   page opens empty, the clusters are computed once a beginning ([bw]) is
   given or all of them asked ([all=on]), and only a slice is sent. *)
let print_clusters conf base =
  let show_all = p_getenv conf.env "all" = Some "on" in
  let href =
    prefix conf ^ "m=PLACE_TERMS" ^ if show_all then "&all=on" else ""
  in
  let hidden () =
    Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS");
    if show_all then Util.hidden_input conf "all" (Adef.encoded "on")
  in
  if begins_with conf = None && not show_all then
    print_list_bar conf ~href ~hidden None
  else
    let ok = begins_ok conf in
    let all =
      PlaceTerms.clusters (PlaceTerms.build_summary conf base)
      |> List.filter (fun (_, (l : PlaceTerms.entry list)) ->
          match l with e :: _ -> ok e.term | [] -> false)
    in
    let total = List.length all in
    let sl = slice conf total in
    print_list_bar conf ~href ~hidden (Some (total, sl));
    let clusters = take sl all in
    if clusters <> [] then (
      Output.printf conf
        "<table class=\"table table-sm table-hover w-auto\">\n\
         <thead><tr><th>%s</th><th class=\"text-end\" title=\"%s\">%s</th><th \
         class=\"text-end\">%s</th>%s</tr></thead>\n\
         <tbody>\n"
        (Utf8.capitalize_fst (transl conf "spellings"))
        (esc (Utf8.capitalize_fst (transl_nth conf "full place/full places" 1)))
        (Utf8.capitalize_fst (transl_nth conf "place/places" 1))
        (Utf8.capitalize_fst (transl conf "uses"))
        (merge_th conf);
      List.iter
        (fun (key, (l : PlaceTerms.entry list)) ->
          let sum f = List.fold_left (fun n e -> n + f e) 0 l in
          Output.printf conf
            "<tr><td>%s</td><td class=\"text-end\">%d</td><td \
             class=\"text-end\">%d</td><td class=\"ps-4\">%s</td></tr>\n"
            (String.concat ", "
               (List.map
                  (fun (e : PlaceTerms.entry) ->
                    Printf.sprintf "%s <span class=\"text-muted\">(%d)</span>"
                      (term_link conf e.term) e.uses)
                  l))
            (sum (fun (e : PlaceTerms.entry) -> e.nb_places))
            (sum (fun (e : PlaceTerms.entry) -> e.uses))
            (ok_button (cluster_link conf key)))
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
      List.iteri
        (fun i (e : PlaceTerms.entry) ->
          Output.printf conf
            "<div class=\"form-check\"><input class=\"form-check-input\" \
             type=\"radio\" name=\"ntc\" id=\"ntc%d\" value=\"%s\"%s><label \
             class=\"form-check-label\" for=\"ntc%d\"><code>%s</code> <span \
             class=\"text-muted\">(%d)</span></label></div>\n"
            i (esc e.term)
            (if i = 0 then " checked" else "")
            i (vis conf e.term) e.uses)
        l;
      Output.printf conf
        "<div class=\"form-check d-flex align-items-center gap-2\"><input \
         class=\"form-check-input\" type=\"radio\" name=\"ntc\" id=\"ntco\" \
         value=\"%s\"><label class=\"form-check-label\" \
         for=\"ntco\">%s</label> <input type=\"text\" class=\"form-control \
         form-control-sm w-auto\" size=\"30\" name=\"nto\" \
         data-check=\"ntco\"></div>\n\
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
                class=\"form-check-input ctx-all me-2\" data-ctx=\"t%d\" \
                checked>%s <span class=\"text-muted small\">%s</span></h3>\n"
               i (term_link conf e.term) (counts_html conf e);
             ( i + 1,
               print_contexts conf e ~first ~cls:(Printf.sprintf " ctxt%d" i) ))
           (0, 0) l
          : int * int);
      Output.print_sstring conf "</form>\n"

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

(* the places, the sources and the occupations have their own pages, with
   the same layout *)
type section = Places | Sources | Occupations

let noun section =
  match section with
  | Places -> "place"
  | Sources -> "source"
  | Occupations -> "occupation"

let kind_html ?(section = Places) conf base (k : PlaceRename.kind) =
  let badge cls txt =
    Printf.sprintf "<span class=\"badge %s\">%s</span>" cls
      (esc (Utf8.capitalize_fst txt))
  in
  let n = noun section in
  match k with
  | New_place -> badge "bg-success" (transl conf ("new " ^ n))
  | Merge_existing j ->
      badge "bg-warning text-dark"
        (transl conf ("merged into an existing " ^ n))
      ^ " "
      ^ esc (Driver.sou base j)
  | Merge_batch ->
      badge "bg-warning text-dark"
        (transl conf ("merged with another renamed " ^ n))

(* back to the term page, or to the cluster page for several terms *)
let back_href conf terms =
  match terms with
  | [ t ] -> Printf.sprintf "%sm=PLACE_TERMS&t=%s" (prefix conf) (enc t)
  | t :: _ -> cluster_link conf (PlaceTerms.cluster_key t)
  | [] -> Printf.sprintf "%sm=PLACE_TERMS" (prefix conf)

(* the terms separated by a bare "/": no space added, so that the spaces and
   invisible characters shown are those of the data *)
let terms_html conf terms =
  String.concat "<span class=\"text-muted\">/</span>"
    (List.map (fun t -> "<code>" ^ vis conf t ^ "</code>") terms)

let invalid_new_term conf terms nt =
  Output.printf conf
    "<div class=\"alert alert-danger\">%s: <code>%s</code></div>\n\
     <p><a href=\"%s\">%s</a></p>\n"
    (Utf8.capitalize_fst (transl conf "invalid new term"))
    (esc nt) (back_href conf terms) (terms_html conf terms)

(* the field of the new place of an item, editable on the preview *)
let new_key_of istr = "np" ^ Driver.Istr.to_string istr
let new_key (it : PlaceRename.item) = new_key_of it.istr
let line_shown conf it = p_getenv_notrim conf.env (new_key it) <> None

(* A preview shows its first lines only, and the confirmation changes the
   lines shown only, then leads to the preview of the next ones. *)
let preview_max = 200

(* [dn]: the lines already confirmed on the previews before this one, to
   number the lines shown ("201–400 / 1234") *)
let done_lines conf =
  match p_getint conf.env "dn" with
  | Some n when n > 0 -> n
  | Some _ | None -> 0

let hidden_done conf dn =
  if dn > 0 then Util.hidden_input conf "dn" (Adef.encoded (string_of_int dn))

(* "201–400 / 1234 ", or nothing when everything is shown at once *)
let lines_range dn total =
  if total > preview_max || dn > 0 then
    Printf.sprintf "%d–%d / " (dn + 1) (dn + min preview_max total)
  else ""

(* [items] as confirmed on the preview. With [shown_only], the items whose
   line was not in the form are left out: the preview of sources and
   occupations shows the first ones only. An edited item gets its new text,
   or is left out when edited back to its old text; the kinds are then
   computed again. [Error l]: the edited texts refused by [valid]. *)
let edited_items ?(valid = fun _ -> true) ?(shown_only = false) conf base items
    =
  let edited = ref false and invalid = ref [] in
  let items =
    List.filter_map
      (fun (it : PlaceRename.item) ->
        match p_getenv_notrim conf.env (new_key it) with
        | None -> if shown_only then None else Some it
        | Some s ->
            let s = Mutil.normalize_utf_8 (PlaceTerms.clean_term s) in
            if s = "" || s = it.new_s then Some it
            else if not (valid s) then (
              invalid := s :: !invalid;
              None)
            else (
              edited := true;
              if s = it.old_s then None
              else Some { it with new_s = s; kind = PlaceRename.New_place }))
      items
  in
  match List.rev !invalid with
  | [] -> Ok (if !edited then PlaceRename.classify base items else items)
  | l -> Error l

(* The preview of a plan: one checkbox per place, all checked; only the
   checked places are changed by [m=PLACE_TERMS_OK]. The new place of each
   item can be edited ([new_key]). [hidden] emits the fields describing the
   change, [old_html] renders the old place of an item. *)
let print_plan conf base u items ~hidden ~back ~old_html =
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS_OK");
  hidden ();
  let dn = done_lines conf and total = List.length items in
  hidden_done conf dn;
  (* the occurrences of every line, shown or not: those of the lines not
     shown are passed on to the next preview *)
  List.iter
    (fun (it : PlaceRename.item) ->
      List.iter
        (fun d ->
          Util.hidden_input conf
            (PlaceRename.occurrence_key (it.istr, d))
            (Adef.encoded "on"))
        it.depths)
    items;
  Output.printf conf "<p>%s%d %s</p>\n" (lines_range dn total) (dn + total)
    (transl_nth conf "full place/full places" (if dn + total > 1 then 1 else 0));
  Output.print_sstring conf
    "<table class=\"table table-sm\">\n\
     <thead><tr><th><input type=\"checkbox\" class=\"form-check-input \
     ctx-all\" data-ctx=\"0\" checked></th><th colspan=\"4\"></th></tr></thead>\n\
     <tbody>\n";
  List.iteri
    (fun i (it : PlaceRename.item) ->
      if i < preview_max then
        Output.printf conf
          "<tr><td><input type=\"checkbox\" class=\"form-check-input ctx0\" \
           name=\"%s\" value=\"on\" \
           checked></td><td>%s</td><td>→</td><td>%s</td><td class=\"text-end \
           text-muted\">%d</td><td>%s</td></tr>\n"
          (PlaceRename.place_key it.istr)
          (old_html it)
          (Printf.sprintf
             "<input type=\"text\" class=\"form-control form-control-sm\" \
              name=\"%s\" value=\"%s\" size=\"%d\">"
             (new_key it) (esc it.new_s)
             (max 20 (min 80 (Utf8.length it.new_s + 2))))
          (PlaceRename.uses u it.istr)
          (kind_html conf base it.kind))
    items;
  Output.print_sstring conf "</tbody></table>\n";
  Output.printf conf
    "<button type=\"submit\" class=\"btn btn-primary\">%s</button> <a \
     class=\"btn btn-outline-secondary\" href=\"%s\">%s</a>\n\
     </form>\n"
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0))
    back
    (Utf8.capitalize_fst (transl conf "cancel"))

(* The proposed correction, editable: [name] is submitted with the value of
   the field, [hidden] emits the other fields of the change, to compute the
   preview again. *)
let print_edit_form ?(m = "PLACE_TERMS") conf ~lhs ~name ~value ~hidden =
  Output.printf conf
    "<form method=\"post\" action=\"%s\" class=\"d-flex flex-wrap \
     align-items-center gap-2 my-3\">\n"
    conf.command;
  Util.hidden_env conf;
  Util.hidden_input conf "m" (Adef.encoded m);
  hidden ();
  Output.printf conf
    "<span>%s</span><label for=\"%s\">→</label>\n\
     <input type=\"text\" class=\"form-control w-auto\" size=\"50\" id=\"%s\" \
     name=\"%s\" value=\"%s\" required>\n\
     <button type=\"submit\" class=\"btn btn-outline-primary\">%s</button>\n\
     </form>\n"
    lhs name name name (esc value)
    (Utf8.capitalize_fst (transl conf "preview of the changes"))

let print_preview conf base terms nt =
  let nt = PlaceTerms.clean_term nt in
  (* the new term can be changed here, the same occurrences kept *)
  print_edit_form conf ~lhs:(terms_html conf terms) ~name:"nt" ~value:nt
    ~hidden:(fun () ->
      hidden_done conf (done_lines conf);
      List.iteri
        (fun i t -> Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) t)
        terms;
      List.iter
        (fun o ->
          Util.hidden_input conf
            (PlaceRename.occurrence_key o)
            (Adef.encoded "on"))
        (PlaceRename.occurrences_of_env conf));
  if not (PlaceRename.valid_new_term nt) then invalid_new_term conf terms nt
  else
    let u = PlaceRename.usage base in
    let items =
      PlaceRename.plan conf base ~terms ~new_term:nt
        (PlaceRename.occurrences_of_env conf)
    in
    if items = [] then
      Output.printf conf "<p>%s</p>\n<p><a href=\"%s\">%s</a></p>\n"
        (Utf8.capitalize_fst (transl conf "no modification"))
        (back_href conf terms) (terms_html conf terms)
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

(* ---- places to complete ---- *)

let groups_href conf = Printf.sprintf "%sm=PLACE_TERMS&groups=on" (prefix conf)

let group_href conf g =
  Printf.sprintf "%sm=PLACE_TERMS&g=%s" (prefix conf)
    (Driver.Istr.to_string (PlaceGroups.key g))

let place_html conf (p : PlaceTerms.place) =
  Printf.sprintf "%s <span class=\"text-muted\">(%d)</span>" (vis conf p.raw)
    p.uses

let print_groups conf groups =
  let ok = begins_ok conf in
  let all =
    List.filter
      (fun (g : PlaceGroups.group) ->
        match g.targets with p :: _ -> ok p.raw | [] -> false)
      groups
  in
  let total = List.length all in
  let sl = slice conf total in
  print_list_bar conf ~href:(groups_href conf)
    ~hidden:(fun () ->
      Util.hidden_input conf "m" (Adef.encoded "PLACE_TERMS");
      Util.hidden_input conf "groups" (Adef.encoded "on"))
    (Some (total, sl));
  let groups = take sl all in
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
          (String.concat "<br>" (List.map (place_html conf) g.targets))
          (String.concat "<br>" (List.map (place_html conf) g.members))
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
            i (place_html conf p))
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
            (place_html conf p)
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
        (Utf8.capitalize_fst (transl conf "preview of the changes"))

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
            (* [gn]: the complete place as edited on the preview *)
            let new_s =
              match p_getenv_notrim conf.env "gn" with
              | Some s when PlaceTerms.clean_term s <> "" ->
                  Some (PlaceTerms.clean_term s)
              | Some _ | None -> None
            in
            let shown = Option.value ~default:(Driver.sou base target) new_s in
            Some (shown, PlaceRename.plan_places ?new_s base ~target places))
  | _ -> None

let print_group_preview conf base =
  match group_items conf base with
  | None ->
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "not found"))
  | Some (shown, items) ->
      (* the complete place can be changed here ("aaaa (bbbb)" for "aaaa,
         bbbb"), the same places kept *)
      let hidden_group () =
        List.iter
          (fun k ->
            Util.hidden_input conf k
              (Option.value ~default:(Adef.encoded "")
                 (List.assoc_opt k conf.env)))
          [ "g"; "gt" ]
      in
      print_edit_form conf ~lhs:"" ~name:"gn" ~value:shown ~hidden:(fun () ->
          hidden_group ();
          List.iter
            (fun i ->
              Util.hidden_input conf (PlaceRename.place_key i)
                (Adef.encoded "on"))
            (PlaceRename.places_of_env conf));
      if items = [] then
        Output.printf conf "<p>%s</p>\n"
          (Utf8.capitalize_fst (transl conf "no modification"))
      else
        print_plan conf base (PlaceRename.usage base) items
          ~hidden:(fun () ->
            hidden_group ();
            Util.hidden_input_s conf "gn" shown)
          ~back:(groups_href conf)
          ~old_html:(fun (it : PlaceRename.item) -> vis conf it.old_s)

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

(* The pages of the sources and of the occupations: their spellings are
   parts (spans) of the raw strings. [m] is the page, [href] the list of the
   clusters (with its fields [hidden]), [data] the strings loaded. *)
type spans_page = {
  section : section;
  m : string;
  href : string;
  hidden : unit -> unit;
  field : PlaceRename.field;
  plural : string;  (** "source/sources" *)
  title : string;  (** of the list of the clusters *)
}

(* As for the places: the page opens empty, the clusters [(key, spellings,
   total)] are computed once a beginning ([bw]) is given or all of them asked
   ([all=on]); only a slice is sent, filtered by the beginning of their first
   spelling ([shown]). *)
let print_spans_clusters conf pg ~shown ~clusters =
  let show_all = p_getenv conf.env "all" = Some "on" in
  let href = pg.href ^ if show_all then "&all=on" else "" in
  let hidden () =
    Util.hidden_input conf "m" (Adef.encoded pg.m);
    pg.hidden ();
    if show_all then Util.hidden_input conf "all" (Adef.encoded "on")
  in
  if begins_with conf = None && not show_all then
    print_list_bar conf ~href ~hidden None
  else
    let ok = begins_ok conf in
    let all =
      List.filter
        (fun (_, (spellings : SourceTerms.spelling list), _) ->
          match spellings with sp :: _ -> ok (shown sp.text) | [] -> false)
        (clusters ())
    in
    let total = List.length all in
    let sl = slice conf total in
    print_list_bar conf ~href ~hidden (Some (total, sl));
    let selected = take sl all in
    if selected <> [] then (
      Output.printf conf
        "<table class=\"table table-sm table-hover w-auto\">\n\
         <thead><tr><th>%s</th><th class=\"text-end\">%s</th>%s</tr></thead>\n\
         <tbody>\n"
        (Utf8.capitalize_fst (transl conf "spellings"))
        (Utf8.capitalize_fst (transl conf "uses"))
        (merge_th conf);
      List.iter
        (fun (key, spellings, total) ->
          Output.printf conf
            "<tr><td>%s</td><td class=\"text-end\">%d</td><td \
             class=\"ps-4\">%s</td></tr>\n"
            (String.concat "<br>" (List.map (spelling_html conf) spellings))
            total
            (ok_button (Printf.sprintf "%s&c=%s" pg.href (enc key))))
        selected;
      Output.print_sstring conf "</tbody></table>\n")

(* the spelling to keep and the spellings to replace by it, each with a few
   examples ([samples]) *)
let print_spellings_form conf pg (spellings : SourceTerms.spelling list) samples
    =
  open_form ~m:pg.m conf;
  pg.hidden ();
  Output.printf conf
    "<fieldset class=\"my-3\"><legend class=\"h5\">%s</legend>\n"
    (Utf8.capitalize_fst (transl conf "keep the spelling"));
  List.iteri
    (fun i (sp : SourceTerms.spelling) ->
      Output.printf conf
        "<div class=\"form-check\"><input class=\"form-check-input\" \
         type=\"radio\" name=\"ntc\" id=\"ntc%d\" value=\"%s\"%s><label \
         class=\"form-check-label\" for=\"ntc%d\">%s</label></div>\n"
        i (esc sp.text)
        (if i = 0 then " checked" else "")
        i (spelling_html conf sp))
    spellings;
  Output.printf conf
    "<div class=\"form-check d-flex align-items-center gap-2\"><input \
     class=\"form-check-input\" type=\"radio\" name=\"ntc\" id=\"ntco\" \
     value=\"%s\"><label class=\"form-check-label\" for=\"ntco\">%s</label> \
     <input type=\"text\" class=\"form-control form-control-sm w-auto\" \
     size=\"30\" name=\"nto\" data-check=\"ntco\"></div>\n\
     </fieldset>\n"
    (esc other_choice)
    (Utf8.capitalize_fst (transl conf "other spelling"));
  (* every spelling, with a few of its strings; the one kept is left as it
     is *)
  List.iteri
    (fun i (sp : SourceTerms.spelling) ->
      Output.printf conf
        "<h3 class=\"h6 mt-3\"><input type=\"checkbox\" \
         class=\"form-check-input me-2\" name=\"t%d\" value=\"%s\" \
         checked>%s</h3>\n\
         <ul class=\"small ms-3\">\n"
        (i + 1) (esc sp.text) (spelling_html conf sp);
      List.iter
        (fun (raw, spans) ->
          Output.printf conf "<li>%s</li>\n" (mark_spans conf raw spans))
        (Option.value ~default:[] (List.assoc_opt sp.text samples));
      Output.print_sstring conf "</ul>\n")
    spellings;
  Output.printf conf
    "<button type=\"submit\" class=\"btn btn-primary\">%s</button>\n</form>\n"
    (Utf8.capitalize_fst (transl conf "preview of the changes"))

(* The preview of a renaming of sources or of occupations. They are many:
   the first [preview_max] lines are shown, and the confirmation changes
   these lines only. [items] computes the plan. *)
let print_spans_preview conf base pg terms nt ~items =
  let nt = PlaceTerms.clean_term nt in
  let dn = done_lines conf in
  print_edit_form ~m:pg.m conf ~lhs:(terms_html conf terms) ~name:"nt" ~value:nt
    ~hidden:(fun () ->
      pg.hidden ();
      hidden_done conf dn;
      List.iteri
        (fun i t -> Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) t)
        terms);
  if nt = "" then
    Output.printf conf "<div class=\"alert alert-danger\">%s</div>\n"
      (Utf8.capitalize_fst (transl conf "invalid new term"))
  else
    let items : (PlaceRename.item * SourceTerms.span list) list = items nt in
    let u = PlaceRename.usage ~field:pg.field base in
    let total = List.length items in
    Output.printf conf "<p>%s%d %s</p>\n" (lines_range dn total) (dn + total)
      (transl_nth conf pg.plural (if dn + total > 1 then 1 else 0));
    if items = [] then
      Output.printf conf "<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "no modification"))
    else (
      Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
      Util.hidden_env conf;
      Util.hidden_input conf "m" (Adef.encoded (pg.m ^ "_OK"));
      pg.hidden ();
      hidden_done conf dn;
      List.iteri
        (fun i t -> Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) t)
        terms;
      Util.hidden_input_s conf "nt" nt;
      let buttons () =
        Output.printf conf
          "<p><button type=\"submit\" class=\"btn btn-primary\">%s</button> <a \
           class=\"btn btn-outline-secondary\" href=\"%s\">%s</a></p>\n"
          (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0))
          pg.href
          (Utf8.capitalize_fst (transl conf "cancel"))
      in
      buttons ();
      (* The first lines only are shown, and only the lines shown are changed
         by the confirmation, which then leads to the preview of the next
         ones. Their new string can be edited ([new_key]), as for the
         places. *)
      Output.print_sstring conf
        "<table class=\"table table-sm small mt-3\"><tbody>\n";
      List.iteri
        (fun i ((it : PlaceRename.item), spans) ->
          if i < preview_max then
            Output.printf conf
              "<tr><td>%s</td><td>→</td><td><input type=\"text\" \
               class=\"form-control form-control-sm\" name=\"%s\" value=\"%s\" \
               size=\"%d\"></td><td class=\"text-end \
               text-muted\">%d</td><td>%s</td></tr>\n"
              (mark_spans conf it.old_s spans)
              (new_key it) (esc it.new_s)
              (max 20 (min 80 (Utf8.length it.new_s + 2)))
              (PlaceRename.uses u it.istr)
              (kind_html ~section:pg.section conf base it.kind))
        items;
      Output.print_sstring conf "</tbody></table>\n";
      buttons ();
      Output.print_sstring conf "</form>\n")

(* ---- sources ---- *)

let src_page_of conf =
  let part = src_part conf in
  {
    section = Sources;
    m = "SOURCE_TERMS";
    href = src_href conf part;
    hidden =
      (fun () ->
        Util.hidden_input conf "part"
          (Adef.encoded (SourceTerms.string_of_part part)));
    field = PlaceRename.Source;
    plural = "source/sources";
    title = "sources to group";
  }

let print_src_clusters conf base =
  let part = src_part conf in
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
  (* the first spelling without its [[w:]] *)
  let shown t =
    let n = String.length t in
    if n > 6 && String.starts_with ~prefix:"[[w:" t then String.sub t 4 (n - 6)
    else t
  in
  print_spans_clusters conf (src_page_of conf) ~shown ~clusters:(fun () ->
      List.map
        (fun (c : SourceTerms.cluster) -> (c.ckey, c.spellings, c.total))
        (SourceTerms.clusters (SourceTerms.load conf base) part))

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
      print_spellings_form conf (src_page_of conf) c.spellings
        (SourceTerms.samples d part
           (List.map (fun (sp : SourceTerms.spelling) -> sp.text) c.spellings)
           5)

let src_items conf base terms nt =
  let d = SourceTerms.load conf base in
  SourceTerms.plan base d (src_part conf) ~terms ~new_term:nt

(* ---- occupations ---- *)

let occu_page_of conf =
  {
    section = Occupations;
    m = "OCCU_TERMS";
    href = prefix conf ^ "m=OCCU_TERMS";
    hidden = (fun () -> ());
    field = PlaceRename.Occupation;
    plural = "occupation/occupations";
    title = "occupations to group";
  }

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
      print_spellings_form conf (occu_page_of conf) c.spellings
        (OccuTerms.samples d
           (List.map (fun (sp : SourceTerms.spelling) -> sp.text) c.spellings)
           5)

let occu_items base terms nt =
  OccuTerms.plan base (OccuTerms.load base) ~terms ~new_term:nt

(* the title of a page and the function printing its body *)
let page conf title f = (Utf8.capitalize_fst (transl conf title), f)

let nonempty conf k =
  match p_getenv_notrim conf.env k with
  | Some s when s <> "" -> Some s
  | Some _ | None -> None

(* the title and the body of a page of sources or occupations *)
let spans_page conf base pg ~clusters ~cluster ~items =
  match (PlaceRename.terms_of_env conf, new_term_of_env conf) with
  | (_ :: _ as terms), Some nt ->
      page conf "preview of the changes" (fun () ->
          print_spans_preview conf base pg terms nt ~items:(items terms))
  | _ -> (
      match nonempty conf "c" with
      | Some key -> page conf "merge spellings" (fun () -> cluster key)
      | None -> page conf pg.title clusters)

let src_page conf base =
  spans_page conf base (src_page_of conf)
    ~clusters:(fun () -> print_src_clusters conf base)
    ~cluster:(print_src_cluster conf base)
    ~items:(src_items conf base)

let occu_page conf base =
  spans_page conf base (occu_page_of conf)
    ~clusters:(fun () ->
      print_spans_clusters conf (occu_page_of conf) ~shown:Fun.id
        ~clusters:(fun () ->
          List.map
            (fun (c : OccuTerms.cluster) -> (c.ckey, c.spellings, c.total))
            (OccuTerms.clusters (OccuTerms.load base))))
    ~cluster:(print_occu_cluster conf base)
    ~items:(occu_items base)

(* the pages of a section start with the same links: the dictionary, then
   the pages of grouping *)
let header ?(section = Places) ~title conf =
  Hutil.header conf (fun _ -> Output.print_sstring conf title);
  let book data plural =
    Printf.sprintf
      "<a href=\"%sm=MOD_DATA&data=%s\"><i class=\"fa fa-%s me-1\"></i>%s</a>"
      (prefix conf) data
      (match data with
      | "src" -> "box-archive"
      | "occu" -> "briefcase"
      | _ -> "location-dot")
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
             book "src" "source/sources";
             link
               (prefix conf ^ "m=SOURCE_TERMS")
               "object-group" "sources to group";
           ]
       | Occupations ->
           [
             book "occu" "occupation/occupations";
             link
               (prefix conf ^ "m=OCCU_TERMS")
               "object-group" "occupations to group";
           ]
       | Places ->
           [
             book "place" "place/places";
             link
               (prefix conf ^ "m=PLACE_TERMS")
               "object-group" "spelling clusters";
             link (groups_href conf) "up-right-and-down-left-from-center"
               "places to complete";
           ]))

let place_page conf base =
  let nonempty = nonempty conf in
  match (PlaceRename.terms_of_env conf, new_term_of_env conf) with
  | (_ :: _ as terms), Some nt ->
      page conf "preview of the changes" (fun () ->
          print_preview conf base terms nt)
  | _ when p_getenv conf.env "gt" <> None ->
      page conf "preview of the changes" (fun () ->
          print_group_preview conf base)
  | _ when nonempty "g" <> None ->
      page conf "complete the places" (fun () ->
          print_group conf
            (PlaceGroups.build conf base)
            (Driver.Istr.of_string (Option.get (nonempty "g"))))
  | _ when p_getenv conf.env "groups" = Some "on" ->
      page conf "places to complete" (fun () ->
          print_groups conf (PlaceGroups.build conf base))
  | _ -> (
      (* the clusters of spellings by default; the index of the terms
         ([s=]) is not linked any more *)
      match (nonempty "c", nonempty "t", p_getenv_notrim conf.env "s") with
      (* only the terms of the cluster shown are detailed; the list of the
         clusters needs only their numbers *)
      | Some key, _, _ ->
          page conf "merge spellings" (fun () ->
              print_cluster conf
                (PlaceTerms.build ~only:(PlaceTerms.in_cluster key) conf base)
                key)
      | None, Some t, _ ->
          ( Utf8.capitalize_fst (transl_nth conf "place term/place terms" 0),
            fun () ->
              print_term conf
                (PlaceTerms.build
                   ~only:(PlaceTerms.in_cluster (PlaceTerms.cluster_key t))
                   conf base)
                t )
      | None, None, Some s ->
          ( Utf8.capitalize_fst (transl_nth conf "place term/place terms" 1),
            fun () -> print_list conf (PlaceTerms.build conf base) s )
      | None, None, None ->
          page conf "spelling clusters" (fun () -> print_clusters conf base))

let print conf base =
  let title, body = place_page conf base in
  header ~title conf;
  body ();
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
         k = "nt" || k = "t" || k = "g" || k = "gt" || k = "gn" || k = "part"
         || k = "dn"
         || PlaceRename.occurrence_of_key k <> None
         || String.starts_with ~prefix:"np" k
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

let print_result ?(section = Places) conf (r : PlaceRename.result) =
  let records = r.persons + r.families in
  Output.printf conf "<div class=\"alert alert-success\">%s: %s</div>\n"
    (Utf8.capitalize_fst (transl conf "modification successful"))
    (match section with
    | Sources ->
        Printf.sprintf
          (ftransl conf "%d sources renamed (%d merged), %d records updated")
          r.places r.merged records
    | Occupations ->
        Printf.sprintf
          (ftransl conf "%d occupations renamed (%d merged), %d records updated")
          r.places r.merged records
    | Places ->
        Printf.sprintf
          (ftransl conf "%d places renamed (%d merged), %d records updated")
          r.places r.merged records);
  if not r.complete then
    continue_form conf
      ~m:
        (match section with
        | Sources -> "SOURCE_TERMS_OK"
        | Occupations -> "OCCU_TERMS_OK"
        | Places -> "PLACE_TERMS_OK");
  if List.assoc_opt "history" conf.base_env = Some "yes" then
    Output.printf conf "<p><a href=\"%sm=HIST&k=20\">%s</a></p>\n" (prefix conf)
      (Utf8.capitalize_fst (transl_nth conf "modification/modifications" 1))

(* Applies the confirmed [items] and prints the result, or the edited lines
   refused by [valid], nothing being changed then. *)
let apply_confirmed ?(section = Places) ?(field = PlaceRename.Place) ?valid
    ?shown_only conf base items =
  match edited_items ?valid ?shown_only conf base items with
  | Ok items ->
      print_result ~section conf (PlaceRename.apply ~field conf base items)
  | Error l ->
      Output.printf conf
        "<div class=\"alert alert-danger\">%s%s %s</div>\n<p>%s</p>\n"
        (Utf8.capitalize_fst (transl conf "invalid new term"))
        (transl conf ":") (terms_html conf l)
        (Utf8.capitalize_fst (transl conf "no modification"))

let print_ok conf base =
  header
    ~title:(Utf8.capitalize_fst (transl conf "modification successful"))
    conf;
  (match (PlaceRename.terms_of_env conf, p_getenv_notrim conf.env "nt") with
  | _ when p_getenv conf.env "gt" <> None -> (
      (* recomputed: the base may have changed since the preview *)
      match group_items conf base with
      | None ->
          Output.printf conf "<p>%s</p>\n"
            (Utf8.capitalize_fst (transl conf "not found"))
      | Some (_, items) ->
          apply_confirmed ~valid:PlaceRename.valid_place conf base items;
          (* back to the group, for the places its preview did not show *)
          Output.printf conf
            "<p><a href=\"%sm=PLACE_TERMS&g=%s\">%s</a> · <a \
             href=\"%s\">%s</a></p>\n"
            (prefix conf)
            (enc (Option.value ~default:"" (p_getenv conf.env "g")))
            (Utf8.capitalize_fst (transl conf "continue correcting"))
            (groups_href conf)
            (Utf8.capitalize_fst (transl conf "places to complete")))
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
        in
        apply_confirmed ~valid:PlaceRename.valid_place conf base items;
        (* the occurrences of the lines the preview did not show go to the
           next preview *)
        let shown (istr, _) =
          p_getenv_notrim conf.env (new_key_of istr) <> None
        in
        let now, later =
          List.partition shown (PlaceRename.occurrences_of_env conf)
        in
        if later <> [] then (
          open_form conf;
          List.iteri
            (fun i t ->
              Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) t)
            terms;
          Util.hidden_input_s conf "nt" nt;
          hidden_done conf
            (done_lines conf
            + List.length (List.sort_uniq compare (List.map fst now)));
          List.iter
            (fun o ->
              Util.hidden_input conf
                (PlaceRename.occurrence_key o)
                (Adef.encoded "on"))
            later;
          Output.printf conf
            "<p>%s <button type=\"submit\" class=\"btn \
             btn-primary\">%s</button></p>\n\
             </form>\n"
            (Utf8.capitalize_fst (transl conf "continue correcting"))
            (Utf8.capitalize_fst (transl conf "preview of the changes")));
        Output.printf conf "<p><a href=\"%sm=PLACE_TERMS&t=%s\">%s</a></p>\n"
          (prefix conf) (enc nt) (esc nt)
  | _ -> ());
  Hutil.trailer conf

let print_sources conf base =
  let title, body = src_page conf base in
  header ~section:Sources ~title conf;
  body ();
  Hutil.trailer conf

(* the confirmed renaming of sources or occupations, recomputed: the base may
   have changed since the preview *)
let print_spans_ok conf base pg ~items =
  header ~section:pg.section
    ~title:(Utf8.capitalize_fst (transl conf "modification successful"))
    conf;
  (match (PlaceRename.terms_of_env conf, p_getenv_notrim conf.env "nt") with
  | (_ :: _ as terms), Some nt ->
      let nt = PlaceTerms.clean_term nt in
      if nt = "" then
        Output.printf conf "<div class=\"alert alert-danger\">%s</div>\n"
          (Utf8.capitalize_fst (transl conf "invalid new term"))
      else
        let items = List.map fst (items terms nt) in
        (* only the lines shown on the preview; the next ones are previewed
           in their turn *)
        apply_confirmed ~section:pg.section ~field:pg.field ~shown_only:true
          conf base items;
        if List.exists (fun it -> not (line_shown conf it)) items then (
          open_form ~m:pg.m conf;
          pg.hidden ();
          hidden_done conf
            (done_lines conf + List.length (List.filter (line_shown conf) items));
          List.iteri
            (fun i t ->
              Util.hidden_input_s conf (Printf.sprintf "t%d" (i + 1)) t)
            terms;
          Util.hidden_input_s conf "nt" nt;
          Output.printf conf
            "<p>%s <button type=\"submit\" class=\"btn \
             btn-primary\">%s</button></p>\n\
             </form>\n"
            (Utf8.capitalize_fst (transl conf "continue correcting"))
            (Utf8.capitalize_fst (transl conf "preview of the changes")));
        Output.printf conf "<p><a href=\"%s\">%s</a></p>\n" pg.href
          (Utf8.capitalize_fst (transl conf pg.title))
  | _ -> ());
  Hutil.trailer conf

let print_sources_ok conf base =
  print_spans_ok conf base (src_page_of conf) ~items:(src_items conf base)

let print_occupations conf base =
  let title, body = occu_page conf base in
  header ~section:Occupations ~title conf;
  body ();
  Hutil.trailer conf

let print_occupations_ok conf base =
  print_spans_ok conf base (occu_page_of conf) ~items:(occu_items base)
