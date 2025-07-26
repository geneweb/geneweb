(* lib/relationMatrix.ml *)
open Config
module Driver = Geneweb_db.Driver
module Sosa = Geneweb_sosa

type ancestor_item =
  [ `Couple of Driver.iper * int * Driver.iper * int * Driver.ifam
  | `Unique of Driver.iper * int ]

type kinship_degree = {
  len1 : int; (* degré ascendant  *)
  len2 : int; (* degré descendant *)
  sols : (Driver.iper * int) list; (* (iper, #chemins)    *)
  ancestors : (Driver.iper * Driver.ifam) list; (* iper + contexte ifam *)
}

let compute_relationship_cell conf base tstab p1 p2 :
    string option * kinship_degree option * Sosa.t * float =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  match Relation.compute_simple_relationship conf base tstab ip1 ip2 with
  | None -> (None, None, Sosa.zero, 0.0)
  | Some (rl, total, relationship, _reltab, ancestors_with_ifam) -> (
      match rl with
      | [] -> (None, None, total, relationship)
      | (len1, len2, sols_person) :: _ ->
          let sols_iper =
            List.map (fun (p, cnt) -> (Driver.get_iper p, cnt)) sols_person
          in
          let anc_filtered =
            List.filter
              (fun (iper, _) -> List.exists (fun (j, _) -> j = iper) sols_iper)
              ancestors_with_ifam
          in
          let kd = { len1; len2; sols = sols_iper; ancestors = anc_filtered } in
          let notation =
            if len1 = 0 && len2 = 0 then "="
            else Printf.sprintf "%d/%d" len1 len2
          in
          (Some notation, Some kd, total, relationship))

let get_person_simple_name base person =
  let first_name = Driver.sou base (Driver.get_first_name person) in
  let surname = Driver.sou base (Driver.get_surname person) in
  Printf.sprintf "%s %s" first_name surname

let group_ancestors_by_couples
    (ancestors_with_ifam : (Driver.iper * Driver.ifam) list)
    (sols : (Driver.iper * int) list) : ancestor_item list =
  let count_tbl = Hashtbl.create 16 in
  List.iter (fun (iper, cnt) -> Hashtbl.add count_tbl iper cnt) sols;
  let fam_tbl = Hashtbl.create 16 in
  List.iter
    (fun (iper, ifam) ->
      let lst = try Hashtbl.find fam_tbl ifam with Not_found -> [] in
      Hashtbl.replace fam_tbl ifam (iper :: lst))
    ancestors_with_ifam;
  let couple_map = Hashtbl.create 16 in
  Hashtbl.iter
    (fun ifam members ->
      match members with
      | [ a; b ] when a <> b -> Hashtbl.add couple_map ifam (a, b)
      | _ -> ())
    fam_tbl;
  let used = Hashtbl.create 16 in
  let items = ref [] in
  List.iter
    (fun (iper, ifam) ->
      if not (Hashtbl.mem used iper) then (
        if Hashtbl.mem couple_map ifam then
          let a, b = Hashtbl.find couple_map ifam in
          if (not (Hashtbl.mem used a)) && not (Hashtbl.mem used b) then (
            let ca = try Hashtbl.find count_tbl a with _ -> 1 in
            let cb = try Hashtbl.find count_tbl b with _ -> 1 in
            items := `Couple (a, ca, b, cb, ifam) :: !items;
            Hashtbl.add used a ();
            Hashtbl.add used b ())
          else ()
        else
          let c = try Hashtbl.find count_tbl iper with _ -> 1 in
          items := `Unique (iper, c) :: !items;
          Hashtbl.add used iper ()))
    ancestors_with_ifam;
  List.rev !items

let format_ancestors_with_couples conf base
    (ancestors_with_ifam : (Driver.iper * Driver.ifam) list)
    (sols : (Driver.iper * int) list) : string =
  let items = group_ancestors_by_couples ancestors_with_ifam sols in
  let fmt = function
    | `Couple (ip1, c1, ip2, c2, _) ->
        let p1 = Driver.poi base ip1 in
        let p2 = Driver.poi base ip2 in
        Printf.sprintf "%s [%d] %s %s [%d]"
          (get_person_simple_name base p1)
          c1
          (Util.transl conf "and" :> string)
          (get_person_simple_name base p2)
          c2
    | `Unique (iper, cnt) ->
        let p = Driver.poi base iper in
        Printf.sprintf "%s [%d]" (get_person_simple_name base p) cnt
  in
  String.concat ", " (List.map fmt items)

let make_tooltip_text conf base p1 p2 (kd_opt : kinship_degree option)
    (total_sosa : Sosa.t) : string =
  let colon = Util.transl conf ":" in
  let p1_name = get_person_simple_name base p1 in
  let p2_name = get_person_simple_name base p2 in
  let total_links =
    if Sosa.eq total_sosa Sosa.zero then 0
    else
      match int_of_string (Sosa.to_string total_sosa) with
      | n when n >= 0 -> n
      | _ -> 0
  in
  let link_word =
    Util.transl_nth conf "relationship link/relationship links"
      (if total_links = 1 then 0 else 1)
  in
  let link_between =
    Printf.sprintf
      (Util.ftransl conf "%d %s links between %s and %s")
      total_links link_word p1_name p2_name
  in
  let header = Printf.sprintf "<b>%s</b><br><br>" link_between in
  let body =
    match kd_opt with
    | None -> ""
    | Some kd ->
        let shortest =
          Util.transl conf "shortest path" |> Utf8.capitalize_fst
        in
        let kin_label =
          Util.transl_nth conf "degree of kinship"
            (if kd.len1 + kd.len2 = 1 then 0 else 1)
        in
        let l1 =
          Printf.sprintf "<b>%s%s</b> %d %s<br>" shortest colon
            (kd.len1 + kd.len2) kin_label
        in
        let asc_idx = if kd.len1 = 1 then 0 else 2 in
        let desc_idx = if kd.len2 = 1 then 1 else 3 in
        let asc_label =
          Util.transl_nth conf "ascending/descending (degree)" asc_idx
        in
        let desc_label =
          Util.transl_nth conf "ascending/descending (degree)" desc_idx
        in
        let asc_icon =
          "<i class=\"fa-solid fa-person-arrow-up-from-line mr-1\"></i>"
        in
        let desc_icon =
          "<i class=\"fa-solid fa-person-arrow-down-to-line fa-flip-horizontal \
           mr-1\"></i>"
        in
        let l2 =
          Printf.sprintf "%s%d %s<br>%s%d %s<br>" asc_icon kd.len1 asc_label
            desc_icon kd.len2 desc_label
        in
        let anc_html =
          format_ancestors_with_couples conf base kd.ancestors kd.sols
        in
        let anc_label =
          Util.transl_nth conf "first common ancestor"
            (if List.length kd.ancestors = 1 then 0 else 1)
          |> Utf8.capitalize_fst
        in
        let l3 =
          Printf.sprintf "<b>%s%s</b><br>%s" anc_label colon anc_html
        in
        l1 ^ l2 ^ l3
  in
  String.map (function '"' -> '\'' | c -> c) (header ^ body)

let encoded_key name = (Mutil.encode (Name.lower name) :> string)

(* TODO: vérifier si access_by_key est voulu/déjà automatisé, pas d'équivalent dans le code m *)
let make_relationship_link conf base p1 p2 =
  let p1_accessible =
    Util.accessible_by_key conf base p1
      (Driver.p_first_name base p1)
      (Driver.p_surname base p1)
  in
  let p2_accessible =
    Util.accessible_by_key conf base p2
      (Driver.p_first_name base p2)
      (Driver.p_surname base p2)
  in
  if p1_accessible && p2_accessible then
    let p1_fname = encoded_key (Driver.p_first_name base p1) in
    let p1_sname = encoded_key (Driver.p_surname base p1) in
    let oc1 = Driver.get_occ p1 in
    let p2_fname = encoded_key (Driver.p_first_name base p2) in
    let p2_sname = encoded_key (Driver.p_surname base p2) in
    let oc2 = Driver.get_occ p2 in
    let oc_param = if oc1 = 0 then "" else "&oc=" ^ string_of_int oc1 in
    let oc2_param = if oc2 = 0 then "" else "&oc2=" ^ string_of_int oc2 in
    Printf.sprintf "%sm=R&p=%s&n=%s%s&p2=%s&n2=%s%s"
      (Util.commd conf :> string)
      p1_fname p1_sname oc_param p2_fname p2_sname oc2_param
  else
    Printf.sprintf "%sm=R&i=%s&i2=%s"
      (Util.commd conf :> string)
      (Driver.Iper.to_string (Driver.get_iper p1))
      (Driver.Iper.to_string (Driver.get_iper p2))

let print_matrix_table conf base persons =
  let n = Array.length persons in
  let tstab = Util.create_topological_sort conf base in
  let person_ipers = Array.map (fun p -> Driver.get_iper p) persons in
  let relation_counts = Array.make n 0 in
  Output.print_sstring conf
    {|
<div class="d-flex justify-content-center">
  <table class="table table-sm" id="rm-table">
    <tbody>|};
  Output.print_sstring conf {|<tr><th class="border-top-0"></th>|};
  for j = 0 to n - 1 do
    let iper_j = person_ipers.(j) in
    Output.printf conf
      {|<th class="text-center rm-thead" id="§%s_">
         <span class="badge badge-pill badge-primary">%d</span>
       </th>|}
      (Driver.Iper.to_string iper_j)
      (j + 1)
  done;
  Output.print_sstring conf {|</tr>|};
  for i = 0 to n - 1 do
    let person = persons.(i) in
    let iper_i = person_ipers.(i) in
    Output.print_sstring conf "<tr>";
    Output.printf conf
      {|
      <th class="rm-head" id="§%s_">
        <div class="d-flex align-items-center">
          <span class="badge badge-pill badge-primary ml-2 mr-3">%d</span>
          <div class="flex-fill text-left">
            <div><a href="%s">%s</a></div>
            <div class="small text-muted ml-3">%s</div>
          </div>
        </div>
      </th>|}
      (Driver.Iper.to_string iper_i)
      (i + 1)
      (Util.acces conf base person :> string)
      (Util.referenced_person_text conf base person :> string)
      (DateDisplay.short_dates_text conf base person :> string);
    for j = 0 to n - 1 do
      let iper_j = person_ipers.(j) in
      if i = j then
        (* Diagonale *)
        Output.printf conf
          {|<td class="text-center rm-diag" id="§%s_§%s">—</td>|}
          (Driver.Iper.to_string iper_i)
          (Driver.Iper.to_string iper_j)
      else if j > i then
        let notation_opt, kd_opt, total_sosa, consang_coeff =
          compute_relationship_cell conf base tstab persons.(i) persons.(j)
        in
        match notation_opt with
        | Some notation ->
            relation_counts.(i) <- relation_counts.(i) + 1;
            relation_counts.(j) <- relation_counts.(j) + 1;
            let tooltip_text =
              make_tooltip_text conf base persons.(i) persons.(j) kd_opt
                total_sosa
            in
            let links_count =
              if Sosa.eq total_sosa Sosa.zero then "0"
              else
                Sosa.to_string_sep
                  (Util.transl conf "(thousand separator)" :> string)
                  total_sosa
            in
            let cell_content =
              if Sosa.eq total_sosa Sosa.zero then notation
              else
                Printf.sprintf "<strong>%s</strong><br>%.2f%%<br>%s" links_count
                  (consang_coeff *. 100.0) notation
            in
            Output.printf conf
              {|<td class="text-center rm-cell" id="§%s_§%s"
               data-toggle="tooltip" data-html="true" data-placement="top" title="%s">
              <a href="%s" class="text-decoration-none">%s</a>
            </td>|}
              (Driver.Iper.to_string iper_i)
              (Driver.Iper.to_string iper_j)
              tooltip_text
              (make_relationship_link conf base persons.(i) persons.(j))
              cell_content
        | None ->
            Output.printf conf
              {|<td class="text-center rm-cell" id="§%s_§%s">—</td>|}
              (Driver.Iper.to_string iper_i)
              (Driver.Iper.to_string iper_j)
      else Output.print_sstring conf {|<td class="rm-empty"></td>|}
    done;
    Output.print_sstring conf "</tr>"
  done;
  Output.print_sstring conf {|</tbody></table></div>|};

  let max_relations = Array.fold_left max 0 relation_counts in
  let total_relations = Array.fold_left ( + ) 0 relation_counts / 2 in

  Output.print_sstring conf
    {|<div class="mt-3">
        <h6 class="text-muted">Statistiques des relations</h6>
        <div class="row">
          <div class="col-md-6">
            <small class="text-muted">Total des relations: <strong>|};
  Output.printf conf "%d" total_relations;
  Output.print_sstring conf
    {|</strong></small>
          </div>
          <div class="col-md-6">
            <small class="text-muted">Plus connecté: |};

  let max_person_idx = ref 0 in
  for i = 1 to n - 1 do
    if relation_counts.(i) > relation_counts.(!max_person_idx) then
      max_person_idx := i
  done;

  if max_relations > 0 then (
    let max_person = persons.(!max_person_idx) in
    Output.print_sstring conf "<strong>";
    Output.print_sstring conf
      (Util.person_title_text conf base max_person :> string);
    Output.printf conf "</strong> (%d)" relation_counts.(!max_person_idx))
  else Output.print_sstring conf "<em>Aucune</em>";

  Output.print_sstring conf
    {|</small>
          </div>
        </div>
      </div>|}

let print conf base =
  if Util.url_has_pnoc_params conf.env then
    let clean_url = Util.normalize_person_pool_url conf base "RM" None in
    Wserver.http_redirect_temporarily clean_url
  else
    let pl =
      let rec loop pl i =
        let k = string_of_int i in
        match Util.find_person_in_env conf base k with
        | Some p -> loop (p :: pl) (i + 1)
        | None -> List.rev pl
      in
      loop [] 1
    in
    let title _ =
      Util.transl_nth conf "relationship table" 2
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    in
    Hutil.header conf title;
    Util.print_loading_overlay conf ();
    let make_tp_link conf =
      let base_url = (Util.commd conf :> string) in
      let other_params =
        List.filter_map
          (fun (k, v) ->
            if k <> "m" then Some (k ^ "=" ^ Adef.as_string v) else None)
          conf.env
      in
      base_url ^ "m=TP&v=updRLM&" ^ String.concat "&" other_params
    in
    Output.printf conf
      {|<div class="d-flex justify-content-between align-items-center mb-3">
          <h2 class="h3">%s</h2>|}
      (Util.transl_nth conf "relationship table" 3 |> Utf8.capitalize_fst);
    Output.printf conf
      {|<a href="%s" class="btn btn-outline-primary">
          <i class="fas fa-edit mr-1"></i>%s</a></div>|}
      (make_tp_link conf)
      (Util.transl_nth conf "add/clear/show/edit the graph" 3
      |> Utf8.capitalize_fst);
    if List.length pl >= 2 then
      let persons = Array.of_list pl in
      print_matrix_table conf base persons
    else
      Output.print_sstring conf
        {|<div class="alert alert-warning">
            <i class="fas fa-exclamation-triangle mr-2"></i>
            Au moins 2 personnes sont requises pour la matrice des relations.
          </div>|};
    Output.print_sstring conf "</div>";
    Output.print_sstring conf
      {|<script>
document.addEventListener('DOMContentLoaded', function() {
  $('.table [id*="§"]').hover(function() {
    var ids = this.id.split('_');
    $('[id*="' + ids[0] + '"]').toggleClass('rm-hl0');
    if (ids[1] && ids[1] !== ids[0] ) {
      $('[id*="' + ids[1] + '"]').toggleClass('rm-hl1');
    }
  });
});
</script>|};
    Util.print_loading_overlay_js conf;
    Hutil.trailer conf
