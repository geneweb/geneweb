(* lib/relationMatrix.ml *)
open Config
module Driver = Geneweb_db.Driver
module Sosa = Geneweb_sosa

let get_person_simple_name base person =
  let first_name = Driver.sou base (Driver.get_first_name person) in
  let surname = Driver.sou base (Driver.get_surname person) in
  Printf.sprintf "%s %s" first_name surname

let format_ancestors_names conf base ancestors =
  match ancestors with
  | [] -> ""
  | [ anc ] -> get_person_simple_name base anc
  | [ anc1; anc2 ] ->
      Printf.sprintf "%s %s %s"
        (get_person_simple_name base anc1)
        (Util.transl conf "and" :> string)
        (get_person_simple_name base anc2)
  | _ -> (
      let names = List.map (get_person_simple_name base) ancestors in
      let rec build_list = function
        | [] -> ""
        | [ last ] -> (Util.transl conf "and" :> string) ^ " " ^ last
        | head :: tail -> head ^ ", " ^ build_list tail
      in
      match names with
      | [] -> ""
      | head :: tail -> head ^ ", " ^ build_list tail)

let make_tooltip_text conf base p1 p2 ancestors total len1 len2 =
  if ancestors = [] then ""
  else
    let p1_name = get_person_simple_name base p1 in
    let p2_name = get_person_simple_name base p2 in
    let total_count =
      if Sosa.eq total Sosa.zero then 0
      else try int_of_string (Sosa.to_string total) with _ -> 1
    in
    let ancestors_count = List.length ancestors in

    (* Ligne 1: nombre de relations entre les personnes *)
    let line1 =
      let relationship_text =
        Util.transl_nth conf "relationship link/relationship links"
          (if total_count = 1 then 0 else 1)
      in
      Printf.sprintf
        (Util.ftransl conf "%d %s links between %s and %s")
        total_count relationship_text p1_name p2_name
    in

    (* Ligne 2: ancêtres communs *)
    let ancestors_names = format_ancestors_names conf base ancestors in
    let ancestor_label =
      Util.transl_nth conf "first common ancestor"
        (if ancestors_count = 1 then 0 else 1)
      |> Utf8.capitalize_fst
    in
    let colon = Util.transl conf ":" in
    let line2 =
      Printf.sprintf "<b>%s%s</b><br>%s" ancestor_label colon ancestors_names
    in

    (* Ligne 3: degré de parenté (titre) avec MAJUSCULE *)
    let kinship_label_idx = if len1 + len2 = 1 then 0 else 1 in
    let kinship_label =
      Util.transl_nth conf "degree of kinship" kinship_label_idx
      |> Utf8.capitalize_fst
    in
    let line3 = Printf.sprintf "<b>%s%s</b>" kinship_label colon in

    (* Ligne 4: degré ascendant *)
    let asc_label_idx = if len1 = 1 then 0 else 2 in
    (* 0=ascendant, 2=ascendants *)
    let asc_label =
      Util.transl_nth conf "ascending/descending (degree)" asc_label_idx
    in
    let asc_icon =
      {|<i class="fa-solid fa-person-arrow-up-from-line mr-1"></i>|}
    in
    let line4 = Printf.sprintf "%s%d %s" asc_icon len1 asc_label in

    (* Ligne 5: degré descendant *)
    let desc_label_idx = if len2 = 1 then 1 else 3 in
    (* 1=descendant, 3=descendants *)
    let desc_label =
      Util.transl_nth conf "ascending/descending (degree)" desc_label_idx
    in
    let desc_icon =
      {|<i class="fa-solid fa-person-arrow-down-to-line fa-flip-horizontal mr-1"></i>|}
    in
    let line5 = Printf.sprintf "%s%d %s" desc_icon len2 desc_label in

    (* Construction du HTML final pour Bootstrap tooltip avec échappement *)
    let escaped_content =
      Printf.sprintf "<b>%s</b><br><br>%s<br><br>%s<br>%s<br>%s" line1 line2
        line3 line4 line5
    in
    (* Échapper les guillemets pour l'attribut HTML title *)
    String.map (function '"' -> '\'' | c -> c) escaped_content

let compute_relationship_cell conf base tstab p1 p2 =
  let ip1 = Driver.get_iper p1 in
  let ip2 = Driver.get_iper p2 in
  match Relation.compute_simple_relationship conf base tstab ip1 ip2 with
  | None -> (None, [], Sosa.zero, 0, 0, 0.0)
  | Some (rl, total, relationship, _reltab) -> (
      match rl with
      | [] -> (None, [], Sosa.zero, 0, 0, 0.0)
      | (len1, len2, sols) :: _ ->
          let ancestors = List.map (fun (anc, _) -> anc) sols in
          let relationship_notation =
            if len1 = 0 && len2 = 0 then "="
            else Printf.sprintf "%d/%d" len1 len2
          in
          ( Some relationship_notation,
            ancestors,
            total,
            len1,
            len2,
            relationship ))

let encoded_key name = (Mutil.encode (Name.lower name) :> string)

(* TODO: vérifier si access_by_key est voulu/déjà automatisé, pas d'équivalent dans le code ml !? *)
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
    let ep = encoded_key (Driver.p_first_name base p1) in
    let en = encoded_key (Driver.p_surname base p1) in
    let fn = encoded_key (Driver.p_first_name base p2) in
    let sn = encoded_key (Driver.p_surname base p2) in
    Printf.sprintf "%sm=R&p=%s&n=%s&ep=%s&en=%s"
      (Util.commd conf :> string)
      ep en fn sn
  else
    Printf.sprintf "%sm=R&i=%s&ei=%s"
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
        Output.printf conf
          {|<td class="text-center rm-diag" id="§%s_§%s">—</td>|}
          (Driver.Iper.to_string iper_i)
          (Driver.Iper.to_string iper_j)
      else if i > j then
        let rel_notation, ancestors, total, len1, len2, relationship =
          compute_relationship_cell conf base tstab persons.(i) persons.(j)
        in
        match rel_notation with
        | Some notation ->
            relation_counts.(i) <- relation_counts.(i) + 1;
            relation_counts.(j) <- relation_counts.(j) + 1;
            let tooltip_text =
              make_tooltip_text conf base persons.(i) persons.(j) ancestors
                total len1 len2
            in
            let link_url =
              make_relationship_link conf base persons.(i) persons.(j)
            in
            let links_count =
              if Sosa.eq total Sosa.zero then "0"
              else
                Sosa.to_string_sep
                  (Util.transl conf "(thousand separator)" :> string)
                  total
            in
            let cell_content =
              if Sosa.eq total Sosa.zero then notation
              else
                Printf.sprintf "<strong>%s</strong><br>%.2f%%<br>%s" links_count
                  (relationship *. 100.0) notation
            in
            Output.printf conf
              {|<td class="text-center rm-cell" id="§%s_§%s"
                   data-toggle="tooltip" data-html="true" data-placement="top" title="%s">
                <a href="%s" class="text-decoration-none">%s</a>
              </td>|}
              (Driver.Iper.to_string iper_i)
              (Driver.Iper.to_string iper_j)
              tooltip_text link_url cell_content
        | None ->
            Output.printf conf
              {|<td class="text-center rm-cell" id="§%s_§%s">—</td>|}
              (Driver.Iper.to_string iper_i)
              (Driver.Iper.to_string iper_j)
      else Output.print_sstring conf {|<td class="rm-empty"></td>|}
    done;
    Output.print_sstring conf "</tr>"
  done;
  Output.print_sstring conf {|<tr><th class="border-top-0"></th>|};
  for i = 0 to n - 1 do
    let iper_i = person_ipers.(i) in
    Output.printf conf
      {|<th class="text-center rm-bhead border-top-0" id="§%s_">
         <span class="badge badge-pill badge-primary">%d</span>
       </th>|}
      (Driver.Iper.to_string iper_i)
      (i + 1)
  done;
  Output.print_sstring conf {|</tr></tbody></table></div>|};

  (* Affichage des statistiques de relations *)
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

  (* Trouver la personne avec le plus de relations *)
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
