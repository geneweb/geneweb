(* lib/relationMatrixDisplay.ml *)
open Config
open RelationMatrix
module Driver = Geneweb_db.Driver
module Sosa = Geneweb_sosa

(* Afficher une cellule de la matrice *)
let print_matrix_cell conf base tstab persons i j cell_storage n =
  let person_i = persons.(i) in
  let person_j = persons.(j) in
  let iper_i = Driver.get_iper person_i in
  let iper_j = Driver.get_iper person_j in
  if i = j then
    Output.printf conf {|<td class="rm-diag" data-id="§%s_§%s">—</td>|}
      (Driver.Iper.to_string iper_i)
      (Driver.Iper.to_string iper_j)
  else
    let cell_data_opt =
      if j > i then (
        match triangular_get cell_storage i j n with
        | Some cached -> Some cached
        | None ->
            let data = compute_cell_data conf base tstab person_i person_j in
            triangular_set cell_storage i j n data;
            data)
      else
        match triangular_get cell_storage j i n with
        | None -> None
        | Some original_cell -> create_inverted_cell_data original_cell
    in
    match cell_data_opt with
    | None ->
        Output.printf conf
          {|<td class="rm-cell" data-id="§%s_§%s">
            <span class="text-muted">—</span>
          </td>|}
          (Driver.Iper.to_string iper_i)
          (Driver.Iper.to_string iper_j)
    | Some cell_data -> (
        match find_shortest_path cell_data.paths with
        | None -> Output.print_sstring conf {|<td class="rm-cell">—</td>|}
        | Some shortest ->
            let notation =
              Printf.sprintf "%d/%d" shortest.Relation.l1 shortest.Relation.l2
            in
            let links_count =
              Sosa.to_string_sep
                (Util.transl conf "(thousand separator)" :> string)
                cell_data.total
            in
            let coeff_str =
              let pct = format_percentage_for_cell conf cell_data.coeff in
              if pct = "" then "" else "<small>" ^ pct ^ "</small>"
            in
            let cell_content =
              Printf.sprintf "<small>%s</small><br><b>%s</b><br>%s" notation
                links_count coeff_str
            in
            let relationship_url =
              make_relationship_link conf base (MatrixLink (person_i, person_j))
            in
            let cell_link =
              Util.make_link ~href:relationship_url ~content:cell_content ()
            in
            let tooltip_text =
              make_tooltip_text conf base person_i person_j shortest
                cell_data.total cell_data.coeff
            in
            Output.printf conf
              {|<td class="rm-cell" data-id="§%s_§%s" title="%s"
                 data-toggle="tooltip" data-html="true" data-placement="top">%s</td>|}
              (Driver.Iper.to_string iper_i)
              (Driver.Iper.to_string iper_j)
              (String.map (function '"' -> '\'' | c -> c) tooltip_text)
              (cell_link :> string))

(* Afficher les en-têtes de colonnes *)
let print_matrix_headers conf persons =
  let n = Array.length persons in
  Output.print_sstring conf {|<tr><th class="border-top-0"></th>|};
  for j = 0 to n - 1 do
    let iper_j = Driver.get_iper persons.(j) in
    Output.printf conf
      {|<th class="text-center rm-thead" data-id="§%s_">
         <span class="badge badge-pill badge-primary">%d</span>
       </th>|}
      (Driver.Iper.to_string iper_j)
      (j + 1)
  done;
  Output.print_sstring conf {|</tr>|}

(* Afficher une ligne de la matrice *)
let print_matrix_row conf base tstab persons i cell_storage n =
  let person = persons.(i) in
  let iper_i = Driver.get_iper person in
  Output.print_sstring conf "<tr>";
  Output.printf conf
    {|<th class="rm-head" data-id="§%s_">
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
    print_matrix_cell conf base tstab persons i j cell_storage n
  done;
  Output.print_sstring conf "</tr>"

(* Point d'entrée principal pour l'affichage de la matrice *)
let print_matrix_table conf base persons =
  let n = Array.length persons in
  let tstab = Util.create_topological_sort conf base in
  let cell_storage = create_triangular_matrix n in
  Output.print_sstring conf
    {|<div class="d-flex justify-content-center">
      <table class="table table-sm w-auto" id="rm-table">
        <tbody>|};
  print_matrix_headers conf persons;
  for i = 0 to n - 1 do
    print_matrix_row conf base tstab persons i cell_storage n
  done;
  Output.print_sstring conf {|</tbody></table></div>|};
  let json_data = matrix_to_json base persons cell_storage n in
  let json_string = Yojson.Safe.to_string json_data in
  Output.printf conf
    {|<script type="application/json" id="matrix-data">
%s
</script>|}
    json_string

(* Point d'entrée principal du module *)
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
    let tp_link conf =
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
      (tp_link conf)
      (Util.transl_nth conf "add/clear/show/edit the graph" 3
      |> Utf8.capitalize_fst);
    if List.length pl >= 2 then (
      let persons = Array.of_list pl in
      print_matrix_table conf base persons;
      Output.print_sstring conf
        {|<script>
document.addEventListener('DOMContentLoaded', function() {
  $('[data-toggle="tooltip"]').tooltip();
  $('.table [data-id]').hover(function() {
    var ids = $(this).data('id').split('_');
    $('[data-id*="' + ids[0] + '"]').toggleClass('rm-hl0');
    if (ids[1] && ids[1] !== ids[0]) {
      $('[data-id*="' + ids[1] + '"]').toggleClass('rm-hl1');
    }
  });
});
</script>|});
    Hutil.trailer conf
