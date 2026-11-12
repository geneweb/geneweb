open Config
module Driver = Geneweb_db.Driver
module Sosa = Geneweb_sosa

let max_dag_branches = 60
let max_rl_paths = 100
let budget = 500
let first_implexes = 50
let sep conf = Util.transl conf "(thousand separator)"
let num conf n = Mutil.string_of_int_sep (sep conf) n
let sosa conf s = Sosa.to_string_sep (sep conf) s
let nowrap s = {|<span class="text-nowrap">|} ^ s ^ "</span>"
let tr conf s = Utf8.capitalize_fst (Util.transl conf s)
let tr_nth conf s n = Utf8.capitalize_fst (Util.transl_nth conf s n)

let gen_title conf gen =
  tr_nth conf "generation/generations" 0 ^ " " ^ string_of_int gen

let attr s = (Util.escape_html s :> string)

let help text label =
  Printf.sprintf
    {|<span class="implex-help" data-bs-toggle="tooltip" data-bs-custom-class="implex-tip" title="%s">%s</span>|}
    (attr text) label

let th ?(cls = "") conf key =
  Printf.sprintf "<th%s>%s</th>"
    (if cls = "" then "" else Printf.sprintf {| class="%s"|} cls)
    (help
       (Utf8.capitalize_fst (Util.transl_nth conf key 1))
       (tr_nth conf key 0))

let percent conf x = nowrap (Util.string_of_decimal_num conf (100. *. x) ^ " %")

let ratio conf x =
  let r = Float.round (x *. 10000.) /. 100. in
  nowrap
    ((if r = 100. && x < 1. then "> " ^ Util.string_of_decimal_num conf 99.99
      else Util.string_of_decimal_num conf (if r = 0. then 100. *. x else r))
    ^ " %")

let generation s = Sosa.gen s - 1
let spouse s = Sosa.inc s (if Sosa.even s then 1 else -1)
let lines f l = String.concat "<br>" (List.map f l)

let person conf base ip =
  let p = Util.pget conf base ip in
  (Util.referenced_person_text conf base p :> string)
  ^ (DateDisplay.short_dates_text conf base p :> string)

let name conf base ip =
  (Util.gen_person_text conf base (Util.pget conf base ip) :> string)

let inbreeding conf base coefficient ip =
  if
    (not (Util.authorized_age conf base (Util.pget conf base ip)))
    || (not conf.manitou)
       && List.assoc_opt "show_consang" conf.base_env = Some "no"
  then None
  else coefficient ip

let optional conf = function Some x -> percent conf x | None -> "—"

let relative anchor s =
  let rec bits k s acc =
    if k = 0 then acc else bits (k - 1) (Sosa.half s) (Sosa.even s :: acc)
  in
  List.fold_left
    (fun r even -> if even then Sosa.twice r else Sosa.inc (Sosa.twice r) 1)
    Sosa.one
    (bits (Sosa.gen s - Sosa.gen anchor) s [])

let branches l =
  let { Implex.sosa1; sosa2; partner; _ } = l in
  match partner with
  | Some _ -> [ sosa1; sosa2; spouse sosa1; spouse sosa2 ]
  | None -> [ sosa1; sosa2 ]

let dag_link conf ip sosas =
  let sosas = List.sort_uniq Sosa.compare sosas in
  let params =
    Printf.sprintf "i1=%s%s&sp=0&im=0" (Driver.Iper.to_string ip)
      (String.concat ""
         (List.mapi
            (fun i s -> Printf.sprintf "&s%d=%s" (i + 1) (Sosa.to_string s))
            sosas))
  in
  let link mode limit =
    if sosas = [] || List.length sosas > limit then ""
    else
      Printf.sprintf {| <a href="%sm=%s&%s">%s</a>|}
        (Util.commd conf :> string)
        mode params mode
  in
  link "DAG" max_dag_branches ^ link "DAG2" 400

let parents conf base ip =
  Option.map
    (fun ifam ->
      let fam = Driver.foi base ifam in
      (Driver.get_father fam, Driver.get_mother fam))
    (Driver.get_parents (Util.pget conf base ip))

let relation_link conf ip1 ip2 =
  Printf.sprintf {| <a href="%sem=R&ei=%s&i=%s&sp=0&im=0">R</a>|}
    (Util.commd conf :> string)
    (Driver.Iper.to_string ip1)
    (Driver.Iper.to_string ip2)

let rl_link conf ip (father, mother) len1 len2 =
  Printf.sprintf
    {| <a href="%sm=RL&i=%s&i1=%s&i2=%s&l1=%d&l2=%d&dag=on&sp=0&im=0">RL</a>|}
    (Util.commd conf :> string)
    (Driver.Iper.to_string ip)
    (Driver.Iper.to_string father)
    (Driver.Iper.to_string mother)
    len1 len2

let paths_link conf base root a =
  let { Implex.ip; multiplicity; levels; _ } = a in
  if multiplicity < 2 || multiplicity > max_rl_paths then num conf multiplicity
  else
    let anc = Util.pget conf base ip in
    Printf.sprintf
      {|<a href="%sm=RL&%s&%s&%s&l1=%s&l2=0&dag=on&sp=0&im=0">%s</a>|}
      (Util.commd conf :> string)
      (Util.acces conf base anc :> string)
      (Util.acces_n conf base (Adef.escaped "1") (Util.pget conf base root)
        :> string)
      (Util.acces_n conf base (Adef.escaped "2") anc :> string)
      (String.concat "," (List.map string_of_int levels))
      (num conf multiplicity)

let junction_link conf href j =
  Printf.sprintf {|<a href="%s">%s</a>|}
    (href (generation j) ("j" ^ Sosa.to_string j))
    (sosa conf j)

let print_summary conf base coefficient root generations st =
  let sum f = List.fold_left (fun acc g -> acc + f g) 0 generations in
  let distinct = sum (fun g -> g.Implex.fresh) in
  let known = sum (fun g -> g.Implex.paths) in
  let loops = sum (fun g -> g.Implex.loops) in
  let complete =
    List.fold_left
      (fun n g ->
        let { Implex.gen; paths; _ } = g in
        if n = gen - 1 && gen < 62 && paths = 1 lsl gen then gen else n)
      0 generations
  in
  let depth = List.fold_left (fun _ g -> g.Implex.gen) 0 generations in
  let row label key value =
    Output.printf conf
      {|<dt class="col-sm-6 text-sm-end">%s</dt>
<dd class="col-sm-6">%s<div class="collapse implex-note">%s</div></dd>|}
      label value
      (tr conf (key ^ " helper"))
  in
  Output.print_sstring conf {|<dl class="row mt-3">|};
  row (tr conf "distinct ancestors") "distinct ancestors" (num conf distinct);
  row (tr conf "known positions (paths)") "known positions" (num conf known);
  row
    (tr conf "repeated positions")
    "repeated positions"
    (num conf (known - distinct));
  row
    (tr conf "implex ancestors")
    "implex ancestors"
    (Printf.sprintf {|<a href="#implexes">%s</a>|}
       (num conf
          (List.fold_left
             (fun n x -> n + List.length x.Implex.persons)
             0 st.Implex.implexes)));
  row (tr_nth conf "loop/loops" 1) "loops" (num conf loops);
  row
    (tr conf "loop rate (loops per parent link)")
    "loop rate"
    (if loops = 0 then "0"
     else ratio conf (float loops /. float (distinct + loops)));
  row
    (tr conf "complete generations")
    "complete generations" (string_of_int complete);
  row (tr conf "deepest generation") "deepest generation" (string_of_int depth);
  row
    (tr conf "inbreeding coefficient")
    "inbreeding coefficient"
    (if coefficient root = None then Util.transl conf "not computed (consang)"
     else optional conf (inbreeding conf base coefficient root));
  row (tr conf "direct part") "direct part"
    (match
       List.find_opt
         (fun j -> Sosa.eq j.Implex.sosa Sosa.one)
         st.Implex.junctions
     with
    | Some j -> percent conf j.Implex.direct
    | None -> "0 %");
  row
    (tr_nth conf "junction/junctions" 1)
    "junctions"
    (Printf.sprintf {|<a href="#junctions">%s</a>|}
       (num conf (List.length st.Implex.junctions)));
  row
    (tr_nth conf "cluster/clusters" 1)
    "clusters"
    (Printf.sprintf {|<a href="#clusters">%s</a>|}
       (num conf (List.length st.Implex.clusters)));
  Output.print_sstring conf "</dl>"

let print_chart conf generations =
  let known = List.fold_left (fun m g -> max m g.Implex.paths) 1 generations in
  let series f = "[" ^ String.concat "," (List.map f generations) ^ "]" in
  let count f = series (fun g -> string_of_int (f g)) in
  Output.printf conf
    {|<button type="button" class="btn btn-sm btn-outline-secondary mt-3" data-bs-toggle="collapse" data-bs-target="#implex-chart" aria-expanded="false" aria-controls="implex-chart"><i class="fa fa-chart-bar fa-fw"></i> %s</button>
<div class="collapse" id="implex-chart" data-series='{"gen":%s,"fresh":%s,"repeated":%s,"theoretical":%s,"known":%s,"distinct":%s}'
data-label-gen="%s" data-label-fresh="%s" data-label-repeated="%s"
data-label-theoretical="%s" data-label-known="%s" data-label-distinct="%s"
data-title-progression="%s" data-title-collapse="%s">
<div class="row mt-2"><div class="col-lg-6"><canvas id="implex-progression"></canvas></div>
<div class="col-lg-6"><canvas id="implex-collapse"></canvas></div></div></div>|}
    (tr conf "implex chart button")
    (count (fun g -> g.Implex.gen))
    (count (fun g -> g.Implex.fresh))
    (count (fun g -> g.Implex.paths - g.Implex.fresh))
    (series (fun g ->
         let t = ldexp 1. g.Implex.gen in
         if t > 1000. *. float known then "null" else Printf.sprintf "%.0f" t))
    (count (fun g -> g.Implex.paths))
    (count (fun g -> g.Implex.present))
    (attr (tr_nth conf "generation/generations" 0))
    (attr (tr conf "new ancestors"))
    (attr (tr conf "repeated positions"))
    (attr (tr conf "theoretical positions"))
    (attr (tr conf "known positions (paths)"))
    (attr (tr conf "distinct ancestors"))
    (attr (tr conf "implex chart progression"))
    (attr (tr conf "implex chart collapse"))

let print_generations conf href generations st =
  let tally = Hashtbl.create 64 in
  let add key n =
    Hashtbl.replace tally key
      (n + Option.value ~default:0 (Hashtbl.find_opt tally key))
  in
  List.iter
    (fun x ->
      add
        ("ig", generation (List.hd x.Implex.occurrences))
        (List.length x.Implex.persons))
    st.Implex.implexes;
  List.iter
    (fun j -> add ("jg", generation j.Implex.sosa) 1)
    st.Implex.junctions;
  let cell key gen =
    match Hashtbl.find_opt tally (key, gen) with
    | Some c ->
        Printf.sprintf {|<a href="%s">%s</a>|}
          (href gen (key ^ string_of_int gen))
          (num conf c)
    | None -> "0"
  in
  Output.printf conf
    {|<div class="table-responsive mt-3"><table class="table table-sm table-striped table-hover text-end">
<thead><tr>%s</tr></thead><tbody>|}
    (String.concat ""
       (List.map (th conf)
          [
            "generation (column)";
            "period (column)";
            "known (column)";
            "completeness (column)";
            "distinct (column)";
            "new (column)";
            "earlier (column)";
            "repeated (column)";
            "loops (column)";
            "repetition (column)";
            "implexes (column)";
            "junctions (column)";
          ]));
  List.iter
    (fun g ->
      let { Implex.gen; paths; present; fresh; loops; period } = g in
      Output.printf conf
        {|<tr><td>%d</td><td class="text-nowrap">%s</td><td>%s</td><td>%s</td>
<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>
<td>%s</td><td>%s</td></tr>|}
        gen
        (match period with
        | Some (a, b) -> Printf.sprintf "%d–%d" a b
        | None -> "—")
        (num conf paths)
        (ratio conf (ldexp (float paths) (-gen)))
        (num conf present) (num conf fresh)
        (num conf (present - fresh))
        (num conf (paths - present))
        (num conf loops)
        (ratio conf (float (paths - fresh) /. float paths))
        (cell "ig" gen) (cell "jg" gen))
    generations;
  Output.print_sstring conf "</tbody></table></div>"

let print_implexes conf base coefficient root shown href data st =
  let clusters = Hashtbl.create 1024 in
  List.iter
    (fun j ->
      Hashtbl.replace clusters (Sosa.to_string j.Implex.sosa) j.Implex.cluster)
    st.Implex.junctions;
  Output.printf conf
    {|<h2 class="h4 mt-4" id="implexes">%s</h2>
<div class="table-responsive"><table class="table table-sm table-striped align-middle">
<thead><tr>%s%s%s%s%s%s%s%s%s%s%s</tr></thead>|}
    (tr_nth conf "implex/implexes" 1)
    (th ~cls:"text-end" conf "Sosa (column)")
    (th conf "implex (column)")
    (th ~cls:"text-end" conf "other occurrences (column)")
    (th ~cls:"text-end" conf "paths (column)")
    (th ~cls:"text-end" conf "contribution (column)")
    (th ~cls:"text-end" conf "F (column)")
    (th ~cls:"text-end" conf "ancestors above (column)")
    (th ~cls:"text-end" conf "loops above (column)")
    (th conf "closing junctions (column)")
    (th ~cls:"text-end" conf "cluster (column)")
    (th conf "graph (column)");
  let last =
    List.fold_left
      (fun prev x ->
        let { Implex.persons; occurrences; closing; above } = x in
        let first = List.hd occurrences in
        let gen = generation first in
        if not (shown gen) then prev
        else (
          if gen <> prev then (
            if prev >= 0 then Output.print_sstring conf "</tbody>";
            Output.printf conf
              {|<tbody id="ig%d"><tr class="table-light"><th colspan="11">%s</th></tr></tbody><tbody>|}
              gen (gen_title conf gen));
          let a = Driver.Iper.Table.find data (List.hd persons) in
          let known, loops = Lazy.force above in
          let each f = String.concat "<br>" (List.mapi f persons) in
          Output.printf conf
            {|<tr id="x%s"><td class="text-end">%s</td><td>%s</td>
<td class="text-end">%s</td><td class="text-end">%s</td><td class="text-end">%s</td>
<td class="text-end">%s</td><td class="text-end">%s</td><td class="text-end">%s</td>
<td>%s</td><td class="text-end">%s</td><td>%s%s</td></tr>|}
            (Sosa.to_string first)
            (each (fun i _ -> sosa conf (Sosa.inc first i)))
            (lines (person conf base) persons)
            (each (fun i _ ->
                 String.concat ", "
                   (List.map
                      (fun s -> sosa conf (Sosa.inc s i))
                      (List.tl occurrences))))
            (paths_link conf base root a)
            (percent conf a.Implex.weight)
            (lines
               (fun ip -> optional conf (inbreeding conf base coefficient ip))
               persons)
            (num conf known) (num conf loops)
            (String.concat ", " (List.map (junction_link conf href) closing))
            (match closing with
            | j :: _ -> (
                match Hashtbl.find_opt clusters (Sosa.to_string j) with
                | Some c -> string_of_int c
                | None -> "")
            | [] -> "")
            (dag_link conf root
               (List.concat
                  (List.mapi
                     (fun i _ -> List.map (fun s -> Sosa.inc s i) occurrences)
                     persons)))
            (relation_link conf root (List.hd persons));
          gen))
      (-1) st.Implex.implexes
  in
  if last >= 0 then Output.print_sstring conf "</tbody>";
  Output.print_sstring conf "</table></div>"

let print_junctions conf base coefficient shown href data index st =
  let implex_href ip =
    match Driver.Iper.Table.find_opt index ip with
    | Some x ->
        let s = List.hd x.Implex.occurrences in
        href (generation s) ("x" ^ Sosa.to_string s)
    | None -> "#"
  in
  Output.printf conf
    {|<h2 class="h4 mt-4" id="junctions">%s</h2>
<div class="table-responsive"><table class="table table-sm table-striped align-middle">
<thead><tr>%s%s%s%s%s%s%s%s</tr></thead>|}
    (tr_nth conf "junction/junctions" 1)
    (th ~cls:"text-end" conf "Sosa (column)")
    (th conf "junction person (column)")
    (th ~cls:"text-end" conf "occurrences (column)")
    (th ~cls:"text-end" conf "degrees (column)")
    (th ~cls:"text-end" conf "Wright term (column)")
    (th ~cls:"text-end" conf "F (column)")
    (th ~cls:"text-end" conf "nested (column)")
    (th conf "graph (column)");
  ignore
    (List.fold_left
       (fun prev j ->
         let {
           Implex.sosa = js;
           person = jp;
           links;
           direct;
           nested;
           enclosing;
           cluster;
         } =
           j
         in
         let gen = generation js in
         if not (shown gen) then prev
         else (
           if gen <> prev then
             Output.printf conf
               {|<tbody id="jg%d"><tr><th colspan="8">%s</th></tr></tbody>|} gen
               (gen_title conf gen);
           let f = inbreeding conf base coefficient jp in
           let couple = parents conf base jp in
           let remainder =
             match f with
             | Some x when x +. 1e-6 < direct -> Util.transl conf "outdated"
             | Some x when x -. direct > 1e-6 -> percent conf (x -. direct)
             | _ -> ""
           in
           let through =
             match nested with
             | [] -> ""
             | l ->
                 "<br><small>"
                 ^ Util.transl conf "nested through"
                 ^ " "
                 ^ String.concat ", "
                     (List.map
                        (fun ip ->
                          Printf.sprintf {|<a href="%s">%s</a>|}
                            (implex_href ip) (name conf base ip))
                        l)
                 ^ "</small>"
           in
           Output.printf conf
             {|<tbody id="j%s" class="border-top"><tr class="table-light">
<td class="text-end">%s</td><td>%s</td><td colspan="2">%d %s%s, %s %d%s</td>
<td class="text-end">%s</td><td class="text-end">%s</td><td class="text-end">%s</td>
<td>%s%s</td></tr>|}
             (Sosa.to_string js) (sosa conf js) (person conf base jp)
             (List.length links)
             (Util.transl_nth conf "loop/loops"
                (if List.length links = 1 then 0 else 1))
             (match enclosing with
             | Some e ->
                 ", " ^ Util.transl conf "within" ^ " "
                 ^ junction_link conf href e
             | None -> "")
             (Util.transl_nth conf "cluster/clusters" 0)
             cluster through (percent conf direct) (optional conf f) remainder
             (dag_link conf jp
                (List.concat_map
                   (fun l -> List.map (relative js) (branches l))
                   links))
             (match couple with
             | Some (father, mother) -> relation_link conf mother father
             | None -> "");
           List.iter
             (fun l ->
               let {
                 Implex.ancestor;
                 partner;
                 sosa1;
                 sosa2;
                 len1;
                 len2;
                 contribution;
               } =
                 l
               in
               let members =
                 (ancestor, sosa1, sosa2)
                 :: Option.fold ~none:[]
                      ~some:(fun b -> [ (b, spouse sosa1, spouse sosa2) ])
                      partner
                 |> List.sort (fun (_, a, _) (_, b, _) -> Sosa.compare a b)
               in
               let graph =
                 match (couple, Driver.Iper.Table.find_opt data ancestor) with
                 | Some c, Some a when a.Implex.multiplicity <= max_rl_paths ->
                     rl_link conf ancestor c len1 len2
                 | _ -> ""
               in
               Output.printf conf
                 {|<tr%s><td></td><td>%s</td><td class="text-end">%s</td>
<td class="text-end">%d/%d=%d</td><td class="text-end">%s</td>
<td></td><td></td><td>%s</td></tr>|}
                 (if len1 + len2 <= 2 then {| class="table-warning"|} else "")
                 (lines (fun (ip, _, _) -> name conf base ip) members)
                 (lines
                    (fun (ip, u, v) ->
                      Printf.sprintf {|<a href="%s">%s, %s</a>|}
                        (implex_href ip) (sosa conf u) (sosa conf v))
                    members)
                 len1 len2 (len1 + len2)
                 (percent conf contribution)
                 graph)
             links;
           Output.print_sstring conf "</tbody>";
           gen))
       (-1) st.Implex.junctions);
  Output.print_sstring conf "</table></div>"

let print_clusters conf root href st =
  let links = Hashtbl.create 64 in
  List.iter
    (fun j ->
      let { Implex.cluster; links = l; _ } = j in
      Hashtbl.replace links cluster
        (List.rev_append l
           (Option.value ~default:[] (Hashtbl.find_opt links cluster))))
    st.Implex.junctions;
  Output.printf conf
    {|<h2 class="h4 mt-4" id="clusters">%s</h2>
<div class="table-responsive"><table class="table table-sm table-striped table-hover">
<thead><tr><th class="text-end">#</th>%s%s%s%s</tr></thead><tbody>|}
    (tr_nth conf "cluster/clusters" 1)
    (th ~cls:"text-end" conf "cluster loops (column)")
    (th ~cls:"text-end" conf "common ancestors (column)")
    (th conf "cluster junctions (column)")
    (th conf "graph (column)");
  List.iter
    (fun c ->
      let { Implex.id; members; common; size } = c in
      Output.printf conf
        {|<tr><td class="text-end">%d</td><td class="text-end">%s</td>
<td class="text-end">%s</td><td>%s%s</td><td>%s</td></tr>|}
        id (num conf size)
        (num conf (Driver.Iper.Set.cardinal common))
        (String.concat ", "
           (List.map (junction_link conf href)
              (List.filteri (fun i _ -> i < 30) members)))
        (if List.length members > 30 then "…" else "")
        (dag_link conf root
           (List.concat_map branches
              (Option.value ~default:[] (Hashtbl.find_opt links id)))))
    st.Implex.clusters;
  Output.print_sstring conf "</tbody></table></div>"

let print_ancestors conf base root n index ancestors =
  let top =
    List.filter (fun a -> a.Implex.multiplicity > 1) ancestors
    |> List.sort (fun a b ->
        compare b.Implex.multiplicity a.Implex.multiplicity)
    |> List.filteri (fun i _ -> i < n)
  in
  Output.printf conf
    {|<h2 class="h4 mt-4" id="ancestors">%s</h2>
<div class="table-responsive"><table class="table table-sm table-striped table-hover">
<thead><tr><th>%s</th>%s%s%s%s%s</tr></thead><tbody>|}
    (tr conf "most repeated ancestors")
    (tr_nth conf "person/persons" 0)
    (th ~cls:"text-end" conf "first generation (column)")
    (th ~cls:"text-end" conf "branches (column)")
    (th ~cls:"text-end" conf "paths (column)")
    (th ~cls:"text-end" conf "contribution (column)")
    (th conf "graph (column)");
  List.iter
    (fun a ->
      let { Implex.ip; first_gen; weight; _ } = a in
      Output.printf conf
        {|<tr><td>%s</td><td class="text-end">%d</td><td class="text-end">%d</td>
<td class="text-end">%s</td><td class="text-end">%s</td>
<td>%s</td></tr>|}
        (person conf base ip) first_gen
        (match Driver.Iper.Table.find_opt index ip with
        | Some x -> List.length x.Implex.occurrences
        | None -> 1)
        (paths_link conf base root a)
        (percent conf weight)
        (relation_link conf root ip))
    top;
  Output.print_sstring conf "</tbody></table></div>"

let print_range conf root page previous first lo hi depth total =
  Output.printf conf
    {|<form method="get" action="%s#range" id="range" class="d-flex flex-wrap align-items-center gap-2 mt-3">|}
    conf.command;
  Util.hidden_env conf;
  Util.hidden_input_s conf "m" "A";
  Util.hidden_input_s conf "t" "I";
  Util.hidden_input_s conf "i" (Driver.Iper.to_string root);
  Output.printf conf
    {|<label for="implex_g">%s</label>
<input type="number" id="implex_g" name="g" value="%d" min="%d" max="%d" class="form-control" style="width:8ch">
<label for="implex_v">%s</label>
<input type="number" id="implex_v" name="v" value="%d" min="%d" max="%d" class="form-control" style="width:8ch">
<button type="submit" class="btn btn-sm btn-outline-primary">OK</button>%s%s
<a href="%s&v=%d#range"%s>%s</a></form>|}
    (tr_nth conf "generation/generations" 1)
    lo first depth
    (Util.transl conf "to (generation range)")
    hi first depth
    (match previous with
    | Some (g, v) ->
        Printf.sprintf {|<a href="%s&v=%d#range">%s</a>|} (page g) v
          (Util.transl conf "previous generations")
    | None -> "")
    (if hi < depth then
       Printf.sprintf {|<a href="%s#range">%s</a>|}
         (page (hi + 1))
         (Util.transl conf "next generations")
     else "")
    (page first) depth
    (if total > 2000 then {| class="text-danger"|} else "")
    (Util.cftransl conf "all (%s rows)" [ num conf total ])

let print conf base p =
  let root = Driver.get_iper p in
  Inbreeding.of_ancestry base root @@ fun coefficient ->
  let { Implex.generations; ancestors } = Implex.profile base root in
  let st = Implex.structure ~coefficient base root in
  let depth = List.fold_left (fun _ g -> g.Implex.gen) 0 generations in
  let n = Option.value ~default:50 (Util.p_getint conf.env "n") in
  let rows = Hashtbl.create 64 in
  let implexes = Hashtbl.create 64 in
  let junctions = Hashtbl.create 64 in
  let add tbl gen k =
    Hashtbl.replace tbl gen
      (k + Option.value ~default:0 (Hashtbl.find_opt tbl gen))
  in
  List.iter
    (fun x ->
      let gen = generation (List.hd x.Implex.occurrences) in
      add rows gen 1;
      add implexes gen 1)
    st.Implex.implexes;
  List.iter
    (fun j ->
      let gen = generation j.Implex.sosa in
      add rows gen (1 + List.length j.Implex.links);
      add junctions gen (1 + List.length j.Implex.links))
    st.Implex.junctions;
  let at tbl gen = Option.value ~default:0 (Hashtbl.find_opt tbl gen) in
  let first =
    match st.Implex.implexes with
    | x :: _ -> generation (List.hd x.Implex.occurrences)
    | [] -> 0
  in
  let total = Hashtbl.fold (fun _ k acc -> acc + k) rows 0 in
  let lo =
    match Util.p_getint conf.env "g" with Some g -> max 0 g | None -> first
  in
  let hi =
    match Util.p_getint conf.env "v" with
    | Some v -> max lo v
    | None ->
        let rec extend hi n r =
          if hi >= depth || n >= first_implexes || r > budget then hi
          else extend (hi + 1) (n + at implexes (hi + 1)) (r + at rows (hi + 1))
        in
        extend lo (at implexes lo)
          (Hashtbl.fold
             (fun gen k acc -> if gen < lo then acc + k else acc)
             junctions (at rows lo))
  in
  let previous =
    let rec back g n r =
      if g <= first || n >= first_implexes || r > budget then g
      else back (g - 1) (n + at implexes (g - 1)) (r + at rows (g - 1))
    in
    if lo > first then
      Some (back (lo - 1) (at implexes (lo - 1)) (at rows (lo - 1)), lo - 1)
    else None
  in
  let shown gen = lo <= gen && gen <= hi in
  let shown_junction gen = gen <= hi in
  let page gen =
    Printf.sprintf "%s%s&m=A&t=I&g=%d%s"
      (Util.commd conf :> string)
      (Util.acces conf base p :> string)
      gen
      (match Util.p_getint conf.env "n" with
      | Some n -> "&n=" ^ string_of_int n
      | None -> "")
  in
  let href gen anchor =
    let shown = if anchor.[0] = 'j' then shown_junction else shown in
    (if shown gen then "" else page gen) ^ "#" ^ anchor
  in
  let data = Driver.Iper.Table.create 4096 in
  List.iter (fun a -> Driver.Iper.Table.replace data a.Implex.ip a) ancestors;
  let index = Driver.Iper.Table.create 1024 in
  List.iter
    (fun x ->
      List.iter
        (fun ip -> Driver.Iper.Table.replace index ip x)
        x.Implex.persons)
    st.Implex.implexes;
  let title h =
    if h then
      Output.printf conf "%s – %s"
        (tr conf "implexes and inbreeding")
        (Util.gen_person_text ~html:false conf base p :> string)
    else
      Output.printf conf
        {|<span class="d-flex justify-content-between align-items-center gap-2">%s<button type="button" class="btn btn-sm btn-outline-secondary" data-bs-toggle="collapse" data-bs-target=".implex-note" aria-expanded="false" title="%s"><i class="fa fa-info fa-fw"></i></button></span>|}
        (tr conf "implexes and inbreeding")
        (attr (tr conf "implex help button"))
  in
  Hutil.header conf title;
  Output.printf conf {|<h2 class="h5">%s %s</h2>|}
    (Util.referenced_person_text conf base p :> string)
    (Util.mod_ind_link conf base p
       (Adef.safe (DateDisplay.short_dates_text_notag conf base p))
      :> string);
  print_summary conf base coefficient root generations st;
  print_chart conf generations;
  print_generations conf href generations st;
  print_range conf root page previous first lo hi depth total;
  print_implexes conf base coefficient root shown href data st;
  print_junctions conf base coefficient shown_junction href data index st;
  print_clusters conf root href st;
  print_ancestors conf base root n index ancestors;
  Hutil.trailer conf
