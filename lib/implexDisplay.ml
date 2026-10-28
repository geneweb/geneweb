open Config
module Driver = Geneweb_db.Driver
module Sosa = Geneweb_sosa

let max_dag_branches = 60
let max_rl_paths = 100
let budget = 500
let sep conf = Util.transl conf "(thousand separator)"
let num conf n = Mutil.string_of_int_sep (sep conf) n
let sosa conf s = Sosa.to_string_sep (sep conf) s
let nowrap s = {|<span class="text-nowrap">|} ^ s ^ "</span>"
let percent conf x = nowrap (Util.string_of_decimal_num conf (100. *. x) ^ " %")

let ratio conf x =
  let r = Float.round (x *. 10000.) /. 100. in
  nowrap
    ((if r = 100. && x < 1. then "> " ^ Util.string_of_decimal_num conf 99.99
      else Util.string_of_decimal_num conf (if r = 0. then 100. *. x else r))
    ^ " %")

let rec pow2 g = if g = 0 then Sosa.one else Sosa.twice (pow2 (g - 1))
let generation s = Sosa.gen s - 1
let spouse s = Sosa.inc s (if Sosa.even s then 1 else -1)
let lines f l = String.concat "<br>" (List.map f l)

let person conf base ip =
  let p = Util.pget conf base ip in
  (Util.referenced_person_text conf base p :> string)
  ^ (DateDisplay.short_dates_text conf base p :> string)

let name conf base ip =
  (Util.gen_person_text conf base (Util.pget conf base ip) :> string)

let inbreeding conf base ip =
  let p = Util.pget conf base ip in
  let c = Driver.get_consang p in
  if
    c = Adef.no_consang
    || (not (Util.authorized_age conf base p))
    || (not conf.manitou)
       && List.assoc_opt "show_consang" conf.base_env = Some "no"
  then None
  else Some (Adef.float_of_fix c)

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
  if sosas = [] || List.length sosas > max_dag_branches then ""
  else
    Printf.sprintf {| <a href="%sm=DAG&i1=%s%s&sp=0&im=0">DAG</a>|}
      (Util.commd conf :> string)
      (Driver.Iper.to_string ip)
      (String.concat ""
         (List.mapi
            (fun i s -> Printf.sprintf "&s%d=%s" (i + 1) (Sosa.to_string s))
            sosas))

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

let junction_link conf href j =
  Printf.sprintf {|<a href="%s">%s</a>|}
    (href (generation j) ("j" ^ Sosa.to_string j))
    (sosa conf j)

let print_summary conf base root href generations st =
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
  let row label value =
    Output.printf conf
      {|<dt class="col-sm-6">%s</dt><dd class="col-sm-6">%s</dd>|} label value
  in
  Output.print_sstring conf {|<dl class="row mt-3">|};
  row "Distinct ancestors" (num conf distinct);
  row "Known positions (paths)" (num conf known);
  row "Repeated positions" (num conf (known - distinct));
  row "Implex ancestors"
    (Printf.sprintf {|<a href="#implexes">%s</a>|}
       (num conf
          (List.fold_left
             (fun n x -> n + List.length x.Implex.persons)
             0 st.Implex.implexes)));
  row "Loops" (num conf loops);
  row "Loop rate (loops / parent links)"
    (if loops = 0 then "0"
     else ratio conf (float loops /. float (distinct + loops)));
  row "Complete generations" (string_of_int complete);
  row "Deepest generation" (string_of_int depth);
  row "Inbreeding coefficient"
    (if Driver.get_consang (Util.pget conf base root) = Adef.no_consang then
       "not computed (consang)"
     else optional conf (inbreeding conf base root));
  row "Direct part"
    (match
       List.find_opt
         (fun j -> Sosa.eq j.Implex.sosa Sosa.one)
         st.Implex.junctions
     with
    | Some j ->
        Printf.sprintf {|<a href="%s">%s</a>|} (href 0 "j1")
          (percent conf j.Implex.direct)
    | None -> "0 %");
  row "Junctions"
    (Printf.sprintf {|<a href="#junctions">%s</a>|}
       (num conf (List.length st.Implex.junctions)));
  row "Clusters"
    (Printf.sprintf {|<a href="#clusters">%s</a>|}
       (num conf (List.length st.Implex.clusters)));
  Output.print_sstring conf "</dl>"

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
  Output.print_sstring conf
    {|<div class="table-responsive mt-3"><table class="table table-sm table-striped table-hover text-end">
<thead><tr><th>Gen.</th><th>Period</th><th>Theoretical</th><th>Known</th>
<th>Completeness</th><th>Distinct</th><th>New</th><th>Seen earlier</th>
<th>Repeated here</th><th>Loops</th><th>Already seen</th><th>Implexes</th>
<th>Junctions</th></tr></thead><tbody>|};
  List.iter
    (fun g ->
      let { Implex.gen; paths; present; fresh; loops; period } = g in
      Output.printf conf
        {|<tr><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>
<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>
<td>%s</td><td>%s</td></tr>|}
        gen
        (match period with
        | Some (a, b) -> Printf.sprintf "%d–%d" a b
        | None -> "—")
        (sosa conf (pow2 gen))
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

let print_implexes conf base root shown href data st =
  let clusters = Hashtbl.create 1024 in
  List.iter
    (fun j ->
      Hashtbl.replace clusters (Sosa.to_string j.Implex.sosa) j.Implex.cluster)
    st.Implex.junctions;
  Output.print_sstring conf
    {|<h2 class="h4 mt-4" id="implexes">Implexes</h2>
<div class="table-responsive"><table class="table table-sm table-striped align-middle">
<thead><tr><th class="text-end">Sosa</th><th>Implex</th>
<th class="text-end">Other occurrences</th><th class="text-end">Paths</th>
<th class="text-end">Expected contribution</th><th class="text-end">Inbreeding</th>
<th class="text-end">Known ancestors</th><th class="text-end">Loops above</th>
<th>Junctions</th><th class="text-end">Cluster</th><th>Graph</th></tr></thead>|};
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
              {|<tbody id="ig%d"><tr class="table-light"><th colspan="11">Generation %d</th></tr></tbody><tbody>|}
              gen gen);
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
            (num conf a.Implex.multiplicity)
            (percent conf a.Implex.weight)
            (lines (fun ip -> optional conf (inbreeding conf base ip)) persons)
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

let print_junctions conf base shown href data index st =
  let implex_href ip =
    match Driver.Iper.Table.find_opt index ip with
    | Some x ->
        let s = List.hd x.Implex.occurrences in
        href (generation s) ("x" ^ Sosa.to_string s)
    | None -> "#"
  in
  Output.print_sstring conf
    {|<h2 class="h4 mt-4" id="junctions">Junctions</h2>
<div class="table-responsive"><table class="table table-sm table-striped align-middle">
<thead><tr><th class="text-end">Sosa</th><th>Person, common ancestors</th>
<th class="text-end">Occurrences</th><th class="text-end">Degrees</th>
<th class="text-end">Contribution</th><th class="text-end">Inbreeding</th>
<th class="text-end">Nested</th><th>Graphs</th></tr></thead>|};
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
               {|<tbody id="jg%d"><tr><th colspan="8">Generation %d</th></tr></tbody>|}
               gen gen;
           let f = inbreeding conf base jp in
           let couple = parents conf base jp in
           let remainder =
             match f with
             | Some x when x +. 1e-6 < direct -> "outdated"
             | Some x when x -. direct > 1e-6 -> percent conf (x -. direct)
             | _ -> ""
           in
           let through =
             match nested with
             | [] -> ""
             | l ->
                 "<br><small>nested through "
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
<td class="text-end">%s</td><td>%s</td><td colspan="2">%d loop(s)%s, cluster %d%s</td>
<td class="text-end">%s</td><td class="text-end">%s</td><td class="text-end">%s</td>
<td>%s%s</td></tr>|}
             (Sosa.to_string js) (sosa conf js) (person conf base jp)
             (List.length links)
             (match enclosing with
             | Some e -> ", within " ^ junction_link conf href e
             | None -> "")
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
  Output.print_sstring conf
    {|<h2 class="h4 mt-4" id="clusters">Clusters</h2>
<div class="table-responsive"><table class="table table-sm table-striped table-hover">
<thead><tr><th class="text-end">#</th><th class="text-end">Loops</th>
<th class="text-end">Common ancestors</th><th>Junctions</th><th>Graph</th></tr>
</thead><tbody>|};
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
  Output.print_sstring conf
    {|<h2 class="h4 mt-4" id="ancestors">Most repeated ancestors</h2>
<div class="table-responsive"><table class="table table-sm table-striped table-hover">
<thead><tr><th>Person</th><th class="text-end">First gen.</th>
<th class="text-end">Branches</th><th class="text-end">Paths</th>
<th class="text-end">Expected contribution</th><th>Graph</th></tr></thead><tbody>|};
  List.iter
    (fun a ->
      let { Implex.ip; first_gen; multiplicity; weight } = a in
      Output.printf conf
        {|<tr><td>%s</td><td class="text-end">%d</td><td class="text-end">%d</td>
<td class="text-end">%s</td><td class="text-end">%s</td>
<td>%s</td></tr>|}
        (person conf base ip) first_gen
        (match Driver.Iper.Table.find_opt index ip with
        | Some x -> List.length x.Implex.occurrences
        | None -> 1)
        (num conf multiplicity) (percent conf weight)
        (relation_link conf root ip))
    top;
  Output.print_sstring conf "</tbody></table></div>"

let print_range conf root page lo hi depth total =
  Output.printf conf
    {|<form method="get" action="%s" class="d-flex flex-wrap align-items-center gap-2 mt-3">|}
    conf.command;
  Util.hidden_env conf;
  Util.hidden_input_s conf "m" "A";
  Util.hidden_input_s conf "t" "I";
  Util.hidden_input_s conf "i" (Driver.Iper.to_string root);
  Output.printf conf
    {|<label for="implex_g">Generations</label>
<input type="number" id="implex_g" name="g" value="%d" min="0" max="%d" class="form-control" style="width:8ch">
<label for="implex_v">to</label>
<input type="number" id="implex_v" name="v" value="%d" min="0" max="%d" class="form-control" style="width:8ch">
<button type="submit" class="btn btn-sm btn-outline-primary">OK</button>%s
<a href="%s&v=%d"%s>all (%s rows)</a></form>|}
    lo depth hi depth
    (if hi < depth then
       Printf.sprintf {|<a href="%s">next generations</a>|} (page (hi + 1))
     else "")
    (page 0) depth
    (if total > 2000 then {| class="text-danger"|} else "")
    (num conf total)

let print conf base p =
  let root = Driver.get_iper p in
  let { Implex.generations; ancestors } = Implex.profile base root in
  let st = Implex.structure base root in
  let depth = List.fold_left (fun _ g -> g.Implex.gen) 0 generations in
  let n = Option.value ~default:50 (Util.p_getint conf.env "n") in
  let rows = Hashtbl.create 64 in
  let add gen k =
    Hashtbl.replace rows gen
      (k + Option.value ~default:0 (Hashtbl.find_opt rows gen))
  in
  List.iter
    (fun x -> add (generation (List.hd x.Implex.occurrences)) 1)
    st.Implex.implexes;
  List.iter
    (fun j -> add (generation j.Implex.sosa) (1 + List.length j.Implex.links))
    st.Implex.junctions;
  let rows_at gen = Option.value ~default:0 (Hashtbl.find_opt rows gen) in
  let total = Hashtbl.fold (fun _ k acc -> acc + k) rows 0 in
  let lo = max 0 (Option.value ~default:0 (Util.p_getint conf.env "g")) in
  let hi =
    match Util.p_getint conf.env "v" with
    | Some v -> max lo v
    | None ->
        let rec extend hi acc =
          if hi >= depth then hi
          else
            let acc = acc + rows_at (hi + 1) in
            if acc > budget then hi else extend (hi + 1) acc
        in
        extend lo (rows_at lo)
  in
  let shown gen = lo <= gen && gen <= hi in
  let page gen =
    Printf.sprintf "%s%s&m=A&t=I&g=%d%s"
      (Util.commd conf :> string)
      (Util.acces conf base p :> string)
      gen
      (match Util.p_getint conf.env "n" with
      | Some n -> "&n=" ^ string_of_int n
      | None -> "")
  in
  let href gen anchor = (if shown gen then "" else page gen) ^ "#" ^ anchor in
  let data = Driver.Iper.Table.create 4096 in
  List.iter (fun a -> Driver.Iper.Table.replace data a.Implex.ip a) ancestors;
  let index = Driver.Iper.Table.create 1024 in
  List.iter
    (fun x ->
      List.iter
        (fun ip -> Driver.Iper.Table.replace index ip x)
        x.Implex.persons)
    st.Implex.implexes;
  let title _ =
    Output.printf conf "%s – %s"
      (Utf8.capitalize_fst (Util.transl_nth conf "implex/implexes" 1))
      (Util.gen_person_text ~html:false conf base p :> string)
  in
  Hutil.header conf title;
  print_summary conf base root href generations st;
  print_generations conf href generations st;
  print_range conf root page lo hi depth total;
  print_implexes conf base root shown href data st;
  print_junctions conf base shown href data index st;
  print_clusters conf root href st;
  print_ancestors conf base root n index ancestors;
  Hutil.trailer conf
