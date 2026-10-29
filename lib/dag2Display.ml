open Config
module Driver = Geneweb_db.Driver
module Iper = Driver.Iper

let box_width = 168
let box_height = 44
let union_width = 8
let person_layer = 76
let union_layer = 28

let person conf base ip =
  let p = Util.pget conf base ip in
  (Util.referenced_person_text conf base p :> string)
  ^ "<br>"
  ^ (DateDisplay.short_dates_text conf base p :> string)

let dag_url conf =
  (Util.commd conf :> string)
  ^ String.concat "&"
      ("m=DAG"
      :: List.filter_map
           (fun (k, v) ->
             if k = "m" then None else Some (k ^ "=" ^ Adef.as_string v))
           conf.env)

let print conf base =
  let start = Unix.gettimeofday () in
  let persons = Array.of_list (Dag.get_dag_elems conf base) in
  let n = Array.length persons in
  if n = 0 then Hutil.incorrect_request ~comment:"DAG2: no person" conf
  else
    let index = Iper.Table.create (2 * n) in
    Array.iteri (fun i ip -> Iper.Table.replace index ip i) persons;
    let unions = Driver.Ifam.Table.create n in
    let next = ref n in
    let parents = ref [] in
    let edges = ref [] in
    Array.iteri
      (fun c ip ->
        match Driver.get_parents (Util.pget conf base ip) with
        | Some ifam -> (
            let fam = Driver.foi base ifam in
            match
              List.filter_map
                (Iper.Table.find_opt index)
                [ Driver.get_father fam; Driver.get_mother fam ]
            with
            | [] -> ()
            | ps ->
                let u =
                  match Driver.Ifam.Table.find_opt unions ifam with
                  | Some u -> u
                  | None ->
                      let u = !next in
                      incr next;
                      Driver.Ifam.Table.replace unions ifam u;
                      parents := (u, ps) :: !parents;
                      List.iter (fun p -> edges := (p, u) :: !edges) ps;
                      u
                in
                edges := (u, c) :: !edges)
        | None -> ())
      persons;
    let total = !next in
    let edges = List.rev !edges in
    let up = Array.make total [] in
    let down = Array.make total [] in
    List.iter
      (fun (a, b) ->
        down.(a) <- b :: down.(a);
        up.(b) <- a :: up.(b))
      edges;
    List.iter (fun (u, ps) -> up.(u) <- ps) !parents;
    let height = Array.make total (-1) in
    let rec measure v =
      if height.(v) < 0 then
        height.(v) <-
          List.fold_left (fun h c -> max h (measure c + 1)) 0 down.(v);
      height.(v)
    in
    for v = 0 to total - 1 do
      ignore (measure v)
    done;
    let top = Array.fold_left max 0 height in
    let key = Array.make total (-1) in
    let count = ref 0 in
    let rec visit v =
      if key.(v) < 0 then (
        key.(v) <- !count;
        incr count;
        List.iter visit up.(v))
    in
    for v = 0 to n - 1 do
      if down.(v) = [] then visit v
    done;
    let slots =
      if Util.p_getenv conf.env "spread" = Some "0" then None
      else Some (fun r -> (top - r) mod 2 = 0)
    in
    let { Layered.x; rank; paths; crossings; breadth; width } =
      Layered.layout ?slots
        ~widths:
          (Array.init total (fun v -> if v < n then box_width else union_width))
        ~ranks:(Array.map (fun h -> top - h) height)
        ~keys:key edges
    in
    let layer_height r =
      if (top - r) mod 2 = 0 then person_layer else union_layer
    in
    let y = Array.make (top + 1) 0 in
    for r = 0 to top do
      y.(r) <-
        (if r = 0 then 0 else y.(r - 1) + (layer_height (r - 1) / 2))
        + (layer_height r / 2)
    done;
    let total_height = y.(top) + (layer_height top / 2) in
    let point v dy = Printf.sprintf "%d,%d" x.(v) (y.(rank.(v)) + dy) in
    let color u = Printf.sprintf "hsl(%d,65%%,42%%)" ((u - n) * 137 mod 360) in
    let path (a, b) p =
      let last = Array.length p - 1 in
      let points =
        String.concat " "
          (Array.to_list
             (Array.mapi
                (fun i v ->
                  point v
                    (if v >= n then 0
                     else if i = 0 then box_height / 2
                     else if i = last then -(box_height / 2)
                     else 0))
                p))
      in
      (* Halo then stroke: a later edge hides the earlier ones it crosses,
         drawing crossings as over/under passes; one colour per union. *)
      Printf.sprintf
        {|<polyline points="%s" style="stroke:var(--bs-body-bg)" stroke-width="5"/><polyline points="%s" stroke="%s"/>|}
        points points
        (color (max a b))
    in
    let elapsed = (Unix.gettimeofday () -. start) *. 1000. in
    Hutil.header ~fluid:true conf (fun _ -> Output.print_sstring conf "DAG2");
    Output.printf conf
      {|<p class="small text-body-secondary">%d persons, %d unions, %d dummy vertices, %d layers, breadth %d, %d crossings, %.0f ms · <a href="%s">m=DAG</a></p>|}
      n (total - n)
      (Array.length x - total)
      (top + 1) breadth crossings elapsed (dag_url conf);
    Output.printf conf
      {|<div class="overflow-auto"><div class="position-relative mx-auto" style="width:%dpx;height:%dpx">
<svg class="position-absolute top-0 start-0" width="%d" height="%d" aria-hidden="true">
<g fill="none" stroke-width="1.5" stroke-linejoin="round">%s</g>
<g>%s</g></svg>|}
      width total_height width total_height
      (String.concat "" (List.map2 path edges paths))
      (String.concat ""
         (List.init (total - n) (fun k ->
              Printf.sprintf {|<circle cx="%d" cy="%d" r="3.5" fill="%s"/>|}
                x.(n + k)
                y.(rank.(n + k))
                (color (n + k)))));
    Array.iteri
      (fun v ip ->
        let children =
          List.fold_left (fun c u -> c + List.length down.(u)) 0 down.(v)
        in
        Output.printf conf
          {|<div class="position-absolute border%s rounded bg-body small lh-sm text-center overflow-hidden px-1" style="left:%dpx;top:%dpx;width:%dpx;height:%dpx">%s</div>|}
          (if down.(v) = [] then " border-primary border-2"
           else if children > 1 then " border-danger border-2"
           else "")
          (x.(v) - (box_width / 2))
          (y.(rank.(v)) - (box_height / 2))
          box_width box_height (person conf base ip))
      persons;
    Output.print_sstring conf "</div></div>";
    Hutil.trailer conf
