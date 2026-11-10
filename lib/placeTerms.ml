module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

type place = { istr : Driver.istr; raw : string; uses : int }

type context = {
  parents : string list;
  suburb : bool;
  places : place list;
  c_uses : int;
}

type entry = {
  term : string;
  key : string;
  cluster : string;
  contexts : context list;
  nb_places : int;
  uses : int;
}

(* The non-breaking spaces are not touched by [Util.only_printable]: check_data
   keeps them inside a text (before a roman numeral, "Louis XIV"). *)
let nbsps = [ "\xC2\xA0"; "\xE2\x80\xAF" ]

let rec strip_nbsp s =
  let s = String.trim s in
  match
    List.find_map
      (fun p ->
        let n = String.length p and l = String.length s in
        if String.starts_with ~prefix:p s then Some (String.sub s n (l - n))
        else if String.ends_with ~suffix:p s then Some (String.sub s 0 (l - n))
        else None)
      nbsps
  with
  | Some s -> strip_nbsp s
  | None -> s

let clean_term s = strip_nbsp (Util.only_printable s)

(* [s] with every [p] replaced by a space *)
let space_for p s =
  let n = String.length p and l = String.length s in
  let b = Buffer.create l in
  let rec loop i =
    if i >= l then Buffer.contents b
    else if i + n <= l && String.sub s i n = p then (
      Buffer.add_char b ' ';
      loop (i + n))
    else (
      Buffer.add_char b s.[i];
      loop (i + 1))
  in
  loop 0

let is_clean s = clean_term s = s
let key_text s = List.fold_left (fun s p -> space_for p s) (clean_term s) nbsps

(* as check_data: spaces at both ends and invisible characters ignored *)
let cluster_key term = Name.abbrev (Name.lower (key_text term))

(* kept as two arrays of integers: 16 bytes per place *)
let uses_memo : (Driver.istr array * int array) DictMemo.t = DictMemo.create ()

let place_uses base =
  let istrs, uses =
    DictMemo.get uses_memo base (fun () ->
        let l = Driver.dict_entries_count base Geneweb_db.Dict.Place "" in
        (Array.of_list (List.map fst l), Array.of_list (List.map snd l)))
  in
  List.init (Array.length istrs) (fun i -> (istrs.(i), uses.(i)))

let compare_place p1 p2 = Place.compare_places p1.raw p2.raw

let compare_context c1 c2 =
  match Mutil.list_compare Gutil.alphabetic_order c1.parents c2.parents with
  | 0 -> compare c1.suburb c2.suburb
  | x -> x

let of_places ?(only = fun _ -> true) inverted places =
  (* term -> (parents, suburb) -> places *)
  let terms : (string, (string list * bool, place list ref) Hashtbl.t) Hashtbl.t
      =
    Hashtbl.create 4096
  in
  let add term ctx p =
    let ctxs =
      match Hashtbl.find_opt terms term with
      | Some h -> h
      | None ->
          let h = Hashtbl.create 4 in
          Hashtbl.add terms term h;
          h
    in
    (* a place is added once to a context: a term of a place has a single
       context in it (its parents differ at each depth), so only the last
       place added can be [p] *)
    match Hashtbl.find_opt ctxs ctx with
    | Some l -> ( match !l with q :: _ when q == p -> () | _ -> l := p :: !l)
    | None -> Hashtbl.add ctxs ctx (ref [ p ])
  in
  List.iter
    (fun (istr, raw, uses) ->
      let p = { istr; raw; uses } in
      let rec loop parents = function
        | [] -> ()
        | (c : Place.component) :: rest ->
            if c.text <> "" && only c.text then
              add c.text (List.rev parents, c.suburb) p;
            loop (if c.suburb then parents else c.text :: parents) rest
      in
      loop [] (Place.components inverted raw))
    places;
  Hashtbl.fold
    (fun term ctxs acc ->
      let contexts =
        Hashtbl.fold
          (fun (parents, suburb) l acc ->
            let places = List.sort compare_place !l in
            let c_uses =
              List.fold_left (fun n (p : place) -> n + p.uses) 0 places
            in
            { parents; suburb; places; c_uses } :: acc)
          ctxs []
        |> List.sort compare_context
      in
      let nb_places, uses =
        List.fold_left
          (fun (n, u) c -> (n + List.length c.places, u + c.c_uses))
          (0, 0) contexts
      in
      let key = Name.lower term in
      { term; key; cluster = cluster_key term; contexts; nb_places; uses }
      :: acc)
    terms []
  |> List.sort (fun e1 e2 ->
      match Gutil.alphabetic_order e1.term e2.term with
      | 0 -> compare e1.term e2.term
      | x -> x)

let places base =
  place_uses base
  |> List.map (fun (istr, uses) -> (istr, Driver.sou base istr, uses))

let build ?only conf base =
  of_places ?only (Place.places_inverted conf) (places base)

let summary inverted places =
  let h = Hashtbl.create 65536 in
  List.iter
    (fun (_, raw, uses) ->
      List.iter
        (fun (c : Place.component) ->
          if c.text <> "" then
            match Hashtbl.find_opt h c.text with
            | Some (n, u) ->
                incr n;
                u := !u + uses
            | None -> Hashtbl.add h c.text (ref 1, ref uses))
        (Place.components inverted raw))
    places;
  Hashtbl.fold
    (fun term (n, u) acc ->
      {
        term;
        key = "";
        cluster = cluster_key term;
        contexts = [];
        nb_places = !n;
        uses = !u;
      }
      :: acc)
    h []

let build_summary conf base = summary (Place.places_inverted conf) (places base)

let in_cluster key =
  let h = Hashtbl.create 1024 in
  fun t ->
    match Hashtbl.find_opt h t with
    | Some b -> b
    | None ->
        let b = cluster_key t = key in
        Hashtbl.add h t b;
        b

let clusters entries =
  let h = Hashtbl.create 1024 in
  List.iter
    (fun e ->
      match Hashtbl.find_opt h e.cluster with
      | Some l -> l := e :: !l
      | None -> Hashtbl.add h e.cluster (ref [ e ]))
    entries;
  let uses l = List.fold_left (fun n e -> n + e.uses) 0 l in
  Hashtbl.fold
    (fun k l acc ->
      match !l with
      | [] -> acc
      | [ e ] when is_clean e.term -> acc
      | l ->
          let l =
            List.sort (fun a b -> compare (b.uses, a.term) (a.uses, b.term)) l
          in
          (k, l) :: acc)
    h []
  |> List.sort (fun (k1, l1) (k2, l2) ->
      match compare (uses l2) (uses l1) with 0 -> compare k1 k2 | x -> x)

let clusters_memo : (string * entry list) list DictMemo.t = DictMemo.create ()

let summary_clusters conf base =
  let inverted = Place.places_inverted conf in
  DictMemo.get clusters_memo ~key:(string_of_bool inverted) base (fun () ->
      clusters (summary inverted (places base)))
