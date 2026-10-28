module Driver = Geneweb_db.Driver

type group = {
  targets : PlaceTerms.place list;
  members : PlaceTerms.place list;
  uses : int;
}

let key g = (List.hd g.targets).PlaceTerms.istr

(* suburb and terms leaf first *)
let terms inverted raw =
  let comps = Place.components inverted raw in
  let sub =
    List.find_map
      (fun (c : Place.component) -> if c.suburb then Some c.text else None)
      comps
    |> Option.value ~default:""
  in
  let main =
    List.filter_map
      (fun (c : Place.component) ->
        if c.suburb || c.text = "" then None else Some c.text)
      comps
  in
  (sub, List.rev main)

let rec is_prefix a b =
  match (a, b) with
  | [], _ -> true
  | x :: a, y :: b -> x = y && is_prefix a b
  | _ :: _, [] -> false

let strict_prefix a b = List.length a < List.length b && is_prefix a b

let by_uses (p1 : PlaceTerms.place) (p2 : PlaceTerms.place) =
  match compare p2.uses p1.uses with 0 -> compare p1.raw p2.raw | x -> x

let of_places inverted places =
  (* buckets of places sharing their suburb and their leaf *)
  let buckets = Hashtbl.create 1024 in
  List.iter
    (fun (istr, raw, uses) ->
      match terms inverted raw with
      | _, [] -> ()
      | sub, (leaf :: _ as l) ->
          let x = ({ PlaceTerms.istr; raw; uses }, l) in
          Hashtbl.replace buckets (sub, leaf)
            (x
            :: Option.value ~default:[] (Hashtbl.find_opt buckets (sub, leaf))))
    places;
  Hashtbl.fold
    (fun _ l acc ->
      match l with
      | [] | [ _ ] -> acc
      | l ->
          let maximal =
            List.filter_map
              (fun (_, t) ->
                if List.exists (fun (_, t') -> strict_prefix t t') l then None
                else Some t)
              l
            |> List.sort_uniq compare
          in
          List.fold_left
            (fun acc t ->
              let targets =
                List.filter_map
                  (fun (p, t') -> if t' = t then Some p else None)
                  l
                |> List.sort by_uses
              in
              let members =
                List.filter_map
                  (fun (p, t') -> if strict_prefix t' t then Some p else None)
                  l
                |> List.sort
                     (fun (p1 : PlaceTerms.place) (p2 : PlaceTerms.place) ->
                       Place.compare_places p1.raw p2.raw)
              in
              if List.length targets + List.length members < 2 then acc
              else
                let uses =
                  List.fold_left
                    (fun n (p : PlaceTerms.place) -> n + p.uses)
                    0 (targets @ members)
                in
                { targets; members; uses } :: acc)
            acc maximal)
    buckets []
  |> List.sort (fun g1 g2 ->
      match compare g2.uses g1.uses with
      | 0 ->
          Place.compare_places (List.hd g1.targets).raw (List.hd g2.targets).raw
      | x -> x)

let build conf base =
  PlaceTerms.place_uses conf base
  |> List.map (fun (istr, uses) -> (istr, Driver.sou base istr, uses))
  |> of_places (Place.places_inverted conf)

let find groups istr =
  List.find_opt (fun g -> Driver.Istr.equal (key g) istr) groups

let count_groups groups istr =
  List.fold_left
    (fun n g ->
      if
        List.exists
          (fun (p : PlaceTerms.place) -> Driver.Istr.equal p.istr istr)
          g.members
      then n + 1
      else n)
    0 groups
