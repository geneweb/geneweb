type proposal = {
  entry : PlaceTerms.entry;
  left : string;
  right : string;
  known : bool;
  occurrences : PlaceRename.occurrence list;
}

let default_min_uses = 1000

(* the words after which a frequent term belongs to the name: "Saint-Pierre de
   Montréal", "Bourg en Bresse", "Stratford upon Avon"; compared lowered and
   without accents *)
let particles =
  [
    "a";
    "af";
    "am";
    "an";
    "au";
    "aux";
    "bei";
    "d";
    "de";
    "del";
    "della";
    "der";
    "des";
    "di";
    "du";
    "en";
    "et";
    "im";
    "in";
    "l";
    "la";
    "le";
    "les";
    "lez";
    "of";
    "on";
    "op";
    "sous";
    "sur";
    "upon";
    "van";
    "von";
    "y";
    "zu";
    "zur";
  ]

let is_particle w =
  let n = String.length w in
  (n > 0 && (w.[n - 1] = '\'' || String.ends_with ~suffix:"\xE2\x80\x99" w))
  || List.mem (Name.lower w) particles

let ends_with_particle left =
  match List.rev (String.split_on_char ' ' left) with
  | w :: _ -> is_particle w
  | [] -> false

(* [Name.abbrev] drops "de", "of"...: "de Canada" has the key of "Canada" *)
let starts_with_particle right =
  match String.split_on_char ' ' right with
  | w :: _ :: _ -> is_particle w
  | [ _ ] | [] -> false

let split_at frequent t =
  let n = String.length t in
  (* the spaces from the left: the longest end first *)
  let rec loop i =
    match String.index_from_opt t i ' ' with
    | None -> None
    | Some j -> (
        let right = String.sub t (j + 1) (n - j - 1) in
        let left = PlaceTerms.clean_term (String.sub t 0 j) in
        let k = PlaceTerms.cluster_key right in
        match Hashtbl.find_opt frequent k with
        | Some _
          when k <> "" && left <> ""
               && (not (ends_with_particle left))
               && not (starts_with_particle right) ->
            Some (left, right, k)
        | Some _ | None -> if j + 1 < n then loop (j + 1) else None)
  in
  if n = 0 then None else loop 0

let new_term p = p.left ^ ", " ^ p.right

(* key -> (uses of the cluster, its most used clean spelling and its uses) *)
let frequent_terms ~min_uses entries =
  let h = Hashtbl.create 4096 in
  List.iter
    (fun (e : PlaceTerms.entry) ->
      let total, best, best_uses =
        Option.value ~default:(0, "", -1) (Hashtbl.find_opt h e.cluster)
      in
      let best, best_uses =
        if PlaceTerms.is_clean e.term && e.uses > best_uses then (e.term, e.uses)
        else (best, best_uses)
      in
      Hashtbl.replace h e.cluster (total + e.uses, best, best_uses))
    entries;
  let frequent = Hashtbl.create 64 and totals = Hashtbl.create 64 in
  Hashtbl.iter
    (fun k (total, best, _) ->
      if k <> "" && best <> "" && total >= min_uses then (
        Hashtbl.replace frequent k best;
        Hashtbl.replace totals k total))
    h;
  (frequent, totals)

let proposals ?(min_uses = default_min_uses) entries =
  let frequent, totals = frequent_terms ~min_uses entries in
  let keys = Hashtbl.create 4096 in
  List.iter
    (fun (e : PlaceTerms.entry) -> Hashtbl.replace keys e.cluster ())
    entries;
  List.filter_map
    (fun (e : PlaceTerms.entry) ->
      if Hashtbl.mem frequent e.cluster then None
      else
        match split_at frequent e.term with
        | None -> None
        | Some (left, _, k) ->
            let occurrences =
              List.fold_left
                (fun acc (c : PlaceTerms.context) ->
                  if
                    c.suburb
                    || List.exists
                         (fun p -> PlaceTerms.cluster_key p = k)
                         c.parents
                  then acc
                  else
                    let d = Some (List.length c.parents) in
                    List.fold_left
                      (fun acc (p : PlaceTerms.place) -> (p.istr, d) :: acc)
                      acc c.places)
                [] e.contexts
              |> List.rev
            in
            let p =
              {
                entry = e;
                left;
                right = Hashtbl.find frequent k;
                known = Hashtbl.mem keys (PlaceTerms.cluster_key left);
                occurrences;
              }
            in
            if occurrences = [] || not (PlaceRename.valid_new_term (new_term p))
            then None
            else Some (Hashtbl.find totals k, p))
    entries
  |> List.sort (fun (t1, p1) (t2, p2) ->
      compare
        (t2, p1.right, p2.entry.uses, p1.entry.term)
        (t1, p2.right, p1.entry.uses, p2.entry.term))
  |> List.map snd

let plan conf base proposals =
  let seen = Hashtbl.create 64 in
  List.fold_left
    (fun acc p ->
      PlaceRename.plan conf base ~terms:[ p.entry.term ] ~new_term:(new_term p)
        p.occurrences
      |> List.fold_left
           (fun acc (it : PlaceRename.item) ->
             if Hashtbl.mem seen it.istr then acc
             else (
               Hashtbl.add seen it.istr ();
               { it with kind = PlaceRename.New_place } :: acc))
           acc)
    [] proposals
  |> PlaceRename.classify base
