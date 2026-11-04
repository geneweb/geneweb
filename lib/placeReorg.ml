type format = Commas | Parens
type piece = Text of string | Group of string

(* The comma-separated parts of [s], each one a list of pieces: the texts
   and the contents of the parentheses at the top level. [None] when the
   parentheses are not balanced. *)
let parse s =
  let n = String.length s in
  let buf = Buffer.create n in
  let flush_text pieces =
    let t = Buffer.contents buf in
    Buffer.clear buf;
    if String.trim t = "" then pieces else Text t :: pieces
  in
  let rec loop i depth pieces parts =
    if i = n then
      if depth > 0 then None
      else Some (List.rev (List.rev (flush_text pieces) :: parts))
    else
      let c = s.[i] in
      match c with
      | '(' when depth = 0 -> loop (i + 1) 1 (flush_text pieces) parts
      | '(' ->
          Buffer.add_char buf c;
          loop (i + 1) (depth + 1) pieces parts
      | ')' when depth = 1 ->
          let g = Buffer.contents buf in
          Buffer.clear buf;
          loop (i + 1) 0 (Group g :: pieces) parts
      | ')' when depth = 0 -> None
      | ')' ->
          Buffer.add_char buf c;
          loop (i + 1) (depth - 1) pieces parts
      | ',' when depth = 0 ->
          let part = List.rev (flush_text pieces) in
          loop (i + 1) 0 [] (part :: parts)
      | _ ->
          Buffer.add_char buf c;
          loop (i + 1) depth pieces parts
  in
  loop 0 0 [] []

(* "2<sup>e</sup> division" is "2e division" *)
let clean_rest s =
  let remove tag s =
    let n = String.length tag in
    let b = Buffer.create (String.length s) in
    let rec loop i =
      if i >= String.length s then Buffer.contents b
      else if
        i + n <= String.length s
        && String.lowercase_ascii (String.sub s i n) = tag
      then loop (i + n)
      else (
        Buffer.add_char b s.[i];
        loop (i + 1))
    in
    loop 0
  in
  PlaceTerms.clean_term (remove "</sup>" (remove "<sup>" s))

type dept = Code of string | Name of string

let department s =
  match FrDepartments.name_of_code s with
  | Some name -> Some (Code name)
  | None ->
      if FrDepartments.is_name s then Some (Name (PlaceTerms.clean_term s))
      else None

let dept_name d = match d with Code n | Name n -> n
let piece_text p = match p with Text t | Group t -> PlaceTerms.clean_term t

(* above a department by default: the country *)
let default_above _dept t = PlaceTerms.cluster_key t = "france"

(* the rest, the town, the department and the terms above it. The department
   may be written several times, as a code or a name ("Nantes, 44,
   Loire-Atlantique"): once only. The other terms after the town are the rest
   too ("Nantes (44), 4e canton, acte 180"), except those [above] the
   department ("France"), kept after it. *)
let analyse ?(above = default_above) raw =
  if String.contains raw '[' || String.contains raw ']' then None
  else
    match parse raw with
    | None | Some [] -> None
    | Some (p0 :: others) -> (
        match p0 with
        | Text head :: cands when PlaceTerms.clean_term head <> "" -> (
            let head = PlaceTerms.clean_term head in
            let texts l =
              List.filter_map
                (fun t -> match t with "" -> None | t -> Some t)
                l
            in
            let cands = texts (List.map piece_text cands) in
            let others =
              texts
                (List.map
                   (fun part ->
                     PlaceTerms.clean_term
                       (String.concat ""
                          (List.map
                             (fun p ->
                               match p with
                               | Text t -> t
                               | Group g -> "(" ^ g ^ ")")
                             part)))
                   others)
            in
            let depts =
              List.filter_map department cands
              |> List.rev_append (List.rev (List.filter_map department others))
            in
            let key d = PlaceTerms.cluster_key (dept_name d) in
            (* the name as written rather than the name of a code *)
            let chosen =
              match
                List.find_opt
                  (fun d -> match d with Name _ -> true | Code _ -> false)
                  depts
              with
              | Some d -> Some d
              | None -> ( match depts with d :: _ -> Some d | [] -> None)
            in
            match chosen with
            | Some d
              when List.for_all (fun d' -> key d' = key d) depts
                   && PlaceTerms.cluster_key head <> key d
                   && not (FrDepartments.is_name head) ->
                let dept = dept_name d in
                let not_dept t = department t = None in
                let rest_parens =
                  List.filter_map
                    (fun t ->
                      if not_dept t then
                        match clean_rest t with "" -> None | r -> Some r
                      else None)
                    cands
                in
                let rest_terms, tail =
                  List.partition
                    (fun t -> not (above dept t))
                    (List.filter not_dept others)
                in
                Some
                  ( List.rev_append (List.rev rest_parens) rest_terms,
                    head,
                    dept,
                    tail )
            | Some _ | None -> None)
        | Text _ :: _ | Group _ :: _ | [] -> None)

let town_and_department ?above raw =
  Option.map (fun (_, head, dept, _) -> (head, dept)) (analyse ?above raw)

let reorganize ?above format raw =
  match analyse ?above raw with
  | None -> None
  | Some (rest, head, dept, tail) ->
      let town =
        match format with
        | Commas -> [ head; dept ]
        | Parens -> [ head ^ " (" ^ dept ^ ")" ]
      in
      let s =
        String.concat ", "
          (List.rev_append (List.rev rest)
             (List.rev_append (List.rev town) tail))
        |> Mutil.normalize_utf_8
      in
      if s = raw then None else Some s

type proposal = {
  istr : Geneweb_db.Driver.istr;
  raw : string;
  new_s : string;
  uses : int;
  known : bool;
}

let proposals format entries places =
  (* the pairs (term, ancestor) of the base *)
  let under = Hashtbl.create 4096 in
  List.iter
    (fun (e : PlaceTerms.entry) ->
      List.iter
        (fun (c : PlaceTerms.context) ->
          List.iter
            (fun p ->
              Hashtbl.replace under (e.cluster, PlaceTerms.cluster_key p) ())
            c.parents)
        e.contexts)
    entries;
  (* the ancestors of the department in the base stay above it *)
  let above dept t =
    default_above dept t
    || Hashtbl.mem under (PlaceTerms.cluster_key dept, PlaceTerms.cluster_key t)
  in
  List.filter_map
    (fun (istr, raw, uses) ->
      match reorganize ~above format raw with
      | None -> None
      | Some new_s ->
          let known =
            match town_and_department ~above raw with
            | Some (town, dept) ->
                Hashtbl.mem under
                  (PlaceTerms.cluster_key town, PlaceTerms.cluster_key dept)
            | None -> false
          in
          Some { istr; raw; new_s; uses; known })
    places
  |> List.sort (fun a b -> Place.compare_places a.raw b.raw)

let plan base proposals =
  List.map
    (fun p ->
      {
        PlaceRename.istr = p.istr;
        old_s = p.raw;
        new_s = p.new_s;
        depths = [];
        kind = PlaceRename.New_place;
      })
    proposals
  |> PlaceRename.classify base
