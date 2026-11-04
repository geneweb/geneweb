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

(* the rest, the town, the department and the terms after it *)
let analyse raw =
  if String.contains raw '[' || String.contains raw ']' then None
  else
    match parse raw with
    | None | Some [] -> None
    | Some (p0 :: others) -> (
        match p0 with
        | Text head :: cands when PlaceTerms.clean_term head <> "" -> (
            let head = PlaceTerms.clean_term head in
            let cands =
              List.filter_map
                (fun p -> match piece_text p with "" -> None | t -> Some t)
                cands
            in
            let depts = List.filter_map department cands in
            let rest =
              List.filter_map
                (fun t ->
                  match department t with
                  | Some _ -> None
                  | None -> (
                      match clean_rest t with "" -> None | r -> Some r))
                cands
            in
            let others =
              List.filter_map
                (fun part ->
                  match
                    PlaceTerms.clean_term
                      (String.concat ""
                         (List.map
                            (fun p ->
                              match p with
                              | Text t -> t
                              | Group g -> "(" ^ g ^ ")")
                            part))
                  with
                  | "" -> None
                  | t -> Some t)
                others
            in
            let key d = PlaceTerms.cluster_key (dept_name d) in
            (* the name as written rather than the name of a code *)
            let chosen ds =
              match
                List.find_opt
                  (fun d -> match d with Name _ -> true | Code _ -> false)
                  ds
              with
              | Some d -> Some d
              | None -> ( match ds with d :: _ -> Some d | [] -> None)
            in
            let same ds =
              match ds with
              | [] -> true
              | d :: l -> List.for_all (fun d' -> key d' = key d) l
            in
            let next, tail =
              match others with
              | o :: os -> (department o, os)
              | [] -> (None, [])
            in
            let dept, tail =
              match (chosen depts, next) with
              | Some d, Some n when key d = key n -> (Some d, tail)
              | Some _, Some _ -> (None, tail)
              | Some d, None -> (Some d, others)
              | None, Some n -> (Some n, tail)
              | None, None -> (None, tail)
            in
            match dept with
            | Some d
              when same depts
                   && PlaceTerms.cluster_key head <> key d
                   && not (FrDepartments.is_name head) ->
                Some (rest, head, dept_name d, tail)
            | Some _ | None -> None)
        | Text _ :: _ | Group _ :: _ | [] -> None)

let town_and_department raw =
  Option.map (fun (_, head, dept, _) -> (head, dept)) (analyse raw)

let reorganize format raw =
  match analyse raw with
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
  List.filter_map
    (fun (istr, raw, uses) ->
      match reorganize format raw with
      | None -> None
      | Some new_s ->
          let known =
            match town_and_department raw with
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
