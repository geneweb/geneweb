let default_min_uses = 100

(* the suffixes of an ordinal number, lowered and without accents, the
   longest first *)
let suffixes = [ "ieme"; "eme"; "ere"; "er"; "re"; "e" ]

let chop_suffix w =
  match List.find_opt (fun s -> String.ends_with ~suffix:s w) suffixes with
  | Some s -> Some (String.sub w 0 (String.length w - String.length s))
  | None -> None

let is_digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let is_roman s =
  s <> ""
  && String.for_all
       (function 'i' | 'v' | 'x' | 'l' | 'c' | 'd' | 'm' -> true | _ -> false)
       s

(* [Some n] when the lowered word [w] is an ordinal: digits with or without
   suffix ("16", "16e", "1er"), roman figures with a suffix ("xive", "ier") *)
let ordinal w =
  if is_digits w then int_of_string_opt w
  else
    match chop_suffix w with
    | Some d when is_digits d -> int_of_string_opt d
    | Some r when is_roman r -> (
        match Mutil.arabian_of_roman (String.uppercase_ascii r) with
        | n when n > 0 -> Some n
        | _ -> None
        | exception Not_found -> None)
    | Some _ | None -> None

(* as [PlaceTerms.cluster_key], the ordinals rewritten before [Name.abbrev],
   which reads "ier" as "i" *)
let key t =
  Name.lower (PlaceTerms.key_text t)
  |> String.split_on_char ' '
  |> List.filter (( <> ) "")
  |> List.map (fun w ->
      match ordinal w with Some n -> string_of_int n ^ "e" | None -> w)
  |> String.concat " " |> Name.abbrev

type place = {
  istr : Geneweb_db.Driver.istr;
  raw : string;
  pos : int;
  len : int;
}

type proposal = {
  pkey : string;
  written : string;
  target : string;
  places : place list;
  uses : int;
}

(* key -> (uses of its terms, most used clean spelling, its uses), for the
   terms of two words or more *)
let frequent_terms ~min_uses entries =
  let h = Hashtbl.create 1024 in
  List.iter
    (fun (e : PlaceTerms.entry) ->
      let k = key e.term in
      if String.contains k ' ' then
        let total, best, best_uses =
          Option.value ~default:(0, "", -1) (Hashtbl.find_opt h k)
        in
        let best, best_uses =
          if PlaceTerms.is_clean e.term && e.uses > best_uses then
            (e.term, e.uses)
          else (best, best_uses)
        in
        Hashtbl.replace h k (total + e.uses, best, best_uses))
    entries;
  let frequent = Hashtbl.create 64 in
  Hashtbl.iter
    (fun k (total, best, _) ->
      if best <> "" && total >= min_uses then Hashtbl.replace frequent k best)
    h;
  frequent

(* "a" and "b" separated by a comma only *)
let comma_between raw (a : Place.component) (b : Place.component) =
  let e = a.pos + a.len in
  b.pos > e && String.trim (String.sub raw e (b.pos - e)) = ","

let proposals ?(min_uses = default_min_uses) inverted entries places =
  let frequent = frequent_terms ~min_uses entries in
  let found = Hashtbl.create 256 in
  let add ~written ~target p uses =
    match Hashtbl.find_opt found written with
    | Some (t, l, u) -> Hashtbl.replace found written (t, p :: l, u + uses)
    | None -> Hashtbl.add found written (target, [ p ], uses)
  in
  List.iter
    (fun (istr, raw, uses) ->
      (* the terms in written order, the suburb left out *)
      let comps =
        Place.components inverted raw
        |> List.filter (fun (c : Place.component) -> not c.suburb)
        |> List.sort (fun (a : Place.component) b -> compare a.pos b.pos)
      in
      let rec loop = function
        | (a : Place.component) :: (b :: rest' as rest) -> (
            match
              if comma_between raw a b then
                Hashtbl.find_opt frequent (key (a.text ^ " " ^ b.text))
              else None
            with
            | Some target ->
                add
                  ~written:(a.text ^ ", " ^ b.text)
                  ~target
                  { istr; raw; pos = a.pos; len = b.pos + b.len - a.pos }
                  uses;
                loop rest'
            | None ->
                single a;
                loop rest)
        | [ a ] -> single a
        | [] -> ()
      and single (c : Place.component) =
        match Hashtbl.find_opt frequent (key c.text) with
        | Some target
          when PlaceTerms.cluster_key c.text <> PlaceTerms.cluster_key target ->
            add ~written:c.text ~target
              { istr; raw; pos = c.pos; len = c.len }
              uses
        | Some _ | None -> ()
      in
      loop comps)
    places;
  let totals = Hashtbl.create 64 in
  Hashtbl.iter
    (fun _ (target, _, uses) ->
      Hashtbl.replace totals target
        (uses + Option.value ~default:0 (Hashtbl.find_opt totals target)))
    found;
  Hashtbl.fold
    (fun written (target, places, uses) acc ->
      { pkey = written; written; target; places = List.rev places; uses } :: acc)
    found []
  |> List.sort (fun p1 p2 ->
      compare
        (Hashtbl.find totals p2.target, p1.target, p2.uses, p1.written)
        (Hashtbl.find totals p1.target, p2.target, p1.uses, p2.written))

let plan base proposals =
  (* istr -> raw and its replacements *)
  let h = Hashtbl.create 64 in
  List.iter
    (fun p ->
      List.iter
        (fun (pl : place) ->
          let raw, l =
            Option.value ~default:(pl.raw, []) (Hashtbl.find_opt h pl.istr)
          in
          Hashtbl.replace h pl.istr (raw, (pl.pos, pl.len, p.target) :: l))
        p.places)
    proposals;
  Hashtbl.fold
    (fun istr (raw, l) acc ->
      let new_s =
        List.sort_uniq (fun (a, _, _) (b, _, _) -> compare b a) l
        |> List.fold_left
             (fun s (pos, len, target) ->
               String.sub s 0 pos ^ target
               ^ String.sub s (pos + len) (String.length s - pos - len))
             raw
        |> Mutil.normalize_utf_8
      in
      if new_s = raw then acc
      else
        {
          PlaceRename.istr;
          old_s = raw;
          new_s;
          depths = [];
          kind = PlaceRename.New_place;
        }
        :: acc)
    h []
  |> PlaceRename.classify base
