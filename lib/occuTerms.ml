module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

type span = SourceTerms.span = { text : string; pos : int; len : int }

let mk s b e = { text = String.sub s b (e - b); pos = b; len = e - b }

let spans s =
  let n = String.length s in
  (* [b]: start of the current occupation, [-1] while skipping the spaces
     before it; [depth]: parentheses and brackets open *)
  let rec loop acc i b depth =
    if i = n then if b >= 0 then mk s b n :: acc else acc
    else
      let c = String.unsafe_get s i in
      if b < 0 then
        if c = ' ' || c = '\t' || c = ',' then loop acc (i + 1) (-1) depth
        else loop acc i i depth
      else
        match c with
        | '(' | '[' -> loop acc (i + 1) b (depth + 1)
        | ')' | ']' -> loop acc (i + 1) b (max 0 (depth - 1))
        | ',' when depth = 0 -> loop (mk s b i :: acc) (i + 1) (-1) 0
        | _ -> loop acc (i + 1) b depth
  in
  List.rev (loop [] 0 (-1) 0)

let key = PlaceTerms.cluster_key

type cluster = {
  ckey : string;
  spellings : SourceTerms.spelling list;
  total : int;
}

type data = (Driver.istr * string * int) list

let load base =
  let counts : (Driver.istr, int ref) Hashtbl.t = Hashtbl.create 4096 in
  Collection.iter
    (fun ip ->
      let i = Driver.get_occupation (Driver.poi base ip) in
      if not (Driver.Istr.is_empty i) then
        match Hashtbl.find_opt counts i with
        | Some r -> incr r
        | None -> Hashtbl.add counts i (ref 1))
    (Driver.ipers base);
  Hashtbl.fold (fun i r acc -> (i, Driver.sou base i, !r) :: acc) counts []

let clusters d =
  let h = Hashtbl.create 4096 in
  List.iter
    (fun (_, raw, uses) ->
      spans raw
      |> List.map (fun sp -> sp.text)
      |> List.sort_uniq compare
      |> List.iter (fun t ->
          let k = key t in
          if k <> "" then
            let sp =
              match Hashtbl.find_opt h k with
              | Some sp -> sp
              | None ->
                  let sp = Hashtbl.create 4 in
                  Hashtbl.add h k sp;
                  sp
            in
            let n, u = Option.value ~default:(0, 0) (Hashtbl.find_opt sp t) in
            Hashtbl.replace sp t (n + 1, u + uses)))
    d;
  Hashtbl.fold
    (fun ckey sp acc ->
      if Hashtbl.length sp < 2 then acc
      else
        let spellings =
          Hashtbl.fold
            (fun text (strings, uses) l ->
              { SourceTerms.text; strings; uses } :: l)
            sp []
          |> List.sort
               (fun (a : SourceTerms.spelling) (b : SourceTerms.spelling) ->
              compare (b.uses, a.text) (a.uses, b.text))
        in
        let total =
          List.fold_left
            (fun n (sp : SourceTerms.spelling) -> n + sp.uses)
            0 spellings
        in
        { ckey; spellings; total } :: acc)
    h []
  |> List.sort (fun a b -> compare (b.total, a.ckey) (a.total, b.ckey))

let samples d spellings n =
  let h = Hashtbl.create 16 in
  List.iter (fun t -> Hashtbl.replace h t []) spellings;
  List.iter
    (fun (_, raw, _) ->
      let sps = spans raw in
      List.iter
        (fun t ->
          match Hashtbl.find_opt h t with
          | Some l when List.length l < n -> (
              match List.filter (fun sp -> sp.text = t) sps with
              | [] -> ()
              | l' -> Hashtbl.replace h t ((raw, l') :: l))
          | Some _ | None -> ())
        spellings)
    d;
  List.map (fun t -> (t, List.rev (Hashtbl.find h t))) spellings

let replace_spans raw spans new_term =
  List.sort (fun a b -> compare b.pos a.pos) spans
  |> List.fold_left
       (fun s sp ->
         String.sub s 0 sp.pos ^ new_term
         ^ String.sub s (sp.pos + sp.len) (String.length s - sp.pos - sp.len))
       raw

let plan base d ~terms ~new_term =
  let new_term = PlaceTerms.clean_term new_term in
  let items =
    List.filter_map
      (fun (istr, raw, _) ->
        match List.filter (fun sp -> List.mem sp.text terms) (spans raw) with
        | [] -> None
        | sps ->
            let new_s =
              Mutil.normalize_utf_8 (replace_spans raw sps new_term)
            in
            if new_s = raw then None
            else
              Some
                ( {
                    PlaceRename.istr;
                    old_s = raw;
                    new_s;
                    depths = [];
                    kind = PlaceRename.New_place;
                  },
                  sps ))
      d
  in
  let spans_of = Hashtbl.create 64 in
  List.iter
    (fun ((it : PlaceRename.item), sps) -> Hashtbl.replace spans_of it.istr sps)
    items;
  PlaceRename.classify base (List.map fst items)
  |> List.map (fun (it : PlaceRename.item) ->
      (it, Option.value ~default:[] (Hashtbl.find_opt spans_of it.istr)))
