module Driver = Geneweb_db.Driver

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
  List.map
    (fun (i, uses) -> (i, Driver.sou base i, uses))
    (Driver.dict_entries_count base Geneweb_db.Dict.Occupation "")

let clusters d =
  SourceTerms.clusters_of ~split:spans ~key d
  |> List.map (fun (ckey, spellings, total) -> { ckey; spellings; total })

let samples d spellings n = SourceTerms.samples_of ~split:spans d spellings n

let plan base d ~terms ~new_term =
  SourceTerms.plan_of ~split:spans base d ~terms ~new_term
