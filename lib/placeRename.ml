open Config
open Def
module Driver = Geneweb_db.Driver
module Dict = Geneweb_db.Dict
module IT = Driver.Istr.Table

type occurrence = Driver.istr * int option

let occurrence_key (istr, depth) =
  Printf.sprintf "o%s.%s"
    (Driver.Istr.to_string istr)
    (match depth with None -> "s" | Some d -> string_of_int d)

let place_key istr = "p" ^ Driver.Istr.to_string istr

let occurrence_of_key k =
  let n = String.length k in
  if n < 4 || k.[0] <> 'o' then None
  else
    match String.index_opt k '.' with
    | None -> None
    | Some i -> (
        let istr = String.sub k 1 (i - 1) in
        let depth = String.sub k (i + 1) (n - i - 1) in
        match int_of_string_opt istr with
        | None -> None
        | Some _ -> (
            let istr = Driver.Istr.of_string istr in
            match depth with
            | "s" -> Some (istr, None)
            | d -> (
                match int_of_string_opt d with
                | Some d when d >= 0 -> Some (istr, Some d)
                | _ -> None)))

let occurrences_of_env conf =
  List.filter_map (fun (k, _) -> occurrence_of_key k) conf.env
  |> List.sort_uniq compare

let terms_of_env conf =
  List.filter_map
    (fun (k, _) ->
      let n = String.length k in
      if
        k = "t"
        || n > 1
           && k.[0] = 't'
           && String.for_all
                (fun c -> c >= '0' && c <= '9')
                (String.sub k 1 (n - 1))
      then
        match Util.p_getenv_notrim conf.env k with
        | Some t when t <> "" -> Some t
        | _ -> None
      else None)
    conf.env
  |> List.sort_uniq compare

let has_delimiter s =
  String.exists (fun c -> c = '[' || c = ']' || c = '(' || c = ')') s

(* ["aaaa (bbbb)"] as the book of places reads it ({!Dict.paren_form}), both
   parts free of brackets and parentheses *)
let paren_form s =
  match Dict.paren_form s with
  | Some (host, parent)
    when (not (has_delimiter host)) && not (has_delimiter parent) ->
      Some (host, parent)
  | Some _ | None -> None

(* the term after the suburb, plain or "aaaa (bbbb)" *)
let valid_main s = (not (has_delimiter s)) || paren_form s <> None

(* ["[faubourg] - ville"] (separator -, – or —) is [Some ("faubourg",
   "ville")]: a suburb isolated before its town, both non empty *)
let suburb_form s =
  match Place.split_suburb s with
  | "", _ -> None
  | sub, rest ->
      let sub = String.trim sub and rest = String.trim rest in
      if sub <> "" && rest <> "" && (not (has_delimiter sub)) && valid_main rest
      then Some (sub, rest)
      else None

(* ["(Haute-Garonne)"] is [Some "Haute-Garonne"]: the last term of a place
   moved into parentheses, "aaaa, Haute-Garonne" becoming "aaaa
   (Haute-Garonne)" *)
let lone_paren s =
  let n = String.length s in
  if n > 2 && s.[0] = '(' && s.[n - 1] = ')' then
    let inner = String.trim (String.sub s 1 (n - 2)) in
    if inner <> "" && not (has_delimiter inner) then Some inner else None
  else None

let valid_new_term s =
  let s = PlaceTerms.clean_term s in
  s <> "" && (valid_main s || suburb_form s <> None || lone_paren s <> None)

let valid_place s =
  let s = PlaceTerms.clean_term s in
  let brackets_ok s = not (String.exists (fun c -> c = '[' || c = ']') s) in
  let parens_ok s =
    let n = String.length s in
    let rec loop i opened =
      if i = n then opened < 0
      else
        match s.[i] with
        | '(' -> opened < 0 && loop (i + 1) i
        | ')' ->
            opened >= 0
            && String.trim (String.sub s (opened + 1) (i - opened - 1)) <> ""
            && loop (i + 1) (-1)
        | _ -> loop (i + 1) opened
    in
    loop 0 (-1)
  in
  s <> ""
  &&
  match Dict.suburb s with
  | None -> brackets_ok s && parens_ok s
  | Some (i, j) ->
      let sub = String.sub s 1 (i - 1) in
      let rest = String.sub s j (String.length s - j) in
      String.trim sub <> ""
      && (not (has_delimiter sub))
      && String.trim rest <> ""
      && brackets_ok rest && parens_ok rest

let rtrim s =
  let rec last i = if i > 0 && s.[i - 1] = ' ' then last (i - 1) else i in
  String.sub s 0 (last (String.length s))

let rewrite_spans inverted ~terms f raw depths =
  let comps = Place.components inverted raw in
  let main = List.filter (fun (c : Place.component) -> not c.suburb) comps in
  let pick depth =
    match depth with
    | None -> List.find_opt (fun (c : Place.component) -> c.suburb) comps
    | Some d -> List.nth_opt main d
  in
  let spans =
    List.filter_map
      (fun d ->
        match pick d with
        | Some (c : Place.component) when List.mem c.text terms -> Some c
        | _ -> None)
      depths
    (* from the end of the string, so the earlier offsets stay valid *)
    |> List.sort_uniq (fun (a : Place.component) (b : Place.component) ->
        compare b.pos a.pos)
  in
  if spans = [] then None
  else
    Some
      (List.fold_left
         (fun s (c : Place.component) ->
           String.sub s 0 c.pos ^ f c
           ^ String.sub s (c.pos + c.len) (String.length s - c.pos - c.len))
         raw spans)

let rewrite_with inverted ~terms f raw depths =
  rewrite_spans inverted ~terms
    (fun (c : Place.component) -> f c.text)
    raw depths

(* A new term "aaaa (bbbb)" is written as is, except next to the parentheses
   of the place, inside them ("Nantes (44)" for "44") or before them ("Nantes"
   in "Nantes (44)"), where it would give two parenthesised parts: there it is
   written "aaaa, bbbb", which has the same meaning. *)
let rewrite inverted ~terms ~new_term raw depths =
  let n = String.length raw in
  let in_parens (c : Place.component) =
    (c.pos > 0 && String.unsafe_get raw (c.pos - 1) = '(')
    ||
    let rec next i =
      if i < n && String.unsafe_get raw i = ' ' then next (i + 1) else i
    in
    let i = next (c.pos + c.len) in
    i < n && String.unsafe_get raw i = '('
  in
  let nested =
    match paren_form new_term with
    | Some (host, parent) -> host ^ ", " ^ parent
    | None -> new_term
  in
  (* a new term "[faubourg] - ville" can only start a place without suburb:
     elsewhere the occurrence is left as it is *)
  let suburb_ok (c : Place.component) =
    (not
       (List.exists
          (fun (x : Place.component) -> x.suburb)
          (Place.components inverted raw)))
    && String.trim (String.sub raw 0 c.pos) = ""
  in
  match lone_paren new_term with
  | Some inner -> (
      (* a new term "(bbbb)" replaces the last term of a place, after a
         comma, and its comma: "aaaa, bbbb" becomes "aaaa (bbbb)"; elsewhere
         (first term, term inside, place already with parentheses) the
         occurrence is left as it is *)
      let paren_ok (c : Place.component) =
        let after = String.sub raw (c.pos + c.len) (n - c.pos - c.len) in
        let before = rtrim (String.sub raw 0 c.pos) in
        let nb = String.length before in
        String.trim after = ""
        && nb > 0
        && before.[nb - 1] = ','
        && not (has_delimiter before)
      in
      match
        rewrite_spans inverted ~terms
          (fun c -> if paren_ok c then "\001" else c.text)
          raw depths
      with
      | Some s -> (
          match String.index_opt s '\001' with
          | Some k ->
              let before = rtrim (String.sub s 0 k) in
              let before = String.sub before 0 (String.length before - 1) in
              Some
                (rtrim before ^ " (" ^ inner ^ ")"
                ^ String.sub s (k + 1) (String.length s - k - 1))
          | None -> Some s)
      | None -> None)
  | None ->
      rewrite_spans inverted ~terms
        (fun c ->
          if suburb_form new_term <> None then
            if suburb_ok c then new_term else c.text
          else if in_parens c then nested
          else new_term)
        raw depths

(* ------------------------------------------------------------------------ *)

type field = Place | Source | Occupation
type usage = Driver.istr -> int

let uses u i = u i

let dict field =
  match field with
  | Place -> Dict.Place
  | Source -> Dict.Source
  | Occupation -> Dict.Occupation

(* the fields of [field] of a person or family moved by [f] *)
let map_person field f (gp : (_, _, Driver.istr) gen_person) =
  match field with
  | Place ->
      {
        gp with
        birth_place = f gp.birth_place;
        baptism_place = f gp.baptism_place;
        death_place = f gp.death_place;
        burial_place = f gp.burial_place;
        pevents =
          List.map
            (fun e -> { e with epers_place = f e.epers_place })
            gp.pevents;
      }
  | Source ->
      {
        gp with
        psources = f gp.psources;
        birth_src = f gp.birth_src;
        baptism_src = f gp.baptism_src;
        death_src = f gp.death_src;
        burial_src = f gp.burial_src;
        pevents =
          List.map (fun e -> { e with epers_src = f e.epers_src }) gp.pevents;
      }
  | Occupation -> { gp with occupation = f gp.occupation }

let map_family field f (gf : (_, _, Driver.istr) gen_family) =
  match field with
  | Place ->
      {
        gf with
        marriage_place = f gf.marriage_place;
        fevents =
          List.map (fun e -> { e with efam_place = f e.efam_place }) gf.fevents;
      }
  | Source ->
      {
        gf with
        fsources = f gf.fsources;
        marriage_src = f gf.marriage_src;
        fevents =
          List.map (fun e -> { e with efam_src = f e.efam_src }) gf.fevents;
      }
  | Occupation -> gf

let action field =
  match field with Place -> "mq" | Source -> "ms" | Occupation -> "mo"

let usage ?(field = Place) base = Driver.dict_count base (dict field)

(* ------------------------------------------------------------------------ *)

type kind = New_place | Merge_existing of Driver.istr | Merge_batch

type item = {
  istr : Driver.istr;
  old_s : string;
  new_s : string;
  depths : int option list;
  kind : kind;
}

let group_by_new_s items =
  let h = Hashtbl.create 64 in
  let order = ref [] in
  List.iter
    (fun it ->
      match Hashtbl.find_opt h it.new_s with
      | Some l -> l := it :: !l
      | None ->
          order := it.new_s :: !order;
          Hashtbl.add h it.new_s (ref [ it ]))
    items;
  List.rev_map (fun s -> (s, List.rev !(Hashtbl.find h s))) !order

let rec plan conf base ~terms ~new_term occs =
  let inverted = Place.places_inverted conf in
  let new_term = PlaceTerms.clean_term new_term in
  (* occurrences grouped by place string *)
  let by_istr = IT.create 64 in
  List.iter
    (fun (istr, d) ->
      match IT.find_opt by_istr istr with
      | Some l -> l := d :: !l
      | None -> IT.add by_istr istr (ref [ d ]))
    occs;
  IT.fold
    (fun istr depths acc ->
      let old_s = Driver.sou base istr in
      match rewrite inverted ~terms ~new_term old_s !depths with
      | Some new_s when new_s <> old_s ->
          let new_s = Mutil.normalize_utf_8 new_s in
          { istr; old_s; new_s; depths = !depths; kind = New_place } :: acc
      | _ -> acc)
    by_istr []
  |> classify base

and classify base items =
  items
  |> List.sort (fun a b -> Place.compare_places a.old_s b.old_s)
  |> group_by_new_s
  |> List.concat_map (fun (new_s, members) ->
      (* an existing string keeps its text, even when it is renamed by this
         batch too: the substitution is simultaneous *)
      match Driver.find_string base new_s with
      | Some j ->
          List.map (fun it -> { it with kind = Merge_existing j }) members
      | None ->
          List.mapi
            (fun i it -> if i = 0 then it else { it with kind = Merge_batch })
            members)
  |> List.sort (fun a b -> Place.compare_places a.old_s b.old_s)

let plan_places ?new_s base ~target istrs =
  let target_s = Driver.sou base target in
  match new_s with
  | Some new_s
    when Mutil.normalize_utf_8 (PlaceTerms.clean_term new_s) <> target_s ->
      (* the complete place itself rewritten ("aaaa (bbbb)" for "aaaa,
         bbbb"): every place given, the target too when given, gets the new
         text *)
      let new_s = Mutil.normalize_utf_8 (PlaceTerms.clean_term new_s) in
      List.sort_uniq compare istrs
      |> List.filter_map (fun istr ->
          let old_s = Driver.sou base istr in
          if old_s = new_s then None
          else Some { istr; old_s; new_s; depths = []; kind = New_place })
      |> classify base
  | Some _ | None ->
      List.sort_uniq compare istrs
      |> List.filter_map (fun istr ->
          if Driver.Istr.equal istr target then None
          else
            Some
              {
                istr;
                old_s = Driver.sou base istr;
                new_s = target_s;
                depths = [];
                kind = Merge_existing target;
              })
      |> List.sort (fun a b -> Place.compare_places a.old_s b.old_s)

let places_of_env conf =
  List.filter_map
    (fun (k, (v : Adef.encoded_string)) ->
      let n = String.length k in
      if
        n > 1
        && k.[0] = 'p'
        && String.for_all
             (fun c -> c >= '0' && c <= '9')
             (String.sub k 1 (n - 1))
        && (v :> string) = "on"
      then Some (Driver.Istr.of_string (String.sub k 1 (n - 1)))
      else None)
    conf.env

(* ------------------------------------------------------------------------ *)

type result = {
  places : int;
  merged : int;
  persons : int;
  families : int;
  complete : bool;
}

let max_updates conf =
  match List.assoc_opt "max_nb_update" conf.base_env with
  | Some n -> (
      match int_of_string_opt n with Some n -> min n 5000 | None -> 5000)
  | None -> 5000

(* Same effect as m=MOD_DATA_OK on each place: the new string gets its own id
   (or the id of the existing string), the place fields of the persons and
   families pointing to the old id are moved to it, each modified person is
   recorded in the history with the action "mq", the spouses of a modified
   family too. Unlike MOD_DATA, all the places are done in a single pass over
   their records, read from the index of the dictionaries. At most
   [max_nb_update] records are modified: the rest is done by applying the same
   plan again. *)
let apply ?(field = Place) conf base items =
  let remap = IT.create 64 in
  List.iter
    (fun (new_s, members) ->
      let target =
        match members with
        | { kind = Merge_existing j; _ } :: _ -> j
        | _ -> Driver.insert_string base new_s
      in
      List.iter (fun it -> IT.replace remap it.istr target) members)
    (group_by_new_s items);
  let f i = match IT.find_opt remap i with Some j -> j | None -> i in
  let ipers, ifams =
    List.fold_left
      (fun (ipers, ifams) it ->
        let ps, fs = Driver.dict_records base (dict field) it.istr in
        (List.rev_append ps ipers, List.rev_append fs ifams))
      ([], []) items
  in
  let max = max_updates conf in
  let persons = ref 0 and families = ref 0 in
  let full () = !persons + !families >= max in
  let complete = ref true in
  let recorded = Hashtbl.create 64 in
  let record gp np =
    Hashtbl.replace recorded gp.key_index ();
    History.record conf base
      (U_Multi (Util.string_gen_person base gp, Util.string_gen_person base np))
      (action field)
  in
  List.iter
    (fun ip ->
      if full () then complete := false
      else
        let gp = Driver.gen_person_of_person (Driver.poi base ip) in
        let np = map_person field f gp in
        Driver.patch_person base ip np;
        incr persons;
        record gp np)
    (List.sort_uniq Driver.Iper.compare ipers);
  List.iter
    (fun ifam ->
      if full () then complete := false
      else
        let fam = Driver.foi base ifam in
        let gf = Driver.gen_family_of_family fam in
        let nf = map_family field f gf in
        Driver.patch_family base ifam nf;
        incr families;
        (* as MOD_DATA, which reaches the families through the spouses *)
        List.iter
          (fun ip ->
            if not (Hashtbl.mem recorded ip) then
              let gp = Driver.gen_person_of_person (Driver.poi base ip) in
              record gp gp)
          [ Driver.get_father fam; Driver.get_mother fam ])
    (List.sort_uniq Driver.Ifam.compare ifams);
  Util.commit_patches conf base;
  History.notify conf base (action field);
  {
    places = List.length items;
    merged = List.length (List.filter (fun it -> it.kind <> New_place) items);
    persons = !persons;
    families = !families;
    complete = !complete;
  }
