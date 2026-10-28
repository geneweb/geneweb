open Config
open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
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

let valid_new_term s =
  String.trim s <> ""
  && not (String.exists (fun c -> c = '[' || c = ']' || c = '(' || c = ')') s)

let rewrite_with inverted ~terms f raw depths =
  let comps = Place.components inverted raw in
  let main = List.filter (fun (c : Place.component) -> not c.suburb) comps in
  let pick = function
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
           String.sub s 0 c.pos ^ f c.text
           ^ String.sub s (c.pos + c.len) (String.length s - c.pos - c.len))
         raw spans)

let rewrite inverted ~terms ~new_term raw depths =
  rewrite_with inverted ~terms (Fun.const new_term) raw depths

(* ------------------------------------------------------------------------ *)

type usage = int ref IT.t

let uses u i = match IT.find_opt u i with Some r -> !r | None -> 0

let person_places p =
  Driver.get_birth_place p :: Driver.get_baptism_place p
  :: Driver.get_death_place p :: Driver.get_burial_place p
  :: List.map (fun e -> e.epers_place) (Driver.get_pevents p)

let family_places f =
  Driver.get_marriage_place f
  :: List.map (fun e -> e.efam_place) (Driver.get_fevents f)

let usage base =
  let u = IT.create 4096 in
  let count l =
    List.filter (fun i -> not (Driver.Istr.is_empty i)) l
    |> List.sort_uniq compare
    |> List.iter (fun i ->
        match IT.find_opt u i with
        | Some r -> incr r
        | None -> IT.add u i (ref 1))
  in
  Collection.iter
    (fun ip -> count (person_places (Driver.poi base ip)))
    (Driver.ipers base);
  Collection.iter
    (fun ifam -> count (family_places (Driver.foi base ifam)))
    (Driver.ifams base);
  u

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

let plan conf base ~terms ~new_term occs =
  let inverted = Place.places_inverted conf in
  let new_term = String.trim new_term in
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

let plan_places base ~target istrs =
  let new_s = Driver.sou base target in
  List.sort_uniq compare istrs
  |> List.filter_map (fun istr ->
      if Driver.Istr.equal istr target then None
      else
        Some
          {
            istr;
            old_s = Driver.sou base istr;
            new_s;
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
   the base. At most [max_nb_update] records are modified: the rest is done by
   applying the same plan again. *)
let apply conf base items =
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
  let touched l = List.exists (fun i -> IT.mem remap i) l in
  let max = max_updates conf in
  let persons = ref 0 and families = ref 0 in
  let full () = !persons + !families >= max in
  let complete = ref true in
  let recorded = Hashtbl.create 64 in
  let record gp np =
    Hashtbl.replace recorded gp.key_index ();
    History.record conf base
      (U_Multi
         (Util.string_gen_person base gp, Util.string_gen_person base np, false))
      "mq"
  in
  Collection.iter
    (fun ip ->
      let p = Driver.poi base ip in
      if touched (person_places p) then (
        if full () then complete := false
        else
          let gp = Driver.gen_person_of_person p in
          let np =
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
          in
          Driver.patch_person base ip np;
          incr persons;
          record gp np))
    (Driver.ipers base);
  Collection.iter
    (fun ifam ->
      let fam = Driver.foi base ifam in
      if touched (family_places fam) then (
        if full () then complete := false
        else
          let gf = Driver.gen_family_of_family fam in
          let nf =
            {
              gf with
              marriage_place = f gf.marriage_place;
              fevents =
                List.map
                  (fun e -> { e with efam_place = f e.efam_place })
                  gf.fevents;
            }
          in
          Driver.patch_family base ifam nf;
          incr families;
          (* as MOD_DATA, which reaches the families through the spouses *)
          List.iter
            (fun ip ->
              if not (Hashtbl.mem recorded ip) then
                let gp = Driver.gen_person_of_person (Driver.poi base ip) in
                record gp gp)
            [ Driver.get_father fam; Driver.get_mother fam ]))
    (Driver.ifams base);
  Util.commit_patches conf base;
  History.notify conf base "mq";
  {
    places = List.length items;
    merged = List.length (List.filter (fun it -> it.kind <> New_place) items);
    persons = !persons;
    families = !families;
    complete = !complete;
  }
