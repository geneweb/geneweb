open Ged_v7
open Ged_v7.Types

(* TODO fix global State *)
(* TODO optimize: try to do only one iteration on substructure lists *)

(* type alias *)
type person = (int, int, int) Def.gen_person
type family = (int, int, int) Def.gen_family
type person_info = int * (person * int Def.gen_ascend * int Def.gen_union)
type family_info = int * (family * int Adef.gen_couple * int Def.gen_descend)

(* use [add_string] on each string to get it's index and build a index->value hashtbl;
   use make_strings to build the final array from the hashtbl *)
let make_strings, add_string =
  let count = ref (-1) in
  let ht = Hashtbl.create 1337 in
  let add_string s =
    match Hashtbl.find_opt ht s with
    | Some i -> i
    | None ->
        incr count;
        Hashtbl.replace ht s !count;
        !count
  in
  let make_strings () =
    let len = Hashtbl.length ht in
    let arr = Array.make len "" in
    Hashtbl.iter (fun k v -> arr.(v) <- k) ht;
    arr
  in
  (make_strings, add_string)

let string_empty, string_quest, string_x =
  let string_empty = add_string "" in
  let string_quest = add_string "?" in
  let string_x = add_string "x" in
  assert (string_empty = 0);
  assert (string_quest = 1);
  assert (string_x = 2);
  (string_empty, string_quest, string_x)

let empty_person : person =
  let empty = Mutil.empty_person string_empty string_quest in
  { empty with key_index = 0 }

let empty_family : family =
  let empty = Mutil.empty_family string_empty in
  { empty with fam_index = 0 }

let persons_infos : person_info list ref = ref []
let families_infos : family_info list ref = ref []

let add_dummy_person_info sex index =
  let info : person_info =
    ( index,
      ( { empty_person with sex; key_index = index; occ = index },
        Def.{ parents = None; consang = Adef.fix (-1) },
        Def.{ family = [||] } ) )
  in
  persons_infos := info :: !persons_infos

(* TODO should take ?family *)
let add_person_xref =
  let count = ref (-1) in
  let ht = Hashtbl.create 1337 in
  fun ?sex xref ->
    let sex = Option.value ~default:Def.Neuter sex in
    match xref with
    | None ->
        incr count;
        add_dummy_person_info sex !count;
        !count
    | Some s -> (
        match Hashtbl.find_opt ht s with
        | Some i -> i
        | None ->
            incr count;
            Hashtbl.replace ht s !count;
            !count)

let add_dummy_family_info index =
  let father = add_person_xref ~sex:Male None in
  let mother = add_person_xref ~sex:Female None in
  let info : family_info =
    ( index,
      ( { empty_family with fam_index = index },
        Adef.couple father mother,
        Def.{ children = [||] } ) )
  in
  families_infos := info :: !families_infos

(* TODO should take ?children *)
let add_family_xref =
  let count = ref (-1) in
  let ht = Hashtbl.create 1337 in
  fun xref ->
    match xref with
    | None ->
        incr count;
        add_dummy_family_info !count;
        !count
    | Some s -> (
        match Hashtbl.find_opt ht s with
        | Some i -> i
        | None ->
            incr count;
            Hashtbl.replace ht s !count;
            !count)

let build_persons_infos tree =
  let l =
    List.filter_map
      (function Types.Individual p -> Some p | _ -> None)
      tree.records
  in
  List.iter
    (fun (xref, substructures) ->
      let i = add_person_xref xref in
      let name =
        match
          List.find_map
            (function Types.Name name -> Some (fst name) | _ -> None)
            substructures
        with
        | None -> add_string ""
        | Some name -> add_string name
      in
      (* TODO get all person fields *)
      (* TODO parse name value and other special fields *)
      let person =
        { empty_person with first_name = name; occ = i; key_index = i }
      in
      let family_child =
        List.find_map
          (function
            | Types.Family_child (xref, _subs) ->
                let i = add_family_xref xref in
                Some i
            | _ -> None)
          substructures
      in
      let ascend = { Def.parents = family_child; consang = Adef.fix (-1) } in
      let family_spouse =
        List.filter_map
          (function
            | Types.Family_spouse (xref, _subs) ->
                let i = add_family_xref xref in
                Some i
            | _ -> None)
          substructures
        |> Array.of_list
      in
      let unions = { Def.family = family_spouse } in
      (* gen_person, gen_ascend, gen_union *)
      persons_infos := (i, (person, ascend, unions)) :: !persons_infos)
    l

let build_families_infos tree =
  let l =
    List.filter_map
      (function Types.Family fam -> Some fam | _ -> None)
      tree.records
  in
  List.iter
    (fun (xref, substructures) ->
      let i = add_family_xref xref in
      let family = { empty_family with fam_index = i } in
      let husband =
        List.find_map
          (function
            | Husband_xref (xref, _subs) ->
                let i = add_person_xref ~sex:Male xref in
                Some i
            | _ -> None)
          substructures
        |> Option.value ~default:0
      in
      let wife =
        List.find_map
          (function
            | Wife_xref (xref, _subs) ->
                let i = add_person_xref ~sex:Female xref in
                Some i
            | _ -> None)
          substructures
        |> Option.value ~default:0
      in
      let couple = Adef.couple husband wife in
      let children =
        List.filter_map
          (function
            | Children_xref (xref, _sub) ->
                let i = add_person_xref xref in
                Some i
            | _ -> None)
          substructures
        |> Array.of_list
      in
      families_infos :=
        (i, (family, couple, Def.{ children })) :: !families_infos)
    l

let arrays_of_tuples tuples =
  let len = List.length tuples in
  (* get first element for Array.make *)
  match tuples with
  | [] -> ([||], [||], [||])
  | (_i, (a, b, c)) :: _l ->
      let arr1 = Array.make len a in
      let arr2 = Array.make len b in
      let arr3 = Array.make len c in
      List.iter
        (fun (i, (a, b, c)) ->
          arr1.(i) <- a;
          arr2.(i) <- b;
          arr3.(i) <- c)
        tuples;
      (arr1, arr2, arr3)

let () =
  (*let _tree = parse "test/assets/same-sex-marriage.ged" in*)
  let tree = parse "test/assets/maximal70.ged" in
  let bname = "alaide" in
  let bnotes =
    (* ??.. *)
    Def.
      {
        nread = (fun s _ -> if s = "" then "this is the base note" else "");
        norigin_file = "";
        efiles = (fun _ -> []);
      }
  in
  Format.eprintf "BEFORE BUILD@\n";
  build_persons_infos tree;
  Format.eprintf "AFTER BUILD PERSONS@\n";
  build_families_infos tree;
  Format.eprintf "AFTER BUILD@\n";
  let persons_arrays = arrays_of_tuples !persons_infos in
  Format.eprintf "AFTER ARRAY OF TUPLES PERSONS@\n";
  let families_arrays = arrays_of_tuples !families_infos in
  Format.eprintf "AFTER ARRAY OF TUPLES@\n";
  let strings = make_strings () in
  let _base =
    Gwdb.make bname [] (persons_arrays, families_arrays, strings, bnotes)
  in
  ()
