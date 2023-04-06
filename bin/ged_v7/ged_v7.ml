open Ged_v7
open Ged_v7.Types

(* TODO no fix global State *)

let make_index_ht () =
  let count = ref (-1) in
  let ht = Hashtbl.create 1337 in
  ht,fun xref ->
    match Hashtbl.find_opt ht xref with
    | Some i -> i
    | None ->
        incr count;
        Hashtbl.replace ht xref !count;
        !count

let make_strings, add_string =
  let ht_strings, add_string = make_index_ht () in
  let make_strings () =
    let len = Hashtbl.length ht_strings in
    let arr = Array.make len "" in
    Hashtbl.iter (fun k v -> arr.(v) <- k) ht_strings;
    arr
  in
  let string_empty = add_string "" in
  let string_quest = add_string "?" in
  let string_x = add_string "x" in
  assert (string_empty = 0);
  assert (string_quest = 1);
  assert (string_x = 2);
  make_strings, add_string

let empty_person : (int,int,int) Def.gen_person =
  let empty = Mutil.empty_person (add_string "") (add_string "") in
  { empty with key_index = 0}

let empty_family : (int,int,int) Def.gen_family =
  let empty = Mutil.empty_family (add_string "") in
  { empty with fam_index = 0 }

let add_family =
  snd (make_index_ht ())
let add_person =
  snd (make_index_ht ())

let make_persons tree =
  let arrays =
    List.filter_map
      (function Types.Individual p -> Some p | _ -> None)
      tree.records
    |> List.map (fun (xref, substructures) ->
           let i = add_person xref in
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
           (* TODO parse name value *)
           let person =
             { empty_person with first_name = name; occ = i; key_index = i }
           in
           let family_child =
             List.find_map
               (function
                 | Types.Family_child (xref, _subs) ->
                     let i = add_family xref in
                     Some i
                 | _ -> None)
               substructures
           in
           let ascend =
             { Def.parents = family_child; consang = Adef.fix (-1) }
           in
           let family_spouse =
             List.filter_map
               (function
                 | Types.Family_spouse (xref, _subs) ->
                     let i = add_family xref in
                     Some i
                 | _ -> None)
               substructures
             |> Array.of_list
           in
           let unions = { Def.family = family_spouse } in
           (person, ascend, unions)
           (* gen_person, gen_ascend, gen_union *))
    |> (* unzip and to_array *)
    List.fold_left
      (fun (aa, bb, cc) (a, b, c) -> (a :: aa, b :: bb, c :: cc))
      ([], [], [])
    |> fun (aa, bb, cc) -> (Array.of_list aa, Array.of_list bb, Array.of_list cc)
  in
  let aa,_,_ = arrays in
  Format.eprintf "@.persons len : %d@\n" (Array.length aa);
  arrays

let make_families tree =
  let arrays =
    List.filter_map
      (function Types.Family fam -> Some fam | _ -> None)
      tree.records
    |> List.map (fun (xref, substructures) ->
           let i = add_family xref in
           let family = { empty_family with fam_index = i } in
           let husband =
             List.find_map
               (function
                 | Husband_xref (xref, _subs) ->
                     let i = add_person xref in
                     Some i
                 | _ -> None)
               substructures
             |> Option.value ~default:0
           in
           let wife =
             List.find_map
               (function
                 | Wife_xref (xref, _subs) ->
                     let i = add_person xref in
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
                     let i = add_person xref in
                     Some i
                 | _ -> None)
               substructures
             |> Array.of_list
           in
           (* TODO add i to tuple here, use it to make array the good way *)
           (family, couple, Def.{ children })
           (*
  ((int, int, int) Def.gen_family
    * int Def.gen_couple
    * int Def.gen_descend )
    *))
    |> (* unzip and to_array *)
    (* TODO no; smae for person *)
    List.fold_left
      (fun (aa, bb, cc) (a, b, c) -> (a :: aa, b :: bb, c :: cc))
      ([], [], [])
    |> fun (aa, bb, cc) -> (Array.of_list aa, Array.of_list bb, Array.of_list cc)
  in
  let aa,_,_ = arrays in
  Format.eprintf "@.families len : %d@\n" (Array.length aa);
  arrays

let () =
  (* ascends : parent_family_index array *)
  (* unions :  {family_in_which_im_a_parent_index array} array *)
  (* descends : {child_index array} array *)
  (* couples : array de (Adef.couple father_index mother_index) *)
  (* strings : string array qui contient tt les strings possible, les strings dans les autres structs sont reference par leur index dans strings *)
  (* bname -> particles list -> arrays -> strings -> bnotes -> base *)
  (*
  string ->
  string list ->
  ((int, int, int) Def.gen_person array
  * int Def.gen_ascend array
  * int Def.gen_union array)
  * ((int, int, int) Def.gen_family array
    * int Def.gen_couple array
    * int Def.gen_descend array)
  * string array
  * Def.base_notes ->
    *)
  let tree = parse "test/assets/same-sex-marriage.ged" in
  let bname = "alaide" in
  let bnotes =
    (* ??.. *)
    Def.
      {
        nread = (fun s _ -> if s = "" then "base note blabla" else "");
        norigin_file = "";
        efiles = (fun _ -> []);
      }
  in
  let persons_arrays = make_persons tree in
  let families_arrays = make_families tree in
  let strings = make_strings () in
  let _base =
    Gwdb.make bname [] (persons_arrays, families_arrays, strings, bnotes)
  in
  ()
