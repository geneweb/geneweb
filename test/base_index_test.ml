open Alcotest
open Def
module Driver = Geneweb_db.Driver
module Dict = Geneweb_db.Dict
module Outbase = Geneweb_db.Outbase
module BaseIndex = Geneweb_db.BaseIndex
module Collection = Geneweb_db.Collection

let particles_txt = [ "de_"; "d'" ]
let particles = Mutil.compile_particles particles_txt

let strings =
  [|
    "";
    "?";
    "Jean";
    "Jeanne";
    "Jean-Pierre";
    "de La Tour";
    "La Tour";
    "Dupont";
    "d'Arc";
    "Paris";
    "[Montmartre] - Paris";
    "Paris, France";
    "Lyon";
    "Archives départementales de la Haute-Garonne, registres paroissiaux de \
     Toulouse, Saint-Étienne";
    "Archives départementales de la Haute-Garonne, registres paroissiaux de \
     Toulouse, Saint-Sernin";
    "laboureur";
    "comte";
    "Lyon, Rhône";
    "Élise";
    "Ève";
  |]

let title ident place =
  {
    t_name = Tnone;
    t_ident = ident;
    t_place = place;
    t_date_start = Date.cdate_None;
    t_date_end = Date.cdate_None;
    t_nth = 0;
  }

let persons =
  Array.mapi
    (fun key_index
         ( first_name,
           surname,
           occupation,
           birth_place,
           death_place,
           birth_src,
           titles ) ->
      {
        (Mutil.empty_person 0 1) with
        key_index;
        first_name;
        surname;
        occupation;
        birth_place;
        death_place;
        birth_src;
        titles;
      })
    [|
      (2, 5, 15, 9, 10, 13, []);
      (3, 6, 0, 12, 0, 14, []);
      (4, 7, 15, 9, 0, 0, [ title 16 5 ]);
      (18, 8, 0, 11, 0, 0, []);
      (19, 7, 0, 17, 0, 0, []);
      (1, 1, 0, 0, 0, 0, []);
      (2, 7, 0, 0, 12, 13, []);
    |]

let couples = [| (0, 1); (2, 3); (5, 5) |]

let families =
  [|
    {
      (Mutil.empty_family 0) with
      fam_index = 0;
      marriage_place = 9;
      fsources = 13;
    };
    { (Mutil.empty_family 0) with fam_index = 1; marriage_place = 12 };
    { (Mutil.empty_family 0) with fam_index = -1; marriage_place = 11 };
  |]

let with_base name budget k =
  let n = Array.length persons in
  let data =
    ( ( persons,
        Array.make n { parents = None; consang = Adef.no_consang },
        Array.init n (fun i ->
            {
              family =
                Array.of_list
                  (List.filter
                     (fun f -> fst couples.(f) = i || snd couples.(f) = i)
                     [ 0; 1 ]);
            }) ),
      ( families,
        Array.map (fun (f, m) -> Adef.couple f m) couples,
        Array.map (fun _ -> { children = [||] }) couples ),
      strings,
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) } )
  in
  Outbase.set_index_budget budget;
  Fun.protect ~finally:(fun () ->
      Outbase.set_index_budget BaseIndex.default_budget)
  @@ fun () -> Driver.make name particles_txt data k

(* Entries of [d] by scanning the records of [base]: key, string id and
   records. *)
let scan base d =
  let tbl = Hashtbl.create 16 in
  let add r istr =
    if istr <> 0 || d = Dict.First_name || d = Dict.Surname then
      Hashtbl.replace tbl istr
        (r :: Option.value ~default:[] (Hashtbl.find_opt tbl istr))
  in
  let add_all r l =
    List.iter (add r) (List.sort_uniq Int.compare (List.map Driver.Istr.hash l))
  in
  Collection.iter
    (fun ip ->
      add_all
        (2 * Driver.Iper.hash ip)
        (Dict.person_strings d
           (Driver.gen_person_of_person (Driver.poi base ip))))
    (Driver.ipers base);
  Collection.iter
    (fun ifam ->
      add_all
        ((2 * Driver.Ifam.hash ifam) + 1)
        (Dict.family_strings d
           (Driver.gen_family_of_family (Driver.foi base ifam))))
    (Driver.ifams base);
  Hashtbl.fold
    (fun istr refs l ->
      let s = Driver.sou base (Driver.Istr.of_string (string_of_int istr)) in
      (Dict.key particles d s, istr, List.sort Int.compare refs) :: l)
    tbl []
  |> List.sort compare

let group_of p k =
  let lp = String.length p in
  let g =
    if String.length k = lp then p
    else
      String.sub k 0 (lp + Uchar.utf_decode_length (String.get_utf_8_uchar k lp))
  in
  let n = String.length g in
  if n > lp && g.[n - 1] = '\000' then p else g

let scan_groups entries p =
  List.fold_left
    (fun acc (k, _, _) ->
      if not (String.starts_with ~prefix:p k) then acc
      else
        let g = group_of p k in
        match acc with
        | (g', n) :: l when g' = g -> (g', n + 1) :: l
        | _ -> (g, 1) :: acc)
    [] entries
  |> List.rev

let prefixes entries =
  List.fold_left
    (fun acc (k, _, _) ->
      List.rev_append (List.init (String.length k + 1) (String.sub k 0)) acc)
    [ "Z"; "\xFF"; "Paris, F"; "La T" ]
    entries
  |> List.sort_uniq compare

let check_dict base d =
  let name = Dict.to_string d in
  let entries = scan base d in
  let istrs l = List.map Driver.Istr.hash l in
  List.iter
    (fun p ->
      let selected =
        List.filter (fun (k, _, _) -> String.starts_with ~prefix:p k) entries
      in
      (check int)
        (Printf.sprintf "%s size %S" name p)
        (List.length selected)
        (Driver.dict_size base d p);
      (check (list int))
        (Printf.sprintf "%s entries %S" name p)
        (List.map (fun (_, i, _) -> i) selected)
        (istrs (Driver.dict_entries base d p));
      (check (list (pair string int)))
        (Printf.sprintf "%s groups %S" name p)
        (scan_groups entries p)
        (Driver.dict_groups base d p))
    (prefixes entries);
  List.iter
    (fun (_, istr, refs) ->
      let ipers, ifams =
        Driver.dict_records base d (Driver.Istr.of_string (string_of_int istr))
      in
      (check (list int))
        (Printf.sprintf "%s records %d" name istr)
        refs
        (List.sort Int.compare
           (List.rev_append
              (List.map (fun i -> 2 * Driver.Iper.hash i) ipers)
              (List.map (fun i -> (2 * Driver.Ifam.hash i) + 1) ifams))))
    entries;
  let check_spi spi =
    let rec walk acc i =
      match i with
      | None -> List.rev acc
      | Some i ->
          walk
            (Driver.Istr.hash i :: acc)
            (try Some (Driver.spi_next spi i) with Not_found -> None)
    in
    (check (list int))
      (name ^ " spi order")
      (List.map (fun (_, i, _) -> i) entries)
      (walk [] (try Some (Driver.spi_first spi "") with Not_found -> None));
    List.iter
      (fun (_, istr, refs) ->
        (check (list int))
          (Printf.sprintf "%s spi_find %d" name istr)
          (List.map (fun r -> r / 2) refs)
          (List.sort Int.compare
             (List.map Driver.Iper.hash
                (Driver.spi_find spi
                   (Driver.Istr.of_string (string_of_int istr))))))
      entries
  in
  match d with
  | Dict.First_name -> check_spi (Driver.persons_of_first_name base)
  | Dict.Surname -> check_spi (Driver.persons_of_surname base)
  | Dict.(
      ( First_name_alias | Surname_alias | Public_name | Qualifier | Alias
      | Occupation | Place | Source | Title | Domain )) ->
      ()

let index name budget () =
  with_base name budget @@ fun base -> List.iter (check_dict base) Dict.all

let examples () =
  with_base "base_index_examples" BaseIndex.default_budget @@ fun base ->
  let istr i = Driver.Istr.of_string (string_of_int i) in
  let istrs l = List.map Driver.Istr.hash l in
  (check (list int))
    "places in key order" [ 12; 17; 9; 10; 11 ]
    (istrs (Driver.dict_entries base Dict.Place ""));
  (check (list (pair string int)))
    "place groups"
    [ ("Paris", 2); ("Paris,", 1) ]
    (Driver.dict_groups base Dict.Place "Paris");
  (check (list (pair string int)))
    "first name groups"
    [ ("?", 1); ("J", 3); ("È", 1); ("É", 1) ]
    (Driver.dict_groups base Dict.First_name "");
  (check (list (pair string int)))
    "exact first name"
    [ ("Jean", 1); ("Jean-", 1); ("Jeann", 1) ]
    (Driver.dict_groups base Dict.First_name "Jean");
  (check int) "surnames after particle" 2
    (Driver.dict_size base Dict.Surname "La T");
  (check int) "domain after particle" 1
    (Driver.dict_size base Dict.Domain "La T");
  let ipers, ifams = Driver.dict_records base Dict.Place (istr 9) in
  (check (list int))
    "persons in Paris" [ 0; 2 ]
    (List.map Driver.Iper.hash ipers);
  (check (list int)) "families in Paris" [ 0 ] (List.map Driver.Ifam.hash ifams);
  (check (list int))
    "deleted family" [ 3 ]
    (List.map Driver.Iper.hash
       (fst (Driver.dict_records base Dict.Place (istr 11))));
  (check (list int))
    "unused string" []
    (List.map Driver.Iper.hash
       (fst (Driver.dict_records base Dict.Place (istr 16))))

let patched () =
  with_base "base_index_patched" 40 @@ fun base ->
  let iper i = Driver.Iper.of_string (string_of_int i) in
  let ifam i = Driver.Ifam.of_string (string_of_int i) in
  let istr i = Driver.Istr.of_string (string_of_int i) in
  let person i = Driver.gen_person_of_person (Driver.poi base (iper i)) in
  let family i = Driver.gen_family_of_family (Driver.foi base (ifam i)) in
  let check_all () = List.iter (check_dict base) Dict.all in
  let lyon69 = Driver.insert_string base "Lyon (69)" in
  Driver.patch_person base (iper 6) { (person 6) with death_place = lyon69 };
  Driver.patch_person base (iper 1) { (person 1) with birth_place = istr 9 };
  ignore
    (Driver.replace_string base "Lyon, Rhône" "Lyon, Rhône-Alpes" : Driver.istr);
  Driver.delete_person base (iper 3);
  ignore
    (Driver.insert_person_with_union_and_ascendants base
       {
         (person 5) with
         key_index = Driver.Iper.dummy;
         first_name = Driver.insert_string base "Zoé";
       }
       Driver.no_ascend Driver.no_union
      : Driver.iper);
  Driver.patch_family base (ifam 0) { (family 0) with marriage_place = istr 12 };
  Driver.commit_patches base;
  check_all ();
  (check (list (pair string int)))
    "moved and new places"
    [ ("Lyon", 1); ("Lyon ", 1); ("Lyon,", 1) ]
    (Driver.dict_groups base Dict.Place "Lyon");
  (check int) "place left out" 0 (Driver.dict_size base Dict.Place "Paris,");
  Driver.patch_person base (iper 0) { (person 0) with occupation = istr 0 };
  ignore (Driver.replace_string base "Paris" "Paris (75)" : Driver.istr);
  Driver.commit_patches base;
  check_all ();
  (check (list int))
    "records of a moved place" [ 0; 1; 2 ]
    (List.sort Int.compare
       (List.map Driver.Iper.hash
          (fst (Driver.dict_records base Dict.Place (istr 9)))))

let shared () =
  let name = "base_index_shared" in
  let iper = Driver.Iper.of_string "6" in
  let move place base =
    Driver.patch_person base iper
      {
        (Driver.gen_person_of_person (Driver.poi base iper)) with
        death_place = Driver.insert_string base place;
      };
    Driver.commit_patches base
  in
  let count base = Driver.dict_size base Dict.Place "Lyon (69)" in
  with_base name 40 (move "Lyon (69)");
  Driver.with_database name (fun base ->
      (check int) "next reader" 1 (count base));
  Driver.with_database name (move "Paris");
  Driver.with_database name (fun base ->
      (check int) "reader after a commit of another one" 0 (count base);
      List.iter (check_dict base) Dict.all)

let v =
  [
    ( "base-index",
      [
        test_case "Index examples" `Quick examples;
        test_case "Index corrected by the patches" `Quick patched;
        test_case "Index shared by the readers" `Quick shared;
        test_case "Index with blocks of 1" `Quick
          (index "base_index_default" BaseIndex.default_budget);
        test_case "Index with small blocks" `Quick (index "base_index_small" 40);
        test_case "Index with one block" `Quick (index "base_index_1" 1);
      ] );
  ]
