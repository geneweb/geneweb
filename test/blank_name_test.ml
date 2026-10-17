open Alcotest
open Def
open Geneweb
module Driver = Geneweb_db.Driver
module Dutil = Geneweb_db.Dutil

let quest = 1
let nn = 2
let jean = 3
let dupont = 4
let blank = 5
let n_dot = 6
let farmer = 7
let names = [ "NN"; "N.N."; "Ne" ]
let wizard = Config.{ empty with wizard = true }
let iper i = Driver.Iper.of_string (string_of_int i)
let ifam i = Driver.Ifam.of_string (string_of_int i)
let istr i = Driver.Istr.of_string (string_of_int i)
let ids hash l = List.sort compare (List.map hash l)

let with_base bname k =
  let persons =
    Array.mapi
      (fun key_index (first_name, surname, occ, occupation) ->
        {
          (Mutil.empty_person 0 1) with
          key_index;
          first_name;
          surname;
          occ;
          occupation;
        })
      [|
        (nn, nn, 0, 0);
        (nn, nn, 1, farmer);
        (nn, nn, 2, 0);
        (jean, dupont, 0, 0);
        (nn, nn, 3, 0);
        (quest, quest, 0, 0);
        (quest, quest, 0, farmer);
        (nn, blank, 0, 0);
        (n_dot, nn, 0, 0);
        (nn, nn, 4, 0);
        (nn, nn, 5, 0);
        (nn, nn, 6, 0);
        (quest, quest, 0, 0);
        (jean, dupont, 1, 0);
        (jean, dupont, 2, 0);
        (nn, nn, 7, 0);
        (nn, nn, 8, 0);
        (jean, dupont, 3, 0);
      |]
  in
  let spouses = [| (3, 4); (9, 10); (11, 12); (15, 16) |] in
  let children = [| [| 2; 5 |]; [||]; [| 13; 14 |]; [| 17 |] |] in
  let families = [ 0; 1; 2; 3 ] in
  let n = Array.length persons in
  let data =
    ( ( persons,
        Array.init n (fun i ->
            {
              Driver.no_ascend with
              parents =
                List.find_opt (fun f -> Array.mem i children.(f)) families;
            }),
        Array.init n (fun i ->
            {
              family =
                Array.of_list
                  (List.filter
                     (fun f -> fst spouses.(f) = i || snd spouses.(f) = i)
                     families);
            }) ),
      ( Array.init 4 (fun i -> { (Mutil.empty_family 0) with fam_index = i }),
        Array.map (fun (f, m) -> Adef.couple f m) spouses,
        Array.map (fun children -> { Def.children }) children ),
      [| ""; "?"; "NN"; "Jean"; "DUPONT"; "??"; "N."; "farmer" |],
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) } )
  in
  Driver.make bname [] data k

let check_lists msg (d, r, cd, cr)
    {
      UnknownPersons.to_delete;
      to_review;
      couples_to_delete;
      couples_to_review;
    } =
  let chk name expected got =
    (check (list int)) (msg ^ ", " ^ name) expected got
  in
  chk "to delete" d (ids Driver.Iper.hash to_delete);
  chk "to review" r (ids Driver.Iper.hash to_review);
  chk "couples to delete" cd (ids Driver.Ifam.hash couples_to_delete);
  chk "couples to review" cr (ids Driver.Ifam.hash couples_to_review)

let with_history_dir name k =
  let dir = Filename.concat (Sys.getcwd ()) name in
  Mutil.rm_rf dir;
  Secure.add_assets dir;
  Fun.protect ~finally:(fun () -> Mutil.rm_rf dir) @@ fun () ->
  k dir
    Config.
      {
        empty with
        base_env = [ ("history_diff", "yes"); ("history_path", dir) ];
      }

let write_file path content =
  Filesystem.create_dir ~parent:true (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> output_string oc content)

let read_file path =
  if Sys.file_exists path then
    Some (In_channel.with_open_bin path In_channel.input_all)
  else None

let test_blank_names () =
  List.iter
    (fun s ->
      (check string) ("lower " ^ s) "" (Name.lower s);
      (check int) ("bucket " ^ s) (Dutil.name_index "") (Dutil.name_index s))
    [ "?"; "??"; "-"; "? ?"; "(?)" ];
  (check string) "placeholder" "n.n." (Name.lower "N.N.")

let test_keys () =
  let key fn sn occ =
    let k = HistoryDiff.history_file fn sn occ in
    (check string) ("image key " ^ k) k (Image.key_dir_basename fn sn occ);
    k
  in
  (check string) "spaces" "jean_marc.2.de_la_tour"
    (key "Jean-Marc" "de La Tour" 2);
  (check string) "blank surname" "jean.0." (key "Jean" "??" 0);
  (check string) "shared key" (key "Jean" "?" 0) (key "Jean" "-" 0)

let test_index_patches () =
  with_base "index_patches_test" @@ fun base ->
  let surname s =
    ids Driver.Iper.hash
      (Driver.spi_find (Driver.persons_of_surname base) (istr s))
  in
  let gen i = Driver.gen_person_of_person (Driver.poi base (iper i)) in
  let p0 = gen 0 and p1 = gen 1 in
  Driver.patch_person base (iper 0) { p0 with occupation = istr farmer };
  Driver.patch_person base (iper 1) { p1 with surname = istr dupont };
  Driver.commit_patches base;
  (check (list int))
    "same surname"
    [ 0; 2; 4; 8; 9; 10; 11; 15; 16 ]
    (surname nn);
  (check (list int)) "new surname" [ 1; 3; 13; 14; 17 ] (surname dupont);
  Driver.patch_person base (iper 1) { p1 with surname = istr blank };
  Driver.commit_patches base;
  (check (list int)) "former surname" [ 3; 13; 14; 17 ] (surname dupont);
  (check (list int)) "blank surname" [ 1; 7 ] (surname blank)

let test_sort_and_delete () =
  with_base "unknown_sort_test" @@ fun base ->
  let l = UnknownPersons.sort wizard base names in
  check_lists "before" ([ 0; 4; 7 ], [ 1; 2; 6 ], [ 1; 3 ], [ 2 ]) l;
  let { UnknownPersons.to_delete; couples_to_delete; _ } = l in
  List.iter (Driver.delete_person_rec base) to_delete;
  List.iter
    (fun f ->
      let fam = Driver.foi base f in
      List.iter
        (Driver.delete_person_rec base)
        [ Driver.get_father fam; Driver.get_mother fam ])
    couples_to_delete;
  Driver.commit_patches base;
  check_lists "after"
    ([], [ 1; 2; 6 ], [], [ 2 ])
    (UnknownPersons.sort wizard base names);
  let p i = Driver.poi base (iper i) in
  let fam f = Driver.foi base (ifam f) in
  let alone i = Util.is_empty_name (p i) && Driver.get_family (p i) = [||] in
  (check bool) "isolated persons deleted" true (alone 0 && alone 7);
  (check bool) "spouse with children kept as ? ?" true
    (Util.is_empty_name (p 4)
    && Driver.Iper.equal (Driver.get_mother (fam 0)) (iper 4));
  (check bool) "couples without data removed" true
    (List.for_all
       (fun f -> Driver.Ifam.is_dummy (Driver.get_ifam (fam f)))
       [ 1; 3 ]
    && List.for_all alone [ 9; 10; 15; 16 ]);
  (check bool) "only child kept without parents" true
    (Option.is_none (Driver.get_parents (p 17))
    && not (Util.is_empty_name (p 17)))

let test_portrait () =
  let dir = "unknown_test_portraits" in
  let portraits_d = !GWPARAM.portraits_d in
  Filesystem.create_dir ~parent:true dir;
  Out_channel.with_open_bin (Filename.concat dir "nn.0.nn.jpg") ignore;
  GWPARAM.portraits_d := Fun.const dir;
  Fun.protect ~finally:(fun () ->
      GWPARAM.portraits_d := portraits_d;
      Mutil.rm_rf dir)
  @@ fun () ->
  with_base "unknown_portrait_test" @@ fun base ->
  check_lists "portrait"
    ([ 4; 7 ], [ 0; 1; 2; 6 ], [ 1; 3 ], [ 2 ])
    (UnknownPersons.sort wizard base names)

let test_archive_deleted () =
  with_history_dir "archive_test_history" @@ fun dir conf ->
  let deleted =
    Config.
      {
        conf with
        base_env = [ ("history_path", Filename.concat dir "_deleted") ];
      }
  in
  let archive fn sn content =
    let key = HistoryDiff.history_file fn sn 0 in
    let path = HistoryDiff.history_path conf key in
    write_file path content;
    HistoryDiff.archive_deleted conf
      { (Mutil.empty_person "" "?") with first_name = fn; surname = sn };
    (read_file path, read_file (HistoryDiff.history_path deleted key))
  in
  let chk = check (pair (option string) (option string)) in
  chk "moved" (None, Some "first") (archive "Jean" "DUPONT" "first");
  chk "appended" (None, Some "firstsecond") (archive "Jean" "DUPONT" "second");
  chk "shared key kept" (Some "shared", None) (archive "Jean" "??" "shared")

let test_key_change () =
  with_base "history_key_test" @@ fun base ->
  with_history_dir "key_change_test_history" @@ fun _ conf ->
  let rename i surname =
    let o =
      Util.string_gen_person base
        (Driver.gen_person_of_person (Driver.poi base (iper i)))
    in
    let path sn =
      HistoryDiff.history_path conf
        (HistoryDiff.history_file o.first_name sn o.occ)
    in
    write_file (path o.surname) "old";
    HistoryDiff.record_diff conf base (U_Modify_person (o, { o with surname }));
    ( read_file (path o.surname),
      Option.map (String.starts_with ~prefix:"old") (read_file (path surname))
    )
  in
  let chk = check (pair (option string) (option bool)) in
  chk "unique key" (None, Some true) (rename 3 "DURAND");
  chk "shared key" (Some "old", Some false) (rename 7 "DURAND")

let test_acces () =
  with_base "index_links_test" @@ fun base ->
  let link access_by_key i =
    (Util.acces
       Config.{ empty with access_by_key }
       base
       (Driver.poi base (iper i))
      :> string)
  in
  (check string) "key" "p=jean&n=dupont" (link true 3);
  (check string) "key and occurrence" "p=jean&n=dupont&oc=1" (link true 13);
  (check string) "unknown person" "i=5" (link true 5);
  (check string) "index" "i=13" (link false 13)

let v =
  [
    ( "blank-names",
      [
        test_case "Name.lower and name_index" `Quick test_blank_names;
        test_case "history and image keys" `Quick test_keys;
      ] );
    ( "unknown-persons",
      [
        test_case "name index with patches" `Quick test_index_patches;
        test_case "sort and delete" `Quick test_sort_and_delete;
        test_case "portrait is data" `Quick test_portrait;
      ] );
    ( "history-files",
      [
        test_case "archive of deleted persons" `Quick test_archive_deleted;
        test_case "key change" `Quick test_key_change;
      ] );
    ("index-links", [ test_case "Util.acces" `Quick test_acces ]);
  ]
