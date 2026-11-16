open Alcotest
open Def
open Geneweb
module Driver = Geneweb_db.Driver

let conf salt = { Config.empty with secret_salt = Some salt }
let ip i = Driver.Iper.of_string (string_of_int i)
let ifam i = Driver.Ifam.of_string (string_of_int i)

let person i =
  {
    (Mutil.empty_person "" "?") with
    first_name = "Jean";
    surname = "Dupont";
    notes = "Première ligne\nseconde ligne";
    key_index = ip i;
  }

let family i =
  let key fn = (fn, "Dupont", 0, Update.Link, "") in
  ( { (Mutil.empty_family "") with marriage_place = "Paris"; fam_index = ifam i },
    Adef.couple (key "Jean") (key "Marie"),
    { children = [| key "Paul" |] } )

let alter s i =
  String.mapi (fun j c -> if j = i then if c = '0' then '1' else '0' else c) s

let test_person_snapshot () =
  let s = UpdateConflict.person_snapshot (conf "salt") (person 1) in
  let read c i s = UpdateConflict.person_of_snapshot c (ip i) s in
  let dot = String.index s '.' in
  (check bool) "read back" true (read (conf "salt") 1 s = Some (person 1));
  (check bool) "other person" true (read (conf "salt") 2 s = None);
  (check bool) "other salt" true (read (conf "other") 1 s = None);
  (check bool) "empty" true (read (conf "salt") 1 "" = None);
  (check bool) "altered digest" true (read (conf "salt") 1 (alter s 0) = None);
  (check bool) "altered value" true
    (read (conf "salt") 1 (alter s (dot + 8)) = None);
  (check bool) "family snapshot" true
    (read (conf "salt") 1
       (UpdateConflict.family_snapshot (conf "salt") (family 1))
    = None)

let test_family_snapshot () =
  let s = UpdateConflict.family_snapshot (conf "salt") (family 3) in
  let read c i s = UpdateConflict.family_of_snapshot c (ifam i) s in
  (check bool) "read back" true (read (conf "salt") 3 s = Some (family 3));
  (check bool) "other family" true (read (conf "salt") 4 s = None);
  (check bool) "person snapshot" true
    (read (conf "salt") 1
       (UpdateConflict.person_snapshot (conf "salt") (person 1))
    = None)

let counts review =
  let count key =
    Yojson.Safe.Util.(
      List.length (to_list (member key (Yojson.Safe.from_string review))))
  in
  (count "changes", count "conflicts")

let pevent ?(place = "") epers_name =
  {
    epers_name;
    epers_date = Date.cdate_None;
    epers_place = place;
    epers_reason = "";
    epers_note = "";
    epers_src = "";
    epers_witnesses = [||];
  }

let test_merge_person () =
  let c = conf "salt" in
  let o =
    {
      (person 1) with
      occupation = "laboureur";
      pevents =
        [ pevent ~place:"Metz" Epers_Birth; pevent ~place:"Metz" Epers_Death ];
    }
  in
  let merge ?(snapshot = UpdateConflict.person_snapshot c o) a b =
    UpdateConflict.merge_person c ~snapshot a b
  in
  let p, review =
    merge { o with notes = "x" } { o with occupation = "meunier" }
  in
  (check string) "their change" "x" p.notes;
  (check string) "my change" "meunier" p.occupation;
  (check (pair int int)) "no conflict" (1, 0) (counts review);
  let p, review =
    merge { o with occupation = "tisserand" } { o with occupation = "meunier" }
  in
  (check string) "conflict keeps theirs" "tisserand" p.occupation;
  (check (pair int int)) "one conflict" (1, 1) (counts review);
  let birth, death =
    match o.pevents with [ b; d ] -> (b, d) | _ -> assert false
  in
  let p, review =
    merge
      {
        o with
        pevents = [ birth; pevent ~place:"Nancy" Epers_Residence; death ];
      }
      { o with pevents = [ birth; { death with epers_place = "Toul" } ] }
  in
  (check (list string))
    "inserted event"
    [ "Metz"; "Nancy"; "Toul" ]
    (List.map (fun e -> e.epers_place) p.pevents);
  (check (pair int int)) "event added" (1, 0) (counts review);
  let p, review =
    merge { o with pevents = [ birth ] } { o with pevents = [ birth; death ] }
  in
  (check int) "event deleted" 1 (List.length p.pevents);
  (check (pair int int)) "deletion" (1, 0) (counts review);
  let _, review =
    merge ~snapshot:""
      { o with occupation = "tisserand" }
      { o with occupation = "meunier" }
  in
  (check (pair int int)) "no snapshot" (0, 1) (counts review)

let test_merge_family () =
  let c = conf "salt" in
  let ((fam, cpl, _) as o) = family 3 in
  let key ?(var = "") fn = (fn, "Dupont", 0, Update.Link, var) in
  let snapshot = UpdateConflict.family_snapshot c o in
  let (_, _, des), review =
    UpdateConflict.merge_family c ~snapshot
      (fam, cpl, { children = [| key "Paul"; key "Jean" |] })
      ( fam,
        cpl,
        { children = [| key ~var:"ch1" "Paul"; key ~var:"ch2" "Luc" |] } )
  in
  (check (list string))
    "children" [ "Paul"; "Jean"; "Luc" ]
    (List.map (fun (fn, _, _, _, _) -> fn) (Array.to_list des.children));
  (check (pair int int)) "no conflict" (1, 0) (counts review)

let test_merge_note () =
  let c = conf "salt" in
  let merge o a b = UpdateConflict.merge_note c o a b in
  let o = "a\nb\nc\nd" in
  let s, review = merge (Some o) "a\nB\nc\nd" "a\nb\nc\nD" in
  (check string) "both changes" "a\nB\nc\nD" s;
  (check (pair int int)) "no conflict" (1, 0) (counts review);
  let s, review = merge (Some o) "a\nX\nc\nd" "a\nY\nc\nd" in
  (check string) "conflict keeps theirs" "a\nX\nc\nd" s;
  (check (pair int int)) "one conflict" (1, 1) (counts review);
  let text =
    Yojson.Safe.Util.(
      Yojson.Safe.from_string review
      |> member "conflicts" |> index 0 |> member "text")
  in
  (check (pair string string))
    "lines to swap" ("a\nX\nc", "a\nY\nc")
    Yojson.Safe.Util.
      (to_string (member "saved" text), to_string (member "mine" text));
  let _, review = merge None "a\nX" "a\nY" in
  (check (pair int int)) "no snapshot" (0, 1) (counts review);
  (check bool) "snapshot of the note" true
    (UpdateConflict.note_of_snapshot c "f"
       (UpdateConflict.note_snapshot c "f" o)
     = Some o
    && UpdateConflict.note_of_snapshot c "g"
         (UpdateConflict.note_snapshot c "f" o)
       = None)

let v =
  [
    ( "update-conflict",
      [
        test_case "person snapshot" `Quick test_person_snapshot;
        test_case "family snapshot" `Quick test_family_snapshot;
        test_case "person merge" `Quick test_merge_person;
        test_case "family merge" `Quick test_merge_family;
        test_case "note merge" `Quick test_merge_note;
      ] );
  ]
