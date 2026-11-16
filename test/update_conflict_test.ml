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

let v =
  [
    ( "update-conflict",
      [
        test_case "person snapshot" `Quick test_person_snapshot;
        test_case "family snapshot" `Quick test_family_snapshot;
      ] );
  ]
