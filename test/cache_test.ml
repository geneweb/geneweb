open Alcotest
module Driver = Geneweb_db.Driver
module Cache = Geneweb.Cache

let istr = Driver.Istr.of_string
let show = Option.map (List.map (fun (i, s) -> (Driver.Istr.to_string i, s)))
let entries = option (list (pair string string))

let in_temp_bases f () =
  let prev = Secure.base_dir () in
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("gw_cache_test_" ^ string_of_int (Random.bits ()))
  in
  Secure.set_base_dir dir;
  Fun.protect
    ~finally:(fun () ->
      Mutil.rm_rf dir;
      Secure.set_base_dir prev)
    f

let round_trip () =
  check entries "missing" None (show (Cache.read "b" "fnames"));
  Cache.write "b" "fnames" [ (istr "2", "Paul"); (istr "3", "Jean") ];
  check bool "exists" true (Cache.exists "b" "fnames");
  check entries "read"
    (Some [ ("2", "Paul"); ("3", "Jean") ])
    (show (Cache.read "b" "fnames"))

let update () =
  Cache.write "b" "fnames"
    [ (istr "2", "Paul"); (istr "3", "Jean"); (istr "4", "Jean-Paul") ];
  check bool "updated" true
    (Cache.update "b" "fnames" ~old_istr:(istr "2") ~new_istr:(istr "4")
       "Jean-Paul");
  check entries "re-keyed"
    (Some [ ("4", "Jean-Paul"); ("3", "Jean") ])
    (show (Cache.read "b" "fnames"));
  check bool "unknown istr" false
    (Cache.update "b" "fnames" ~old_istr:(istr "9") ~new_istr:(istr "5") "x");
  check bool "missing cache" false
    (Cache.update "b" "places" ~old_istr:(istr "2") ~new_istr:(istr "5") "x")

let v =
  [
    ( "cache",
      [
        test_case "Cache write and read" `Quick (in_temp_bases round_trip);
        test_case "Cache update" `Quick (in_temp_bases update);
      ] );
  ]
