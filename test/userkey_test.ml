open Alcotest
open Def
module Driver = Geneweb_db.Driver
module GWPARAM = Geneweb.GWPARAM

let s_paul = 2
let s_martin = 3
let s_jean_pierre = 4
let s_louis = 5
let s_particled = 6

let person key_index first_name surname occ =
  { (Mutil.empty_person 0 1) with occ; key_index; first_name; surname }

let with_base k =
  let persons =
    [|
      person 0 s_paul s_martin 0;
      person 1 s_jean_pierre s_martin 0;
      person 2 s_louis s_particled 0;
      person 3 s_paul s_martin 1;
    |]
  in
  let ascends =
    Array.init 4 (fun _ -> { Driver.no_ascend with parents = None })
  in
  let unions = Array.init 4 (fun _ -> { family = [||] }) in
  let families = [||] and couples = [||] and descends = [||] in
  let strings =
    [| ""; "?"; "Paul"; "Martin"; "Jean Pierre"; "Louis"; "de La Tour" |]
  in
  let base_notes =
    { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) }
  in
  let data =
    ( (persons, ascends, unions),
      (families, couples, descends),
      strings,
      base_notes )
  in
  Driver.make "userkey_test_base" [] data k

let resolve base key =
  match GWPARAM.person_of_string_user_key base key with
  | Some ip -> Driver.Iper.to_string ip
  | None -> "NONE"

(* shared by the OIDC person mapping and the legacy .auth "|userkey" binding *)
let userkey_cases () =
  with_base @@ fun base ->
  let chk label key expected =
    (check string) label expected (resolve base key)
  in
  chk "single-word" "Paul.0 Martin" "0";
  chk "single-word lowercased" "paul.0 martin" "0";
  chk "occurrence 1" "Paul.1 Martin" "3";
  chk "compound first name" "Jean Pierre.0 Martin" "1";
  chk "compound lowercased" "jean pierre.0 martin" "1";
  chk "particled surname" "Louis.0 de La Tour" "2";
  chk "particled '+'-encoded" "Louis.0+de+La+Tour" "2";
  chk "no occurrence" "Paul Martin" "0";
  chk "unknown" "Zoe.0 Nobody" "NONE";
  chk "non-numeric occurrence (must not raise)" "Zoe.x Nobody" "NONE"

let v =
  [
    ( "userkey-resolution",
      [ test_case "person_of_string_user_key" `Quick userkey_cases ] );
  ]
