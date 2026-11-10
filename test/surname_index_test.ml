open Alcotest
open Def
module Driver = Geneweb_db.Driver

(* Search of a surname by one of its components, particles left out, in the
   index written by gwc (names.inx) and in the surname list of m=N. *)

let surnames =
  [|
    "de Lagoutte du Vivier";
    "Martin du Gard";
    "du Bois de La Motte";
    "Le Bas de Bouclans";
    "van der Meer";
    "de La Tour d'Auvergne";
    "Vivier";
  |]

let particles = [ "de_"; "du_"; "d'"; "van_der_" ]

let with_base k =
  let strings = Array.append [| ""; "?"; "Jean" |] surnames in
  let n = Array.length surnames in
  let persons =
    Array.init n (fun i ->
        {
          (Mutil.empty_person 0 1) with
          key_index = i;
          first_name = 2;
          surname = i + 3;
        })
  in
 let data =
    ( ( persons,
        Array.make n { parents = None; consang = Adef.no_consang },
        Array.make n { family = [||] } ),
      ([||], [||], [||]),
      strings,
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) } )
  in  Driver.make "surname_index_test" particles data k

(* Surnames of the index bucket of [query]. *)
let bucket base query =
  List.map (Driver.sou base)
    (try Driver.base_strings_of_surname base query with Not_found -> [])

let test_index () =
  with_base @@ fun base ->
  List.iter
    (fun (query, surname) ->
      (check bool)
        (Printf.sprintf "%S finds %S" query surname)
        true
        (List.mem surname (bucket base query)))
    [
      ("Vivier", "de Lagoutte du Vivier");
      ("Lagoutte", "de Lagoutte du Vivier");
      ("Gard", "Martin du Gard");
      ("Motte", "du Bois de La Motte");
      ("La Motte", "du Bois de La Motte");
      ("Bouclans", "Le Bas de Bouclans");
      ("Meer", "van der Meer");
      ("Auvergne", "de La Tour d'Auvergne");
      ("La Tour", "de La Tour d'Auvergne");
      (* still found as before *)
      ("du Vivier", "de Lagoutte du Vivier");
      ("de Lagoutte du Vivier", "de Lagoutte du Vivier");
    ]

let test_surname_list () =
  with_base @@ fun base ->
  let conf = Geneweb.Config.{ empty with wizard = true } in
  let found query =
    let l, _ =
      Geneweb.Some.persons_of_fsname conf base Driver.base_strings_of_surname
        (Driver.spi_find (Driver.persons_of_surname base))
        Driver.get_surname query
    in
    List.sort compare (List.map (fun (str, _, _) -> str) l)
  in
  (* An exact surname hides the others, as before: "Vivier" lists only the
     plain "Vivier". *)
  (check (list string)) "Vivier" [ "Vivier" ] (found "Vivier");
  (check (list string)) "Gard" [ "Martin du Gard" ] (found "Gard");
  (check (list string)) "Motte" [ "du Bois de La Motte" ] (found "Motte");
  (check (list string)) "Meer" [ "van der Meer" ] (found "Meer")

let v =
  [
    ( "surname-components",
      [
        test_case "index of the components" `Quick test_index;
        test_case "surname list" `Quick test_surname_list;
      ] );
  ]
