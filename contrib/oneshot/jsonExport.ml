(* $Id: public2.ml,v 4.1 2008/03/31 11:34:34 deraugla Exp $ *)
open Geneweb
open Def
open Gwdb
open Json
open Yojson

let npoc_of_person base person =
  let firstname =  sou base (get_first_name person) in
  let lastname =  sou base (get_surname person) in
  let n = Name.lower lastname in
  let p = Name.lower firstname in
  let occ = get_occ person in
    Printf.sprintf "%s:%s:%d" n p occ


let key_of_person base person basename =
  let index =  Adef.int_of_iper (get_key_index person) in
  let index = string_of_int index in
  basename ^ ":" ^ index

let print_person base i basename =
  let person = poi base (Adef.iper_of_int i) in
  let json_person = json_of_person base person in
  let key = key_of_person base person basename in
  let json = `Assoc [
    ("_key", `String key);
    ("basename", `String basename);
    ("person", json_person);
  ] in
  Printf.printf "%s\n" (Yojson.to_string json)

let print_parents base i basename =
  let person = poi base (Adef.iper_of_int i) in match get_parents person with
    None -> ()
  | Some ifam -> let fam = foi base ifam in
      let father = poi base (get_father fam) in
      let mother = poi base (get_mother fam) in
      let person_id = Printf.sprintf "persons/%s" (key_of_person base person basename) in
      let father_id = Printf.sprintf "persons/%s" (key_of_person base father basename) in
      let mother_id = Printf.sprintf "persons/%s" (key_of_person base mother basename) in
      let json_father = `Assoc [ ("_from", `String father_id); ("_to", `String person_id); ("basename", `String basename) ] in
      let json_mother = `Assoc [ ("_from", `String mother_id); ("_to", `String person_id); ("basename", `String basename)] in
        Printf.printf "%s\n%s\n" (Yojson.Basic.to_string json_father)  (Yojson.Basic.to_string json_mother)

let print_family base i basename =
  let family = foi base (Adef.ifam_of_int i) in
  if is_deleted_family family then ()
  else
    let key = Printf.sprintf "%s:%d" basename i in 
    let father = poi base (get_father family) in
    let mother = poi base (get_mother family) in
    let father_id = Printf.sprintf "persons/%s" (key_of_person base father basename) in
    let mother_id = Printf.sprintf "persons/%s" (key_of_person base mother basename) in
    let json_family = json_of_family base family in
    let json_edge = `Assoc [ ("_from", `String father_id); ("_to", `String mother_id); ("family", json_family); ("basename", `String basename); ("_key", `String key) ] in
    Printf.printf "%s\n" (Yojson.to_string json_edge)

let rec print_persons_from base index total basename =
  let () = print_person base index basename in
  if index <> total then print_persons_from base (index+1) total basename

let rec print_parents_from base index total basename =
  let () = print_parents base index basename in
  if index <> total then print_parents_from base (index+1) total basename

let rec print_families_from base index total basename =
  let () = print_family base index basename in
  if index <> total then print_families_from base (index+1) total basename

let print_json basename mode =
  let base = Gwdb.open_base basename in
  match mode with
    "persons" -> print_persons_from base 0 (nb_of_persons base - 1) basename
  | "parents" -> print_parents_from base 0 (nb_of_persons base - 1) basename
  | "families" -> print_families_from base 0 (nb_of_families base - 1) basename
  | _ -> ()

let bname = ref ""
let mode = ref ""

let set_mode mode_work = mode := mode_work
let anonfun i = bname := i

let speclist = [
  ("-mode", Arg.String (set_mode), "the mode to work in : persons|parents|families");
]

let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control_retry
    (Mutil.lock_file !bname)
    ~onerror:Lock.print_error_and_exit @@
  fun () ->
  print_json !bname !mode

let _ = main ()
