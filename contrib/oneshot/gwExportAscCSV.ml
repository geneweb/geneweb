(* camlp4r *)
(* $Id: gwExportAscCSV.ml.ml,v 0.01 2014-04-16 10:05:38 flh Exp $ *)

open Gwdb


let is_printable =
  function
    '\000'..'\031' -> false
  | _ -> true

let escape_quote s =
  let s = if !(Mutil.utf_8_db) then s else Mutil.utf_8_of_iso_8859_1 s in
  let rec loop i len =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
        '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | '"' ->
          let len = Buff.store len '\\' in loop (i + 1) (Buff.store len s.[i])
      | c ->
          let c = if is_printable c then c else '_' in
          loop (i + 1) (Buff.store len c)
  in
  loop 0 0

(* TODO : tableau de mark pour les implexes. *)
let print_asc_csv base ip nb_gen =
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let p = poi base ip in
  let nb_person = 1 in
  let rec loop i nb_person p =
    if i = nb_gen then ()
    else
      let (birth, death, _) = "", "", "" in
      Printf.fprintf stdout "\"%d\";\"%s\";\"%s\";\"%s\";\"%s\";\n" nb_person
        (escape_quote (sou base (get_surname p)))
        (escape_quote (sou base (get_first_name p))) birth death;
      match get_parents p with
        Some ifam ->
          let cpl = foi base ifam in
          loop (succ i) (nb_person * 2) (poi base (get_father cpl));
          loop (succ i) (nb_person * 2 + 1) (poi base (get_mother cpl))
      | None -> ()
  in
  loop 0 nb_person p

let ind = ref 0
let nb_gen = ref 4
let bname = ref ""

let speclist =
  ["-i", Arg.Int (fun i -> ind := i), "ip of the person (default = 0)";
   "-n", Arg.Int (fun i -> nb_gen := i),
   "number of generation (default = " ^ string_of_int !nb_gen ^ ")"]
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [-i #] [-n #] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let base = Gwdb.open_base !bname in
  let ip = Adef.iper_of_int !ind in print_asc_csv base ip !nb_gen

let _ = main ()










