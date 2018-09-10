(* $Id: gw_most_asc.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Gwdb


let trace = ref false

let ht = Hashtbl.create 7001

let rec compute_nb_asc base ifam =
  let fam = foi base ifam in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let father_asc =
    try Hashtbl.find ht ifath with
      Not_found ->
        let father = poi base ifath in
        let nb_asc =
          match get_parents father with
            None -> 0
          | Some ifam -> compute_nb_asc base ifam
        in
        let () = Hashtbl.add ht ifath nb_asc in nb_asc
  in
  let mother_asc =
    try Hashtbl.find ht imoth with
      Not_found ->
        let mother = poi base imoth in
        let nb_asc =
          match get_parents mother with
            None -> 0
          | Some ifam -> compute_nb_asc base ifam
        in
        let () = Hashtbl.add ht imoth nb_asc in nb_asc
  in
  father_asc + mother_asc

let compute_most_asc base =
  (* on récupère tous les enfants *)
  let l = ref [] in
  let () =
    for i = 0 to Gwdb.nb_of_persons base - 1 do
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      let has_children =
        let rec loop faml =
          match faml with
            [] -> false
          | ifam :: faml ->
              let fam = foi base ifam in
              if get_children fam <> [| |] then true else loop faml
        in
        loop (Array.to_list (get_family p))
      in
      if has_children then () else l := ip :: !l
    done
  in
  let _ =
    List.map
      (fun ip ->
         let p = poi base ip in
         match get_parents p with
           Some ifam -> compute_nb_asc base ifam
         | None -> 0)
      !l
  in
  ()


(**/**)


let bname = ref ""
let fname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" || !fname = "" then
    begin Arg.usage speclist usage; exit 2 end;
  let base = Gwdb.open_base !bname in compute_most_asc base

let _ = main ()
