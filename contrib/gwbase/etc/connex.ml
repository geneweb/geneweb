(* $Id: connex.ml,v 4.20 2013-10-06 17:35:18 deraugla Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def
open Gwdb

let all = ref false
let detail = ref 0
let ignore = ref []
let ignore_files = ref true
let ask_for_delete = ref 0

let rec merge_families ifaml1f ifaml2f =
  match ifaml1f, ifaml2f with
    ifam1 :: ifaml1, ifam2 :: ifaml2 ->
      let m1 = List.memq ifam1 ifaml2 in
      let m2 = List.memq ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then ifam2 :: merge_families ifaml1f ifaml2
      else if m2 then ifam1 :: merge_families ifaml1 ifaml2f
      else if ifam1 == ifam2 then ifam1 :: merge_families ifaml1 ifaml2
      else ifam1 :: ifam2 :: merge_families ifaml1 ifaml2
  | ifaml1, [] -> ifaml1
  | [], ifaml2 -> ifaml2

let rec filter f =
  function
    x :: l -> if f x then x :: filter f l else filter f l
  | [] -> []

let connected_families base ifam cpl =
  let rec loop ifaml ipl_scanned =
    function
      ip :: ipl ->
        if List.memq ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let u = poi base ip in
          let ifaml1 = Array.to_list (get_family u) in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                 let cpl = foi base ifam in
                 get_father cpl :: get_mother cpl :: ipl)
              ifaml1 ipl
          in
          loop ifaml (ip :: ipl_scanned) ipl
    | [] -> ifaml
  in
  loop [ifam] [] [get_father cpl]

let neighbourgs base ifam =
  let fam = foi base ifam in
  let ifaml = connected_families base ifam fam in
  let ifaml =
    match get_parents (poi base (get_father fam)) with
      Some ifam -> ifam :: ifaml
    | None -> ifaml
  in
  let ifaml =
    match get_parents (poi base (get_mother fam)) with
      Some ifam -> ifam :: ifaml
    | None -> ifaml
  in
  let ifaml =
    List.fold_left
      (fun ifaml ip ->
         let u = poi base ip in
         List.fold_left (fun ifaml ifam -> ifam :: ifaml) ifaml
           (Array.to_list (get_family u)))
      ifaml (Array.to_list (get_children fam))
  in
  ifaml

let utf8_designation base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let s = first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname in
  if first_name = "?" || surname = "?" then
    s ^ " (i=" ^ string_of_int (Adef.int_of_iper (get_key_index p)) ^ ")"
  else s

let print_family base ifam =
  let fam = foi base ifam in
  let p = poi base (get_father fam) in
  if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?" then
    Printf.printf "i=%d" (Adef.int_of_iper (get_key_index p))
  else Printf.printf "  - %s" (utf8_designation base p);
  Printf.printf "\n";
  Printf.printf "  - %s\n" (utf8_designation base (poi base (get_mother fam)))

let kill_family base fam ip =
  let u = {family = Array.of_list []} in patch_union base ip u

let kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in patch_ascend base ip a

let effective_del base (ifam, fam) =
  kill_family base fam (get_father fam);
  kill_family base fam (get_mother fam);
  Array.iter (kill_parents base) (get_children fam);
  Gwdb.delete_family base ifam

let move base =
  load_ascends_array base;
  load_unions_array base;
  load_couples_array base;
  load_descends_array base;
  let nb_fam = nb_of_families base in
  let mark = Array.make nb_fam false in
  let min = ref max_int in
  let max = ref 0 in
  for i = 0 to nb_fam - 1 do
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    let origin_file = get_origin_file fam in
    if List.mem (sou base origin_file) !ignore then ()
    else
      let (nb, ifaml) =
        let rec loop nb rfaml =
          function
            [] -> nb, rfaml
          | ifam :: ifaml ->
              let j = Adef.int_of_ifam ifam in
              if not (is_deleted_family (foi base ifam)) && not mark.(j) &&
                 (!ignore_files || eq_istr (get_origin_file fam) origin_file)
              then
                begin
                  mark.(j) <- true;
                  let nl = neighbourgs base ifam in
                  let rfaml =
                    if nb > !detail then
                      if !ask_for_delete > 0 && nb <= !ask_for_delete then
                        ifam :: rfaml
                      else []
                    else ifam :: rfaml
                  in
                  loop (nb + 1) rfaml (List.rev_append nl ifaml)
                end
              else loop nb rfaml ifaml
        in
        loop 0 [] [ifam]
      in
      if nb > 0 && (!all || nb <= !min) then
        begin
          if nb <= !min then min := nb;
          if nb >= !max then max := nb;
          Printf.printf "Connex component \"%s\" length %d\n"
            (sou base origin_file) nb;
          if !detail == nb then List.iter (print_family base) ifaml
          else print_family base ifam;
          flush stdout;
          if !ask_for_delete > 0 && nb <= !ask_for_delete then
            begin
              Printf.eprintf "Delete that branch (y/N) ? ";
              flush stderr;
              let r = input_line stdin in
              if r = "y" then
                begin
                  List.iter
                    (fun ifam ->
                       let fam = foi base ifam in
                       if not (is_deleted_family fam) then
                         effective_del base (ifam, fam))
                    ifaml;
                  Printf.eprintf "%d families deleted\n" (List.length ifaml);
                  flush stderr
                end
              else begin Printf.printf "Nothing done.\n"; flush stdout end
            end
        end
  done;
  if !ask_for_delete > 0 then Gwdb.commit_patches base

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist =
  ["-a", Arg.Set all, ": all connex components";
   "-d", Arg.Int (fun x -> detail := x), "<int> : detail for this length";
   "-i", Arg.String (fun x -> ignore := x :: !ignore),
   "<file> : ignore this file";
   "-bf", Arg.Clear ignore_files, ": by origin files";
   "-del", Arg.Int (fun i -> ask_for_delete := i),
   "<int> : ask for deleting branches whose size <= that value"]

let main () =
  Arg.parse speclist (fun s -> bname := s) usage;
  if !ask_for_delete > 0 then
    match
      Lock.control (Mutil.lock_file !bname) false
        (fun () -> move (Gwdb.open_base !bname))
    with
      Some () -> ()
    | None -> Printf.eprintf "Base locked. Try again.\n"; flush stdout
  else move (Gwdb.open_base !bname)

let _ = main ()
