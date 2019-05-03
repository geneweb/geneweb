(* Copyright (c) 1999 INRIA *)

open Geneweb
open Def
open Gwdb

let all = ref false
let statistics = ref false
let detail = ref 0
let ignore = ref []
let output = ref ""
let ignore_files = ref true
let ask_for_delete = ref 0
let cnt_for_delete = ref 0
let exact = ref false
let gwd_port = ref 2317
let server = ref "127.0.0.1"

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
  List.fold_left
    (fun ifaml ip ->
       let u = poi base ip in
       List.fold_left (fun ifaml ifam -> ifam :: ifaml) ifaml
         (Array.to_list (get_family u)))
    ifaml (Array.to_list (get_children fam))

let utf8_designation base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let s = first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname in
  if first_name = "?" || surname = "?" then
    s ^ " (i=" ^ string_of_iper (get_iper p) ^ ")"
  else s

let wiki_designation base basename p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let s = "[[" ^ first_name ^ "/" ^ surname ^ "/" ^ string_of_int (get_occ p) ^ "/" ^
          first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname ^ "]]" in
  if first_name = "?" || surname = "?" then
    let indx = string_of_iper (get_iper p) in
    s ^ " <a href=\"http://" ^ !server ^ ":" ^ (string_of_int !gwd_port) ^ "/" ^
    basename ^ "?i=" ^ indx ^ "\">(i=" ^ indx ^ ")</a><br>"
  else s ^ "<br>"

let print_family base basename ifam =
  let fam = foi base ifam in
  let p = poi base (get_father fam) in
  if !output <> "" then begin
    if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?"
    then Printf.eprintf "i=%s" (string_of_iper (get_iper p))
    else Printf.eprintf "  - %s" (utf8_designation base p);
    Printf.eprintf "\n" ;
    Printf.eprintf "  - %s\n" (utf8_designation base (poi base (get_mother fam))) ;
    flush stderr
  end ;
  if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?"
  then
    let indx = string_of_iper (get_iper p) in
    Printf.printf "  - <a href=\"http://%s:%d/%s?i=%s\">i=%s</a><br>"
      !server !gwd_port basename indx indx
  else Printf.printf "  - %s" (wiki_designation base basename p);
  Printf.printf "\n";
  Printf.printf "  - %s\n"
    (wiki_designation base basename (poi base (get_mother fam)))

let kill_family base ip =
  let u = {family = Array.of_list []} in patch_union base ip u

let kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in patch_ascend base ip a

let effective_del base (ifam, fam) =
  kill_family base (get_father fam);
  kill_family base (get_mother fam);
  Array.iter (kill_parents base) (get_children fam);
  Gwdb.delete_family base ifam

let move base basename =
  load_ascends_array base;
  load_unions_array base;
  load_couples_array base;
  load_descends_array base;
  Printf.printf "<h3>Connected components of base %s</h3><br>\n" basename;
  let ic = Unix.open_process_in "date" in
  let date = input_line ic in
  let () = close_in ic in
  Printf.printf "Computed on %s<br><br>\n" date;
  flush stderr;
  let mark = Gwdb.ifam_marker (Gwdb.ifams base) false in
  let min = ref max_int in
  let max = ref 0 in
  let hts = Hashtbl.create 100 in
  Gwdb.Collection.iter (fun ifam ->
    let fam = foi base ifam in
    let origin_file = get_origin_file fam in
    if List.mem (sou base origin_file) !ignore then ()
    else
      let (nb, ifaml) =
        let rec loop nb rfaml =
          function
            [] -> nb, rfaml
          | ifam :: ifaml ->
            let j = ifam in
            if not (Gwdb.Marker.get mark j)
            && (!ignore_files || eq_istr (get_origin_file fam) origin_file)
            then
              begin
                (Gwdb.Marker.set mark j true);
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
          if !output <> "" then begin
            Printf.eprintf "Connex component \"%s\" length %d\n"
              (sou base origin_file) nb;
            flush stderr
          end ;
          Printf.printf "Connex component \"%s\" length %d<br>\n"
            (sou base origin_file) nb;
          if !detail == nb then List.iter (print_family base basename) ifaml
          else print_family base basename ifam;
          if !statistics then begin
            try Hashtbl.replace hts nb (Hashtbl.find hts nb + 1)
            with Not_found -> Hashtbl.add hts nb 1
          end ;
          flush stdout;
          let check_ask =
            if !exact then nb = !ask_for_delete else nb <= !ask_for_delete
          in
          if !ask_for_delete > 0 && check_ask then
            begin
              (* if -o file, repeat branch definition to stderr! *)
              Printf.eprintf "Delete up to %d branches of size %s %d ?\n"
                !cnt_for_delete (if !exact then "=" else "<=") !ask_for_delete ;
              flush stderr;
              let r =
                if !cnt_for_delete > 0 then "y"
                else begin
                  Printf.eprintf "Delete that branch (y/N) ?";
                  flush stderr;
                  input_line stdin
                end
              in
              if r = "y" then
                begin
                  decr cnt_for_delete ;
                  List.iter
                    (fun ifam ->
                       let fam = foi base ifam in
                       effective_del base (ifam, fam))
                    ifaml;
                  Printf.eprintf "%d families deleted\n" (List.length ifaml);
                  flush stderr
                end
              else begin Printf.printf "Nothing done.\n"; flush stdout end
            end
        end)
    (Gwdb.ifams base) ;
  if !ask_for_delete > 0 then Gwdb.commit_patches base ;
  if !statistics then begin
    Printf.printf "<br>\nStatistics:<br>\n";
    let ls = Hashtbl.fold (fun nb n ls -> (nb, n) :: ls) hts [] in
    let ls = List.sort compare ls in
    let ls = List.rev ls in
    List.iter (fun (nb, n) -> Printf.printf "%d(%d) " nb n) ls;
    Printf.printf "\n"
  end

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist =
  [("-gwd_p", Arg.Int (fun x -> gwd_port := x),
    "<number>: Specify the port number of gwd (default = " ^
    string_of_int !gwd_port ^ "); > 1024 for normal users.");
   ("-server", Arg.String (fun x -> server := x ),
    "<string>: Name of the server (default is 127.0.0.1).");
   ("-a", Arg.Set all, ": all connex components");
   ("-s", Arg.Set statistics, ": produce statistics");
   ("-d", Arg.Int (fun x -> detail := x), "<int> : detail for this length");
   ("-i", Arg.String (fun x -> ignore := x :: !ignore),
    "<file> : ignore this file");
   ("-bf", Arg.Clear ignore_files, ": by origin files");
   ("-del", Arg.Int (fun i -> ask_for_delete := i),
    "<int> : ask for deleting branches whose size <= that value");
   ("-cnt", Arg.Int (fun i -> cnt_for_delete := i),
    "<int> : delete cnt branches whose size <= -del value");
   ("-exact", Arg.Set exact, ": delete only branches whose size strictly = -del value");
   ("-o", Arg.String (fun x -> output := x),
    "<file> : output to this file")
  ]

let main () =
  Arg.parse speclist (fun s -> bname := s) usage;
  if !ask_for_delete > 0 then
    Lock.control (Mutil.lock_file !bname) false
      (fun () -> move (Gwdb.open_base !bname) !bname)
      ~onerror:Lock.print_try_again
  else move (Gwdb.open_base !bname) !bname

let _ = main ()
