(* $Id: connex.ml,v 4.20 2013-10-06 17:35:18 deraugla Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gwdb;

value all = ref False;
value statistics = ref False;
value detail = ref 0;
value ignore = ref [];
value output = ref "";
value ignore_files = ref True;
value ask_for_delete = ref 0;
value cnt_for_delete = ref 0;
value exact = ref False;
value gwd_port = ref 2317;
value bname = ref "";
value server = ref "127.0.0.1";

value rec merge_families ifaml1f ifaml2f =
  match (ifaml1f, ifaml2f) with
  [ ([ifam1 :: ifaml1], [ifam2 :: ifaml2]) ->
      let m1 = List.memq ifam1 ifaml2 in
      let m2 = List.memq ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then [ifam2 :: merge_families ifaml1f ifaml2]
      else if m2 then [ifam1 :: merge_families ifaml1 ifaml2f]
      else if ifam1 == ifam2 then [ifam1 :: merge_families ifaml1 ifaml2]
      else [ifam1; ifam2 :: merge_families ifaml1 ifaml2]
  | (ifaml1, []) -> ifaml1
  | ([], ifaml2) -> ifaml2 ]
;

value rec filter f =
  fun
  [ [x :: l] -> if f x then [x :: filter f l] else filter f l
  | [] -> [] ]
;

value connected_families base ifam cpl =
  loop [ifam] [] [get_father cpl]
  where rec loop ifaml ipl_scanned =
    fun
    [ [ip :: ipl] ->
        if List.memq ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let u = poi base ip in
          let ifaml1 = Array.to_list (get_family u) in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                 let cpl = foi base ifam in
                 [get_father cpl; get_mother cpl :: ipl])
              ifaml1 ipl
          in
          loop ifaml [ip :: ipl_scanned] ipl
    | [] -> ifaml ]
;

value neighbourgs base ifam =
  let fam = foi base ifam in
  let ifaml = connected_families base ifam fam in
  let ifaml =
    match get_parents (poi base (get_father fam)) with
    [ Some ifam -> [ifam :: ifaml]
    | None -> ifaml ]
  in
  let ifaml =
    match get_parents (poi base (get_mother fam)) with
    [ Some ifam -> [ifam :: ifaml]
    | None -> ifaml ]
  in
  let ifaml =
    List.fold_left
      (fun ifaml ip ->
         let u = poi base ip in
         List.fold_left (fun ifaml ifam -> [ifam :: ifaml]) ifaml
           (Array.to_list (get_family u)))
      ifaml (Array.to_list (get_children fam))
  in
  ifaml
;

value utf8_designation base p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let s = first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname in
  if first_name = "?" || surname = "?" then
    s ^ " (i=" ^ string_of_int (Adef.int_of_iper (get_key_index p)) ^ ")"
  else s
;

value wiki_designation base basename p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  let s = "[[" ^ first_name ^ "/" ^ surname ^ "/" ^ string_of_int (get_occ p) ^ "/" ^
    first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname ^ "]]" in
  if first_name = "?" || surname = "?" then
    let indx = string_of_int (Adef.int_of_iper (get_key_index p)) in
    s ^ " <a href=\"http://" ^ server.val ^ ":" ^ (string_of_int gwd_port.val) ^ "/" ^
          basename ^ "?i=" ^ indx ^ "\">(i=" ^ indx ^ ")</a><br>"
  else s ^ "<br>"
;

value print_family base basename ifam = do {
  let fam = foi base ifam in
  let p = poi base (get_father fam) in
  do {
    if output.val <> "" then do {
      if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?"
      then
        Printf.eprintf "i=%d" (Adef.int_of_iper (get_key_index p))
      else Printf.eprintf "  - %s" (utf8_designation base p);
      Printf.eprintf "\n";
      Printf.eprintf "  - %s\n"
        (utf8_designation base (poi base (get_mother fam)));
      flush stderr;
    } else ();
    if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?"
    then
      let indx = (Adef.int_of_iper (get_key_index p)) in
      Printf.printf "  - <a href=\"http://%s:%d/%s?i=%d\">i=%d</a><br>"
        server.val gwd_port.val basename indx indx
    else Printf.printf "  - %s" (wiki_designation base basename p);
    Printf.printf "\n";
    Printf.printf "  - %s\n"
      (wiki_designation base basename (poi base (get_mother fam)));
  }
};

value kill_family base fam ip =
  let u = {family = Array.of_list []} in
  patch_union base ip u
;

value kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in
  patch_ascend base ip a
;

value effective_del base (ifam, fam) = do {
  kill_family base fam (get_father fam);
  kill_family base fam (get_mother fam);
  Array.iter (kill_parents base) (get_children fam);
  Gwdb.delete_family base ifam;
};

value move base basename = do {
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
  let nb_fam = nb_of_families base in
  let mark = Array.make nb_fam False in
  let min = ref max_int in
  let max = ref 0 in
  let hts = Hashtbl.create 100 in
  for i = 0 to nb_fam - 1 do {
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    let origin_file = get_origin_file fam in
    if List.mem (sou base origin_file) ignore.val then ()
    else
      let (nb, ifaml) =
        loop 0 [] [ifam] where rec loop nb rfaml =
          fun
          [ [] -> (nb, rfaml)
          | [ifam :: ifaml] ->
              let j = Adef.int_of_ifam ifam in
              if not (is_deleted_family (foi base ifam)) && not mark.(j) &&
                 (ignore_files.val ||
                  eq_istr (get_origin_file fam) origin_file)
              then do {
                mark.(j) := True;
                let nl = neighbourgs base ifam in
                let rfaml =
                  if nb > detail.val then
                    if ask_for_delete.val > 0 && nb <= ask_for_delete.val then
                      [ifam :: rfaml]
                     else []
                   else [ifam :: rfaml]
                in
                loop (nb + 1) rfaml (List.rev_append nl ifaml)
              }
              else loop nb rfaml ifaml ]
      in
      if nb > 0 && (all.val || nb <= min.val) then do {
        if nb <= min.val then min.val := nb else ();
        if nb >= max.val then max.val := nb else ();
        if output.val <> "" then do {
          Printf.eprintf "Connex component \"%s\" length %d\n"
            (sou base origin_file) nb;
          flush stderr;
        } else ();
        Printf.printf "Connex component \"%s\" length %d<br>\n"
          (sou base origin_file) nb;
        if detail.val == nb then List.iter (print_family base basename) ifaml
        else print_family base basename ifam;
        if statistics.val then
          try
            let n = Hashtbl.find hts nb in
            Hashtbl.replace hts nb (n+1)
          with
            [ Not_found -> Hashtbl.add hts nb 1 ]
        else ();
        flush stdout;
        let check_ask =
          if exact.val then nb = ask_for_delete.val else nb <= ask_for_delete.val
        in
        if ask_for_delete.val > 0 && check_ask then do {
          (* if -o file, repeat branch definition to stderr! *)
          Printf.eprintf "Delete up to %d branches of size %s %d ?\n"
            cnt_for_delete.val (if exact.val then "=" else "<=") ask_for_delete.val;
          flush stderr;
          let r = if cnt_for_delete.val > 0 then "y" 
            else do {
              Printf.eprintf "Delete that branch (y/N) ?";
              flush stderr;
              input_line stdin }
          in
          if r = "y" then do {
            decr cnt_for_delete;
            List.iter
              (fun ifam -> do {
                 let fam = foi base ifam in
                 if not (is_deleted_family fam) then
                   effective_del base (ifam, fam)
                 else ();
               })
              ifaml;
            Printf.eprintf "%d families deleted\n" (List.length ifaml);
            flush stderr;
          }
          else do {
            Printf.printf "Nothing done.\n";
            flush stdout;
          }
        }
        else ();
      }
      else ();
  };
  if ask_for_delete.val > 0 then Gwdb.commit_patches base else ();

  if statistics.val then do {
    Printf.printf "<br>\nStatistics:<br>\n";
    let ls = 
      Hashtbl.fold (fun nb n ls -> [(nb, n) :: ls] ) hts []
    in
    let ls = List.sort compare ls in
    let ls = List.rev ls in
    List.iter 
      (fun item -> 
        match item with 
            [(nb, n) -> Printf.printf "%d(%d) " nb n ]
      ) ls;
    Printf.printf "\n"
  }
  else ();
};

value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist =
  [("-gwd_p", Arg.Int (fun x -> gwd_port.val := x),
    "<number>: Specify the port number of gwd (default = " ^
      string_of_int gwd_port.val ^ "); > 1024 for normal users.");
   ("-server", Arg.String (fun x -> server.val := x ),
    "<string>: Name of the server (default is 127.0.0.1).");
   ("-a", Arg.Set all, ": all connex components");
   ("-s", Arg.Set statistics, ": produce statistics");
   ("-d", Arg.Int (fun x -> detail.val := x),
    "<int> : detail for this length");
   ("-i", Arg.String (fun x -> ignore.val := [x :: ignore.val]),
    "<file> : ignore this file");
   ("-bf", Arg.Clear ignore_files, ": by origin files");
   ("-del", Arg.Int (fun i -> ask_for_delete.val := i),
    "<int> : ask for deleting branches whose size <= that value");
   ("-cnt", Arg.Int (fun i -> cnt_for_delete.val := i),
    "<int> : delete cnt branches whose size <= -del value");
   ("-exact", Arg.Set exact, ": delete only branches whose size strictly = -del value");
   ("-o", Arg.String (fun x -> output.val := x),
    "<file> : output to this file")
]
;

value main () =
  do {
    Arg.parse speclist (fun s -> bname.val := s) usage;
    if ask_for_delete.val > 0 then
      match
        Lock.control (Mutil.lock_file bname.val) False
          (fun () ->  move (Gwdb.open_base bname.val) bname.val)
      with
      [ Some () -> ()
      | None -> do {
          Printf.eprintf "Base locked. Try again.\n";
          flush stdout
        } ]
    else move (Gwdb.open_base bname.val) bname.val
  }
;

main ();
