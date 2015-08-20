(* $Id: connex.ml,v 4.20 2013-10-06 17:35:18 deraugla Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Gwdb;

value all = ref False;
value detail = ref 0;
value ignore = ref [];
value ignore_files = ref True;
value ask_for_delete = ref 0;

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

value print_family base ifam = do {
  let fam = foi base ifam in
  let p = poi base (get_father fam) in
  if sou base (get_first_name p) = "?" || sou base (get_surname p) = "?"
  then
    Printf.printf "i=%d" (Adef.int_of_iper (get_key_index p))
  else Printf.printf "  - %s" (utf8_designation base p);
  Printf.printf "\n";
  Printf.printf "  - %s\n"
    (utf8_designation base (poi base (get_mother fam)));
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

value move base = do {
  load_ascends_array base;
  load_unions_array base;
  load_couples_array base;
  load_descends_array base;
  let nb_fam = nb_of_families base in
  let mark = Array.make nb_fam False in
  let min = ref max_int in
  let max = ref 0 in
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
        Printf.printf "Connex component \"%s\" length %d\n"
          (sou base origin_file) nb;
        if detail.val == nb then List.iter (print_family base) ifaml
        else print_family base ifam;
        flush stdout;
        if ask_for_delete.val > 0 && nb <= ask_for_delete.val then do {
          Printf.eprintf "Delete that branch (y/N) ? ";
          flush stderr;
          let r = input_line stdin in
          if r = "y" then do {
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
};

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist =
  [("-a", Arg.Set all, ": all connex components");
   ("-d", Arg.Int (fun x -> detail.val := x),
    "<int> : detail for this length");
   ("-i", Arg.String (fun x -> ignore.val := [x :: ignore.val]),
    "<file> : ignore this file");
   ("-bf", Arg.Clear ignore_files, ": by origin files");
   ("-del", Arg.Int (fun i -> ask_for_delete.val := i),
    "<int> : ask for deleting branches whose size <= that value")]
;

value main () =
  do {
    Arg.parse speclist (fun s -> bname.val := s) usage;
    if ask_for_delete.val > 0 then
      match
        Lock.control (Mutil.lock_file bname.val) False
          (fun () ->  move (Gwdb.open_base bname.val))
      with
      [ Some () -> ()
      | None -> do {
          Printf.eprintf "Base locked. Try again.\n";
          flush stdout
        } ]
    else move (Gwdb.open_base bname.val)
  }
;

main ();
