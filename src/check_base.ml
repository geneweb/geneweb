(* $Id: check_base.ml,v 5.5 2007-03-19 10:59:31 ddr Exp $ *)

open Printf;

value print_error base x = do {
  printf "\nError: ";
  Check.print_base_error stdout base x;
};

value print_warning base =
  fun
  [ Def.UndefinedSex _ -> ()
(*
  | Def. IncoherentAncestorDate anc p ->
      fprintf stdout
        "# <a href=\"%%sem=R;ep=%s;en=%s;eoc=%d;spouse=on;et=A;color=;p=%s;n=%s;oc=%d\">%s.%d %s &amp; %s.%d %s</a>\n"
        (Name.lower (Gwdb.p_first_name base p))
        (Name.lower (Gwdb.p_surname base p)) (Gwdb.get_occ p)
        (Name.lower (Gwdb.p_first_name base anc))
        (Name.lower (Gwdb.p_surname base anc)) (Gwdb.get_occ anc)
        (Name.lower (Gwdb.p_first_name base p)) (Gwdb.get_occ p)
        (Name.lower (Gwdb.p_surname base p))
        (Name.lower (Gwdb.p_first_name base anc)) (Gwdb.get_occ anc)
        (Name.lower (Gwdb.p_surname base anc))
*)
  | x -> do {
      printf "\nWarning: ";
      Check.print_base_warning stdout base x;
    } ]
;

value set_list l v = l.val := [v :: l.val];

value check_base bname = do {
  Secure.set_base_dir (Filename.dirname bname);
  let base = Gwdb.open_base bname in
  let changed_p (ip, p, o_sex, o_rpar) = do {
    let fn = Gwdb.p_first_name base p in
    let sn = Gwdb.p_surname base p in
    printf "%s.%d %s not changed" fn (Gwdb.get_occ p) sn;
  }
  in
  let errors = ref [] in
  let warnings = ref [] in
  Check.check_base base (set_list errors) (set_list warnings)
    (fun _ -> True) changed_p False;
  List.iter (print_error base) (List.rev errors.val);
  (* On rend la liste unique, parce qu'il se peut qu'un warning soit *)
  (* levé par plusieurs fonctions différents selon le context.       *)
  let wl =
    let ht = Hashtbl.create 1 in
    loop warnings.val [] where rec loop wl accu =
      match wl with
      [ [] -> accu
      | [x :: wl] ->
          if Hashtbl.mem ht (Hashtbl.hash x) then loop wl accu
          else do {
            Hashtbl.add ht (Hashtbl.hash x) True;
            loop wl [x :: accu]
          } ]
  in
  List.iter (print_warning base) wl;
  flush stdout;
};

value main () = check_base Sys.argv.(1);

main ();
