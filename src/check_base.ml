(* $Id: check_base.ml,v 5.2 2006-12-25 22:56:03 ddr Exp $ *)

open Printf;

value set_error base x = do {
  printf "\nError: ";
  Check.print_base_error stdout base x;
};

value set_warning base =
  fun
  [ Def.UndefinedSex _ -> ()
  | x -> do {
      printf "\nWarning: ";
      Check.print_base_warning stdout base x;
    } ]
;

value p_first_name base p = Mutil.nominative (Gwdb.sou base p.Def.first_name);
value p_surname base p = Mutil.nominative (Gwdb.sou base p.Def.surname);

value check_base bname = do {
  let base = Gwdb.open_base bname in
  let changed_p (ip, p) = do {
    let fn = p_first_name base p in
    let sn = p_surname base p in
    printf "%s.%d %s not changed" fn p.Def.occ sn;
  }
  in
  Check.check_base base (set_error base) (set_warning base)
    (fun _ -> True) changed_p False;
  flush stdout;
};

value main () = check_base Sys.argv.(1);

main ();
