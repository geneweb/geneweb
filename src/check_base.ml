(* $Id: check_base.ml,v 5.3 2006-12-27 14:57:47 ddr Exp $ *)

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

value check_base bname = do {
  let base = Gwdb.open_base bname in
  let changed_p (ip, p, o_sex, o_rpar) = do {
    let fn = Gwdb.p_first_name base p in
    let sn = Gwdb.p_surname base p in
    printf "%s.%d %s not changed" fn (Gwdb.get_occ p) sn;
  }
  in
  Check.check_base base (set_error base) (set_warning base)
    (fun _ -> True) changed_p False;
  flush stdout;
};

value main () = check_base Sys.argv.(1);

main ();
