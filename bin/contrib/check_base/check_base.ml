open Geneweb

let print_error base x =
  Printf.printf "\nError: "; Check.print_base_error stdout base x

let print_warning base =
  function
    Def.UndefinedSex _ -> ()
  | x -> Printf.printf "\nWarning: "; Check.print_base_warning stdout base x

let set_list l v = l := v :: !l

let check_base bname =
  Secure.set_base_dir (Filename.dirname bname);
  let base = Gwdb.open_base bname in
  let changed_p (_, p, _, _) =
    let fn = Gwdb.p_first_name base p in
    let sn = Gwdb.p_surname base p in
    Printf.printf "%s.%d %s not changed" fn (Gwdb.get_occ p) sn
  in
  let errors = ref [] in
  let warnings = ref [] in
  Check.check_base base (set_list errors) (set_list warnings) (fun _ -> true)
    changed_p false;
  List.iter (print_error base) (List.rev !errors);
  List.iter (print_warning base) (List.sort_uniq compare !warnings);
  flush stdout

let main () = check_base Sys.argv.(1)

let _ = main ()
