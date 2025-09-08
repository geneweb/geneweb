let initialize_search_indexes database =
  let on_lock_error () =
    Lock.print_try_again ();
    raise Exit
  in
  Gwdb.initialize_lowercase_name_index ~on_lock_error ~kind:`First_name database;
  Gwdb.initialize_lowercase_name_index ~on_lock_error ~kind:`Surname database

let main () =
  let options = [] in
  let usage = Printf.sprintf "Usage: %s <database-name>" Sys.argv.(0) in
  if Array.length Sys.argv <> 2 then (
    Arg.usage options usage;
    exit 2)
  else
    Arg.parse options
      (fun database ->
        let database =
          try Gwdb.open_base database
          with e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
        in
        Fun.protect
          ~finally:(fun () -> Gwdb.close_base database)
          (fun () -> initialize_search_indexes database))
      usage

let () = main ()
