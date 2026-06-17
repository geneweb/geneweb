let files_exist database_path =
  List.exists
    (fun fname -> Sys.file_exists @@ Filename.concat database_path fname)
    [
      (* "snames_marital.dat";
         "snames_marital.inx";
         "snames_marital_lower.dat";
         "snames_marital_lower.inx";*)
      "patches";
    ]

let initialize_search_indexes database =
  Gwdb.load_persons_array database;
  Gwdb.load_strings_array database;
  Gwdb.sync ~save_mem:false
    ~tasks:
      [
        (fun () -> Geneweb.Caches.write_caches database);
        (fun () ->
          let conf =
            Geneweb.Util.minimal_wiz_conf ~bname:(Gwdb.bname database)
          in
          Geneweb.Sosa_cache.write_static_sosa_cache ~conf ~base:database);
      ]
    database;
  Gwdb.clear_persons_array database;
  Gwdb.clear_strings_array database

let main () =
  let options = [] in
  let usage = Printf.sprintf "Usage: %s <database-name>" Sys.argv.(0) in
  if Array.length Sys.argv <> 2 then (
    Arg.usage options usage;
    exit 2)
  else
    let on_lock_error () =
      Lock.print_try_again ();
      raise Exit
    in
    Arg.parse options
      (fun database_path ->
        let database_path =
          if Filename.check_suffix database_path ".gwb" then database_path
          else database_path ^ ".gwb"
        in
        Lock.control
          (Files.lock_file database_path)
          false ~onerror:on_lock_error
        @@ fun () ->
        let database =
          let () = Secure.set_base_dir @@ Filename.dirname database_path in
          try Gwdb.open_base database_path
          with e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
        in
        if files_exist database_path then (
          Printf.eprintf "Base %s already has necessary files\n" database_path;
          raise Exit);
        Fun.protect
          ~finally:(fun () -> Gwdb.close_base database)
          (fun () -> initialize_search_indexes database))
      usage

let () = main ()
