module State = State

(** [next_family_fun_templ gwo_list fi] creates a function that read
    sucessivly a [Gwcomp.gw_syntax] for all .gwo files. In details it does :

    - Switch to the next element in the [gwo_list] if reached the end
      of the current file. Each element is [(gwo,separate, bnotes, shift)]
      where [gwo] is .gwo filename and [separate], [bnotes], [shift] are
      captured options from command line related to the giving file.
    - Modify [fi] with mentioned previusly information if needed.
    - Start/continue to read current .gwo file content and return
      [Gwcomp.gw_syntax]. [None] is returned when reading of the last
      .gwo file reaches end of file *)
let next_family_fun_templ gwo_list fi =
  let ngwo = List.length gwo_list in
  let run =
    if ngwo < 10 || not !Mutil.verbose then fun () -> ()
    else if ngwo < 60 then (fun () ->
      Printf.eprintf ".";
      flush stderr)
    else
      let bar_cnt = ref 0 in
      let run () =
        ProgrBar.run !bar_cnt ngwo;
        incr bar_cnt
      in
      ProgrBar.empty := 'o';
      ProgrBar.full := '*';
      ProgrBar.start ();
      run
  in
  let ic_opt = ref None in
  let gwo_list = ref gwo_list in
  fun () ->
    let rec loop () =
      let r =
        match !ic_opt with
        | Some ic -> (
            match
              try Some (input_value ic : Gwcomp.gw_syntax)
              with End_of_file -> None
            with
            | Some fam -> Some fam
            | None ->
                close_in ic;
                ic_opt := None;
                None)
        | None -> None
      in
      let bnotes_of_string = function
        | "merge" -> `merge
        | "erase" -> `erase
        | "first" -> `first
        | "drop" -> `drop
        | _ -> assert false
      in
      match r with
      | Some fam -> Some fam
      | None -> (
          (* switch to the next .gwo file *)
          match !gwo_list with
          | (x, separate, bnotes, shift) :: rest ->
              run ();
              gwo_list := rest;
              let ic = open_in_bin x in
              Gwcomp.check_magic x ic;
              fi.Db1link.f_curr_src_file <- input_value ic;
              fi.Db1link.f_curr_gwo_file <- x;
              fi.Db1link.f_separate <- separate;
              fi.Db1link.f_bnotes <- bnotes_of_string bnotes;
              fi.Db1link.f_shift <- shift;
              Hashtbl.clear fi.Db1link.f_local_names;
              ic_opt := Some ic;
              loop ()
          | [] ->
              if ngwo < 10 || not !Mutil.verbose then ()
              else if ngwo < 60 then (
                Printf.eprintf "\n";
                flush stderr)
              else ProgrBar.finish ();
              None)
    in
    loop ()

let make_base ~save_mem state =
  let open State in
  Secure.set_base_dir (Filename.dirname state.out_file);
  let gwo = ref [] in
  List.iter
    (fun (x, separate, bnotes, shift) ->
      if Filename.check_suffix x ".gw" then (
        (try Gwcomp.comp_families state x
         with e ->
           Printf.printf "File \"%s\", line %d:\n" x state.line_cnt;
           raise e);
        gwo := (x ^ "o", separate, bnotes, shift) :: !gwo)
      else if Filename.check_suffix x ".gwo" then
        gwo := (x, separate, bnotes, shift) :: !gwo
      else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\"")))
    (List.rev state.files);
  if not state.just_comp then (
    let bdir =
      if Filename.check_suffix state.out_file ".gwb" then state.out_file
      else state.out_file ^ ".gwb"
    in
    if (not state.force) && Sys.file_exists bdir then (
      Printf.printf
        "The database \"%s\" already exists. Use option -f to overwrite it."
        state.out_file;
      flush stdout;
      exit 2);
    Lock.control (Files.lock_file state.out_file)
      false ~onerror:Lock.print_error_and_exit (fun () ->
        let bdir =
          if Filename.check_suffix state.out_file ".gwb" then state.out_file
          else state.out_file ^ ".gwb"
        in
        let next_family_fun = next_family_fun_templ (List.rev !gwo) in
        if Db1link.link ~save_mem state next_family_fun bdir then ()
        else (
          Printf.eprintf "*** database not created\n";
          flush stderr;
          exit 2)))
