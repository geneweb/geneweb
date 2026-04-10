type index = int list Ext_int.Map.t

let read_index inx dat : index =
  let ix =
    let ic = Secure.open_in inx in
    let data : (int * int) array = input_value ic in
    close_in ic;
    data
  in
  let empty_index = Ext_int.Map.empty in
  let ic = Secure.open_in dat in
  let index =
    Array.fold_left
      (fun index (k, off) ->
        seek_in ic off;
        let len = input_binary_int ic in
        let rec loop ipl len =
          if len = 0 then List.rev ipl
          else loop (input_binary_int ic :: ipl) (len - 1)
        in
        let ipl = loop [] len in
        Ext_int.Map.add k ipl index)
      empty_index ix
  in
  close_in ic;
  index

let pstring_of_iper database iper =
  let p = Gwdb.poi database (Gwdb.iper_of_int iper) in
  Printf.sprintf "%d %s %s %d" iper
    (Gwdb.p_surname database p)
    (Gwdb.p_first_name database p)
    (Gwdb.get_occ p)

let dump_index key database index =
  let index =
    match key with
    | None -> index
    | Some key ->
        Ext_int.Map.filter
          (fun k _ ->
            Name.lower
              (Gwdb.sou database (Gwdb.istr_of_string (string_of_int k)))
            = Name.lower key)
          index
  in
  Ext_int.Map.iter
    (fun k ipl ->
      let key = Gwdb.sou database (Gwdb.istr_of_string (string_of_int k)) in
      print_endline @@ Printf.sprintf "==========%s==========" key;
      List.iter (fun iper -> print_endline @@ pstring_of_iper database iper) ipl)
    index

type fnames = { inx : string; dat : string }

let filenames ~path ~(kind : [ `Surname | `Firstname ])
    ~(index_type : [ `Default | `Lower ]) =
  let path =
    if Filename.check_suffix path ".gwb" then path else path ^ ".gwb"
  in
  let f fn = Filename.concat path fn in
  match (index_type, kind) with
  | `Lower, `Surname ->
      { inx = f "snames_lower.inx"; dat = f "snames_lower.dat" }
  | `Default, `Surname -> { inx = f "snames.inx"; dat = f "snames.dat" }
  | `Lower, `Firstname ->
      { inx = f "fnames_lower.inx"; dat = f "fnames_lower.dat" }
  | `Default, `Firstname -> { inx = f "fnames.inx"; dat = f "fnames.dat" }

let dump_indexes ?key ~kind ~index_type path database =
  let fnames = filenames ~path ~kind ~index_type in
  let index = read_index fnames.inx fnames.dat in
  dump_index key database index

let main () =
  let key = ref "" in
  let options = [ ("-key", Arg.Set_string key, "set key to display") ] in
  let usage = Printf.sprintf "Usage: %s <database-name>" Sys.argv.(0) in
  if Array.length Sys.argv < 2 then (
    Arg.usage options usage;
    exit 2)
  else
    Arg.parse options
      (fun database_path ->
        let database =
          let () = Secure.set_base_dir @@ Filename.dirname database_path in
          try Gwdb.open_base database_path
          with e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
        in
        Fun.protect
          ~finally:(fun () -> Gwdb.close_base database)
          (fun () ->
            Gwdb.load_persons_array database;
            Gwdb.load_strings_array database;
            print_endline "==========SNAMES==========";
            dump_indexes ~key:!key ~kind:`Surname ~index_type:`Default
              database_path database;
            print_endline "==========FNAMES==========";
            dump_indexes ~key:!key ~kind:`Firstname ~index_type:`Default
              database_path database;
            print_endline "==========SNAMES LOWER==========";
            dump_indexes ~key:!key ~kind:`Surname ~index_type:`Lower
              database_path database;
            print_endline "==========FNAMES LOWER==========";
            dump_indexes ~key:!key ~kind:`Firstname ~index_type:`Lower
              database_path database;
            Gwdb.clear_persons_array database;
            Gwdb.clear_strings_array database))
      usage

let () = main ()
