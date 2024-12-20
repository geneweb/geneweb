(* This file is used to generate the file 'data.cppo.ml', containing all
   the files (cmis, cmas, .so) that could be used at runtime by
   a geneweb interpreter.

   See 'data.mli' for the signature of the generated file. *)

let read_lines p =
  let rec loop () =
    match input_line p with
    | exception End_of_file ->
        close_in p;
        []
    | line -> line :: loop ()
  in
  loop ()

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b
end

let partition_map p l =
  let rec part left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match p x with
        | Some (Either.Left v) -> part (v :: left) right l
        | Some (Either.Right v) -> part left (v :: right) l
        | None -> part left right l)
  in
  part [] [] l

let ( // ) = Filename.concat

let if_sosa_zarith out fn =
  Printf.fprintf out "\n#ifdef SOSA_ZARITH\n";
  fn ();
  Printf.fprintf out "\n#endif\n"

let before_after_ocaml_version ~before ~after version =
  (if String.compare Sys.ocaml_version version < 0 then before else after) ()

let before_after_ocaml_5_1_0 ~before ~after =
  before_after_ocaml_version "5.1.0" ~before ~after

let () =
  let opam_switch_prefix = Sys.getenv "OPAM_SWITCH_PREFIX" in
  let opam_switch_prefix_lib = opam_switch_prefix // "lib" in
  let ocaml_stdlib_directory =
    let output_filename, error_filename =
      let temporary_filename = Filename.temp_file "gwrepl_" "_ocaml_stdlib" in
      (temporary_filename ^ ".out", temporary_filename ^ ".err")
    in
    let command =
      let double_quote_if_win32 = if Sys.win32 then "\"" else "" in
      Printf.sprintf "%sopam exec -- ocamlc -where > %s 2> %s%s"
        double_quote_if_win32
        (Filename.quote output_filename)
        (Filename.quote error_filename)
        double_quote_if_win32
    in
    let exit_code = Sys.command command in
    if exit_code <> 0 then
      failwith
      @@ Printf.sprintf "Command '%s' failed:\nexit code: %d\nerror: %s" command
           exit_code
           (String.concat "\n" (read_lines @@ open_in error_filename))
    else
      match read_lines @@ open_in output_filename with
      | ([] | _ :: _ :: _) as lines ->
          failwith
          @@ Printf.sprintf "Unexpected output of command '%s':\n%s" command
               (String.concat "\n" lines)
      | [ line ] -> line
  in

  let dune_root, root, (directories0, files0) =
    let ic = open_in ".depend" in
    let lines = read_lines ic in
    let dune_root, out =
      match lines with
      | [] -> assert false
      | dune_root :: out -> (dune_root, out)
    in
    let root = dune_root // "_build" // "default" // "lib" in
    let aux fn =
      let aux prefix =
        if
          String.length fn > String.length prefix
          && String.sub fn 0 (String.length prefix) = prefix
        then
          Some
            (String.sub fn (String.length prefix)
               (String.length fn - String.length prefix))
        else None
      in
      match aux opam_switch_prefix_lib with
      | Some x -> Some (`opam (opam_switch_prefix_lib, x))
      | None -> ( match aux root with Some x -> Some (`root x) | None -> None)
    in
    ( dune_root,
      root,
      partition_map
        (fun s ->
          try
            Scanf.sscanf s {|#directory "%[^"]";;|} (fun s ->
                match aux s with Some s -> Some (Either.Left s) | _ -> None)
          with _ -> (
            try
              Scanf.sscanf s {|#load "%[^"]";;|} (fun s ->
                  match aux s with Some s -> Some (Either.Right s) | _ -> None)
            with _ -> failwith s))
        out )
  in

  let directories =
    ("etc" // "lib" // "ocaml")
    :: ("etc" // "lib" // "ocaml" // "stublibs")
    :: List.map
         (function
           | `opam (_, d) -> "etc" // "lib" // d
           | `root d ->
               "etc" // "lib" // "geneweb"
               // (d |> Filename.dirname |> Filename.dirname))
         directories0
  in
  let files0 =
    `opam (Filename.dirname ocaml_stdlib_directory, "ocaml" // "stdlib.cma")
    :: files0
  in
  let cmas, cmis =
    List.fold_right
      (fun x (cmas, cmis) ->
        match x with
        | `opam (prefix_directory, fn) ->
            let aux fn = (prefix_directory // fn, "etc" // "lib" // fn) in
            let cmas = aux fn :: cmas in
            let ((src, _) as cmi) =
              aux (Filename.remove_extension fn ^ ".cmi")
            in
            let cmis = if Sys.file_exists src then cmi :: cmis else cmis in
            (cmas, cmis)
        | `root fn ->
            let cma = (root // fn, "etc" // "lib" // "geneweb" // fn) in
            let cmas = cma :: cmas in
            let dir =
              dune_root // "_build" // "install" // "default" // "lib"
              // "geneweb"
              // Filename.(dirname fn |> basename)
            in
            let cmis =
              Array.fold_left
                (fun cmis s ->
                  if Filename.check_suffix (Filename.concat dir s) "cmi" then
                    ( Filename.concat dir s,
                      "etc" // "lib" // "geneweb"
                      // Filename.concat (Filename.basename dir) s )
                    :: cmis
                  else cmis)
                cmis
                (try Sys.readdir dir
                 with exn ->
                   Printf.eprintf "Error in Sys.readdir(%S)\n%!" dir;
                   raise exn)
            in
            (cmas, cmis))
      files0 ([], [])
  in
  let cmis =
    let select =
      let pref = ocaml_stdlib_directory // "stdlib__" in
      let len = String.length pref in
      fun s -> String.length s > len && String.sub s 0 len = pref
    in
    Array.fold_left
      (fun cmis s ->
        let fname = ocaml_stdlib_directory // s in
        if Filename.check_suffix fname "cmi" && select fname then
          (fname, "etc" // "lib" // "ocaml" // s) :: cmis
        else cmis)
      cmis
      (Sys.readdir ocaml_stdlib_directory)
  in
  let data = "data.cppo.ml" in
  let out = open_out_bin data in
  (let print_dir d = Printf.fprintf out {|"%s";|} d in
   Printf.fprintf out {|let directories=[||};
   List.iter print_dir directories;
   if_sosa_zarith out (fun () -> print_dir ("etc" // "lib" // "stublibs"));
   Printf.fprintf out {||];;|});
  (let aux s list =
     Printf.fprintf out {|let %s=[||} s;
     List.iter
       (fun (src, dst) ->
         Printf.fprintf out {blob|{|%s|},[%%blob {|%s|}];|blob} dst src)
       list;
     Printf.fprintf out {||];;|}
   in
   aux "cmis" cmis;
   aux "cmas" cmas);
  Printf.fprintf out {|let shared=[||};
  if Sys.unix then (
    (* FIXME: what is the windows version? *)
    let aux (prefix_directory, s) =
      Printf.fprintf out
        {blob|Filename.(concat "etc" (concat "lib" {|%s|})),[%%blob {|%s|}];|blob}
        s (prefix_directory // s)
    in
    List.iter aux
      [
        ( Filename.dirname ocaml_stdlib_directory,
          "ocaml" // "stublibs"
          // before_after_ocaml_5_1_0
               ~before:(fun () -> "dllcamlstr.so")
               ~after:(fun () -> "dllcamlstrbyt.so") );
        ( Filename.dirname ocaml_stdlib_directory,
          "ocaml" // "stublibs"
          // before_after_ocaml_5_1_0
               ~before:(fun () -> "dllunix.so")
               ~after:(fun () -> "dllunixbyt.so") );
      ];
    if_sosa_zarith out (fun () ->
        aux (opam_switch_prefix_lib, "stublibs" // "dllzarith.so")));
  Printf.fprintf out {||];;|};
  let b = Buffer.create 1024 in
  let aux =
    List.iter (fun (src, _) ->
        Digest.file src |> Digest.to_hex |> Buffer.add_string b)
  in
  aux cmis;
  aux cmas;
  Printf.fprintf out {|let md5="%s";;|}
    (Buffer.contents b |> Digest.string |> Digest.to_hex)
