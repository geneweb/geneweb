(* camlp5r ./pa_lock.cmo *)
(* $Id: gwc2.ml,v 5.68 2008-01-15 11:06:04 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwcomp;
open Printf;

value check_magic =
  let b = String.create (String.length magic_gwo) in
  fun fname ic ->
    do {
      really_input ic b 0 (String.length b);
      if b <> magic_gwo then
        if String.sub magic_gwo 0 4 = String.sub b 0 4 then
          failwith
            ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
        else
          failwith
            ("\"" ^ fname ^
               "\" is not a GeneWeb object file, or it is a very old version")
      else ()
    }
;

value next_family_fun_templ gwo_list fi = do {
  let ngwo = List.length gwo_list in
  let run =
    if ngwo < 10 || not Mutil.verbose.val then fun () -> ()
    else if ngwo < 60 then
      fun () -> do { Printf.eprintf "."; flush stderr; }
    else do {
      let bar_cnt = ref 0 in
      let run () = do { ProgrBar.run bar_cnt.val ngwo; incr bar_cnt } in
      ProgrBar.empty.val := 'o';
      ProgrBar.full.val := '*';
      ProgrBar.start ();
      run
    }
  in
  let ic_opt = ref None in
  let gwo_list = ref gwo_list in
  fi.Db2link.f_sep_file_inx := 0;
  fun () ->
    loop () where rec loop () =
      let r =
        match ic_opt.val with
        [ Some ic ->
            match
              try Some (input_value ic : gw_syntax) with
              [ End_of_file -> None ]
            with
            [ Some fam -> Some fam
            | None -> do {
                close_in ic;
                ic_opt.val := None;
                fi.Db2link.f_sep_file_inx := fi.Db2link.f_sep_file_inx + 1;
                None
              } ]
        | None -> None ]
      in
      match r with
      [ Some fam -> Some fam
      | None ->
          match gwo_list.val with
          [ [(x, separate, shift) :: rest] -> do {
              run ();
              gwo_list.val := rest;
              let ic = open_in_bin x in
              check_magic x ic;
              fi.Db2link.f_curr_src_file := input_value ic;
              fi.Db2link.f_curr_gwo_file := x;
              fi.Db2link.f_separate := separate;
              fi.Db2link.f_has_separates :=
                fi.Db2link.f_has_separates || separate;
              ic_opt.val := Some ic;
              loop ();
            }
          | [] -> do {
              if ngwo < 10 || not Mutil.verbose.val then ()
              else if ngwo < 60 then do { Printf.eprintf "\n"; flush stderr }
              else ProgrBar.finish ();
              None
            } ] ]
};

value just_comp = ref False;
value out_file = ref (Filename.concat Filename.current_dir_name "a");
value force = ref False;

value separate = ref False;
value shift = ref 0;
value files = ref [];

value speclist =
  [("-c", Arg.Set just_comp, "Only compiling");
   ("-o", Arg.String (fun s -> out_file.val := s),
    "<file> Output database (default: a.gwb)");
   ("-f", Arg.Set force, " Remove database if already existing");
   ("-stats", Arg.Set Db2link.pr_stats, "Print statistics");
   ("-nc", Arg.Clear Db2link.do_check, "No consistency check");
   ("-cg", Arg.Set Db2link.do_consang, "Compute consanguinity");
   ("-sep", Arg.Set separate, " Separate all persons in next file");
   ("-sh", Arg.Int (fun x -> shift.val := x),
    "<int> Shift all persons numbers in next files");
   ("-ds", Arg.String (fun s -> Db2link.default_source.val := s), "\
     <str> Set the source field for persons and families without source data");
   ("-part", Arg.String (fun s -> Db2link.particules_file.val := s), "\
     <file> Particles file (default = predefined particles)");
   ("-mem", Arg.Unit (fun () -> ()), " (obsolete option)");
   ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.");
   ("-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.");
   ("-q", Arg.Clear Mutil.verbose, " no verbose");
   ("-v", Arg.Set Mutil.verbose, " verbose")]
;

value anonfun x =
  let sep = separate.val in
  do {
    if Filename.check_suffix x ".gw" then ()
    else if Filename.check_suffix x ".gwo" then ()
    else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
    separate.val := False;
    files.val := [(x, sep, shift.val) :: files.val]
  }
;

value errmsg =
  "\
Usage: gwc2 [options] [files]
where [files] are a list of files:
  source files end with .gw
  object files end with .gwo
and [options] are:"
;

value main () =
  do {
    Mutil.verbose.val := False;
    Argl.parse speclist anonfun errmsg;
    Secure.set_base_dir (Filename.dirname out_file.val);
    let gwo = ref [] in
    List.iter
      (fun (x, separate, shift) ->
         if Filename.check_suffix x ".gw" then do {
           try Gwcomp.comp_families x with e -> do {
             printf "File \"%s\", line %d:\n" x line_cnt.val;
             raise e
           };
           gwo.val := [(x ^ "o", separate, shift) :: gwo.val];
         }
         else if Filename.check_suffix x ".gwo" then
           gwo.val := [(x, separate, shift) :: gwo.val]
         else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\"")))
      (List.rev files.val);
    if not just_comp.val then do {
      let bdir =
        if Filename.check_suffix out_file.val ".gwb" then out_file.val
        else out_file.val ^ ".gwb"
      in
      if not force.val && Sys.file_exists bdir then do {
        printf "\
The database \"%s\" already exists. Use option -f to overwrite it.
" out_file.val;
        flush stdout;
        exit 2
      }
      else ();
      lock (Mutil.lock_file out_file.val) with
      [ Accept ->
          let bdir =
            if Filename.check_suffix out_file.val ".gwb" then out_file.val
            else out_file.val ^ ".gwb"
          in
          let next_family_fun = next_family_fun_templ (List.rev gwo.val) in
          if Db2link.link next_family_fun bdir then ()
          else do {
            eprintf "*** database not created\n";
            flush stderr;
            exit 2;
          }
      | Refuse -> do {
          printf "Base is locked: cannot write it\n";
          flush stdout;
          exit 2
        } ];
    }
    else ();
  }
;

value print_exc =
  fun
  [ Failure txt ->
      do { printf "Failed: %s\n" txt; flush stdout; exit 2 }
  | exc -> Printexc.catch raise exc ]
;

try main () with exc -> print_exc exc;
