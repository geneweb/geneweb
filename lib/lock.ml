(* $Id: lock.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

let no_lock_flag = ref false

let print_error_and_exit () =
  Printf.eprintf "\nSorry. Impossible to lock base.\n";
  flush stdout;
  exit 20

let print_try_again () =
  Printf.eprintf "Base locked. Try again.\n";
  flush stdout

let control ~onerror lname wait f =
  let _ = Printf.eprintf "Lock control %s\n" lname in
  if !no_lock_flag then f ()
  else
    try
      let fd = Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
      (try Unix.chmod lname 0o666 with _ -> ()) ;
      (try
         if Sys.unix
         then if wait
           then Unix.lockf fd Unix.F_LOCK 0
           else Unix.lockf fd Unix.F_TLOCK 0 ;
         let r = f () in Unix.close fd ; r
       with e -> Unix.close fd ; raise e)
    with
    | Unix.Unix_error (code, funct, param) ->
        Printf.eprintf "Unix_error %s %s %s\n" (Unix.error_message code) funct param ;
        onerror ()
    | e ->
        Printf.eprintf "Raise e %s\n" (Printexc.to_string e) ;
        raise e

let control_retry ~onerror lname f =
  control lname false
    ~onerror:(fun () ->
        Printf.eprintf "Base is locked. Waiting... ";
        flush stderr;
        control lname true ~onerror
          (fun () ->
             Printf.eprintf "Ok\n";
             flush stderr;
             f ()))
    f

