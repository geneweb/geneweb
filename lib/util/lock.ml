(* Copyright (c) 1998-2007 INRIA *)

let no_lock_flag = ref false

(* TODO: move this generic function in a more appropriate location. *)
let pp_exception ppf (exn, bt) =
  let sexn = Printexc.to_string exn in
  if Printexc.backtrace_status () then
    Format.fprintf ppf "@[Raised exception %s:@ %s@]" sexn
      (Printexc.raw_backtrace_to_string bt)
  else Format.fprintf ppf "@[Raised exception %s@]" sexn

let close_noerr fd = try Unix.close fd with _ -> ()
let chmod_noerr fl perm = try Unix.chmod fl perm with _ -> ()

let acquire_lock ~wait lock_file =
  let fd = Unix.openfile lock_file Unix.[ O_RDWR; O_CREAT ] 0o666 in
  chmod_noerr lock_file 0o666;
  if wait then Unix.lockf fd Unix.F_LOCK 0 else Unix.lockf fd Unix.F_TLOCK 0;
  fd

let release_lock_noerr fd = try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ()

let control ~on_exn ~wait ~lock_file k =
  if !no_lock_flag || Filename.basename lock_file = ".lck" then k ()
  else
    match acquire_lock ~wait lock_file with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        on_exn exn bt
    | fd ->
        let finally () =
          release_lock_noerr fd;
          close_noerr fd
        in
        Fun.protect ~finally k
