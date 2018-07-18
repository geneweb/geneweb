(* $Id: lock.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type ('a, 'b) choice =
    Left of 'a
  | Right of 'b

let no_lock_flag = ref false

let control lname wait f =
  if !no_lock_flag then Some (f ())
  else if Sys.unix then
    match
      try Some (Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666) with
        Unix.Unix_error (_, _, _) -> None
    with
      Some fd ->
        (try Unix.chmod lname 0o666 with _ -> ());
        let r =
          try
            if wait then Unix.lockf fd Unix.F_LOCK 0
            else Unix.lockf fd Unix.F_TLOCK 0;
            Left fd
          with e -> Right e
        in
        begin match r with
          Left fd ->
            let r = try f () with e -> Unix.close fd; raise e in
            Unix.close fd; Some r
        | Right (Unix.Unix_error (_, _, _)) -> Unix.close fd; None
        | Right exc -> Unix.close fd; raise exc
        end
    | None -> None
  else
    let r =
      try Left (Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666) with
        e -> Right e
    in
    match r with
      Left fd ->
        let r = try f () with e -> Unix.close fd; raise e in
        Unix.close fd; Some r
    | Right (Unix.Unix_error (_, _, _)) -> None
    | Right exc -> raise exc
