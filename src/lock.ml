(* $Id: lock.ml,v 3.3 2001-01-06 09:55:57 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

type choice 'a 'b = [ Left of 'a | Right of 'b ];

value no_lock_flag = ref False;

value control lname wait f =
  if no_lock_flag.val then Some (f ())
  else ifdef UNIX then
    match
      try Some (Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666) with
      [ Unix.Unix_error _ _ _ -> None ]
    with
    [ Some fd ->
        do try Unix.chmod lname 0o666 with _ -> (); return
        let r =
          try
            do if wait then Unix.lockf fd Unix.F_LOCK 0
               else Unix.lockf fd Unix.F_TLOCK 0;
            return Left fd
          with e -> Right e
        in
        match r with
        [ Left fd ->
            let r = try f () with e -> do Unix.close fd; return raise e in
            do Unix.close fd; return Some r
        | Right (Unix.Unix_error _ _ _) -> do Unix.close fd; return None
        | Right exc -> do Unix.close fd; return raise exc ]
    | None -> None ]
  else
    let r =
      try Left (Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666) with
        e -> Right e
    in
    match r with
    [ Left fd ->
        let r = try f () with e -> do Unix.close fd; return raise e in
        do Unix.close fd; return Some r
    | Right (Unix.Unix_error _ _ _) -> None
    | Right exc -> raise exc ]
;
