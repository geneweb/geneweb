(* $Id: lock.ml,v 1.1 1998-09-01 14:32:03 ddr Exp $ *)

type choice 'a 'b = [ Left of 'a | Right of 'b ];

value no_lock_flag = ref False;

value control lname wait f =
  if no_lock_flag.val then do f (); return True
  else ifdef UNIX then
    let fd = Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666 in
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
        do try f () with e -> do Unix.close fd; return raise e;
           Unix.close fd;
        return True
    | Right (Unix.Unix_error _ _ _) -> do Unix.close fd; return False
    | Right exc -> do Unix.close fd; return raise exc ]
  else
    let r =
      try Left (Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666) with
        e -> Right e
    in
    match r with
    [ Left fd ->
        do try f () with e -> do Unix.close fd; return raise e;
           Unix.close fd;
        return True
    | Right (Unix.Unix_error _ _ _) -> False
    | Right exc -> raise exc ]
;
