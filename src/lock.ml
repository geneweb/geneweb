(* $Id: lock.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type choice 'a 'b = [ Left of 'a | Right of 'b ];

value no_lock_flag = ref False;

value control lname wait f =
  if no_lock_flag.val then Some (f ())
  else IFDEF UNIX THEN
    match
      try Some (Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666) with
      [ Unix.Unix_error _ _ _ -> None ]
    with
    [ Some fd ->
        do {
          try Unix.chmod lname 0o666 with _ -> ();
          let r =
            try
              do {
                if wait then Unix.lockf fd Unix.F_LOCK 0
                else Unix.lockf fd Unix.F_TLOCK 0;
                Left fd
              }
            with e ->
              Right e
          in
          match r with
          [ Left fd ->
              let r = try f () with e -> do { Unix.close fd; raise e } in
              do { Unix.close fd; Some r }
          | Right (Unix.Unix_error _ _ _) -> do { Unix.close fd; None }
          | Right exc -> do { Unix.close fd; raise exc } ]
        }
    | None -> None ]
  ELSE
    let r =
      try
        Left (Unix.openfile lname [Unix.O_RDWR; Unix.O_CREAT] 0o666)
      with e ->
        Right e
    in
    match r with
    [ Left fd ->
        let r = try f () with e -> do { Unix.close fd; raise e } in
        do { Unix.close fd; Some r }
    | Right (Unix.Unix_error _ _ _) -> None
    | Right exc -> raise exc ]
  END
;
