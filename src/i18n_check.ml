(* $Id: i18n_check.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value usage () =
  do {
    Printf.eprintf "Usage: i18n_check lang lexicon\n";
    flush stderr;
    exit 2;
  }
;

value main () =
  if Array.length Sys.argv <> 3 then usage ()
  else
    let lang = Sys.argv.(1) in
    let file = Sys.argv.(2) in
    let ic = open_in_bin file in
    try
      while True do {
        let line_ref = input_line ic in
        let rec loop line =
          if line = "" then ()
          else do {
            if String.sub line 0 3 = lang ^ ":" then
              Printf.printf "%s\n"
                (String.sub line_ref 4 (String.length line_ref - 4))
            else ();
            loop (input_line ic)
          }
        in
        loop (input_line ic)
      }
    with
    [ End_of_file -> () ]
;

Printexc.catch main ();
