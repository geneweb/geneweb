#!/bin/sh
#cd (*
exec ocaml camlp4r.cma $0
*) ".";
(* $Id: utf8-to-iso.sh,v 1.1 2006-11-03 11:09:42 deraugla Exp $ *)

value iso_8859_1_of_utf_8 ic =
  try
    loop () where rec loop () = do {
      let c = input_char ic in
      match Char.code c with
      [ 0xC2 -> print_char (input_char ic)
      | 0xC3 -> print_char (Char.chr (Char.code (input_char ic) + 0x40))
      | _ -> print_char c ];
      if c = '\n' then flush stdout else ();
      loop ()
    }
  with
  [ End_of_file -> flush stdout ]
;

iso_8859_1_of_utf_8 stdin;
