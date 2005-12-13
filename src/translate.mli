(* camlp4r *)
(* $Id: translate.mli,v 5.1 2005-12-13 18:52:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

value inline : string -> char -> (char -> string) -> string -> (string * bool);
    (* [Translate.inline lang macro_char macro str] return the translation
       and a boolean telling True if it is actually the English version *)

value language_name : string -> string -> string;
    (* [Translate.language_name lang lang_def] *)

(* [concat str] return a transformation of [str]. The input string may
   contain actions between "@(" and ")" whose contents are evaluated like
   this:
     - @(x) set the predicate "x"
     - @(xyz...) set the predicates x, y, z...
     - @(expr) where expr is of the form "x?e1:e2", if predicate "x" then
       e1 else e2 where e1 and e2 are either other "y?e3:e4" or a string
       whose 2nd char is not "?"
     - @(n--) where n is a number: moves the n previous words to the end
       of the whole string.
*)

value concat : string -> string;
