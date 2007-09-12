(* camlp5r *)
(* $Id: translate.mli,v 5.7 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value inline : string -> char -> (char -> string) -> string -> (string * bool);
    (* [Translate.inline lang macro_char macro str] return the translation
       and a boolean telling True if it is actually the English version *)

value language_name : string -> string -> string;
    (* [Translate.language_name lang lang_def] *)

value eval : string -> string;
(* [eval str] return a transformation of [str]. The input string may
   contain actions between "@(" and ")" whose contents are evaluated like
   this:
     - @(x) set the predicate "x"
     - @(xyz...) set the predicates x, y, z...
     - @(expr) where expr is of the form "x?e1:e2": if predicate "x" then
       evaluates e1 else evaluates e2 where e1 and e2 are either another
       expression "y?e3:e4" or a string (whose 2nd char is not "?").
     - @(n--) where n is a number: move the n preceding words to the end
       of the string.
     - @(@string) evaluates "@string" recursively; in particular, predicates
       inside the string remain local.

   Warning: this function makes obsolete many functions in "Util" taking
   care of declinations using the system with :x:.
*)
