(* camlp4r *)
(* $Id: translate.mli,v 4.3 2004-02-02 11:47:54 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

value inline : string -> char -> (char -> string) -> string -> (string * bool);
    (* [Translate.inline lang macro_char macro str] return the translation
       and a boolean telling True if it is actually the English version *)

value language_name : string -> string -> string;
    (* [Translate.language_name lang lang_def] *)
