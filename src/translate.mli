(* camlp4r *)
(* $Id: translate.mli,v 4.2 2002-11-03 20:16:09 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

value inline : string -> char -> (char -> string) -> string -> string;
    (* [Translate.inline lang macro_char macro str] *)

value language_name : string -> string -> string;
    (* [Translate.language_name lang lang_def] *)
