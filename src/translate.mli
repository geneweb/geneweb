(* camlp4r *)
(* $Id: translate.mli,v 4.1 2002-02-14 10:19:42 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

value inline : string -> char -> (char -> string) -> string -> string;
    (* [Translate.inline lang macro_char macro str] *)
