(* camlp4r *)
(* $Id: gwd.ml,v 1.1 1999-05-03 07:09:35 ddr Exp $ *)

do Sys.chdir "gw";
   Unix.execv (Filename.concat "." "gwd") [| "gwd" |];
return ();
