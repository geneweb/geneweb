(* camlp4r *)
(* $Id: ppdef.ml,v 4.2 2005-03-01 10:38:27 ddr Exp $ *)

open Pa_macro;

if Sys.ocaml_version >= "3.07" then
  defined.val := [("OCAML_307", None) :: defined.val]
else ();

if Sys.ocaml_version >= "3.08" then
  defined.val := [("OCAML_308", None) :: defined.val]
else ();
