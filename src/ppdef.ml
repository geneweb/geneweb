(* camlp4r *)
(* $Id: ppdef.ml,v 4.1 2005-03-01 05:50:43 ddr Exp $ *)

open Pa_macro;

defined.val := [("OCAML_307", None) :: defined.val];

if Sys.ocaml_version >= "3.08" then
  defined.val := [("OCAML_308", None) :: defined.val]
else ();
