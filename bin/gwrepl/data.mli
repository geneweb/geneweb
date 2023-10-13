(* the array of etc/lib/XXX where XXX are either dependencies of geneweb or
   geneweb/COMPONENT *)
val directories : string array

(* associations between file names and their (generated) contents: *)
val cmas : (string * string) array (* .cma *)
val cmis : (string * string) array (* .cmi *)
val shared : (string * string) array (* .so  *)

(* An md5 of all the names of the files in [cmis] and [cmas] (not
   their contents). *)
val md5 : string
