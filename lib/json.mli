(** Json of gw operations.*)

open Gwdb
open Yojson

(** Json of person type.*)
val json_of_person : base -> person -> json

(** Json of family type.*)
val json_of_family : base -> family -> json
