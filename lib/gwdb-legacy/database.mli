(* Copyright (c) 1998-2007 INRIA *)

val opendb : string -> Dbdisk.dsk_base

val make
  : string
  -> string list
  -> ( ( (int, int, int) Def.gen_person array
         * int Def.gen_ascend array
         * int Def.gen_union array )
       * ( (int, int, int) Def.gen_family array
           * int Def.gen_couple array
           * int Def.gen_descend array )
       * string array
       * Def.base_notes )
  -> Dbdisk.dsk_base

(* Ajout pour l'API *)
type synchro_patch =
  { mutable synch_list : (string * int list * int list) list }
val input_synchro : string -> synchro_patch
