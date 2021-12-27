(* Copyright (c) 1998-2007 INRIA *)

(** Initialise [dsk_base] from the database situated in the specified directory. 
    Initialises both data and functionallity part. *)
val opendb : string -> Dbdisk.dsk_base

(** [make bname particles ((persons, ascendants, unions) (families, couples, 
    descendants) strings base_notes)] returns initialised with giving data
    [dsk_base]. This function is called exclusively for database creating 
    purpose. It means that, it contains only data without functionalities. 
    Either call [opendb] on existing database or call [Gwdb.make], if you 
    want to make requests. *)
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

(** List of commited modifications inside the database. First element is a timestamp of a commit, 
    second - changed/added by considered commit person ids, third - changed/added by considered commit families ids. *)
type synchro_patch =
  { mutable synch_list : (string * int list * int list) list }

(** Get [synchro_patch] from the giving database directory. *)
val input_synchro : string -> synchro_patch
