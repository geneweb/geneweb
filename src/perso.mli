(* $Id: perso.mli,v 5.7 2007-03-30 18:57:19 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb;
open Config;

value string_of_titles : config -> base -> bool -> string -> person -> string;
value string_of_marriage_text : config -> base -> family -> string;
value interp_templ : string -> config -> base -> person -> unit;
value interp_templ_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit;
value interp_notempl_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit;

value print : config -> base -> person -> unit;
value print_ascend : config -> base -> person -> unit;
value print_what_links : config -> base -> person -> unit;

value links_to_ind :
  config -> base -> list (NotesLinks.page * ('a * list ('b * 'c))) ->
    'b -> list NotesLinks.page;

value build_sosa_tree_ht : config -> base -> person -> unit;
value build_sosa_ht : config -> base -> unit;
value get_sosa_person : config -> base -> person -> Sosa.t;
value get_single_sosa : config -> base -> person -> Sosa.t;
value print_sosa : config -> base -> person -> bool -> unit;

value string_of_num : string -> Sosa.t -> string;
value get_linked_page : config -> base -> person -> string -> string;
value get_birth_text : config -> base -> person -> bool -> string;
value get_baptism_text : config -> base -> person -> bool -> string;
value get_death_text : config -> base -> person -> bool -> string;
value get_burial_text : config -> base -> person -> bool -> string;
value get_cremation_text : config -> base -> person -> bool -> string;
value get_marriage_date_text : config -> base -> family -> bool -> string;

(**)

value infinite : int;
value limit_desc : config -> int;
value make_desc_level_table :
  config -> base -> int -> person -> (array int * array int);
value default_max_cousin_lev : int;

type dup =
  [ DupFam of Adef.ifam and Adef.ifam
  | DupInd of Adef.iper and Adef.iper
  | NoDup ]
;

type excl_dup = (list (Adef.iper * Adef.iper) * list (Adef.ifam * Adef.ifam));

value excluded_possible_duplications : config -> excl_dup;
value first_possible_duplication : base -> Adef.iper -> excl_dup -> dup;


(* Ajout pour l'API *)
value nobility_titles_list : 
  config -> base -> person -> 
    list (int * Def.gen_title_name istr * istr * list istr *
       list (option Adef.date * option Adef.date));

value has_history : config -> base -> person -> bool -> bool;
value has_possible_duplications : config -> base -> person -> bool;

value string_of_title : 
  config -> base -> string -> person -> 
    (int * Def.gen_title_name istr * istr * list istr *
       list (option Def.date * option Def.date)) -> string;

type event_name =
  [ Pevent of Def.gen_pers_event_name istr
  | Fevent of Def.gen_fam_event_name istr ]
;

value events_list : 
  config -> base -> person -> 
    list (event_name * Def.codate * istr * istr * istr *
            array (Def.iper * Def.witness_kind) * option Def.iper);
