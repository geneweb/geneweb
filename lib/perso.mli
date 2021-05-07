(* $Id: perso.mli,v 5.7 2007-03-30 18:57:19 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb
open Config

type generation_person =
    GP_person of Sosa.t * iper * ifam option
  | GP_same of Sosa.t * Sosa.t * iper
  | GP_interv of (Sosa.t * Sosa.t * (Sosa.t * Sosa.t) option) option
  | GP_missing of Sosa.t * iper

val string_of_marriage_text : config -> base -> family -> string

val interp_templ : ?no_headers:bool -> string -> config -> base -> person -> unit
val interp_templ_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit
val interp_notempl_with_menu :
  (bool -> unit) -> string -> config -> base -> person -> unit

val print : ?no_headers:bool -> config -> base -> person -> unit
val print_ascend : config -> base -> person -> unit
val print_what_links : config -> base -> person -> unit

val links_to_ind
  : Config.config
  -> Gwdb.base
  -> ((iper, ifam) Def.NLDB.page * ('a * ((string * string * int) * 'b) list)) list
  -> string * string * int
  -> (iper, ifam) Def.NLDB.page list

val build_sosa_tree_ht : config -> base -> person -> unit
val build_sosa_ht : config -> base -> unit
val get_sosa_person : person -> Sosa.t
val get_single_sosa : config -> base -> person -> Sosa.t
val print_sosa : config -> base -> person -> bool -> unit

val get_linked_page : config -> base -> person -> string -> string
val get_birth_text : config -> person -> bool -> string
val get_baptism_text : config -> person -> bool -> string
val get_death_text : config -> person -> bool -> string
val get_burial_text : config -> person -> bool -> string
val get_cremation_text : config -> person -> bool -> string
val get_marriage_date_text : config -> family -> bool -> string

val linked_page_text
  : Config.config
  -> Gwdb.base
  -> Gwdb.person
  -> string
  -> 'a
  -> string
  -> (iper, ifam) Def.NLDB.page * ('b * ('a * Def.NLDB.ind) list)
  -> string

val max_ancestor_level : config -> base -> iper -> int -> int

val string_of_died : config -> person -> bool -> string

val string_of_parent_age : config -> base -> person * bool -> (family -> iper) -> string

val string_of_image_url : config -> base -> person * bool -> bool -> string

val round_2_dec : float -> float

val has_children : base -> person -> bool

val string_of_image_size : config -> base -> person * bool -> string
val string_of_image_medium_size : config -> base -> person * bool -> string
val string_of_image_small_size : config -> base -> person * bool -> string

val get_link : generation_person list -> Util.IperSet.elt -> generation_person option

(**)

val infinite : int
val limit_desc : config -> int
val make_desc_level_table
  : config
  -> base
  -> int
  -> person
  -> (Util.IperSet.elt, int) Gwdb.Marker.t * (ifam, int) Gwdb.Marker.t

val default_max_cousin_lev : int

type dup =
    DupFam of ifam * ifam
  | DupInd of iper * iper
  | NoDup

type excl_dup = (iper * iper) list * (ifam * ifam) list

val excluded_possible_duplications : config -> excl_dup
val first_possible_duplication : base -> iper -> excl_dup -> dup


(* Ajout pour l'API *)
val nobility_titles_list :
  config -> base -> person ->
    (int * istr Def.gen_title_name * istr * istr list *
       (Adef.date option * Adef.date option) list)
      list

val has_history : config -> base -> person -> bool -> bool
val has_possible_duplications : config -> base -> person -> bool

(** Optionnal [link] argument is passed to {!val:DateDisplay.string_of_ondate} *)
val string_of_title
  : ?link:bool
  -> config
  -> base
  -> string
  -> person
  -> int * istr Def.gen_title_name * istr * istr list * (Def.date option * Def.date option) list
  -> string

module SortedList : Set.S
type sosa_t
type cell
type ancestor_surname_info

type 'a env =
  | Vallgp of generation_person list
  | Vanc of generation_person
  | Vanc_surn of ancestor_surname_info
  | Vcell of cell
  | Vcelll of cell list
  | Vcnt of int ref
  | Vdesclevtab of ((iper, int) Marker.t * (ifam, int) Marker.t) lazy_t
  | Vdmark of (iper, bool) Marker.t ref
  | Vslist of SortedList.t ref
  | Vslistlm of string list list
  | Vind of person
  | Vfam of ifam * family * (iper * iper * iper) * bool
  | Vrel of relation * person option
  | Vbool of bool
  | Vint of int
  | Vgpl of generation_person list
  | Vnldb of (Gwdb.iper, Gwdb.ifam) Def.NLDB.t
  | Vstring of string
  | Vsosa_ref of person option Lazy.t
  | Vsosa of (iper * (Sosa.t * person) option) list ref
  | Vt_sosa of sosa_t option
  | Vtitle of person * title_item
  | Vevent of person * event_item
  | Vlazyp of string option ref
  | Vlazy of 'a env Lazy.t
  | Vother of 'a
  | Vnone
and title_item =
  int
  * istr Def.gen_title_name
  * istr
  * istr list
  * (Adef.date option * Adef.date option) list
and event_item =
  event_name
  * Adef.cdate
  * istr
  * istr
  * istr
  * (iper * Def.witness_kind) array
  * iper option
and event_name =
  | Pevent of istr Def.gen_pers_event_name
  | Fevent of istr Def.gen_fam_event_name

val events_list
  : config
  -> base
  -> person
  -> (event_name * Def.cdate * istr * istr * istr * (iper * Def.witness_kind) array * iper option) list

(**/**)

type p_pauth = (person * bool)
type event_field = (event_name * Adef.cdate * istr * istr * istr * (iper * Def.witness_kind) array * iper option)
type fcd = (ifam * family * (iper * iper * iper) * bool)

type 'a eval_var = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_simple_var = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> string list -> p_pauth TemplAst.expr_val
and 'a eval_simple_bool_var = 'a eval -> config -> base -> 'a Templ.env -> string -> bool
and 'a eval_simple_str_var = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> string -> string
and 'a eval_compound_var = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_item_field_var = 'a eval -> string list list -> string list -> p_pauth TemplAst.expr_val
and 'a eval_relation_field_var = 'a eval -> config -> base -> 'a Templ.env -> (int * Def.relation_type * iper * bool) -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_cell_field_var = 'a eval -> config -> base -> 'a Templ.env -> cell -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_ancestor_field_var = 'a eval -> config -> base -> 'a Templ.env -> generation_person -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_anc_by_surnl_field_var = 'a eval -> config -> base -> 'a Templ.env -> person * bool -> ancestor_surname_info -> string list -> p_pauth TemplAst.expr_val
and 'a eval_num = 'a eval -> config -> Sosa.t -> string list -> string
and 'a eval_person_field_var = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_date_field_var = 'a eval -> config -> Adef.date -> string list -> p_pauth TemplAst.expr_val
and 'a eval_nobility_title_field_var = 'a eval -> (string * string) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_bool_event_field = 'a eval -> base -> p_pauth -> event_field -> string -> bool
and 'a eval_str_event_field = 'a eval -> config -> base -> p_pauth -> event_field -> string -> string
and 'a eval_event_field_var = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> event_field -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_event_witness_relation_var = 'a eval -> config -> base -> 'a Templ.env -> (person * event_field) -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_bool_person_field = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> string -> bool
and 'a eval_str_person_field = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> string -> string
and 'a eval_witness_relation_var = 'a eval -> config -> base -> 'a Templ.env -> fcd -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_family_field_var = 'a eval -> config -> base -> 'a Templ.env -> fcd -> (int * int) -> string list -> p_pauth TemplAst.expr_val
and 'a eval_str_family_field = 'a eval -> 'a Templ.env -> fcd -> string -> string
and 'a obsolete_eval = 'a eval -> config -> base -> 'a Templ.env -> p_pauth -> (int * int) -> string -> p_pauth TemplAst.expr_val

and simple_person_text = config -> base -> person -> bool -> string
and string_of_died = config -> person -> bool -> string
and string_of_image_url = config -> base -> p_pauth -> bool -> string
and string_of_parent_age = config -> base -> p_pauth -> (family -> iper) -> string
and 'a string_of_int_env = 'a eval -> string -> 'a Templ.env -> string
and 'a eval =
  { eval_var : 'a eval_var
  ; eval_simple_var : 'a eval_simple_var
  ; eval_simple_bool_var : 'a eval_simple_bool_var
  ; eval_simple_str_var : 'a eval_simple_str_var
  ; eval_compound_var : 'a eval_compound_var
  ; eval_item_field_var : 'a eval_item_field_var
  ; eval_relation_field_var : 'a eval_relation_field_var
  ; eval_cell_field_var : 'a eval_cell_field_var
  ; eval_ancestor_field_var : 'a eval_ancestor_field_var
  ; eval_anc_by_surnl_field_var : 'a eval_anc_by_surnl_field_var
  ; eval_num : 'a eval_num
  ; eval_person_field_var : 'a eval_person_field_var
  ; eval_date_field_var : 'a eval_date_field_var
  ; eval_nobility_title_field_var : 'a eval_nobility_title_field_var
  ; eval_bool_event_field : 'a eval_bool_event_field
  ; eval_str_event_field : 'a eval_str_event_field
  ; eval_event_field_var : 'a eval_event_field_var
  ; eval_event_witness_relation_var : 'a eval_event_witness_relation_var
  ; eval_bool_person_field : 'a eval_bool_person_field
  ; eval_str_person_field : 'a eval_str_person_field
  ; eval_witness_relation_var : 'a eval_witness_relation_var
  ; eval_family_field_var : 'a eval_family_field_var
  ; eval_str_family_field : 'a eval_str_family_field
  ; simple_person_text : simple_person_text
  ; string_of_died : string_of_died
  ; string_of_image_url : string_of_image_url
  ; string_of_parent_age : string_of_parent_age
  ; string_of_int_env : 'a string_of_int_env
  ; obsolete_eval : 'a obsolete_eval
  }

val eval_simple_str_var : 'a env eval_simple_str_var
val eval_person_field_var : 'a env eval_person_field_var
val eval_compound_var : 'a env eval_compound_var
val get_env : 'a -> ('a * 'b env) list -> 'b env

val print_foreach
  : config
  -> base
  -> ((string * 'a env) list -> person * bool -> 'b -> unit)
  -> ((string * 'a env) list -> person * bool -> 'c -> string)
  -> (string * 'a env) list
  -> person * bool
  -> TemplAst.loc
  -> string
  -> string list
  -> 'c list list
  -> 'b list
  -> unit
