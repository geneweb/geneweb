(* $Id: util.mli,v 3.19 2000-05-14 18:07:36 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Config;

value lang_dir : ref string;
value base_dir : ref string;
value doc_dir: ref string;
value cnt_dir : ref string;
value images_url : ref string;
value image_prefix : config -> string;

value secure : string -> string;

value nl : unit -> unit;
value html : config -> unit;
value html_br : config -> unit;
value html_p : config -> unit;
value html_li : config -> unit;
value unauthorized : config -> string -> unit;

value commd : config -> string;
value code_varenv : string -> string;
value decode_varenv : string -> string;

value age_autorise : config -> base -> person -> bool;
value fast_auth_age : config -> person -> bool;
value nb_year_for_public : int;

value enter_nobr : unit -> unit;
value exit_nobr : unit -> unit;

value connais : base -> person -> bool;
value acces_n : config -> base -> string -> person -> string;
value acces : config -> base -> person -> string;
value calculer_age : config -> person -> option dmy;
value wprint_hidden_person : config -> base -> string -> person -> unit;

value geneweb_link : config -> string -> string -> string;
value wprint_geneweb_link : config -> string -> string -> unit;

type p_access = (base -> person -> string * base -> person -> string);
value std_access : p_access;
value raw_access : p_access;

value gen_person_text : p_access -> config -> base -> person -> string;
value gen_person_text_no_html : p_access -> config -> base -> person -> string;
value gen_person_text_without_title :
  p_access -> config -> base -> person -> string
;
value gen_person_title_text : p_access -> config -> base -> person -> string;
value gen_referenced_person_title_text :
  p_access -> config -> base -> person -> string
;

value reference : config -> base -> person -> string -> string;
value person_text : config -> base -> person -> string;
value person_text_no_html : config -> base -> person -> string;
value person_text_without_surname : config -> base -> person -> string;
value person_text_without_title : config -> base -> person -> string;
value titled_person_text : config -> base -> person -> title -> string;
value one_title_text : config -> base -> person -> title -> string;
value person_title_text : config -> base -> person -> string;
value referenced_person_title_text : config -> base -> person -> string;

value main_title : base -> person -> option title;
value afficher_titre : config -> base -> person -> unit;
value p_getenv : list (string * string) -> string -> option string;
value p_getint : list (string * string) -> string -> option int;
value create_env : string -> list (string * string);
value capitale : string -> string;
value list_find : ('a -> bool) -> list 'a -> 'a;
value list_filter : ('a -> bool) -> list 'a -> list 'a;

value header_no_page_title : config -> (bool -> unit) -> unit;
value header : config -> (bool -> unit) -> unit;
value rheader : config -> (bool -> unit) -> unit;
value trailer : config -> unit;
value gen_trailer : bool -> config -> unit;
value open_etc_file : string -> option in_channel;
value copy_from_etc : list (char * string) -> string -> in_channel -> unit;
value copy_string_with_macros : config -> string -> unit;
value body_prop : config -> string;
value include_hed_trl : config -> string -> unit;

value print_alphab_list :
  config -> ('a -> string) -> ('a -> unit) -> list 'a -> unit;
value of_course_died : config -> person -> bool;

value surname_begin : string -> string;
value surname_end : string -> string;

value enter_nobr : unit -> unit;
value exit_nobr : unit -> unit;

value preciser_homonyme : config -> base -> person -> unit;

value transl : config -> string -> string;
value transl_nth : config -> string -> int -> string;
value transl_decline : config -> string -> string -> string;
value transl_decline2 : config -> string -> string -> string -> string;
value ftransl : config -> format 'a 'b 'c -> format 'a 'b 'c;
value ftransl_nth : config -> format 'a 'b 'c -> int -> format 'a 'b 'c;
value fdecline : config -> format 'a 'b 'c -> string -> format 'a 'b 'c;
value fcapitale : format 'a 'b 'c -> format 'a 'b 'c;
value nth_field : string -> int -> string;

value cftransl : config -> string -> list string -> string;

value std_color : config -> string -> string;

value index_of_sex : sex -> int;
value spouse : iper -> couple -> iper;

value incorrect_request : config -> unit;

value print_decimal_num : config -> float -> unit;

value find_person_in_env : config -> base -> string -> option person;

value quote_escaped : string -> string;

value get_server_string : config -> string;
value get_request_string : config -> string;

value create_topological_sort : config -> base -> array int;

value branch_of_sosa : base -> iper -> Num.t -> option (list (iper * sex));
value sosa_of_branch : list (iper * sex) -> Num.t;

value print_link_to_welcome : config -> bool -> unit;

value image_file_name : string -> string;

value image_size : string -> option (int * int);
value limited_image_size : int -> int -> string -> option (int * int);
value image_and_size :
  config -> base -> person -> (string -> option (int * int)) ->
    option (string * option (int * int));

value default_image_name_of_key : string -> string -> int -> string;
value default_image_name : base -> person -> string;
value auto_image_file : config -> base -> person -> option string;

value only_printable : string -> string;

value relation_type_text : config -> relation_type -> int -> string;
value rchild_type_text : config -> relation_type -> int -> string;

value has_nephews_or_nieces : base -> person -> bool;

value browser_doesnt_have_tables : config -> bool;

(* Printing for browsers without tables *)

value pre_text_size : string -> int;
value print_pre_center : int -> string -> unit;
value print_pre_left : int -> string -> unit;
value print_pre_right : int -> string -> unit;

(* Deprecated *)
value afficher_personne : config -> base -> person -> unit;
value afficher_prenom_de_personne : config -> base -> person -> unit;
value afficher_prenom_de_personne_referencee :
  config -> base -> person -> unit;
value afficher_personne_referencee : config -> base -> person -> unit;
value afficher_personne_titre : config -> base -> person -> unit;
value afficher_personne_titre_referencee : config -> base -> person -> unit;
value afficher_personne_sans_titre : config -> base -> person -> unit;
(**)
