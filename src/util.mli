(* $Id: util.mli,v 2.4 1999-04-05 23:42:30 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value lang_dir : ref string;
value base_dir : ref string;

value html : config -> unit;
value html_br : config -> unit;
value html_p : config -> unit;
value html_li : config -> unit;

value commd : config -> string;
value code_varenv : string -> string;
value decode_varenv : string -> string;

value lendemain : (int * int * int) -> (int * int * int);
value age_autorise : config -> base -> person -> bool;

value enter_nobr : unit -> unit;
value exit_nobr : unit -> unit;

value connais : base -> person -> bool;
value acces : config -> base -> person -> string;
value calculer_age : config -> person -> option date;

value person_text : config -> base -> person -> string;
value person_text_no_html : config -> base -> person -> string;
value person_text_without_surname : config -> base -> person -> string;
value titled_person_text : config -> base -> person -> title -> string;

value main_title : base -> person -> option title;
(**)
value afficher_personne : config -> base -> person -> unit;
value afficher_prenom_de_personne_referencee :
  config -> base -> person -> unit;
value afficher_personne_referencee : config -> base -> person -> unit;
(**)
value afficher_prenom_de_personne : config -> base -> person -> unit;
value afficher_personne_titre : config -> base -> person -> unit;
value afficher_personne_titre_referencee : config -> base -> person -> unit;
value afficher_personne_sans_titre : config -> base -> person -> unit;
value afficher_titre : config -> base -> person -> unit;
value afficher_un_titre : config -> base -> person -> title -> unit;
value p_getenv : list (string * string) -> string -> option string;
value p_getint : list (string * string) -> string -> option int;
value create_env : string -> list (string * string);
value capitale : string -> string;

value header_no_page_title : config -> (bool -> unit) -> unit;
value header : config -> (bool -> unit) -> unit;
value trailer : config -> unit;

value print_alphab_list :
  config -> ('a -> string) -> ('a -> unit) -> list 'a -> unit;

value surname_begin : string -> string;
value surname_end : string -> string;

value enter_nobr : unit -> unit;
value exit_nobr : unit -> unit;

value preciser_homonyme : config -> base -> person -> unit;

value transl : config -> string -> string;
value transl_nth : config -> string -> int -> string;
value transl_decline : config -> string -> string -> string;
value ftransl : config -> format 'a 'b 'c -> format 'a 'b 'c;
value ftransl_nth : config -> format 'a 'b 'c -> int -> format 'a 'b 'c;
value fcapitale : format 'a 'b 'c -> format 'a 'b 'c;

value index_of_sex : sex -> int;
value conjoint : person -> couple -> iper;

value incorrect_request : config -> unit;

value print_decimal_num : config -> float -> unit;

value find_person_in_env : config -> base -> string -> option person;

value quote_escaped : string -> string;
value rindex : string -> char -> option int;

value charset_of_ansel : config -> string -> string;
value coa : config -> string -> string;

value ansel_of_charset : config -> string -> string;
value aoc : config -> string -> string;

value get_server_string : config -> string;
value get_request_string : config -> string;

value create_topological_sort : config -> base -> array int;

value branch_of_sosa : base -> iper -> Num.t -> option (list (iper * sex));
value sosa_of_branch : list (iper * sex) -> Num.t;

value image_file_name : string -> string -> string;
value print_link_to_welcome : config -> bool -> unit;
value image_size : string -> option (int * int);

value default_image_name_of_key : string -> string -> int -> string;
value default_image_name : base -> person -> string;
value auto_image_file : config -> base -> person -> option string;
