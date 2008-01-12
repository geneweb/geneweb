(* $Id: gwcomp.mli,v 5.1 2008-01-12 08:41:18 ddr Exp $ *)
(* Copyright (c) 2007-2008 INRIA *)

open Def;

type key = { pk_first_name : string; pk_surname : string; pk_occ : int };
type somebody =
  [ Undefined of key
  | Defined of gen_person iper string ]
;

type gw_syntax =
  [ Family of
      gen_couple somebody and sex and sex and
        list (somebody * sex) and
        gen_family (gen_person iper string) string and
        gen_descend (gen_person iper string)
  | Notes of key and string
  | Relations of somebody and sex and list (gen_relation somebody string)
  | Bnotes of string and string
  | Wnotes of string and string ]
;

value magic_gwo : string;
value line_cnt : ref int;
value no_fail : ref bool;
value comp_families : string -> unit;
