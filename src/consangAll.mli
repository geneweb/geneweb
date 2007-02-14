(* $Id: consangAll.mli,v 5.4 2007-02-14 09:23:13 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb;

value compute : base -> bool -> bool -> option consang_tab;
