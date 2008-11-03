(* camlp5r pa_extend.cmo q_MLast.cmo *)
(* $Id: def_syn.ml,v 5.1 2008-11-03 15:40:10 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Pcaml;

EXTEND
  expr: BEFORE "<"
    [ [ x = expr; "strictly_before"; y = expr ->
          <:expr< strictly_before $x$ $y$ >>
      | x = expr; "strictly_after"; y = expr ->
          <:expr< strictly_after $x$ $y$ >>
      | x = expr; "avant"; y = expr ->
          <:expr< not (strictly_after $x$ $y$) >>
      | x = expr; "apres"; y = expr ->
          <:expr< not (strictly_before $x$ $y$) >> ] ];
END;
