(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: def.syn.ml,v 1.1.1.1 1998-09-01 14:32:02 ddr Exp $ *)

open Pcaml;

EXTEND
  expr: BEFORE "<"
    [ [ x = expr; "strictement_avant"; y = expr ->
          <:expr< strictement_avant $x$ $y$ >>
      | x = expr; "strictement_apres"; y = expr ->
          <:expr< strictement_apres $x$ $y$ >>
      | x = expr; "avant"; y = expr ->
          <:expr< not (strictement_apres $x$ $y$) >>
      | x = expr; "apres"; y = expr ->
          <:expr< not (strictement_avant $x$ $y$) >> ] ];
END;
