(* $Id: pa_newseq.ml,v 4.3 2001-04-17 22:41:57 ddr Exp $ *)

ifndef NEWSEQ then

declare
  open Pcaml;
  value o2b =
    fun
    [ Some _ -> True
    | None -> False ]
  ;
  value mkseq loc el =
    match List.rev el with
    [ [] -> <:expr< () >>
    | [e] -> e
    | [e :: el] -> MLast.ExSeq loc (List.rev el) e ]
  ;
  let direction_flag =
    (Obj.magic Grammar.Entry.find Pcaml.expr "direction_flag" :
     Grammar.Entry.e bool)
  in
  EXTEND
    GLOBAL: expr direction_flag;
    expr: LEVEL "top"
      [ [ "do"; "{"; seq = sequence; "}" ->
            mkseq loc seq
        | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
          "do"; "{"; el = sequence; "}" ->
            MLast.ExFor loc i e1 e2 df el
        | "while"; e = SELF; "do"; "{"; el = sequence; "}" ->
            MLast.ExWhi loc e el ] ]
    ;
    sequence:
      [ [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
           el = sequence ->
            [<:expr< let $rec:o2b o$ $list:l$ in $mkseq loc el$ >>]
        | e = expr; ";"; el = sequence ->
            [e :: el]
        | e = expr; ";" ->
            [e]
        | e = expr ->
            [e] ] ]
    ;
  END;
end;
