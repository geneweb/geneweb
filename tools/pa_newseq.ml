(* $Id: pa_newseq.ml,v 4.1 2001-04-15 05:40:56 ddr Exp $ *)

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
  | [e :: el] -> <:expr< do $list:List.rev el$ return $e$ >> ]
;

let direction_flag =
  (Obj.magic Grammar.Entry.find Pcaml.expr "direction_flag" :
   Grammar.Entry.e bool)
in
do
(*
  DELETE_RULE expr: "do"; LIST0 [ expr; ";" ]; "return"; SELF END;
  DELETE_RULE
    expr:
      "for"; LIDENT; "="; SELF; direction_flag; SELF; "do";
      LIST0 [ expr; ";" ]; "done"
  END;
*)
  EXTEND
    GLOBAL: expr direction_flag;
    expr: LEVEL "top"
      [ [ "do"; "{"; seq = sequence; "}" ->
            mkseq loc seq
        | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
          "do"; "{"; el = sequence; "}" ->
            <:expr< for $i$ = $e1$ $to:df$ $e2$ do $list:el$ done >>
        | "while"; e = SELF; "do"; "{"; el = sequence; "}" ->
            <:expr< while $e$ do $list:el$ done >> ] ]
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
return ();
