(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_html.ml,v 4.1 2001-04-17 21:56:50 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Pcaml;

value rec unfold_apply list =
  fun
  [ <:expr< $x1$ $x2$ >> -> unfold_apply [x2 :: list] x1
  | e -> (e, list) ]
;

value tag_encloser loc tag newl a el =
  let s = if newl then "\n" else "" in
  let e =
    let (frm, al) =
      match a with
      [ Some e ->
          let (e, al) = unfold_apply [] e in
          let frm =
            match e with
            [ <:expr< $str:frm$ >> -> frm
            | _ ->
                Stdpp.raise_with_loc (MLast.loc_of_expr e)
                  (Stream.Error "string or 'do' expected") ]
          in
          (" " ^ frm, al)
      | None -> ("", []) ]
    in
    List.fold_left (fun f e -> <:expr< $f$ $e$ >>)
      <:expr< Wserver.wprint $str:"<" ^ tag ^ frm ^ ">" ^ s$ >> al
  in
  [e :: el @ [<:expr< Wserver.wprint $str:"</" ^ tag ^ ">" ^ s$ >>]]
;

EXTEND
  GLOBAL: expr;
  expr: LEVEL "top"
    [ [ "tag"; (tn, al, el) = tag_body ->
          let el = tag_encloser loc tn True al el in
          ifndef NEWSEQ then MLast.ExSeq loc el (MLast.ExUid loc "()")
          else MLast.ExSeq loc el
      | "stag"; (tn, al, el) = tag_body ->
          let el = tag_encloser loc tn False al el in
          ifndef NEWSEQ then MLast.ExSeq loc el (MLast.ExUid loc "()")
          else MLast.ExSeq loc el ] ]
  ;
  tag_body:
    [ [ tn = STRING; a = OPT expr; "begin"; el = LIST0 expr_semi; "end" ->
          (tn, a, el) ] ]
  ;
  expr_semi:
    [ [ e = expr; ";" -> e ] ]
  ;
END;
