(* camlp5r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_html.ml,v 5.5 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Pcaml;

value rec unfold_apply list =
  fun
  [ <:expr< $x1$ $x2$ >> -> unfold_apply [x2 :: list] x1
  | e -> (e, list) ]
;

value tag_encloser loc tag newl enewl a el =
  let s = if newl then "\\n" else "" in
  let se = if newl || enewl then "\\n" else "" in
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
  [e :: el @ [<:expr< Wserver.wprint $str:"</" ^ tag ^ ">" ^ se$ >>]]
;

value tag_alone loc tag a =
  let s = "\\n" in
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
      <:expr< Wserver.wprint $str:"<" ^ tag ^ frm ^ "%s>" ^ s$ >> al
  in
  <:expr< $e$ conf.xhs >>
;

EXTEND
  GLOBAL: expr;
  expr: LEVEL "top"
    [ [ "tag"; (tn, al, el) = tag_body ->
          let el = tag_encloser loc tn True True al el in
          <:expr< do { $list:el$ } >>
      | "stag"; (tn, al, el) = tag_body ->
          let el = tag_encloser loc tn False False al el in
          <:expr< do { $list:el$ } >>
      | "stagn"; (tn, al, el) = tag_body ->
          let el = tag_encloser loc tn False True al el in
          <:expr< do { $list:el$ } >>
      | "xtag"; tn = STRING; a = OPT expr ->
          tag_alone loc tn a ] ]
  ;
  tag_body:
    [ [ tn = STRING; a = OPT expr; "begin"; el = LIST0 expr_semi; "end" ->
          (tn, a, el) ] ]
  ;
  expr_semi:
    [ [ e = expr; ";" -> e ] ]
  ;
END;
