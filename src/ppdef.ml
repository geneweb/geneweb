(* camlp4r *)
(* $Id: ppdef.ml,v 4.8 2005-09-16 22:05:49 ddr Exp $ *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";

open Pcaml;

Stdpp.loc_name.val := "loc";

type item_or_def 'a =
  [ SdStr of 'a
  | SdDef of string and option (list string * MLast.expr)
  | SdUnd of string
  | SdNop ]
;

value rec list_remove x =
  fun
  [ [(y, _) :: l] when y = x -> l
  | [d :: l] -> [d :: list_remove x l]
  | [] -> [] ]
;

value defined = ref [];

value is_defined i = List.mem_assoc i defined.val;

value loc = Grammar.loc_of_token_interval 0 0;

value subst mloc env =
  loop where rec loop =
    fun
    [ MLast.ExLet _ rf pel e ->
        let pel = List.map (fun (p, e) -> (p, loop e)) pel in
        MLast.ExLet loc rf pel (loop e)
    | MLast.ExIfe _ e1 e2 e3 -> MLast.ExIfe loc (loop e1) (loop e2) (loop e3)
    | MLast.ExApp _ e1 e2 -> MLast.ExApp loc (loop e1) (loop e2)
    | MLast.ExLid _ x | MLast.ExUid _ x as e ->
        try MLast.ExAnt loc (List.assoc x env) with [ Not_found -> e ]
    | MLast.ExTup _ x -> MLast.ExTup loc (List.map loop x)
    | MLast.ExRec _ pel None ->
        let pel = List.map (fun (p, e) -> (p, loop e)) pel in
        MLast.ExRec loc pel None
    | e -> e ]
;

value substp mloc env =
  loop where rec loop =
    fun
    [ MLast.ExApp _ e1 e2 -> MLast.PaApp loc (loop e1) (loop e2)
    | MLast.ExChr _ c -> MLast.PaChr loc c
    | MLast.ExLid _ x ->
        try MLast.PaAnt loc (List.assoc x env) with
        [ Not_found -> MLast.PaLid loc x ]
    | MLast.ExUid _ x ->
        try MLast.PaAnt loc (List.assoc x env) with
        [ Not_found -> MLast.PaUid loc x ]
    | MLast.ExInt _ x -> MLast.PaInt loc x
    | MLast.ExTup _ x -> MLast.PaTup loc (List.map loop x)
    | MLast.ExRec _ pel None ->
        let ppl = List.map (fun (p, e) -> (p, loop e)) pel in
        MLast.PaRec loc ppl
    | x ->
        Stdpp.raise_with_loc mloc
          (Failure
             "this macro cannot be used in a pattern (see its definition)") ]
;

value incorrect_number loc l1 l2 =
  Stdpp.raise_with_loc loc
    (Failure
       (Printf.sprintf "expected %d parameters; found %d"
          (List.length l2) (List.length l1)))
;

value define eo x =
  do {
    let gloc = loc in
    match eo with
    [ Some ([], e) ->
        EXTEND
          expr: LEVEL "simple"
            [ [ UIDENT $x$ -> Pcaml.expr_reloc (fun _ -> loc) (fst gloc) e ] ]
          ;
          patt: LEVEL "simple"
            [ [ UIDENT $x$ ->
                  let p = substp loc [] e in
                  Pcaml.patt_reloc (fun _ -> loc) (fst gloc) p ] ]
          ;
        END
    | Some (sl, e) ->
        EXTEND
          expr: LEVEL "apply"
            [ [ UIDENT $x$; param = SELF ->
                  let el =
                    match param with
                    [ <:expr< ($list:el$) >> -> el
                    | e -> [e] ]
                  in
                  if List.length el = List.length sl then
                    let env = List.combine sl el in
                    let e = subst loc env e in
                    Pcaml.expr_reloc (fun _ -> loc) (fst gloc) e
                  else
                    incorrect_number loc el sl ] ]
          ;
          patt: LEVEL "simple"
            [ [ UIDENT $x$; param = SELF ->
                  let pl =
                    match param with
                    [ <:patt< ($list:pl$) >> -> pl
                    | p -> [p] ]
                  in
                  if List.length pl = List.length sl then
                    let env = List.combine sl pl in
                    let p = substp loc env e in
                    Pcaml.patt_reloc (fun _ -> loc) (fst gloc) p
                  else
                    incorrect_number loc pl sl ] ]
          ;
        END
    | None -> () ];
    defined.val := [(x, eo) :: defined.val];
  }
;

value undef x =
  try
    do {
      let eo = List.assoc x defined.val in
      match eo with
      [ Some ([], _) ->
          do {
            DELETE_RULE expr: UIDENT $x$ END;
            DELETE_RULE patt: UIDENT $x$ END;
          }
      | Some (_, _) ->
          do {
            DELETE_RULE expr: UIDENT $x$; SELF END;
            DELETE_RULE patt: UIDENT $x$; SELF END;
          }
      | None -> () ];
      defined.val := list_remove x defined.val;
    }
  with
  [ Not_found -> () ]
;

EXTEND
  GLOBAL: expr patt str_item sig_item;
  str_item: FIRST
    [ [ x = macro_def ->
          match x with
          [ SdStr [si] -> si
          | SdStr sil -> <:str_item< declare $list:sil$ end >>
          | SdDef x eo -> do { define eo x; <:str_item< declare end >> }
          | SdUnd x -> do { undef x; <:str_item< declare end >> }
          | SdNop -> <:str_item< declare end >> ] ] ]
  ;
  macro_def:
    [ [ "DEFINE"; i = uident; def = opt_macro_value -> SdDef i def
      | "UNDEF"; i = uident -> SdUnd i
      | "IFDEF"; i = uident; "THEN"; d = str_item_or_macro; "END" ->
          if is_defined i then d else SdNop
      | "IFDEF"; i = uident; "THEN"; d1 = str_item_or_macro; "ELSE";
        d2 = str_item_or_macro; "END" ->
          if is_defined i then d1 else d2
      | "IFNDEF"; i = uident; "THEN"; d = str_item_or_macro; "END" ->
          if is_defined i then SdNop else d
      | "IFNDEF"; i = uident; "THEN"; d1 = str_item_or_macro; "ELSE";
        d2 = str_item_or_macro; "END" ->
          if is_defined i then d2 else d1 ] ]
  ;
  str_item_or_macro:
    [ [ d = macro_def -> d
      | si = LIST1 str_item -> SdStr si ] ]
  ;
  opt_macro_value:
    [ [ "("; pl = LIST1 LIDENT SEP ","; ")"; "="; e = expr -> Some (pl, e)
      | "="; e = expr -> Some ([], e)
      | -> None ] ]
  ;
  expr: LEVEL "top"
    [ [ "IFDEF"; idl = LIST1 id_then_expr SEP "ELSIFDEF"; "ELSE";
        e2 = expr; "END" ->
          loop idl where rec loop =
            fun
            [ [(i, e) :: idl] -> if is_defined i then e else loop idl
            | [] -> e2 ]
      | "IFNDEF"; i = uident; "THEN"; e1 = expr; "ELSE"; e2 = expr; "END" ->
          if is_defined i then e2 else e1 ] ]
  ;
  id_then_expr:
    [ [ i = uident; "THEN"; e = expr -> (i, e) ] ]
  ;
  expr: LEVEL "simple"
    [ [ LIDENT "__FILE__" -> <:expr< $str:Pcaml.input_file.val$ >> ] ]
  ;
  patt:
    [ [ "IFDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; "END" ->
          if is_defined i then p1 else p2
      | "IFNDEF"; i = uident; "THEN"; p1 = patt; "ELSE"; p2 = patt; "END" ->
          if is_defined i then p2 else p1 ] ]
  ;
  uident:
    [ [ i = UIDENT -> i ] ]
  ;
END;

Pcaml.add_option "-D" (Arg.String (define None))
  "<string> Define for IFDEF instruction."
;
Pcaml.add_option "-U" (Arg.String undef)
  "<string> Undefine for IFDEF instruction."
;

if Sys.ocaml_version >= "3.07" then
  defined.val := [("OCAML_307", None) :: defined.val]
else ();

if Sys.ocaml_version >= "3.08" then
  defined.val := [("OCAML_308", None) :: defined.val]
else ();
