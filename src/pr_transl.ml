(* camlp4r q_MLast.cmo *)
(* $Id: pr_transl.ml,v 4.4 2001-06-25 14:59:47 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open MLast;

value not_impl name x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  failwith ("Pr_transl." ^ name ^ ": not impl " ^ desc)
;

value trace =
  ["cftransl"; "transl"; "transl_nth"; "transl_decline"; "transl_decline2";
   "ftransl"; "ftransl_nth"]
;

value rec expr e =
  match e with
  [ <:expr< $lid:f$ $_$ $str:s$ >> when List.mem f trace ->
      Printf.printf "%s\n" (Token.eval_string s)
  | <:expr< $lid:f$ $_$ ($lid:g$ $_$ $str:s$ $_$) >>
    when List.mem f trace && List.mem g trace ->
      Printf.printf "%s\n" (Token.eval_string s)
  | <:expr< $lid:x$ >> when List.mem x trace ->
      Stdpp.raise_with_loc (MLast.loc_of_expr e) (Failure "Bad source")
  | <:expr< let $rec:_$ $list:pel$ in $e$ >> ->
      do { binding_list pel; expr e; () }
  | <:expr< fun [ $list:pel$ ] >> -> List.iter fun_binding pel
  | <:expr< match $e$ with [ $list:pel$ ] >> ->
      do { expr e; List.iter fun_binding pel; () }
  | <:expr< try $e$ with [ $list:pel$ ] >> ->
      do { expr e; List.iter fun_binding pel; () }
  | <:expr< do { $list:el$ } >> -> List.iter expr el
  | <:expr< if $e1$ then $e2$ else $e3$ >> ->
      do { expr e1; expr e2; expr e3; () }
  | <:expr< for $_$ = $_$ $to:_$ $_$ do { $list:el$ } >> -> List.iter expr el
  | <:expr< while $_$ do { $list:el$ } >> -> List.iter expr el
  | <:expr< let module $m$ = $me$ in $e$ >> -> expr e
  | <:expr< ($list:el$) >> -> List.iter expr el
  | <:expr< ($e$:$_$) >> -> expr e
  | <:expr< [| $list:el$ |] >> -> List.iter expr el
  | <:expr< $x$ $y$ >> -> do { expr x; expr y; () }
  | <:expr< { $list:fel$ } >> -> List.iter (fun (_, e) -> expr e) fel
  | <:expr< { ($e$) with $list:fel$ } >> ->
      do { expr e; List.iter (fun (_, e) -> expr e) fel; () }
  | <:expr< $_$ := $_$ >> -> ()
  | <:expr< $_$.($_$) >> -> ()
  | <:expr< $_$.[$_$] >> -> ()
  | <:expr< $lid:_$ >> -> ()
  | <:expr< $uid:_$ >> -> ()
  | <:expr< $str:_$ >> -> ()
  | <:expr< $int:_$ >> -> ()
  | <:expr< $flo:_$ >> -> ()
  | <:expr< $chr:_$ >> -> ()
  | <:expr< $x$.$y$ >> -> ()
  | x -> not_impl "expr" x ]
and binding_list pel = List.iter binding pel
and binding (p, e) =
  match p with
  [ <:patt< $lid:s$ >> when List.mem s trace -> ()
  | _ -> expr e ]
and fun_binding (p, _, e) = expr e;

value rec module_expr =
  fun
  [ <:module_expr< $_$ . $_$ >> -> ()
  | <:module_expr< $me1$ $me2$ >> ->
      do { module_expr me1; module_expr me2; () }
  | <:module_expr< struct $list:sil$ end >> -> List.iter str_item sil
  | x -> not_impl "module_expr" x ]
and str_item =
  fun
  [ <:str_item< declare $list:sil$ end >> -> List.iter str_item sil
  | <:str_item< open $_$ >> -> ()
  | <:str_item< value $rec:_$ $list:pel$ >> -> binding_list pel
  | <:str_item< type $list:_$ >> -> ()
  | <:str_item< exception $_$ of $list:_$ >> -> ()
  | <:str_item< module $_$ = $me$ >> -> module_expr me
  | <:str_item< $exp:e$ >> -> expr e
  | x -> not_impl "str_item" x ]
;

value f (ast, loc) = do { str_item ast; flush stdout; () };

Pcaml.print_implem.val := List.iter f;
