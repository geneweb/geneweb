(* camlp5r q_MLast.cmo *)
(* $Id: pr_transl.ml,v 5.5 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

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

value loc = Token.dummy_loc;
value token_eval_string loc s = Token.eval_string loc s;

value rec expr e =
  match e with
  [ <:expr< $lid:f$ $_$ $str:s$ >> when List.mem f trace ->
      Printf.printf "%s\n" (token_eval_string loc s)
  | <:expr< Util.$lid:f$ $_$ $str:s$ >> when List.mem f trace ->
      Printf.printf "%s\n" (token_eval_string loc s)
  | <:expr< $lid:f$ $_$ ($lid:g$ $_$ $str:s$ $_$) >>
    when List.mem f trace && List.mem g trace ->
      Printf.printf "%s\n" (token_eval_string loc s)
  | <:expr< $lid:x$ >> when List.mem x trace ->
      let loc = MLast.loc_of_expr e in
      let (fn, ln, bp, ep) = Stdpp.line_of_loc Pcaml.input_file.val loc in
      let fn = if fn = "" then Pcaml.input_file.val else fn in
      Printf.printf "File \"%s\", line %d, characters %d-%d: bad source\n"
        fn ln bp ep
  | <:expr< let $opt:_$ $list:pel$ in $e$ >> ->
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
  | <:expr< $_$ := $e$ >> -> expr e
  | <:expr< $_$.($_$) >> -> ()
  | <:expr< $_$.[$_$] >> -> ()
  | <:expr< $lid:_$ >> -> ()
  | <:expr< $uid:_$ >> -> ()
  | <:expr< $str:_$ >> -> ()
  | <:expr< $int:_$ >> -> ()
  | <:expr< $flo:_$ >> -> ()
  | <:expr< $chr:_$ >> -> ()
  | <:expr< $x$.$y$ >> -> ()
  | <:expr< assert False >> -> ()
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
  | <:module_expr< $uid:_$ >> -> ()
  | <:module_expr< ($me$ : $_$) >> -> module_expr me
  | x -> not_impl "module_expr" x ]
and str_item =
  fun
  [ <:str_item< declare $list:sil$ end >> -> List.iter str_item sil
  | <:str_item< open $_$ >> -> ()
  | <:str_item< value $opt:_$ $list:pel$ >> -> binding_list pel
  | <:str_item< type $list:_$ >> -> ()
  | <:str_item< exception $_$ of $list:_$ >> -> ()
  | <:str_item< module $_$ = $me$ >> -> module_expr me
  | <:str_item< module type $_$ = $_$ >> -> ()
  | <:str_item< $exp:e$ >> -> expr e
  | <:str_item< # $_$ $_$ >> -> ()
  | x -> not_impl "str_item" x ]
;

value f (ast, loc) = do { str_item ast; flush stdout; () };

Pcaml.print_implem.val := List.iter f;
