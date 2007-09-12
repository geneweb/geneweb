(* camlp5r q_MLast.cmo pa_extfun.cmo *)
(* $Id: pr_html.ml,v 5.3 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Pcaml;
open Spretty;

value loc = (0, 0);

value expr e dg k = pr_expr.pr_fun "top" e dg k;
value expr_dot e dg k = pr_expr.pr_fun "dot" e dg k;
value patt e dg k = pr_patt.pr_fun "top" e dg k;

value end_with s pat =
  loop (String.length s - 1) (String.length pat - 1)
  where rec loop i j =
    if j < 0 then True
    else if i < 0 then False
    else if s.[i] = pat.[j] then loop (i - 1) (j - 1)
    else False
;

value start_with s pat =
  loop 0 0 where rec loop i j =
    if j = String.length pat then True
    else if i = String.length s then False
    else if s.[i] = pat.[j] then loop (i + 1) (j + 1)
    else False
;

value get_tag =
  fun
  [ [e :: el] ->
      match List.rev el with
      [ [<:expr< Wserver.wprint $str:t$ >> :: el]
        when start_with t "</" && (end_with t ">\n" || end_with t ">") ->
          let stag = end_with t ">" in
          let elen = if stag then 1 else 2 in
          let tg = String.sub t 2 (String.length t - elen - 2) in
          let pl =
            loop e where rec loop =
              fun
              [ <:expr< Wserver.wprint $str:s$ >> ->
                  if start_with s "<"
                  && (stag && end_with s ">" || end_with s ">\n") then
                    let ptg = String.sub s 1 (String.length s - elen - 1) in
                    if ptg = tg then []
                    else if start_with ptg (tg ^ " ") then
                      let i = 1 + String.length tg + 1 in
                      let s = String.sub s i (String.length s - elen - i) in
                      [<:expr< $str:s$ >>]
                    else raise Not_found
                  else raise Not_found
              | <:expr< $a$ $b$ >> -> [b :: loop a]
              | _ -> raise Not_found ]
          in
          (stag, tg, List.rev pl, List.rev el)
      | _ -> raise Not_found ]
  | _ -> raise Not_found ]
;

value is_tag el =
  try let _ = get_tag el in True with
  [ Not_found -> False ]
;

value rec sequence_loop =
  fun
  [ [e :: el] -> [: `expr e "" [: `S RO ";" :]; sequence_loop el :]
  | [] -> [: :] ]
;

value rec list elem el dg k =
  match el with
  [ [] -> k
  | [x] -> [: `elem x dg k :]
  | [x :: l] -> [: `elem x "" [: :]; list elem l dg k :] ]
;

value tag_box stag t pl el k =
  BEbox
    [: `HOVbox
          [: `HVbox [: :];
             `HOVbox
                [: `S LR (if stag then "stag" else "tag");
                   `S LR ("\"" ^ t ^ "\"");
                   list expr_dot pl "" [: :] :];
             `S LR "begin" :];
       `HVbox [: `HVbox [: :]; sequence_loop el :];
       `HVbox [: `S LR "end"; k :] :]
;

let lev = find_pr_level "top" pr_expr.pr_levels in
lev.pr_rules :=
  extfun lev.pr_rules with
  [ <:expr< do { $list:el$ } >> when is_tag el ->
      fun curr next _ k ->
        let (stag, t, pl, el) = get_tag el in
        [: `tag_box stag t pl el k :]
  | <:expr< if $e1$ then do { $list:el$ } else $e3$ >> when is_tag el ->
      fun curr next _ k ->
        let (stag, t, pl, el) = get_tag el in
        [: `HVbox
              [: `HVbox [: :];
                 `HVbox
                   [: `HVbox [: `S LR "if"; `expr e1 "" [: `S LR "then" :] :];
                       `tag_box stag t pl el [: :] :];
                 `HVbox [: `S LR "else"; `expr e3 "" k :] :] :] ];
