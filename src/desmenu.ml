(* camlp4r *)
(* $Id: desmenu.ml,v 4.3 2005-05-12 02:51:10 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;
open Gutil;
open Util;
open TemplAst;

type env = 
  [ Vint of int
  | Vfun of list string and list ast
  | Vnone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  ">Desmenu." ^ func ^ ": not impl " ^ desc ^ "<p>\n"
;

value limit_desc conf =
  match p_getint conf.base_env "max_desc_level" with
  [ Some x -> max 1 x
  | None -> 12 ]
;

value infinite = 10000;

value make_level_table conf base max_level p =
  let mark = Array.create base.data.persons.len False in
  let levt = Array.create base.data.persons.len infinite in
  let rec fill ip u lev =
    if max_level == infinite && mark.(Adef.int_of_iper ip) then ()
    else do {
      mark.(Adef.int_of_iper ip) := True;
      if lev <= max_level then do {
        if lev < levt.(Adef.int_of_iper ip) then
          levt.(Adef.int_of_iper ip) := lev
        else ();
        Array.iter
          (fun ifam ->
             let ipl = (doi base ifam).children in
             Array.iter (fun ip -> fill ip (uget conf base ip) (succ lev)) ipl)
          u.family
      }
      else ()
    }
  in
  do { fill p.cle_index (uget conf base p.cle_index) 0; levt }
;

value level_max conf base p =
  let levt = make_level_table conf base infinite p in
  let x = ref 0 in
  do {
    for i = 0 to Array.length levt - 1 do {
      let lev = levt.(i) in
      if lev != infinite && x.val < lev then x.val := lev else ()
    };
    x.val
  }
;

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value rec eval_var conf base env ep loc sl =
  try VVbool (eval_bool_var conf base ep loc sl) with
  [ Not_found -> VVstring (eval_str_var conf base env ep loc sl) ]
and eval_bool_var conf base (p, p_auth_name) loc =
  fun
  [ ["access_by_key"] ->
      Util.accessible_by_key conf base p (p_first_name base p)
        (p_surname base p)
  | _ -> raise Not_found ]
and eval_str_var conf base env (p, p_auth_name) loc =
  fun
  [ ["alias"] ->
      match p.aliases with
      [ [nn :: _] -> if not p_auth_name then "" else sou base nn
      | [] -> "" ]
  | ["first_name"] -> if not p_auth_name then "x" else p_first_name base p
  | ["first_name_key_val"] ->
      if not p_auth_name then "" else Name.lower (p_first_name base p)
  | ["index"] -> string_of_int (Adef.int_of_iper p.cle_index)
  | ["level"] ->
      match get_env "level" env with
      [ Vint i -> string_of_int i
      | _ -> "" ]
  | ["max_desc_level"] ->
      match get_env "max_level" env with
      [ Vint i -> string_of_int i
      | _ -> "" ]
  | ["occ"] -> if p_auth_name then string_of_int p.occ else ""
  | ["public_name"] -> if not p_auth_name then "" else sou base p.public_name
  | ["qualifier"] ->
      match p.qualifiers with
      [ [nn :: _] -> if not p_auth_name then "" else sou base nn
      | [] -> "" ]
  | ["surname"] -> if not p_auth_name then "x" else p_surname base p
  | ["surname_key_val"] ->
      if not p_auth_name then "" else Name.lower (p_surname base p)
  | [s] ->
      let v = extract_var "bvar_" s in
      if v <> "" then try List.assoc v conf.base_env with [ Not_found -> "" ]
      else raise Not_found
  | _ -> raise Not_found ]
;

value rec eval_ast conf base env ep =
  fun
  [ Atext s -> s
  | Avar loc s sl ->
      Templ.eval_string_var conf (eval_var conf base env ep loc) s sl
  | Atransl upp s c -> Templ.eval_transl conf upp s c
  | Awid_hei s ->
      match image_size (image_file_name s) with
      [ Some (wid, hei) -> Printf.sprintf " width=\"%d\" height=\"%d\"" wid hei
      | None -> "" ]
  | Aif e alt ale -> eval_if conf base env ep e alt ale
  | AapplyWithAst "a_of_b_gr_eq_lev" [al1; al2] ->
      let eval_ast = eval_ast conf base env ep in
      let s1 = String.concat "" (List.map eval_ast al1) in
      let s2 = String.concat "" (List.map eval_ast al2) in
      capitale (transl_a_of_gr_eq_gen_lev conf s1 s2)
  | AapplyWithAst "nth" [al1; al2] ->
      let eval_ast = eval_ast conf base env ep in
      let s1 = String.concat "" (List.map eval_ast al1) in
      let s2 = String.concat "" (List.map eval_ast al2) in
      Util.nth_field s1 (try int_of_string s2 with [ Failure _ -> 0 ])
  | AapplyWithAst f all -> eval_apply conf base env ep f all
  | x -> not_impl "eval_ast" x ]
and eval_apply conf base env ep f all =
  match get_env f env with
  [ Vfun xl al ->
      let eval_ast = eval_ast conf base env ep in
      Templ.eval_apply f eval_ast xl al all
  | _ -> Printf.sprintf "%%apply;%s?" f ]
and eval_if conf base env p e alt ale =
  let eval_var = eval_var conf base env p in
  let al = if Templ.eval_bool_expr conf eval_var e then alt else ale in
  String.concat "" (List.map (eval_ast conf base env p) al)
;

value rec print_ast conf base env ep =
  fun
  [ Avar loc s sl ->
      Templ.print_var conf base (eval_var conf base env ep loc) s sl
  | Aif e alt ale -> print_if conf base env ep e alt ale
  | Aforeach v al -> print_foreach conf base env ep v al
  | Adefine f xl al alk -> print_define conf base env ep f xl al alk
  | Aapply f el -> print_apply conf base env ep f el
  | x -> Wserver.wprint "%s" (eval_ast conf base env ep x) ]
and print_define conf base env ep f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env] ep) alk
and print_apply conf base env ep f el =
  match get_env f env with
  [ Vfun xl al ->
      let eval_var = eval_var conf base env ep in
      let print_ast = print_ast conf base env ep in
      Templ.print_apply conf f print_ast eval_var xl al el
  | _ -> Wserver.wprint "%%apply;%s?" f ]
and print_if conf base env p e alt ale =
  let eval_var = eval_var conf base env p in
  let al = if Templ.eval_bool_expr conf eval_var e then alt else ale in
  List.iter (print_ast conf base env p) al
and print_foreach conf base env ep (loc, s, sl) al =
  match [s :: sl] with
  [ ["descendant_level"] -> print_foreach_level conf base env ep al
  | _ ->
      do {
        Wserver.wprint ">%%foreach;%s" s;
        List.iter (fun s -> Wserver.wprint ".%s" s) sl;
        Wserver.wprint "?";
       } ]
and print_foreach_level conf base env ((p, _) as ep) al =
  let max_level =
    match get_env "max_level" env with
    [ Vint n -> n
    | _ -> 0 ]
  in
  loop 0 where rec loop i =
    if i > max_level then ()
    else
      let env = [("level", Vint i) :: env] in
      do {
        List.iter (print_ast conf base env ep) al;
        loop (succ i)
      }
;

value print conf base p =
  let max_level = min (limit_desc conf) (level_max conf base p) in
  let astl = Templ.input conf "desmenu" in
  let p_auth_name = authorized_age conf base p || not conf.hide_names in
  let env = [("max_level", Vint max_level)] in
  let ep = (p, p_auth_name) in
  do {
    Util.html1 conf;
    List.iter (print_ast conf base env ep) astl;
  }
;
