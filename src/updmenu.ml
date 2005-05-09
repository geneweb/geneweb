(* camlp4r *)
(* $Id: updmenu.ml,v 4.1 2005-05-09 04:43:42 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;
open Gutil;
open Util;
open TemplAst;

type env = 
  [ Vint of int
  | Vfam of Adef.ifam
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
  ">Updmenu." ^ func ^ ": not impl " ^ desc ^ "<p>\n"
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
  [ ["has_parents"] -> parents (aoi base p.cle_index) <> None
  | ["is_female"] -> p.sex = Female
  | ["is_male"] -> p.sex = Male
  | _ -> raise Not_found ]
and eval_str_var conf base env (p, p_auth_name) loc =
  fun
  [ ["auto_image_file_name"] ->
      match auto_image_file conf base p with
      [ Some s when p_auth_name -> s
      | _ -> "" ]
  | ["cnt"] ->
      match get_env "cnt" env with
      [ Vint i -> string_of_int i
      | _ -> "" ]
  | ["first_name"] -> if not p_auth_name then "x" else p_first_name base p
  | ["fam_father"] ->
      match get_env "fam" env with
      [ Vfam ifam -> string_of_int (Adef.int_of_iper (father (coi base ifam)))
      | _ -> "" ]
  | ["fam_index"] ->
      match get_env "fam" env with
      [ Vfam ifam -> string_of_int (Adef.int_of_ifam ifam)
      | _ -> "" ]
  | ["fam_mother"] ->
      match get_env "fam" env with
      [ Vfam ifam -> string_of_int (Adef.int_of_iper (mother (coi base ifam)))
      | _ -> "" ]
  | ["image"] -> if not p_auth_name then "" else sou base p.image
  | ["index"] -> string_of_int (Adef.int_of_iper p.cle_index)
  | ["nb_children"] ->
      match get_env "fam" env with
      [ Vfam ifam -> string_of_int (Array.length (doi base ifam).children)
      | _ ->
          let n =
            List.fold_left
              (fun n ifam -> n + Array.length (doi base ifam).children) 0
              (Array.to_list (uoi base p.cle_index).family)
          in
          string_of_int n ]
  | ["nb_families"] ->
      string_of_int (Array.length (uoi base p.cle_index).family)
  | ["occ"] -> if p_auth_name then string_of_int p.occ else ""
  | ["prev_fam_father"] ->
      match get_env "prev_fam" env with
      [ Vfam ifam -> string_of_int (Adef.int_of_iper (father (coi base ifam)))
      | _ -> "" ]
  | ["prev_fam_index"] ->
      match get_env "prev_fam" env with
      [ Vfam ifam -> string_of_int (Adef.int_of_ifam ifam)
      | _ -> "" ]
  | ["prev_fam_mother"] ->
      match get_env "prev_fam" env with
      [ Vfam ifam -> string_of_int (Adef.int_of_iper (mother (coi base ifam)))
      | _ -> "" ]
  | ["spouse"] ->
      match get_env "fam" env with
      [ Vfam ifam ->
          let p = poi base (spouse p.cle_index (coi base ifam)) in
          p_first_name base p ^
            (if p.occ = 0 then "" else "." ^ string_of_int p.occ) ^ " " ^
            p_surname base p
      | _ -> "" ]
  | ["surname"] -> if not p_auth_name then "x" else p_surname base p
  | [s] ->
      let v = extract_var "bvar_" s in
      if v <> "" then try List.assoc v conf.base_env with [ Not_found -> "" ]
      else raise Not_found
  |_ -> raise Not_found ]
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
  | x -> not_impl "eval_ast" x ]
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
  [ ["family"] -> print_foreach_family conf base env ep al
  | _ ->
      do {
        Wserver.wprint ">%%foreach;%s" s;
        List.iter (fun s -> Wserver.wprint ".%s" s) sl;
        Wserver.wprint "?";
       } ]
and print_foreach_family conf base env ((p, p_auth) as ep) al =
  let a = (uoi base p.cle_index).family in
  loop None 0 where rec loop prev_fam i =
    if i = Array.length a then ()
    else
      let ifam = a.(i) in
      let env = [("fam", Vfam ifam); ("cnt", Vint (i + 1)) :: env] in
      let env =
        match prev_fam with
        [ Some ifam -> [("prev_fam", Vfam ifam) :: env]
        | None -> env ]
      in
      do {
        List.iter (print_ast conf base env ep) al;
        loop (Some ifam) (i + 1);
      }
;

value print conf base p =
  let astl = Templ.input conf "updmenu" in
  let p_auth_name = True in
  let env = [] in
  let ep = (p, p_auth_name) in
  do {
    Util.html1 conf;
    List.iter (print_ast conf base env ep) astl;
  }
;
