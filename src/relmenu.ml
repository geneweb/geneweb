(* camlp4r *)
(* $Id: relmenu.ml,v 4.2 2005-05-07 17:50:50 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Gutil;
open Config;
open Util;
open TemplAst;

type env = 
  [ Vind of person
  | Vrel of string and person
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
  Wserver.wprint ">%s<p>\n" ("Relmenu." ^ func ^ ": not impl " ^ desc)
;

value rec eval_var conf base env ep loc sl =
  try VVbool (eval_bool_var conf base ep loc sl) with
  [ Not_found -> VVstring (eval_str_var conf base env ep loc sl) ]
and eval_bool_var conf base (p, p_auth_name) loc =
  fun
  [ ["access_by_key"] ->
      Util.accessible_by_key conf base p (p_first_name base p)
        (p_surname base p)
  | ["sosa_ref"] -> Util.find_sosa_ref conf base <> None
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
  | ["occ"] -> if p_auth_name then string_of_int p.occ else ""
  | ["public_name"] -> if not p_auth_name then "" else sou base p.public_name
  | ["qualifier"] ->
      match p.qualifiers with
      [ [nn :: _] -> if not p_auth_name then "" else sou base nn
      | [] -> "" ]
  | ["related"] ->
      match get_env "related" env with
      [ Vrel txt p -> person_title_text conf base p
      | _ -> "" ]
  | ["related_index"] ->
      match get_env "related" env with
      [ Vrel txt p -> string_of_int (Adef.int_of_iper p.cle_index)
      | _ -> "" ]
  | ["related_type"] ->
      match get_env "related" env with
      [ Vrel txt p -> txt
      | _ -> "" ]
  | ["relation"] ->
      match get_env "relation" env with
      [ Vrel txt p -> person_title_text conf base p
      | _ -> "" ]
  | ["relation_index"] ->
      match get_env "relation" env with
      [ Vrel txt p -> string_of_int (Adef.int_of_iper p.cle_index)
      | _ -> "" ]
  | ["relation_type"] ->
      match get_env "relation" env with
      [ Vrel txt p -> txt
      | _ -> "" ]
  | ["sosa_link"] ->
      match Util.find_sosa_ref conf base with
      [ Some p -> referenced_person_title_text conf base p
      | None -> "" ]
  | ["spouse"] ->
      match get_env "spouse" env with
      [ Vind c -> person_title_text conf base c
      | _ -> "" ]
  | ["spouse_index"] ->
      match get_env "spouse" env with
      [ Vind c -> string_of_int (Adef.int_of_iper c.cle_index)
      | _ -> "" ]
  | ["surname"] -> if not p_auth_name then "x" else p_surname base p
  | ["surname_key_val"] ->
      if not p_auth_name then "" else Name.lower (p_surname base p)
  | ["witness"] ->
      match get_env "witness" env with
      [ Vind p -> person_title_text conf base p
      | _ -> "" ]
  | ["witness_index"] ->
      match get_env "witness" env with
      [ Vind p -> string_of_int (Adef.int_of_iper p.cle_index)
      | _ -> "" ]
  | _ -> raise Not_found ]
;

value rec print_ast conf base env ep =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Avar loc s sl ->
      Templ.print_var conf base (eval_var conf base env ep loc) s sl
  | Atransl upp s c -> Wserver.wprint "%s" (Templ.eval_transl conf upp s c)
  | Aif e alt ale -> print_if conf base env ep e alt ale
  | Aforeach v al -> print_foreach conf base env ep v al
  | Adefine f xl al alk -> print_define conf base env ep f xl al alk
  | Aapply f el -> print_apply conf base env ep f el
  | x -> not_impl "print_ast" x ]
and print_define conf base env ep f xl al alk =
  List.iter (print_ast conf base [(f, Vfun xl al) :: env] ep) alk
and print_apply conf base env p f el =
  match get_env f env with
  [ Vfun xl al ->
      let eval_var = eval_var conf base env p in
      let print_ast = print_ast conf base env p in
      Templ.print_apply conf f print_ast eval_var xl al el
  | _ -> Wserver.wprint " %%%s?" f ]
and print_if conf base env p e alt ale =
  let eval_var = eval_var conf base env p in
  let al = if Templ.eval_bool_expr conf eval_var e then alt else ale in
  List.iter (print_ast conf base env p) al
and print_foreach conf base env ep (loc, s, sl) al =
  match [s :: sl] with
  [ ["related"] -> print_foreach_related conf base env ep al
  | ["relation"] -> print_foreach_relation conf base env ep al
  | ["spouse"] -> print_foreach_spouse conf base env ep al
  | ["witness"] -> print_foreach_witness conf base env ep al
  | _ ->
      do {
        Wserver.wprint ">%%foreach;%s" s;
        List.iter (fun s -> Wserver.wprint ".%s" s) sl;
        Wserver.wprint "?";
       } ]
and print_foreach_related conf base env ((p, _) as ep) al =
  List.iter
    (fun ip ->
       let c = pget conf base ip in
       if is_hidden c then ()
       else
         List.iter
           (fun r ->
              List.iter
                (fun
                 [ Some ip1 when p.cle_index == ip1 ->
                     let rs = index_of_sex c.sex in
                     let txt = rchild_type_text conf r.r_type rs in
                     let env = [("related", Vrel txt c) :: env] in
                     List.iter (print_ast conf base env ep) al
                 | _ -> () ])
                [r.r_fath; r.r_moth])
           c.rparents)
    p.related
and print_foreach_relation conf base env ((p, _) as ep) al =
  List.iter
    (fun r ->
       List.iter
         (fun
          [ (rs, Some ic) ->
              let c = pget conf base ic in
              if is_hidden c then ()
              else
                let txt = relation_type_text conf r.r_type rs in
                let env = [("relation", Vrel txt (poi base ic)) :: env] in
                List.iter (print_ast conf base env ep) al
          | (_, None) -> () ])
         [(0, r.r_fath); (1, r.r_moth)])
    p.rparents
and print_foreach_spouse conf base env ((p, _) as ep) al =
  let u = uoi base p.cle_index in
  Array.iter
    (fun ifam ->
       let cpl = coi base ifam in
       let c = spouse p.cle_index cpl in
       let c = pget conf base c in
       let env = [("spouse", Vind c) :: env] in
       if (p_first_name base c <> "?" || p_surname base c <> "?")
          && not (is_hidden c)
       then
         List.iter (print_ast conf base env ep) al
       else ())
    u.family
and print_foreach_witness conf base env ((p, _) as ep) al =
  let u = uoi base p.cle_index in
  Array.iter
    (fun ifam ->
       let fam = foi base ifam in
       Array.iter
         (fun ip ->
            let w = pget conf base ip in
            if is_hidden w then ()
            else
              let env = [("witness", Vind w) :: env] in
              List.iter (print_ast conf base env ep) al
         )
         fam.witnesses)
    u.family
;

value print conf base p =
  let astl = Templ.input conf "relmenu" in
  let p_auth_name = authorized_age conf base p || not conf.hide_names in
  let ep = (p, p_auth_name) in
  do {
    Util.html1 conf;
    List.iter (print_ast conf base [] ep) astl;
  }
;
