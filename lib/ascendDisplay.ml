
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Dag2html
open Gwdb
open TemplAst
open Util

let limit_by_tree conf =
  match Util.p_getint conf.base_env "max_anc_tree" with
    Some x -> max 1 x
  | None -> 7

let print_ancestors_dag conf base v p =
  let v = min (limit_by_tree conf) v in
  let set =
    let rec loop set lev ip =
      let set = Dag.Pset.add ip set in
      if lev <= 1 then set
      else
        match Gwdb.get_parents (Util.pget conf base ip) with
          Some ifam ->
            let cpl = Gwdb.foi base ifam in
            let set = loop set (lev - 1) (Gwdb.get_mother cpl) in
            loop set (lev - 1) (Gwdb.get_father cpl)
        | None -> set
    in
    loop Dag.Pset.empty v (Gwdb.get_iper p)
  in
  let elem_txt p = DagDisplay.Item (p, "") in
  (* Récupère les options d'affichage. *)
  let options = "&" ^ (Util.display_options conf) in
  let vbar_txt ip =
    let p = pget conf base ip in
    Printf.sprintf "%sm=A&t=T&v=%d%s&dag=1&%s"
      (commd conf) v options (acces conf base p)
  in
  let page_title = Utf8.capitalize_fst (Util.transl conf "tree") in
  DagDisplay.make_and_print_dag conf base elem_txt vbar_txt true set [] page_title ""

let print conf base p =
  match
    Util.p_getenv conf.env "t", Util.p_getenv conf.env "dag", p_getint conf.env "v"
  with
    Some "T", Some "1", Some v -> print_ancestors_dag conf base v p
  | _ ->
      let templ =
        match Util.p_getenv conf.env "t" with
          Some ("E" | "F" | "H" | "L") -> "anclist"
        | Some ("D" | "G" | "M" | "N" | "P" | "X" | "Y" | "Z") -> "ancsosa"
        | Some ("A" | "C" | "T") -> "anctree"
        | Some "FC" -> "fanchart"
        | _ -> "ancmenu"
      in
      Perso.interp_templ templ conf base p
