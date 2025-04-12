(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Driver = Geneweb_db.Driver

let limit_by_tree conf =
  match List.assoc_opt "max_anc_tree" conf.base_env with
  | Some x -> max 1 (int_of_string x)
  | None -> 7

let print_ancestors_dag conf base v p =
  let v = min (limit_by_tree conf) v in
  let set =
    (* TODO this should be a get_ancestors_set lvl ip *)
    let rec loop set lev ip =
      let set = Dag.Pset.add ip set in
      if lev <= 0 then set
      else
        match Driver.get_parents (pget conf base ip) with
        | Some ifam ->
            let cpl = Driver.foi base ifam in
            let get_left, get_right =
              match p_getenv conf.env "mf" with
              | Some "1" -> (Driver.get_father, Driver.get_mother)
              | _ -> (Driver.get_mother, Driver.get_father)
            in
            let set = loop set (lev - 1) (get_left cpl) in
            loop set (lev - 1) (get_right cpl)
        | None -> set
    in
    loop Dag.Pset.empty v (Driver.get_iper p)
  in
  let elem_txt p = DagDisplay.Item (p, Adef.safe "") in
  (* Récupère les options d'affichage. *)
  let options = Util.display_options conf in
  let vbar_txt ip =
    let p = pget conf base ip in
    Printf.sprintf {|%s%s&m=A&t=T&dag=on&v=%d%s |}
      (commd conf :> string)
      (acces conf base p :> string)
      v
      (options :> string)
    |> Adef.escaped
  in
  let page_title =
    Util.transl conf "tree" |> Utf8.capitalize_fst |> Adef.safe
  in
  DagDisplay.make_and_print_dag conf base elem_txt vbar_txt true set []
    page_title (Adef.escaped "")

let print conf base p =
  match
    ( Util.p_getenv conf.env "t",
      Util.p_getenv conf.env "dag",
      p_getint conf.env "v" )
  with
  | Some "T", Some "on", Some v -> print_ancestors_dag conf base v p
  | _ ->
      let templ =
        match Util.p_getenv conf.env "t" with
        | Some ("E" | "F" | "H" | "L") -> "anclist"
        | Some ("D" | "G" | "M" | "N" | "P" | "X" | "Y" | "Z") -> "ancsosa"
        | Some ("A" | "C" | "T") -> "anctree"
        | Some "FC" -> "fanchart"
        | _ -> "ancmenu"
      in
      Perso.interp_templ templ conf base p
