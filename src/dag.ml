(* $Id: dag.ml,v 3.3 1999-12-04 05:10:45 ddr Exp $ *)

open Dag2html;
open Def;
open Config;
open Gutil;
open Printf;

(* testing *)

value map_dag f d =
  let a =
    Array.map (fun d -> {pare = d.pare; valu = f d.valu; chil = d.chil}) d.dag
  in
  {dag = a}
;

value tag_dag d =
  let c = ref 'A' in
  map_dag
    (fun v ->
       let v = c.val in
       do c.val :=
            if c.val = 'Z' then 'a'
            else if c.val = 'z' then '1'
            else Char.chr (Char.code c.val + 1);
       return v)
    d
;

(* input dag *)

value get_dag_elems conf base =
  let module O = struct type t = iper; value compare = compare; end in
  let module S = Set.Make O in
  let set =
    loop S.empty 1 where rec loop set i =
      let s = string_of_int i in
      let po = Util.find_person_in_env conf base s in
      let so = Util.p_getenv conf.env ("s" ^ s) in
      match (po, so) with
      [ (Some p, Some s) ->
          let set =
            match Util.branch_of_sosa base p.cle_index (Num.of_string s) with
            [ Some ipsl ->
                List.fold_left (fun set (ip, _) -> S.add ip set) set ipsl
            | None -> set ]
          in
          loop set (i + 1)
      | _ -> set ]
  in
  S.elements set
;

value make_dag base list =
  let module O = struct type t = iper; value compare = compare; end in
  let module M = Map.Make O in
  let nodes = Array.of_list list in
  let map =
    loop M.empty 0 where rec loop map i =
      if i = Array.length nodes then map
      else loop (M.add nodes.(i) (idag_of_int i) map) (i + 1)
  in
  let nodes =
    Array.map
      (fun ip ->
(*
do Printf.eprintf "\no %s\n" (denomination base (poi base ip)); flush stderr; return
*)
         let pare =
           match (aoi base ip).parents with
           [ Some ifam ->
               let c = coi base ifam in
               let l = try [M.find c.mother map] with [ Not_found -> [] ] in
               try [M.find c.father map :: l] with [ Not_found -> l ]
           | None -> [] ]
         in
(*
do Printf.eprintf "parents\n"; flush stderr; return
do List.iter (fun id -> Printf.eprintf "- %s\n" (denomination base (poi base nodes.(int_of_idag id)))) pare; flush stderr; return
*)
         let chil =
           let u = uoi base ip in
           Array.fold_left
             (fun chil ifam ->
                let des = doi base ifam in
                Array.fold_left
                  (fun chil ip ->
                     try [M.find ip map :: chil] with [ Not_found -> chil ])
                  chil des.children)
             [] u.family
         in
         let chil = List.rev chil in
(*
do Printf.eprintf "children\n"; flush stderr; return
do List.iter (fun id -> Printf.eprintf "- %s\n" (denomination base (poi base nodes.(int_of_idag id)))) chil; flush stderr; return
*)
         {pare = pare; valu = ip; chil = chil})
      nodes
  in
  {dag = nodes}
;

(* main *)

value print_dag conf base d =
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "tree"))
  in
  let t = table_of_dag False d in
  let print_indi ip =
    let p = poi base ip in
    do Wserver.wprint "%s" (Util.referenced_person_title_text conf base p);
       Wserver.wprint "%s" (Date.short_dates_text conf base p);
    return ()
  in
(*
do let d = tag_dag d in print_char_table d (table_of_dag d); flush stderr; return
*)
  do Util.header_no_page_title conf title;
     print_html_table (fun x -> Wserver.wprint "%s" x) print_indi conf.border
       d t;
     Util.trailer conf;
  return ()
;

value print conf base =
  let d = make_dag base (get_dag_elems conf base) in
  print_dag conf base d
;
