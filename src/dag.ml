(* $Id: dag.ml,v 3.10 1999-12-29 18:12:01 ddr Exp $ *)

open Dag2html;
open Def;
open Config;
open Gutil;
open Printf;

module Pset = Set.Make (struct type t = iper; value compare = compare; end);

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
  loop Pset.empty 1 where rec loop set i =
    let s = string_of_int i in
    let po = Util.find_person_in_env conf base s in
    let so = Util.p_getenv conf.env ("s" ^ s) in
    match (po, so) with
    [ (Some p, Some s) ->
        let set =
          match Util.branch_of_sosa base p.cle_index (Num.of_string s) with
          [ Some ipsl ->
              List.fold_left (fun set (ip, _) -> Pset.add ip set) set ipsl
          | None -> set ]
        in
        loop set (i + 1)
    | _ -> set ]
;

type sum 'a 'b = [ Left of 'a | Right of 'b ];

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
         {pare = pare; valu = Left ip; chil = chil})
      nodes
  in
  {dag = nodes}
;

(* main *)

value print_only_dag conf base spouse_on invert set spl d =
  let d = if invert then invert_dag d else d in
  let t = table_of_dag False d in
  let t = if invert then invert_table t else t in
  let print_indi n =
    match n.valu with
    [ Left ip ->
        let p = poi base ip in
        do Wserver.wprint "%s" (Util.referenced_person_title_text conf base p);
           Wserver.wprint "%s" (Date.short_dates_text conf base p);
           let spouses =
             if (spouse_on && n.chil <> [] || n.pare = []) && not invert then
               List.fold_left
                 (fun list id ->
                    match d.dag.(int_of_idag id).valu with
                    [ Left cip ->
                        match (aoi base cip).parents with
                        [ Some ifam ->
                            let cpl = coi base ifam in
                            let ips = Util.spouse ip cpl in
                            if List.mem_assoc ips list then list
                            else [(ips, Some ifam) :: list]
                        | None -> list ]
                    | Right _ -> list ])
                 [] n.chil
             else if n.chil = [] then
               try [List.assq ip spl] with [ Not_found -> [] ]
             else []
           in
           List.iter
             (fun (ips, ifamo) ->
                if Pset.mem ips set then ()
                else
                  let ps = poi base ips in
                  let d =
                    match ifamo with
                    [ Some ifam ->
                        Date.short_marriage_date_text conf base (foi base ifam)
                          p ps
                    | None -> "" ]
                  in
                  do Wserver.wprint "<br>\n&amp;%s " d;
                     Wserver.wprint "%s"
                       (Util.referenced_person_title_text conf base ps);
                     Wserver.wprint "%s" (Date.short_dates_text conf base ps);
                  return ())
             spouses;
        return ()
    | Right _ -> Wserver.wprint "&nbsp;" ]
  in
  let phony n =
    match n.valu with
    [ Left _ -> False
    | Right _ -> True ]
  in
  print_html_table (fun x -> Wserver.wprint "%s" x) print_indi phony
    conf.border d t
;

value gen_print_dag conf base spouse_on invert set spl d =
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "tree"))
  in
(*
do let d = tag_dag d in print_char_table d (table_of_dag d); flush stderr; return
*)
  do Util.header_no_page_title conf title;
     print_only_dag conf base spouse_on invert set spl d;
     Util.trailer conf;
  return ()
;

value print_dag conf base set spl d =
  let spouse_on =
    match Util.p_getenv conf.env "spouse" with
    [ Some "on" -> True
    | _ -> False ]
  in
  let invert =
    match Util.p_getenv conf.env "invert" with
    [ Some "on" -> True
    | _ -> False ]
  in
  gen_print_dag conf base spouse_on invert set spl d
;

value print conf base =
  let set = get_dag_elems conf base in
  let d = make_dag base (Pset.elements set) in
  print_dag conf base set [] d
;
