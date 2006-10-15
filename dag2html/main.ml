(* $Id: main.ml,v 5.1 2006-10-15 15:39:38 ddr Exp $ *)

open Dag2html;
open Printf;

value version = "1.02-exp";

(* input dag *)

value strip_spaces str =
  let start =
    loop 0 where rec loop i =
      if i = String.length str then i
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
        | _ -> i ]
  in
  let stop =
    loop (String.length str - 1) where rec loop i =
      if i = -1 then i + 1
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  if start = 0 && stop = String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)
;

value rec get_line ic =
  try
    let line = input_line ic in
    if String.length line > 0 && line.[0] = '#' then get_line ic
    else Some (strip_spaces line)
  with
  [ End_of_file -> None ]
;

value input_dag ic =
  let rec find cnt s =
    fun
    [ [n :: nl] ->
        if n.valu = s then (n, idag_of_int cnt) else find (cnt - 1) s nl
    | [] -> raise Not_found ]
  in
  let add_node pl cl nl cnt =
    let cl = List.rev cl in
    let pl = List.rev pl in
    let (pl, pnl, nl, cnt) =
      List.fold_left
        (fun (pl, pnl, nl, cnt) p ->
           try
             let (n, p) = find (cnt - 1) p nl in
             ([p :: pl], [n :: pnl], nl, cnt)
           with
           [ Not_found ->
               let n = {pare = []; valu = p; chil = []} in
               let p = idag_of_int cnt in
               ([p :: pl], [n :: pnl], [n :: nl], cnt + 1) ])
        ([], [], nl, cnt) pl
    in
    let pl = List.rev pl in
    let (cl, nl, cnt) =
      List.fold_left
        (fun (cl, nl, cnt) c ->
           try
             let (n, c) = find (cnt - 1) c nl in
             do { n.pare := n.pare @ pl; ([c :: cl], nl, cnt) }
           with
           [ Not_found ->
               let n = {pare = pl; valu = c; chil = []} in
               let c = idag_of_int cnt in
               ([c :: cl], [n :: nl], cnt + 1) ])
        ([], nl, cnt) cl
    in
    let cl = List.rev cl in
    do { List.iter (fun p -> p.chil := p.chil @ cl) pnl; (nl, cnt) }
  in
  let rec input_parents nl pl cnt =
    fun
    [ Some "" -> input_parents nl pl cnt (get_line ic)
    | Some line ->
        match line.[0] with
        [ 'o' ->
            let p =
              strip_spaces (String.sub line 1 (String.length line - 1))
            in
            if p = "" then failwith line
            else input_parents nl [p :: pl] cnt (get_line ic)
        | '-' ->
            if pl = [] then failwith line
            else input_children nl pl [] cnt (Some line)
        | _ -> failwith line ]
    | None -> if pl = [] then (nl, cnt) else failwith "end of file 1" ]
  and input_children nl pl cl cnt =
    fun
    [ Some "" -> input_children nl pl cl cnt (get_line ic)
    | Some line ->
        match line.[0] with
        [ 'o' ->
            if cl = [] then failwith line
            else
              let (nl, cnt) = add_node pl cl nl cnt in
              input_parents nl [] cnt (Some line)
        | '-' ->
            let c =
              strip_spaces (String.sub line 1 (String.length line - 1))
            in
            if c = "" then failwith line
            else input_children nl pl [c :: cl] cnt (get_line ic)
        | _ -> failwith line ]
    | None ->
        if cl = [] then failwith "end of file 2" else add_node pl cl nl cnt ]
  in
  let (nl, _) = input_parents [] [] 0 (get_line ic) in
  {dag = Array.of_list (List.rev nl)}
;

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
       do {
         c.val :=
           if c.val = 'Z' then 'a'
           else if c.val = 'z' then '1'
           else Char.chr (Char.code c.val + 1);
         String.make 1 v
       })
    d
;

(* *)

value phony _ = False;
value indi_txt n = n.valu;
value vbar_txt n = "";

value print_char_table d t =
  let print_elem =
    fun
    [ Elem e -> Printf.eprintf "  %s" d.dag.(int_of_idag e).valu
    | Ghost x -> Printf.eprintf "  |"
    | Nothing -> Printf.eprintf "   " ]
  in
  let print_span i j r =
    if j > 0 && t.table.(i).(j - 1).span = r then Printf.eprintf "---"
    else Printf.eprintf "  -"
  in
  let print_newline = prerr_newline in
  for i = 0 to Array.length t.table - 1 do {
    for j = 0 to Array.length t.table.(i) - 1 do {
      print_elem t.table.(i).(j).elem
    };
    print_newline ();
    for j = 0 to Array.length t.table.(i) - 1 do {
      print_span i j t.table.(i).(j).span
    };
    print_newline ()
  }
;

value print_table border hts =
  do {
    printf "<center><table border=%d" border;
    printf " cellspacing=0 cellpadding=0>\n";
    for i = 0 to Array.length hts - 1 do {
      printf "<tr align=left>\n";
      for j = 0 to Array.length hts.(i) - 1 do {
        let (colspan, align, td) = hts.(i).(j) in
        printf "<td";
        (*
            if colspan > 1 then printf " colspan=%d" colspan else ();
*)
if colspan = 1 && (td = TDitem "&nbsp;" || td = TDhr CenterA) then
          ()
        else printf " colspan=%d" colspan;
        (**)
        match (align, td) with
        [ (LeftA, TDhr LeftA) -> printf " align=left"
        | (LeftA, _) -> ()
        | (CenterA, _) -> printf " align=center"
        | (RightA, _) -> printf " align=right" ];
        printf ">";
        match td with
        [ TDitem s -> printf "%s" s
        | TDtext s -> printf "%s" s
        | TDbar _ -> printf "|"
        | TDhr align ->
            do {
              printf "<hr noshade size=1";
              match align with
              [ LeftA -> printf " width=\"50%%\" align=left"
              | RightA -> printf " width=\"50%%\" align=right"
              | _ -> () ];
              printf ">"
            }
        | TDnothing -> printf "&nbsp;" ];
        printf "</td>\n"
      }
    };
    printf "</table></center>\n"
  }
;

value fname = ref "";
value invert = ref False;
value char = ref False;
value border = ref 0;
value no_optim = ref False;
value no_group = ref False;

value print_version () =
  do { eprintf "Dag2html version %s\n" version; flush stderr; exit 0 }
;

value speclist =
  [("-b", Arg.Int (fun x -> border.val := x), "<int>: table border");
   ("-c", Arg.Set char, ": char output");
   ("-i", Arg.Set invert, ": apply algorithm from bottom to top");
   ("-n", Arg.Set no_optim, ": no optimization");
   ("-g", Arg.Set no_group, ": no group");
   ("-v", Arg.Unit print_version, ": print version number and exit")]
;
value anonfun s = fname.val := s;
value usage =
  "Usage: " ^ Sys.argv.(0) ^ " [options] file\nwhere options are:"
;

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if fname.val = "" then do { Arg.usage speclist usage; exit 2 } else ();
    let d = input_dag (open_in fname.val) in
    if char.val then
      let d = tag_dag d in
      let t = table_of_dag phony no_optim.val invert.val no_group.val d in
      print_char_table d t
    else
      let t = table_of_dag phony no_optim.val invert.val no_group.val d in
      let hts = html_table_struct indi_txt vbar_txt phony d t in
      print_table border.val hts
  }
;

Printexc.catch main ();
