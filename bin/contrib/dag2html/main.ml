(* $Id: main.ml,v 5.1 2006-10-15 15:39:38 ddr Exp $ *)

open Geneweb
open Dag2html

let version = "1.02-exp"

(* input dag *)

let strip_spaces str =
  let start =
    let rec loop i =
      if i = String.length str then i
      else
        match str.[i] with
          ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
        | _ -> i
    in
    loop 0
  in
  let stop =
    let rec loop i =
      if i = -1 then i + 1
      else
        match str.[i] with
          ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1
    in
    loop (String.length str - 1)
  in
  if start = 0 && stop = String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)

let rec get_line ic =
  try
    let line = input_line ic in
    if String.length line > 0 && line.[0] = '#' then get_line ic
    else Some (strip_spaces line)
  with End_of_file -> None

let input_dag ic =
  let rec find cnt s =
    function
      n :: nl ->
        if n.valu = s then n, idag_of_int cnt else find (cnt - 1) s nl
    | [] -> raise Not_found
  in
  let add_node pl cl nl cnt =
    let cl = List.rev cl in
    let pl = List.rev pl in
    let (pl, pnl, nl, cnt) =
      List.fold_left
        (fun (pl, pnl, nl, cnt) p ->
           try
             let (n, p) = find (cnt - 1) p nl in p :: pl, n :: pnl, nl, cnt
           with Not_found ->
             let n = {pare = []; valu = p; chil = []} in
             let p = idag_of_int cnt in p :: pl, n :: pnl, n :: nl, cnt + 1)
        ([], [], nl, cnt) pl
    in
    let pl = List.rev pl in
    let (cl, nl, cnt) =
      List.fold_left
        (fun (cl, nl, cnt) c ->
           try
             let (n, c) = find (cnt - 1) c nl in
             n.pare <- n.pare @ pl; c :: cl, nl, cnt
           with Not_found ->
             let n = {pare = pl; valu = c; chil = []} in
             let c = idag_of_int cnt in c :: cl, n :: nl, cnt + 1)
        ([], nl, cnt) cl
    in
    let cl = List.rev cl in
    List.iter (fun p -> p.chil <- p.chil @ cl) pnl; nl, cnt
  in
  let rec input_parents nl pl cnt =
    function
      Some "" -> input_parents nl pl cnt (get_line ic)
    | Some line ->
        begin match line.[0] with
          'o' ->
            let p =
              strip_spaces (String.sub line 1 (String.length line - 1))
            in
            if p = "" then failwith line
            else input_parents nl (p :: pl) cnt (get_line ic)
        | '-' ->
            if pl = [] then failwith line
            else input_children nl pl [] cnt (Some line)
        | _ -> failwith line
        end
    | None -> if pl = [] then nl, cnt else failwith "end of file 1"
  and input_children nl pl cl cnt =
    function
      Some "" -> input_children nl pl cl cnt (get_line ic)
    | Some line ->
        begin match line.[0] with
          'o' ->
            if cl = [] then failwith line
            else
              let (nl, cnt) = add_node pl cl nl cnt in
              input_parents nl [] cnt (Some line)
        | '-' ->
            let c =
              strip_spaces (String.sub line 1 (String.length line - 1))
            in
            if c = "" then failwith line
            else input_children nl pl (c :: cl) cnt (get_line ic)
        | _ -> failwith line
        end
    | None ->
        if cl = [] then failwith "end of file 2" else add_node pl cl nl cnt
  in
  let (nl, _) = input_parents [] [] 0 (get_line ic) in
  {dag = Array.of_list (List.rev nl)}

(* testing *)

let map_dag f d =
  let a =
    Array.map (fun d -> {pare = d.pare; valu = f d.valu; chil = d.chil}) d.dag
  in
  {dag = a}

let tag_dag d =
  let c = ref 'A' in
  map_dag
    (fun _ ->
       let v = !c in
       c :=
         if !c = 'Z' then 'a'
         else if !c = 'z' then '1'
         else Char.chr (Char.code !c + 1);
       String.make 1 v)
    d

(* *)

let phony _ = false
let indi_txt n = n.valu
let vbar_txt _ = ""

let print_char_table d t =
  let print_elem =
    function
      Elem e -> Printf.eprintf "  %s" d.dag.(int_of_idag e).valu
    | Ghost _ -> Printf.eprintf "  |"
    | Nothing -> Printf.eprintf "   "
  in
  let print_span i j r =
    if j > 0 && t.table.(i).(j-1).span = r then Printf.eprintf "---"
    else Printf.eprintf "  -"
  in
  let print_newline = prerr_newline in
  for i = 0 to Array.length t.table - 1 do
    for j = 0 to Array.length t.table.(i) - 1 do
      print_elem t.table.(i).(j).elem
    done;
    print_newline ();
    for j = 0 to Array.length t.table.(i) - 1 do
      print_span i j t.table.(i).(j).span
    done;
    print_newline ()
  done

let print_table border hts =
  Printf.printf "<center><table border=%d" border;
  Printf.printf " cellspacing=0 cellpadding=0>\n";
  for i = 0 to Array.length hts - 1 do
    Printf.printf "<tr align=left>\n";
    for j = 0 to Array.length hts.(i) - 1 do
      let (colspan, align, td) = hts.(i).(j) in
      Printf.printf "<td";
      if colspan = 1 && (td = TDitem "&nbsp;" || td = TDhr CenterA) then ()
      else Printf.printf " colspan=%d" colspan;
      begin match align, td with
        LeftA, TDhr LeftA -> Printf.printf " align=left"
      | LeftA, _ -> ()
      | CenterA, _ -> Printf.printf " align=center"
      | RightA, _ -> Printf.printf " align=right"
      end;
      Printf.printf ">";
      begin match td with
        TDitem s -> Printf.printf "%s" s
      | TDtext s -> Printf.printf "%s" s
      | TDbar _ -> Printf.printf "|"
      | TDhr align ->
          Printf.printf "<hr noshade size=1";
          begin match align with
            LeftA -> Printf.printf " width=\"50%%\" align=left"
          | RightA -> Printf.printf " width=\"50%%\" align=right"
          | _ -> ()
          end;
          Printf.printf ">"
      | TDnothing -> Printf.printf "&nbsp;"
      end;
      Printf.printf "</td>\n"
    done
  done;
  Printf.printf "</table></center>\n"

let fname = ref ""
let invert = ref false
let char = ref false
let border = ref 0
let no_optim = ref false
let no_group = ref false

let print_version () =
  Printf.eprintf "Dag2html version %s\n" version; flush stderr; exit 0

let speclist =
  ["-b", Arg.Int (fun x -> border := x), "<int>: table border";
   "-c", Arg.Set char, ": char output";
   "-i", Arg.Set invert, ": apply algorithm from bottom to top";
   "-n", Arg.Set no_optim, ": no optimization";
   "-g", Arg.Set no_group, ": no group";
   "-v", Arg.Unit print_version, ": print version number and exit"]
let anonfun s = fname := s
let usage = "Usage: " ^ Sys.argv.(0) ^ " [options] file\nwhere options are:"

let main () =
  Arg.parse speclist anonfun usage;
  if !fname = "" then begin Arg.usage speclist usage; exit 2 end;
  let d = input_dag (open_in !fname) in
  if !char then
    let d = tag_dag d in
    let t = table_of_dag phony !no_optim !invert !no_group d in
    print_char_table d t
  else
    let t = table_of_dag phony !no_optim !invert !no_group d in
    let hts = html_table_struct indi_txt vbar_txt phony d t in
    print_table !border hts

let _ = Printexc.print main ()
