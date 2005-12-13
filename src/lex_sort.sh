#!/bin/sh
#cd (*
exec ocaml $0
*) ".";;
(* $Id: lex_sort.sh,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)

open Printf

let linenum = ref 0
let input_line_cnt ic = incr linenum; input_line ic

let rec skip_to_same_line ic line_ref =
  let line = input_line_cnt ic in
  printf "%s\n" line;
  if line = line_ref then ()
  else skip_to_same_line ic line_ref

let rec get_all_versions ic =
  let line = try input_line_cnt ic with End_of_file -> "" in
  if line = "" then []
  else if String.length line < 3 then begin
    eprintf "small line %d: \"%s\"\n" !linenum (String.escaped line);
    flush stderr;
    []
  end
  else
    try
      let i = String.index line ':' in
      let lang = String.sub line 0 i in
      let transl = String.sub line (i + 1) (String.length line - i - 1) in
      (lang, transl) :: get_all_versions ic
    with Not_found ->
      []

let print_languages (lang, str) =
  let str = String.sub str 1 (String.length str - 1) in
  let list =
    let rec loop ibeg i =
      if i = String.length str then [String.sub str ibeg (i - ibeg)]
      else if str.[i] = '/' then
        String.sub str ibeg (i - ibeg) :: loop (i + 1) (i + 1)
      else loop ibeg (i + 1)
    in
    let compare x y =
      let i = String.index x '=' in
      let j = String.index y '=' in
      compare (String.sub x 0 i) (String.sub y 0 j)
    in
    List.sort compare (loop 0 0)
  in
  printf "%s: " lang;
  let _ = List.fold_left
    (fun sep s -> printf "%s%s" sep s; "/") "" list
  in ();
  printf "\n"

let lex_sort () =
  linenum := 0;
  let ic_lex = open_in "../hd/lang/lexicon.txt" in
  let ic_i18n = open_in "i18n" in
  let rec loop line =
    skip_to_same_line ic_lex ("    " ^ line);
    let list = get_all_versions ic_lex in
    let list = List.sort (fun (l1, _) (l2, _) -> compare l1 l2) list in
    if line = " !languages" then
      List.iter print_languages list
    else
      List.iter (fun (lang, str) -> printf "%s:%s\n" lang str) list;
    match try Some (input_line ic_i18n) with End_of_file -> None with
      Some line -> printf "\n"; loop line
    | None -> ()
  in
  loop (input_line ic_i18n)

let _ = lex_sort ();
flush stdout
