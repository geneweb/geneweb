#!/bin/sh
#cd (*
exec ocaml $0
*) ".";;
(* $Id: mk_missing_i18n.sh,v 4.2 2001-04-05 13:59:42 ddr Exp $ *)

open Printf

let languages =
  ["af"; "ca"; "cs"; "da"; "de"; "en"; "es"; "eo"; "et"; "fi";
   "fr"; "he"; "is"; "it"; "lv"; "nl"; "no"; "pl"; "pt"; "pt-br";
   "ru"; "sv"; "zh"]

let linenum = ref 0
let input_line_cnt ic = incr linenum; input_line ic

let rec skip_to_same_line ic line_ref =
  let line = input_line_cnt ic in
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

let compare_assoc (l1, _) (l2, _) = l1 <= l2

let check first lang =
  let derived_lang =
    try String.sub lang 0 (String.index lang '-') with Not_found -> ""
  in
  linenum := 0;
  let ic_lex = open_in "../hd/lang/lexicon.txt" in
  let ic_i18n = open_in "i18n" in
  printf "<h3><a name=%s>%s</h3>\n" lang lang;
  printf "<pre>\n";
  let has_missing = ref false in
  begin try
    while true do
      let line = input_line ic_i18n in
      skip_to_same_line ic_lex ("    " ^ line);
      let list = get_all_versions ic_lex in
      if first && Sort.list compare_assoc list <> list then begin
	eprintf "Misordered for:\n   \"%s\"\n" line;
	flush stderr;
      end;
      if not (List.mem_assoc lang list || List.mem_assoc derived_lang list)
      then begin
        let list =
	  Sort.list compare_assoc
            [(lang, ""); ("en", List.assoc "en" list);
	     ("fr", List.assoc "fr" list)]
	in
        printf "    %s\n" line;
        List.iter (fun (lang, transl) -> printf "%s:%s\n" lang (if transl = "" then " " else transl))
	  list;
	printf "\n";
	has_missing := true
      end
      else if List.length (List.find_all (fun (l, _) -> l = lang) list) >= 2
      then begin
        eprintf "Several translations in %s for:\n   \"%s\"\n" lang line;
        flush stderr;
      end;
   done
  with End_of_file -> ()
  end;
  if not !has_missing then printf "     - no missing phrases -\n";
  printf "</pre>\n";
  false

let header lang =
  printf "<a href=#%s>%s</a>\n" lang lang

let _ =
printf "\
<head><title>Missing phrases</title></head>
<body background=\"images/gwback.jpg\">
<h1>Missing phrases</h1>
Here are the list of all languages with the missing phrases in the lexicon
of the current version of <b><font color=#2f6400>GeneWeb</font></b>. The
missing entries are displayed together with their English and French
translations.
<p>
If you want to collaborate, you can <a
href=\"mailto:daniel.de_rauglaudre@inria.fr?subject=Missing translations\">send
me</a>
the translations in
the given language if you know it. Thanks.
<p align=center>
<tt>
";
List.iter header languages;
printf "</tt>\n";
let _ = List.fold_left check true languages in ();
printf "</body>\n";
flush stdout
