#!/bin/sh
#cd (*
exec ocaml $0
*) ".";;
(* $Id: mk_missing_i18n.sh,v 3.7 2001-01-31 09:50:46 ddr Exp $ *)

open Printf

let languages =
  ["af"; "cn"; "cs"; "de"; "dk"; "en"; "eo"; "es"; "et"; "fi"; "fr"; "he";
   "is"; "it"; "lv"; "nl"; "no"; "pt"; "ru"; "se"]

let rec skip_to_same_line ic line_ref =
  let line = input_line ic in
  if line = line_ref then () else skip_to_same_line ic line_ref

let rec get_all_versions ic =
  let line = input_line ic in
  if line = "" then []
  else
    let lang = String.sub line 0 2 in
    let transl = String.sub line 4 (String.length line - 4) in
    (lang, transl) :: get_all_versions ic

let compare_assoc (l1, _) (l2, _) = l1 <= l2

let check first lang =
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
	eprintf "Misordered for: \"%s\"\n" line;
	flush stderr;
      end;
      if not (List.mem_assoc lang list) then begin
        let list =
	  Sort.list compare_assoc
            [(lang, ""); ("en", List.assoc "en" list);
	     ("fr", List.assoc "fr" list)]
	in
        printf "    %s\n" line;
        List.iter (fun (lang, transl) -> printf "%s: %s\n" lang transl)
	  list;
	printf "\n";
	has_missing := true
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
