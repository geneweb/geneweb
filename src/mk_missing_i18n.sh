#!/bin/sh
#cd (*
exec ocaml -I +camlp5 camlp5r.cma $0
*) ".";
(* $Id: mk_missing_i18n.sh,v 5.4 2007-07-28 10:13:42 ddr Exp $ *)

open Printf;

value languages =
  ["af"; "bg"; "br"; "ca"; "cs"; "da"; "de"; "en"; "eo"; "es"; "et"; "fi";
   "fr"; "he"; "is"; "it"; "lv"; "nl"; "no"; "pl"; "pt"; "pt-br"; "ro"; "ru";
   "sl"; "sv"(*; "zh": too many missing things, gave up *)]
;

value linenum = ref 0;
value input_line_cnt ic = do { incr linenum; input_line ic };

value rec skip_to_same_line ic line_ref =
  let line = input_line_cnt ic in
  if line = line_ref then () else skip_to_same_line ic line_ref
;

value rec get_all_versions first ic =
  let line = try input_line_cnt ic with [ End_of_file -> "" ] in
  if line = "" then []
  else if first && String.length line < 3 then do {
    eprintf "small line %d: \"%s\"\n" linenum.val (String.escaped line);
    flush stderr;
    []
  }
  else
    try
      let i = String.index line ':' in
      let lang = String.sub line 0 i in
      do {
        if first && String.length line > i + 1 && line.[i + 1] <> ' '
        then do {
          eprintf "space missing after colon in line %d:\n%s\n" linenum.val
            line;
          flush stderr
        }
        else ();
        let transl = String.sub line (i + 1) (String.length line - i - 1) in
        [(lang, transl) :: get_all_versions first ic]
      }
    with
    [ Not_found -> [] ]
;

value compare_assoc (l1, _) (l2, _) = l1 <= l2;

value check first lang =
  let derived_lang =
    try String.sub lang 0 (String.index lang '-') with [ Not_found -> "" ]
  in
  do {
    linenum.val := 0;
    let ic_lex = open_in "hd/lang/lex_utf8.txt" in
    let ic_i18n = open_in "src/i18n" in
    printf "<h3><a name=%s>%s</h3>\n" lang lang;
    printf "<pre>\n";
    let has_missing = ref False in
    try
      while True do {
        let line = input_line ic_i18n in
        skip_to_same_line ic_lex ("    " ^ line);
        let list = get_all_versions first ic_lex in
        if first && Sort.list compare_assoc list <> list then do {
          eprintf "Misordered for:\n   \"%s\"\n" line; flush stderr
        }
        else ();
        if not (List.mem_assoc lang list || List.mem_assoc derived_lang list)
        then do {
          let list =
            Sort.list compare_assoc
              [(lang, ""); ("en", List.assoc "en" list);
               ("fr", List.assoc "fr" list)]
          in
          printf "    %s\n" line;
          List.iter
            (fun (lang, transl) ->
               printf "%s:%s\n" lang
                 (if transl = "" then " ..." else transl))
            list;
          printf "\n";
          has_missing.val := True
        }
        else if
          List.length (List.find_all (fun (l, _) -> l = lang) list) >= 2
        then do {
          eprintf "Several translations in %s for:\n   \"%s\"\n" lang line;
          flush stderr
        }
        else ()
      }
    with
    [ End_of_file -> () ];
    if not has_missing.val then printf "     - no missing phrases -\n"
    else ();
    printf "</pre>\n";
    close_in ic_lex;
    close_in ic_i18n;
    False
  }
;

value header lang = printf "<a href=#%s>%s</a>\n" lang lang;

do {
  printf "\
<head>
  <title>Missing phrases</title>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
</head>
<body background=\"images/gwback.jpg\">
<h1>Missing phrases</h1>

<p>
Address of this page:
<a href=\"http://www.geneweb.org/missing.utf8.html\">http://www.geneweb.org/missing.utf8.html</a>
</p>

<p>
Here are the list of all languages with the missing phrases in the lexicon
of the current version of <b><font color=#2f6400>GeneWeb</font></b>. The
missing entries are displayed together with their English and French
translations.
</p>
<p>
If you want to collaborate, you can <a
href=\"../email.html\">send me</a>
the translations in
the given language if you know it. Thanks.
</p>

<p align=center>
<tt>
";
  List.iter header languages;
  printf "</tt>\n";
  let _ = List.fold_left check True languages in
  ();
  printf "</body>\n";
  flush stdout
};
