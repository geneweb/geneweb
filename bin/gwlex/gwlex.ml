module C = Cmdliner

let default_lexicon = "hd/lang/lexicon.txt"

let report fname diags =
  List.iter
    (fun (lnum, msg) -> Printf.eprintf "%s:%d: %s\n" fname lnum msg)
    diags

let lexicon =
  let doc = "The lexicon, $(b,hd/lang/lexicon.txt) by default." in
  C.Arg.(value & pos 0 file default_lexicon & info [] ~docv:"FILE" ~doc)

let write fname s =
  if fname = "-" then (
    set_binary_mode_out stdout true;
    print_string s)
  else
    Out_channel.with_open_bin fname (fun oc -> Out_channel.output_string oc s)

let sort_cmd =
  let output =
    let doc =
      "Write the sorted lexicon to $(docv) instead of $(i,FILE), $(b,-) for \
       the standard output."
    in
    C.Arg.(value & opt (some string) None & info [ "o" ] ~docv:"OUTPUT" ~doc)
  in
  let run fname output =
    let lex = Lex.read fname in
    if lex.Lex.errors <> [] then (
      report fname lex.Lex.errors;
      1)
    else (
      report fname (Lex.duplicates lex.Lex.entries);
      Lex.sort lex.Lex.entries
      |> Lex.to_string ~crlf:lex.Lex.crlf
      |> write (Option.value output ~default:fname);
      0)
  in
  let doc =
    "Sort the entries by key and their translations by language, separate them \
     by a single blank line, and report the duplicates to merge by hand. \
     Nothing is written if a line is malformed."
  in
  C.Cmd.make (C.Cmd.info "sort" ~doc) C.Term.(const run $ lexicon $ output)

let check_cmd =
  let formats =
    let doc =
      "Also check that the placeholders of the translations match the English \
       ones."
    in
    C.Arg.(value & flag & info [ "formats" ] ~doc)
  in
  let run fname formats =
    let lex = Lex.read fname in
    let entries = lex.Lex.entries in
    let diags =
      List.concat
        [
          lex.Lex.errors;
          Lex.duplicates entries;
          (if formats then Lex.format_errors entries else []);
        ]
      |> List.stable_sort (fun (l1, _) (l2, _) -> Int.compare l1 l2)
    in
    report fname diags;
    let unsorted =
      lex.Lex.errors = []
      && Lex.to_string ~crlf:lex.Lex.crlf (Lex.sort entries) <> lex.Lex.source
    in
    if unsorted then
      Printf.eprintf "%s: not sorted or not normalized, run gwlex sort\n" fname;
    if diags = [] && not unsorted then 0 else 1
  in
  let doc =
    "Check that the lexicon is well formed, sorted and free of duplicates."
  in
  C.Cmd.make (C.Cmd.info "check" ~doc) C.Term.(const run $ lexicon $ formats)

let missing_cmd =
  let langs =
    let doc =
      "Languages to check, separated by commas; all those of the lexicon by \
       default."
    in
    C.Arg.(value & opt (list string) [] & info [ "lang" ] ~docv:"LANGS" ~doc)
  in
  let run fname langs =
    let lex = Lex.read fname in
    let entries = lex.Lex.entries in
    if lex.Lex.errors <> [] then (
      report fname lex.Lex.errors;
      1)
    else
      let langs = if langs = [] then Lex.languages entries else langs in
      print_string (Lex.missing entries langs);
      0
  in
  let doc =
    "Print the entries to translate: missing languages, and translations whose \
     number of $(b,/)-separated variants differs from the English one."
  in
  C.Cmd.make (C.Cmd.info "missing" ~doc) C.Term.(const run $ lexicon $ langs)

let orphans_cmd =
  let run fname =
    Lex_scan.orphans fname;
    0
  in
  let doc =
    "From the root of the GeneWeb sources, print the unused entries of the \
     lexicon and the keys missing from it."
  in
  C.Cmd.make (C.Cmd.info "orphans" ~doc) C.Term.(const run $ lexicon)

let () =
  let doc = "Maintain the GeneWeb lexicon." in
  [ sort_cmd; check_cmd; missing_cmd; orphans_cmd ]
  |> C.Cmd.group (C.Cmd.info "gwlex" ~doc)
  |> C.Cmd.eval' |> exit
