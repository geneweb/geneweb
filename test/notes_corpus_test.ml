open Geneweb
module Database = Geneweb_db.Database
module Driver = Geneweb_db.Driver

(* A fake base directory with only the files NotesCorpus looks at: "base"
   (fingerprint) and "synchro_patches". *)
let with_fake_base f =
  (* Filename.temp_dir needs OCaml 5.1 *)
  let dir = Filename.temp_file "notes_corpus_test" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  let old = Secure.base_dir () in
  Secure.set_base_dir dir;
  let gwb = Filename.concat dir "fake.gwb" in
  Sys.mkdir gwb 0o755;
  Fun.protect
    ~finally:(fun () -> Secure.set_base_dir old)
    (fun () -> f (Filename.concat gwb))

let write_file path s =
  Out_channel.with_open_bin path (fun oc -> output_string oc s)

let write_synchro path l =
  Out_channel.with_open_bin path (fun oc ->
      output_value oc { Database.synch_list = l })

let entry kind id text = NotesCorpus.{ kind; id; evt = -1; text }

let status_name s =
  match s with
  | NotesCorpus.Missing -> "missing"
  | NotesCorpus.Outdated -> "outdated"
  | NotesCorpus.Available _ -> "available"

let available bname =
  match NotesCorpus.read bname with
  | NotesCorpus.Available h -> h
  | NotesCorpus.Missing | NotesCorpus.Outdated -> Alcotest.fail "not available"

let test_round_trip () =
  with_fake_base @@ fun file ->
  write_file (file "base") "v1";
  Alcotest.(check string)
    "no file" "missing"
    (status_name (NotesCorpus.read "fake"));
  let entries =
    [
      entry NotesCorpus.Person "3" "Le Moulin de l’Église";
      entry NotesCorpus.Family "0" "x";
    ]
  in
  NotesCorpus.write "fake" entries;
  ignore (available "fake");
  let read =
    NotesCorpus.fold_entries "fake"
      (fun kind id text acc ->
        (NotesCorpus.(
           match kind with
           | Person -> "p"
           | Family -> "f"
           | Person_event -> "pe"
           | Family_event -> "fe")
        ^ id ^ ":" ^ text)
        :: acc)
      []
    |> List.rev
  in
  Alcotest.(check (list string))
    "entries, folded"
    [ "p3:le moulin de l'eglise"; "f0:x" ]
    read;
  (* a rebuilt base (other "base" file) makes the corpus outdated *)
  write_file (file "base") "version 2";
  Alcotest.(check string)
    "base rebuilt" "outdated"
    (status_name (NotesCorpus.read "fake"));
  (* a file of the previous (Marshal) format is outdated, not misread *)
  write_file (file "base") "v1";
  Out_channel.with_open_bin (NotesCorpus.file "fake") (fun oc ->
      Marshal.to_channel oc (1, "old", [||]) []);
  Alcotest.(check string)
    "old format" "outdated"
    (status_name (NotesCorpus.read "fake"))

let test_changes () =
  with_fake_base @@ fun file ->
  write_file (file "base") "v1";
  write_synchro (file "synchro_patches") [ ("100", [ 1 ], [ 0 ]) ];
  NotesCorpus.write "fake" [];
  let corpus = available "fake" in
  let names (ps, fs) =
    ( List.map Driver.Iper.to_string (Driver.Iper.Set.elements ps),
      List.map Driver.Ifam.to_string (Driver.Ifam.Set.elements fs) )
  in
  Alcotest.(check (pair (list string) (list string)))
    "nothing since the build" ([], [])
    (names (NotesCorpus.changes "fake" corpus));
  (* two commits after the build, newest first as gwd writes them *)
  write_synchro (file "synchro_patches")
    [ ("300", [ 7 ], [ 2 ]); ("200", [ 5; 7 ], []); ("100", [ 1 ], [ 0 ]) ];
  Alcotest.(check (pair (list string) (list string)))
    "later commits"
    ([ "5"; "7" ], [ "2" ])
    (names (NotesCorpus.changes "fake" corpus));
  Alcotest.(check string)
    "still available" "available"
    (status_name (NotesCorpus.read "fake"));
  (* fewer commits than at build time: synchro reset, corpus unusable *)
  write_synchro (file "synchro_patches") [];
  Alcotest.(check string)
    "synchro reset" "outdated"
    (status_name (NotesCorpus.read "fake"))

let test_candidate () =
  let cand q = NotesSearch.candidate (NotesSearch.query false q) in
  let f =
    NotesSearch.fold "Jean-Pierre Dupond, baptisé à l’Église de Saint‑Pair"
  in
  Alcotest.(check bool) "folded word" true (cand "eglise" f);
  Alcotest.(check bool) "accented query" true (cand "Église baptisé" f);
  Alcotest.(check bool) "apostrophe" true (cand "l'eglise" f);
  Alcotest.(check bool) "joker pieces" true (cand "jean?pierre dupon?" f);
  Alcotest.(check bool) "stars dropped" true (cand "*pierre*" f);
  Alcotest.(check bool) "missing word" false (cand "moulin" f);
  Alcotest.(check bool)
    "exclusions left to the exact match" true (cand "dupond -eglise" f);
  (* never rejects what matches accepts, whatever the mode *)
  let texts =
    [ "Église Saint-Pierre"; "l’aube"; "Jean Pierre"; "HENRIETTE"; "Dupont" ]
  in
  let queries =
    [ "eglise"; "l'a"; "jean?pierre"; "henri*"; "*ette"; "dupon?" ]
  in
  List.iter
    (fun cs ->
      List.iter
        (fun fw ->
          List.iter
            (fun q ->
              let terms = NotesSearch.query cs q in
              List.iter
                (fun t ->
                  if NotesSearch.matches ~full_word:fw cs terms t then
                    Alcotest.(check bool)
                      (Printf.sprintf "superset %S %S cs=%b fw=%b" q t cs fw)
                      true
                      (NotesSearch.candidate terms (NotesSearch.fold t)))
                texts)
            queries)
        [ false; true ])
    [ false; true ]

let test_matches () =
  let q = NotesSearch.query false "moulin -dupont" in
  Alcotest.(check bool)
    "required" true
    (NotesSearch.matches false q "le moulin");
  Alcotest.(check bool)
    "excluded" false
    (NotesSearch.matches false q "le moulin de Dupont");
  Alcotest.(check bool) "missing" false (NotesSearch.matches false q "rien")

let v =
  [
    ( "notes-corpus",
      [
        Alcotest.test_case "round trip and fingerprint" `Quick test_round_trip;
        Alcotest.test_case "synchro changes" `Quick test_changes;
        Alcotest.test_case "matches" `Quick test_matches;
        Alcotest.test_case "folded candidates" `Quick test_candidate;
      ] );
  ]
