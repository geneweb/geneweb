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

let test_round_trip () =
  with_fake_base @@ fun file ->
  write_file (file "base") "v1";
  Alcotest.(check string)
    "no file" "missing"
    (status_name (NotesCorpus.read "fake"));
  let entries =
    [ entry NotesCorpus.Person "3" "moulin"; entry NotesCorpus.Family "0" "x" ]
  in
  NotesCorpus.write "fake" (NotesCorpus.create "fake" entries);
  (match NotesCorpus.read "fake" with
  | NotesCorpus.Available c ->
      Alcotest.(check int) "entries" 2 (Array.length c.NotesCorpus.entries);
      Alcotest.(check string) "text" "moulin" c.NotesCorpus.entries.(0).text
  | NotesCorpus.Missing | NotesCorpus.Outdated -> Alcotest.fail "not available");
  (* a rebuilt base (other "base" file) makes the corpus outdated *)
  write_file (file "base") "version 2";
  Alcotest.(check string)
    "base rebuilt" "outdated"
    (status_name (NotesCorpus.read "fake"))

let test_changes () =
  with_fake_base @@ fun file ->
  write_file (file "base") "v1";
  write_synchro (file "synchro_patches") [ ("100", [ 1 ], [ 0 ]) ];
  let corpus = NotesCorpus.create "fake" [] in
  NotesCorpus.write "fake" corpus;
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
      ] );
  ]
