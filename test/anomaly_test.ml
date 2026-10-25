open Geneweb
open Anomaly

let p first_name occ surname = { first_name; surname; occ }
let jean = p "Jean" 0 "Dupont"
let marie = p "Marie" 1 "Martin"
let paul = p "Paul" 0 "Dupont"
let cpl = { father = jean; mother = marie }

(* one value per constructor of Anomaly.t *)
let samples =
  [
    BigAgeBetweenSpouses (jean, marie, 52);
    BirthAfterDeath jean;
    ChangedOrderOfChildren cpl;
    ChangedOrderOfFamilyEvents cpl;
    ChangedOrderOfMarriages jean;
    ChangedOrderOfPersonEvents jean;
    ChildrenNotInOrder (cpl, paul, jean);
    CloseChildren (cpl, paul, jean);
    DeadOld (jean, 104);
    DeadTooEarlyToBeFather (jean, paul);
    DistantChildren (cpl, paul, jean);
    FEventOrder (jean, Def.Efam_Divorce, Def.Efam_Name "Fiançailles");
    FWitnessEventAfterDeath (paul, Def.Efam_Marriage, cpl);
    FWitnessEventBeforeBirth (paul, Def.Efam_Marriage, cpl);
    IncoherentAncestorDate (jean, paul);
    IncoherentSex marie;
    MarriageDateAfterDeath jean;
    MarriageDateBeforeBirth jean;
    MotherDeadBeforeChildBirth (marie, paul);
    OldForMarriage (jean, 101, cpl);
    ParentBornAfterChild (jean, paul);
    ParentTooOld (jean, 75, paul);
    ParentTooYoung (marie, 11, paul);
    PEventOrder (jean, Def.Epers_Burial, Def.Epers_Name "Recensement 1851");
    PossibleDuplicateFam cpl;
    PossibleDuplicateFamHomonymous (cpl, jean);
    PWitnessEventAfterDeath (paul, Def.Epers_Baptism, jean);
    PWitnessEventBeforeBirth (paul, Def.Epers_Baptism, jean);
    TitleDatesError (jean, "comte Mortain");
    UndefinedSex paul;
    YoungForMarriage (marie, 12, cpl);
  ]

let tmp_dir () =
  let d =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "anomaly_test_%d_%.0f" (Unix.getpid ())
         (Unix.gettimeofday () *. 1e6))
  in
  Unix.mkdir d 0o755;
  d

let test_kinds () =
  Alcotest.(check int)
    "one sample per kind" (List.length all_kinds)
    (List.length (List.sort_uniq compare (List.map kind samples)));
  List.iter
    (fun k ->
      Alcotest.(check bool)
        (kind_name k) true
        (kind_of_name (kind_name k) = Some k))
    all_kinds

let test_json_roundtrip () =
  let dir = tmp_dir () in
  let report =
    {
      date = "2026-10-25 10:00:00";
      producer = "test";
      history_days = 8;
      anomalies = samples;
      changes =
        [ (person_key jean, { time = "2026-10-24 09:00:00"; wizard = "hg" }) ];
    }
  in
  save dir report;
  match load dir with
  | Error e -> Alcotest.fail e
  | Ok r ->
      Alcotest.(check int)
        "count" (List.length samples) (List.length r.anomalies);
      List.iter2
        (fun a b -> Alcotest.(check bool) (kind_name (kind a)) true (a = b))
        samples r.anomalies;
      Alcotest.(check bool) "changes" true (r.changes = report.changes);
      (* reading one kind skips the others, PossibleDuplicateFam being a
         prefix of PossibleDuplicateFamHomonymous *)
      List.iter
        (fun k ->
          match
            fold ~kind:k dir (fun n a -> if kind a = k then n + 1 else -100) 0
          with
          | Ok (_, n) -> Alcotest.(check int) (kind_name k) 1 n
          | Error e -> Alcotest.fail e)
        all_kinds

let test_identity () =
  Alcotest.(check bool)
    "age left out" true
    (identity (DeadOld (jean, 104)) = identity (DeadOld (jean, 106)));
  Alcotest.(check bool)
    "other child, other anomaly" false
    (identity (ParentTooYoung (marie, 11, paul))
    = identity (ParentTooYoung (marie, 11, jean)))

let test_whitelist () =
  let dir = tmp_dir () in
  let ok = function Ok () -> () | Error e -> Alcotest.fail e in
  let record ?(wizard = "hg") verified a =
    AnomalyWhitelist.record dir ~wizard ~verified a
  in
  let young = YoungForMarriage (marie, 12, cpl) in
  ok (record true young);
  ok (record ~wizard:"a2/../x" true (DeadOld (jean, 104)));
  Alcotest.(check bool)
    "kind that cannot be verified" true
    (Result.is_error (record true (BirthAfterDeath jean)));
  Alcotest.(check bool)
    "unnamed person" true
    (Result.is_error (record true (DeadOld (p "?" 0 "?", 104))));
  Alcotest.(check (list string))
    "one fragment per wizard, safe names" [ "a2____x"; "hg" ]
    (List.map fst (AnomalyWhitelist.pending dir));
  let check t name expected a =
    Alcotest.(check bool) name expected (AnomalyWhitelist.mem t a)
  in
  let t = AnomalyWhitelist.load dir in
  check t "pending verification counts" true young;
  check t "same anomaly, new age" true (DeadOld (jean, 105));
  check t "other anomaly of the same person" false
    (ParentTooYoung (marie, 11, paul));
  (match AnomalyWhitelist.find t young with
  | Some e -> Alcotest.(check string) "who" "hg" e.wizard
  | None -> Alcotest.fail "not found");
  (* a later cancellation by another wizard wins *)
  ok (record ~wizard:"zz" false young);
  let t = AnomalyWhitelist.load dir in
  check t "cancelled" false young;
  check t "other kept" true (DeadOld (jean, 104));
  let r = AnomalyWhitelist.fold dir in
  Alcotest.(check int) "fragments merged" 3 r.fragments;
  Alcotest.(check int) "verified after merge" 1 r.entries;
  Alcotest.(check int)
    "nothing pending" 0
    (List.length (AnomalyWhitelist.pending dir));
  let t = AnomalyWhitelist.load dir in
  check t "after merge" true (DeadOld (jean, 104));
  check t "cancellation merged" false young;
  ok (record true young);
  Alcotest.(check int)
    "verified again" 2
    (AnomalyWhitelist.size (AnomalyWhitelist.load dir));
  ignore (AnomalyWhitelist.fold dir);
  Alcotest.(check bool)
    "second merge is a no-op" false (AnomalyWhitelist.fold dir).rewritten

let v =
  [
    ( "anomaly",
      [
        Alcotest.test_case "kinds" `Quick test_kinds;
        Alcotest.test_case "json roundtrip" `Quick test_json_roundtrip;
        Alcotest.test_case "identity" `Quick test_identity;
        Alcotest.test_case "whitelist" `Quick test_whitelist;
      ] );
  ]
