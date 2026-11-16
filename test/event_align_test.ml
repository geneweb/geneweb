open Alcotest
open Def
open Geneweb

let year y =
  Date.cdate_of_od
    (Some
       (Adef.Dgreg
          ({ day = 0; month = 0; year = y; prec = Sure; delta = 0 }, Dgregorian)))

let pevent ?(date = Date.cdate_None) ?(note = "") epers_name epers_place =
  {
    epers_name;
    epers_date = date;
    epers_place;
    epers_reason = "";
    epers_note = note;
    epers_src = "";
    epers_witnesses = [||];
  }

let fevent efam_name efam_place =
  {
    efam_name;
    efam_date = Date.cdate_None;
    efam_place;
    efam_reason = "";
    efam_note = "";
    efam_src = "";
    efam_witnesses = [||];
  }

let pairs = list (pair (option string) (option string))

let pevent_labels l =
  List.map
    (fun (b, a) ->
      let label e = e.epers_place ^ e.epers_note in
      (Option.map label b, Option.map label a))
    l

let test_align_pevents () =
  let check_align msg expected l1 l2 =
    (check pairs) msg expected (pevent_labels (Event.align_pevents l1 l2))
  in
  let birth = pevent Epers_Birth "b" and death = pevent Epers_Death "d" in
  check_align "unchanged"
    [ (Some "b", Some "b"); (Some "d", Some "d") ]
    [ birth; death ] [ birth; death ];
  check_align "inserted event"
    [
      (Some "b", Some "b");
      (None, Some "new");
      (Some "r", Some "r");
      (Some "d", Some "d");
    ]
    [ birth; pevent Epers_Residence "r"; death ]
    [ birth; pevent Epers_Residence "new"; pevent Epers_Residence "r"; death ];
  check_align "edited event"
    [ (Some "p", Some "p"); (None, Some "q") ]
    [ pevent ~date:(year 1900) Epers_Residence "p" ]
    [
      pevent ~date:(year 1901) Epers_Residence "p";
      pevent ~date:(year 1950) Epers_Residence "q";
    ];
  check_align "removed event"
    [ (Some "b", Some "b"); (Some "r", None); (Some "d", Some "d") ]
    [ birth; pevent Epers_Residence "r"; death ]
    [ birth; death ];
  check_align "removed last event"
    [ (Some "b", Some "b"); (Some "d", None) ]
    [ birth; death ] [ birth ];
  check_align "other name"
    [ (None, Some "x"); (Some "x", None) ]
    [ pevent Epers_Residence "x" ]
    [ pevent Epers_Occupation "x" ];
  check_align "moved event"
    [ (Some "r2", Some "r2"); (Some "r1", Some "r1") ]
    [
      pevent ~date:(year 1900) Epers_Residence "r1";
      pevent ~date:(year 1910) Epers_Residence "r2";
    ]
    [
      pevent ~date:(year 1910) Epers_Residence "r2";
      pevent ~date:(year 1900) Epers_Residence "r1";
    ];
  check_align "same score, same order"
    [ (Some "n1", Some "m1"); (Some "n2", Some "m2") ]
    [
      pevent ~note:"n1" Epers_Residence ""; pevent ~note:"n2" Epers_Residence "";
    ]
    [
      pevent ~note:"m1" Epers_Residence ""; pevent ~note:"m2" Epers_Residence "";
    ]

let test_align_fevents () =
  let labels l =
    List.map
      (fun (b, a) ->
        let label e = e.efam_place in
        (Option.map label b, Option.map label a))
      l
  in
  (check pairs) "inserted engagement"
    [ (None, Some "e"); (Some "m", Some "m"); (Some "d", Some "d") ]
    (labels
       (Event.align_fevents
          [ fevent Efam_Marriage "m"; fevent Efam_Divorce "d" ]
          [
            fevent Efam_Engage "e";
            fevent Efam_Marriage "m";
            fevent Efam_Divorce "d";
          ]))

let v =
  [
    ( "event-align",
      [
        test_case "personal events" `Quick test_align_pevents;
        test_case "family events" `Quick test_align_fevents;
      ] );
  ]
