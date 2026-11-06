open Alcotest
open Def
open Geneweb
module Driver = Geneweb_db.Driver

let year y =
  Date.cdate_of_od
    (Some
       (Adef.Dgreg
          ({ day = 0; month = 0; year = y; prec = Sure; delta = 0 }, Dgregorian)))

let text s = Date.cdate_of_od (Some (Adef.Dtext s))

let free_order name marriages =
  let n = List.length marriages in
  let person i = { (Mutil.empty_person 0 1) with occ = i; key_index = i } in
  let data =
    ( ( Array.init (n + 1) person,
        Array.make (n + 1) { Driver.no_ascend with parents = None },
        Array.init (n + 1) (fun i ->
            { family = (if i = 0 then Array.init n Fun.id else [| i - 1 |]) })
      ),
      ( Array.of_list
          (List.mapi
             (fun i marriage ->
               { (Mutil.empty_family 0) with marriage; fam_index = i })
             marriages),
        Array.init n (fun i -> Adef.couple 0 (i + 1)),
        Array.make n { children = [||] } ),
      [| ""; "?" |],
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) } )
  in
  Driver.make name [] data @@ fun base ->
  CheckItem.free_order
    (CheckItem.compare_unions base)
    (Array.to_list
       (Driver.get_family (Driver.poi base (Driver.Iper.of_string "0"))))

let test_free_family_order () =
  List.iteri
    (fun i (msg, expected, marriages) ->
      (check bool) msg expected
        (free_order ("update_order_" ^ string_of_int i) marriages))
    [
      ("one family", false, [ year 1850 ]);
      ("ordered dates", false, [ year 1850; year 1860 ]);
      ("missing date", true, [ year 1850; Date.cdate_None ]);
      ("textual date", true, [ year 1850; text "vers 1860" ]);
      ("same year", true, [ year 1850; year 1850 ]);
      ("contradicted dates", true, [ year 1860; year 1850 ]);
    ]

let pevent epers_name epers_date =
  {
    epers_name;
    epers_date;
    epers_place = Driver.Istr.empty;
    epers_reason = Driver.Istr.empty;
    epers_note = Driver.Istr.empty;
    epers_src = Driver.Istr.empty;
    epers_witnesses = [||];
  }

let fevent efam_name efam_date =
  {
    efam_name;
    efam_date;
    efam_place = Driver.Istr.empty;
    efam_reason = Driver.Istr.empty;
    efam_note = Driver.Istr.empty;
    efam_src = Driver.Istr.empty;
    efam_witnesses = [||];
  }

let test_free_event_order () =
  let none = Date.cdate_None in
  List.iter
    (fun (msg, expected, events) ->
      (check bool) msg expected
        (CheckItem.free_order CheckItem.compare_pevents events))
    [
      ("one event", false, [ pevent Epers_Birth none ]);
      ( "life events",
        false,
        [
          pevent Epers_Birth none;
          pevent Epers_Baptism none;
          pevent Epers_Death none;
          pevent Epers_Burial none;
        ] );
      ( "dated events",
        false,
        [
          pevent Epers_Birth (year 1850);
          pevent Epers_Residence (year 1870);
          pevent Epers_Death (year 1900);
        ] );
      ( "undated event",
        true,
        [ pevent Epers_Birth (year 1850); pevent Epers_Residence none ] );
    ];
  List.iter
    (fun (msg, expected, events) ->
      (check bool) msg expected
        (CheckItem.free_order CheckItem.compare_fevents events))
    [
      ("union alone", false, [ fevent Efam_Marriage none ]);
      ( "dated family events",
        false,
        [ fevent Efam_Engage (year 1849); fevent Efam_Marriage (year 1850) ] );
      ( "union and divorce",
        false,
        [ fevent Efam_Marriage none; fevent Efam_Divorce none ] );
      ( "undated family event",
        true,
        [
          fevent Efam_Marriage (year 1850);
          fevent (Efam_Name Driver.Istr.empty) none;
        ] );
    ];
  (check bool) "event before birth" false
    (Option.is_some
       (CheckItem.misplaced CheckItem.compare_pevents
          [
            pevent (Epers_Name Driver.Istr.empty) none; pevent Epers_Birth none;
          ]));
  (check bool) "baptism before birth" true
    (Option.is_some
       (CheckItem.misplaced CheckItem.compare_pevents
          [ pevent Epers_Baptism none; pevent Epers_Birth none ]));
  (check bool) "divorce before marriage" true
    (Option.is_some
       (CheckItem.misplaced CheckItem.compare_fevents
          [ fevent Efam_Divorce none; fevent Efam_Marriage none ]));
  (check bool) "dated remarriage" false
    (Option.is_some
       (CheckItem.misplaced CheckItem.compare_fevents
          [
            fevent Efam_Marriage (year 1850);
            fevent Efam_Divorce (year 1860);
            fevent Efam_Marriage (year 1870);
          ]))

let v =
  [
    ( "update-order",
      [
        test_case "free family order" `Quick test_free_family_order;
        test_case "free event order" `Quick test_free_event_order;
      ] );
  ]
