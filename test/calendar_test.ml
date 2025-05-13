let data_sure =
  [
    Def.{ day = 1; month = 1; year = 1900; delta = 0; prec = Sure };
    Def.{ day = 0; month = 1; year = 1900; delta = 0; prec = Sure };
    Def.{ day = 0; month = 0; year = 1900; delta = 0; prec = Sure };
  ]

let data_oryear =
  [
    Def.
      {
        day = 1;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 1; month2 = 1; year2 = 1901; delta2 = 0 };
      };
    Def.
      {
        day = 0;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 0; month2 = 1; year2 = 1901; delta2 = 0 };
      };
    Def.
      {
        day = 0;
        month = 0;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 0; month2 = 0; year2 = 1901; delta2 = 0 };
      };
  ]

open Alcotest
open Calendar

(* TODO Fmt *)
let testable_calendar = testable Fmt.nop ( = )

let round_trip of_ to_ l () =
  let f d = of_ (to_ d) in
  (* todo should iter in v? *)
  List.iter (fun d -> (check testable_calendar) "" d (f d)) l

let expect_failure name speed f =
  Alcotest.test_case name speed (fun () ->
      try
        f ();
        Alcotest.fail "Expected this test to fail, but it passed"
      with _ -> ())

let v =
  [
    ( (* this fail because Calendars library does not work on incomplete dates (day|month) = 0 *)
      (* see issue 2172 *)
      "calendar-sdn",
      [
        expect_failure "Calendar gregorian <-> sdn" `Quick
          (round_trip (gregorian_of_sdn Def.Sure) sdn_of_gregorian data_sure);
        expect_failure "Calendar julian <-> sdn" `Quick
          (round_trip (julian_of_sdn Def.Sure) sdn_of_julian data_sure);
        expect_failure "Calendar french <-> sdn" `Quick
          (round_trip (french_of_sdn Def.Sure) sdn_of_french data_sure);
        expect_failure "Calendar hebrew <-> sdn" `Quick
          (round_trip (hebrew_of_sdn Def.Sure) sdn_of_hebrew data_sure);
      ] );
    ( "calendar-greg",
      [
        test_case "Calendar gregorian <-> julian" `Quick
          (round_trip gregorian_of_julian julian_of_gregorian
             (data_sure @ data_oryear));
        test_case "Calendar gregorian <-> french" `Quick
          (round_trip gregorian_of_french french_of_gregorian
             (data_sure @ data_oryear));
        test_case "Calendar gregorian <-> hebrew" `Quick
          (round_trip gregorian_of_hebrew hebrew_of_gregorian
             (data_sure @ data_oryear));
      ] );
  ]

(*

let suite =
  [
    "Calendar"
    >::: []
         (* @ (sdn_round_trip "gregorian" Calendar.sdn_of_gregorian Calendar.gregorian_of_sdn)
          * @ (sdn_round_trip "julian" Calendar.sdn_of_julian Calendar.julian_of_sdn)
          * @ (sdn_round_trip "french" Calendar.sdn_of_french Calendar.french_of_sdn)
          * @ (sdn_round_trip "hebrew" Calendar.sdn_of_hebrew Calendar.hebrew_of_sdn) *)
  ]
  *)
