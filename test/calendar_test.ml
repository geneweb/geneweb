let pp_dmy fmt d =
  Format.fprintf fmt "{day=%d;month=%d;year=%d;delta=%d;prec=...}" d.Def.day
    d.Def.month d.Def.year d.Def.delta

let testable_dmy = Alcotest.testable pp_dmy ( = )

let data_complete =
  [
    Def.{ day = 1; month = 1; year = 1900; delta = 0; prec = Sure };
    Def.{ day = 15; month = 6; year = 2000; delta = 0; prec = Sure };
    Def.{ day = 29; month = 2; year = 2000; delta = 0; prec = Sure };
  ]

let data_partial =
  [
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

let round_trip of_ to_ l () =
  let f d = of_ (to_ d) in
  List.iter (fun d -> (check testable_dmy) "" d (f d)) l

let sdn_round_trip name of_sdn sdn_of =
  test_case name `Quick (round_trip (of_sdn Def.Sure) sdn_of data_complete)

let v =
  [
    ( "calendar-sdn",
      [
        sdn_round_trip "gregorian <-> sdn" gregorian_of_sdn sdn_of_gregorian;
        sdn_round_trip "julian <-> sdn" julian_of_sdn sdn_of_julian;
        sdn_round_trip "french <-> sdn" french_of_sdn sdn_of_french;
        sdn_round_trip "hebrew <-> sdn" hebrew_of_sdn sdn_of_hebrew;
      ] );
    ( "calendar-conv",
      [
        test_case "gregorian <-> julian" `Quick
          (round_trip gregorian_of_julian julian_of_gregorian
             (data_complete @ data_partial @ data_oryear));
        test_case "gregorian <-> french" `Quick
          (round_trip gregorian_of_french french_of_gregorian
             (data_complete @ data_partial @ data_oryear));
        test_case "gregorian <-> hebrew" `Quick
          (round_trip gregorian_of_hebrew hebrew_of_gregorian
             (data_complete @ data_partial @ data_oryear));
      ] );
  ]
