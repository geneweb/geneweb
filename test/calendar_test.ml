let data_sure =
  [
    Date.{ day = 1; month = 1; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 0; month = 1; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 0; month = 0; year = 1900; delta = 0; prec = Sure };
  ]

let data_oryear =
  [
    Date.
      {
        day = 1;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 1; month2 = 1; year2 = 1901; delta2 = 0 };
      };
    Date.
      {
        day = 0;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 0; month2 = 1; year2 = 1901; delta2 = 0 };
      };
    Date.
      {
        day = 0;
        month = 0;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 0; month2 = 0; year2 = 1901; delta2 = 0 };
      };
  ]

open Alcotest

(* TODO Fmt *)
let testable_calendar = testable Fmt.nop ( = )

let round_trip of_ to_ l () =
  let f d = of_ (to_ d) in
  (* todo should iter in v? *)
  List.iter (fun d -> (check testable_calendar) "" d (f d)) l

let v =
  [
    ( (* this fail because Calendars library does not work on incomplete dates (day|month) = 0 *)
      "calendar-sdn",
      [
        test_case "Calendar gregorian <-> sdn" `Quick
          (round_trip
             (Date.gregorian_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Date.Dgregorian)
             data_sure);
        test_case "Calendar julian <-> sdn" `Quick
          (round_trip
             (Date.julian_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Date.Djulian)
             data_sure);
        test_case "Calendar french <-> sdn" `Quick
          (round_trip
             (Date.french_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Date.Dfrench)
             data_sure);
        test_case "Calendar hebrew <-> sdn" `Quick
          (round_trip
             (Date.hebrew_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Dhebrew)
             data_sure);
      ] );
    ( "calendar-greg",
      [
        test_case "Calendar gregorian <-> julian" `Quick
          (round_trip
             (Date.convert ~from:Date.Djulian ~to_:Date.Dgregorian)
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Djulian)
             (data_sure @ data_oryear));
        test_case "Calendar gregorian <-> french" `Quick
          (round_trip
             (Date.convert ~from:Date.Dfrench ~to_:Date.Dgregorian)
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Dfrench)
             (data_sure @ data_oryear));
        test_case "Calendar gregorian <-> hebrew" `Quick
          (round_trip
             (Date.convert ~from:Date.Dhebrew ~to_:Date.Dgregorian)
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Dhebrew)
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
