let data_sure =
  [
    Date.{ day = 1; month = 1; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 2; month = 1; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 3; month = 2; year = 1900; delta = 0; prec = Sure };
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
        day = 1;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 1; month2 = 1; year2 = 1901; delta2 = 0 };
      };
    Date.
      {
        day = 1;
        month = 1;
        year = 1900;
        delta = 0;
        prec = OrYear { day2 = 1; month2 = 1; year2 = 1901; delta2 = 0 };
      };
  ]

(* note: convert do not handle day>0 AND month = 0 correctly
   they should not exists in geneweb *)
let data_incomplete =
  [
    Date.{ day = 0; month = 1; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 0; month = 12; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 0; month = 0; year = 1900; delta = 0; prec = Sure };
    Date.
      {
        day = 1;
        month = 2;
        year = 1900;
        delta = 0;
        prec = OrYear Date.{ day2 = 3; month2 = 4; year2 = 1900; delta2 = 6 };
      };
    Date.
      {
        day = 0;
        month = 2;
        year = 1900;
        delta = 0;
        prec = OrYear Date.{ day2 = 3; month2 = 4; year2 = 1900; delta2 = 6 };
      };
    Date.
      {
        day = 0;
        month = 2;
        year = 1900;
        delta = 0;
        prec = OrYear Date.{ day2 = 0; month2 = 4; year2 = 1860; delta2 = 0 };
      };
    Date.
      {
        day = 0;
        month = 0;
        year = 1900;
        delta = 0;
        prec = OrYear Date.{ day2 = 0; month2 = 4; year2 = 1986; delta2 = 0 };
      };
    Date.
      {
        day = 0;
        month = 0;
        year = 1900;
        delta = 0;
        prec = OrYear Date.{ day2 = 0; month2 = 0; year2 = 1986; delta2 = 0 };
      };
  ]

(* TODO Fmt *)
(* TODO use Date.compare_date *)
let testable_date = Alcotest.testable Fmt.nop ( = )

let round_trip of_ to_ l () =
  let f d = of_ (to_ d) in
  (* todo should iter in v? *)
  List.iter (fun d -> (Alcotest.check testable_date) "" d (f d)) l

let incomplete_date to_t of_t () =
  List.iter
    (fun d -> (Alcotest.check testable_date) "" d (of_t (to_t d)))
    data_incomplete

let v =
  [
    ( (* this fail because Calendars library does not work on incomplete dates (day|month) = 0 *)
      "date-sdn",
      [
        Alcotest.test_case "Calendar gregorian <-> sdn" `Quick
          (round_trip
             (Date.gregorian_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Date.Dgregorian)
             data_sure);
        Alcotest.test_case "Date julian <-> sdn" `Quick
          (round_trip
             (Date.julian_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Date.Djulian)
             data_sure);
        Alcotest.test_case "Date french <-> sdn" `Quick
          (round_trip
             (Date.french_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Date.Dfrench)
             data_sure);
        Alcotest.test_case "Date hebrew <-> sdn" `Quick
          (round_trip
             (Date.hebrew_of_sdn ~prec:Date.Sure)
             (Date.to_sdn ~from:Date.Dhebrew)
             data_sure);
      ] );
    ( "date-greg",
      [
        Alcotest.test_case "Date gregorian <-> julian" `Quick
          (round_trip
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Djulian)
             (Date.convert ~from:Date.Djulian ~to_:Date.Dgregorian)
             (data_sure @ data_oryear));
        Alcotest.test_case "Date gregorian <-> french" `Quick
          (round_trip
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Dfrench)
             (Date.convert ~from:Date.Dfrench ~to_:Date.Dgregorian)
             (data_sure @ data_oryear));
        Alcotest.test_case "Date gregorian <-> hebrew" `Quick
          (round_trip
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Dhebrew)
             (Date.convert ~from:Date.Dhebrew ~to_:Date.Dgregorian)
             (data_sure @ data_oryear));
      ] );
    ( "date-incomplete",
      [
        Alcotest.test_case "Date incomplete gregorian <-> julian" `Quick
          (incomplete_date
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Djulian)
             (Date.convert ~from:Date.Djulian ~to_:Date.Dgregorian));
        Alcotest.test_case "Date incomplete gregorian <-> french" `Quick
          (incomplete_date
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Dfrench)
             (Date.convert ~from:Date.Dfrench ~to_:Date.Dgregorian));
        Alcotest.test_case "Date incomplete gregorian <-> hebrew" `Quick
          (incomplete_date
             (Date.convert ~from:Date.Dgregorian ~to_:Date.Dhebrew)
             (Date.convert ~from:Date.Dhebrew ~to_:Date.Dgregorian));
      ] );
  ]
