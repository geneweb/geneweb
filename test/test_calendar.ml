(* TODO move test to Calendars; test case with day|month = 0 *)
open OUnit2

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

let round_trip_aux =
  let printer = Def_show.show_dmy in
  fun label to_t of_t ->
    List.mapi (fun i d ->
        label ^ string_of_int i >:: fun _ ->
        assert_equal ~printer d (of_t (to_t d)))

let sdn_round_trip label to_sdn of_sdn =
  round_trip_aux (label ^ " <-> sdn round trip ") to_sdn of_sdn data_sure

let gregorian_round_trip label to_g of_g =
  round_trip_aux
    (label ^ " <-> gregorian round trip ")
    to_g of_g (data_sure @ data_oryear)

let data_incomplete =
  [
    ( Date.{ day = 0; month = 1; year = 1900; delta = 0; prec = Sure },
      Date.{ day = 1; month = 2; year = 1900; delta = 0; prec = Before } );
    ( Date.{ day = 0; month = 12; year = 1900; delta = 0; prec = Sure },
      Date.{ day = 1; month = 1; year = 1901; delta = 0; prec = Before } );
    ( Date.{ day = 0; month = 0; year = 1900; delta = 0; prec = Sure },
      Date.{ day = 1; month = 1; year = 1901; delta = 0; prec = Before } );
    ( Date.{ day = 3; month = 0; year = 1900; delta = 0; prec = Sure },
      Date.{ day = 1; month = 1; year = 1901; delta = 0; prec = Before } );
    ( Date.
        {
          day = 1;
          month = 2;
          year = 1900;
          delta = 0;
          prec = OrYear Date.{ day2 = 0; month2 = 1; year2 = 1900; delta2 = 0 };
        },
      Date.
        {
          day = 1;
          month = 2;
          year = 1900;
          delta = 0;
          prec = OrYear Date.{ day2 = 1; month2 = 2; year2 = 1900; delta2 = 0 };
        } );
    ( Date.
        {
          day = 1;
          month = 2;
          year = 1900;
          delta = 0;
          prec = OrYear Date.{ day2 = 3; month2 = 0; year2 = 1900; delta2 = 0 };
        },
      Date.
        {
          day = 1;
          month = 2;
          year = 1900;
          delta = 0;
          prec = OrYear Date.{ day2 = 1; month2 = 1; year2 = 1901; delta2 = 0 };
        } );
    ( Date.
        {
          day = 0;
          month = 2;
          year = 1900;
          delta = 0;
          prec = OrYear Date.{ day2 = 3; month2 = 4; year2 = 1900; delta2 = 0 };
        },
      Date.{ day = 1; month = 3; year = 1900; delta = 0; prec = Before } );
  ]

(* test incomplete date *)
let incomplete_date label to_t of_t =
  let printer = Def_show.show_dmy in
  let label = "incomplete_date - " ^ label in
  List.mapi
    (fun i (d1, d2) ->
      label ^ string_of_int i >:: fun _ ->
      assert_equal ~printer d2 (of_t (to_t d1)))
    data_incomplete

let suite =
  [
    "Calendar"
    >::: []
         @ sdn_round_trip "gregorian"
             (Date.to_sdn ~from:Dgregorian)
             (Date.gregorian_of_sdn ~prec:Sure)
         @ sdn_round_trip "julian"
             (Date.to_sdn ~from:Djulian)
             (Date.julian_of_sdn ~prec:Sure)
         @ sdn_round_trip "french"
             (Date.to_sdn ~from:Dfrench)
             (Date.french_of_sdn ~prec:Sure)
         @ sdn_round_trip "hebrew"
             (Date.to_sdn ~from:Dhebrew)
             (Date.hebrew_of_sdn ~prec:Sure)
         @ gregorian_round_trip "julian"
             (Date.convert ~from:Djulian ~to_:Dgregorian)
             (Date.convert ~from:Dgregorian ~to_:Djulian)
         @ gregorian_round_trip "french"
             (Date.convert ~from:Dfrench ~to_:Dgregorian)
             (Date.convert ~from:Dgregorian ~to_:Dfrench)
         @ gregorian_round_trip "hebrew"
             (Date.convert ~from:Dhebrew ~to_:Dgregorian)
             (Date.convert ~from:Dgregorian ~to_:Dhebrew)
         @ incomplete_date "julian"
             (Date.convert ~from:Djulian ~to_:Dgregorian)
             (Date.convert ~from:Dgregorian ~to_:Djulian);
  ]
