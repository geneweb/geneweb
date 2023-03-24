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
    Date.{ day = 0; month = 1; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 0; month = 12; year = 1900; delta = 0; prec = Sure };
    Date.{ day = 0; month = 0; year = 1900; delta = 0; prec = Sure };
    (*
    convert do not handle day>0 and month = 0 correctly
    Date.{ day = 3; month = 0; year = 1900; delta = 0; prec = Sure };
    *)
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

(* test incomplete date *)
let incomplete_date label to_t of_t =
  let printer = Def_show.show_dmy in
  let label = "incomplete_date - " ^ label in
  List.mapi
    (fun i d ->
      label ^ string_of_int i >:: fun _ ->
      assert_equal ~printer d (of_t (to_t d)))
    data_incomplete

let chronology =
  let open Date in
  [
    { day = 0; month = 0; year = 1900; delta = 0; prec = Sure };
    { day = 0; month = 1; year = 1900; delta = 0; prec = Sure };
    { day = 3; month = 1; year = 1900; delta = 0; prec = Sure };
    { day = 3; month = 1; year = 1901; delta = 0; prec = Maybe };
    {
      day = 4;
      month = 1;
      year = 1901;
      delta = 0;
      prec = YearInt Date.{ day2 = 13; month2 = 12; year2 = 1986; delta2 = 0 };
    };
    { day = 0; month = 0; year = 1986; delta = 0; prec = Before };
    (*
    { day = 0; month = 0; year = 1986; delta = 0; prec = Sure };
    { day = 0; month = 0; year = 1986; delta = 0; prec = After };
    { day = 31; month = 12; year = 1986; delta = 0; prec = Sure };
    *)
  ]

let duration_between_chronology_items =
  let open Duration in
  [ of_sdn ~prec:Exact 0; of_sdn ~prec:Exact 2; of_years 1; of_days 1; add (of_years (1986-1901)) (of_days 3) ;(* of_sdn ~prec:More 0 *) ]

let check_chronology () =
  let shuffle l =
    let nd = List.map (fun c -> (Random.bits (), c)) l in
    let sond = List.sort compare nd in
    List.map snd sond
  in
  let printer l =
    Format.asprintf "@.%a@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline Def_show.pp_dmy)
      l
  in
  let shuffled_chronologies = List.init 10 (fun _i -> shuffle chronology) in
  List.map
    (fun l ->
      "chronology - " >:: fun _ ->
      assert_equal ~printer chronology (List.sort Date.compare_dmy l))
    shuffled_chronologies

let check_elapsed_time () =
  let printer d = Format.asprintf "%a" Duration.pp_duration_debug d in
  let cmp a b =
    (* TODO test prec too *)
    (* Duration should not be compared with '=' because they can have same duration (SDN) but different display ... *)
    Duration.compare a b = 0
  in
  let shift_list l =
    (* shift by one *)
    match l with _first :: l -> l | _l -> assert false
  in
  let l1 =
    (*remove last *)
    List.rev chronology |> shift_list |> List.rev
  in
  let l2 = shift_list chronology in
  let l = List.combine (List.combine l1 l2) duration_between_chronology_items in
  List.map
    (fun ((a, b), duration) ->
      "elapsed_time - " >:: fun _ ->
      assert_equal ~printer ~cmp
        duration
        (Duration.time_elapsed a b))
    l

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
             (Date.convert ~from:Dgregorian ~to_:Djulian)
         @ check_chronology () @ check_elapsed_time ();
  ]
