open OUnit2

(* we test that those items are correctly sorted *)

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
    { day = 0; month = 0; year = 1986; delta = 0; prec = Sure };
    { day = 31; month = 12; year = 1986; delta = 0; prec = Sure };
    { day = 0; month = 0; year = 1986; delta = 0; prec = After };
  ]

let () = Random.init 0

let shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  let sond = List.sort compare nd in
  List.map snd sond

let check_chronology () =
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

let suite = [ "Chronology" >::: [] @ check_chronology () ]
