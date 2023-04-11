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
    { day = 0; month = 0; year = 1986; delta = 0; prec = After };
    { day = 31; month = 12; year = 1986; delta = 0; prec = Sure };
  ]

let event_chronology : (int * int Geneweb.Event.event_name * int option) list =
  let open Geneweb.Event in
  let open Def in
  [
    (0, Pevent Epers_Birth, Some 1);
    (1, Pevent Epers_Baptism, Some 2);
    (2, Fevent Efam_Marriage, Some 3);
    (3, Fevent Efam_Marriage, None);
    (4, Pevent Epers_Death, Some 4);
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

let check_event_chronology () =
  let pp_event fmt (id, _e, _d) = Format.fprintf fmt "%d;@." id in
  let printer l =
    Format.asprintf "@.%a@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_event)
      l
  in
  let get_name (_id, name, _d_opt) = name in
  let get_date (_id, _name, d_opt) =
    (* int option -> cdate *)
    match d_opt with
    | None -> Adef.Cnone
    | Some i ->
        Date.cdate_of_date
        @@ Date.Dgreg (Date.gregorian_of_sdn ~prec:Sure i, Date.Dgregorian)
  in
  let shuffled_event_chronologies =
    List.init 10 (fun _i -> shuffle event_chronology)
  in
  List.map
    (fun l ->
      "event chronology - " >:: fun _ ->
      assert_equal ~printer event_chronology
        (Geneweb.Event.sort_events get_name get_date l))
    shuffled_event_chronologies

let suite =
  [ "Chronology" >::: [] @ check_chronology () @ check_event_chronology () ]
