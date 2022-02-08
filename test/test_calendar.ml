open OUnit2

let data_sure =
  [ Def.{ day = 1 ; month = 1 ; year = 1900 ; delta = 0 ; prec = Sure }
  ; Def.{ day = 0 ; month = 1 ; year = 1900 ; delta = 0 ; prec = Sure }
  ; Def.{ day = 0 ; month = 0 ; year = 1900 ; delta = 0 ; prec = Sure }
  ]

let data_oryear =
  [ Def.{ day = 1 ; month = 1 ; year = 1900 ; delta = 0
        ; prec = OrYear { day2 = 1 ; month2 = 1 ; year2 = 1901 ; delta2 = 0 } }
  ; Def.{ day = 0 ; month = 1 ; year = 1900 ; delta = 0
        ; prec = OrYear { day2 = 0 ; month2 = 1 ; year2 = 1901 ; delta2 = 0 } }
  ; Def.{ day = 0 ; month = 0 ; year = 1900 ; delta = 0
        ; prec = OrYear { day2 = 0 ; month2 = 0 ; year2 = 1901 ; delta2 = 0 } }
  ]

let round_trip_aux =
  let printer = Def_show.show_dmy in
  fun label to_t of_t ->
    List.mapi (fun i d ->
        (label ^ string_of_int i) >:: (fun _ -> assert_equal ~printer d (of_t (to_t d)))
      )

let sdn_round_trip label to_sdn of_sdn =
  round_trip_aux (label ^ " <-> sdn round trip ") to_sdn (of_sdn Def.Sure) data_sure

let gregorian_round_trip label to_g of_g =
  round_trip_aux (label ^ " <-> gregorian round trip ") to_g of_g (data_sure @ data_oryear)

let suite =
  [ "Calendar" >:::
    []

    (* @ (sdn_round_trip "gregorian" Calendar.sdn_of_gregorian Calendar.gregorian_of_sdn)
     * @ (sdn_round_trip "julian" Calendar.sdn_of_julian Calendar.julian_of_sdn)
     * @ (sdn_round_trip "french" Calendar.sdn_of_french Calendar.french_of_sdn)
     * @ (sdn_round_trip "hebrew" Calendar.sdn_of_hebrew Calendar.hebrew_of_sdn) *)

    @ (gregorian_round_trip "julian" Calendar.gregorian_of_julian Calendar.julian_of_gregorian)
    @ (gregorian_round_trip "french" Calendar.gregorian_of_french Calendar.french_of_gregorian)
    @ (gregorian_round_trip "hebrew" Calendar.gregorian_of_hebrew Calendar.hebrew_of_gregorian)

  ]
