open Geneweb
open OUnit2
open Config
open Def

let date (d, m, y, p) = {day = d; month = m; year = y; prec = p; delta = 0 }

let date_compare_dmy_opt_aux strict exp d1 d2 =
  let printer = function Some x -> "Some " ^ string_of_int x | None -> "None" in
  assert_equal ~printer exp @@ Date.compare_dmy_opt ~strict d1 d2

let label d1 d2 =
  Def_show.show_dmy d1
  ^ " / "
  ^ Def_show.show_dmy d2
  ^ " ( "
  ^ Def_show.show_dmy (Date.time_elapsed d1 d2)
  ^ " ) "

let t_aux strict exp d1 d2 =
  let d1 = date d1 in
  let d2 = date d2 in
  label d1 d2 >:: (fun _ -> date_compare_dmy_opt_aux strict exp d1 d2)

let t = t_aux false

let t_strict = t_aux true

let eq (d1, m1, y1) (d2, m2, y2) =
  let z = { day = 0 ; month = 0 ; year = 0 ; prec = Sure ; delta = 0 } in
  let d1 = date (d1, m1, y1, Sure) in
  let d2 = date (d2, m2, y2, Sure) in
  let printer = Def_show.show_dmy in
  label d1 d2 >:: (fun _ -> assert_equal ~printer z @@ Date.time_elapsed d1 d2)

let suite =
  [ "Date.date_compare_dmy_opt" >:::
    [ t (Some ( 1)) (26, 11, 1759, Sure) (26, 11, 1759, Before)
    ; t (Some ( 0)) (26, 11, 1759, Sure) (27, 11, 1759, Before)
    ; t (Some ( 0)) (26, 11, 1759, Sure) (25, 11, 1759, After)
    ]
  ; "Date.date_compare_dmy_opt (strict)" >:::
    [ t_strict (Some ( 1)) (26, 11, 1759, Sure) (26, 11, 1759, Before)
    ; t_strict (None     ) (26, 11, 1759, Sure) (27, 11, 1759, Before)
    ; t_strict (None     ) (26, 11, 1759, Sure) (25, 11, 1759, After)
    ]
  ; "Date.eq" >:::
    [ eq (31, 11, 1759) (1, 12, 1759)
    ]
  ]
