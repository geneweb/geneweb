open Alcotest
open Def
open Geneweb
module Driver = Geneweb_db.Driver

let year y =
  Date.cdate_of_od
    (Some
       (Adef.Dgreg
          ({ day = 0; month = 0; year = y; prec = Sure; delta = 0 }, Dgregorian)))

let text s = Date.cdate_of_od (Some (Adef.Dtext s))

let free_order name marriages =
  let n = List.length marriages in
  let person i = { (Mutil.empty_person 0 1) with occ = i; key_index = i } in
  let data =
    ( ( Array.init (n + 1) person,
        Array.make (n + 1) { Driver.no_ascend with parents = None },
        Array.init (n + 1) (fun i ->
            { family = (if i = 0 then Array.init n Fun.id else [| i - 1 |]) })
      ),
      ( Array.of_list
          (List.mapi
             (fun i marriage ->
               { (Mutil.empty_family 0) with marriage; fam_index = i })
             marriages),
        Array.init n (fun i -> Adef.couple 0 (i + 1)),
        Array.make n { children = [||] } ),
      [| ""; "?" |],
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) } )
  in
  Driver.make name [] data @@ fun base ->
  CheckItem.free_order
    (CheckItem.compare_unions base)
    (Array.to_list
       (Driver.get_family (Driver.poi base (Driver.Iper.of_string "0"))))

let test_free_family_order () =
  List.iteri
    (fun i (msg, expected, marriages) ->
      (check bool) msg expected
        (free_order ("update_order_" ^ string_of_int i) marriages))
    [
      ("one family", false, [ year 1850 ]);
      ("ordered dates", false, [ year 1850; year 1860 ]);
      ("missing date", true, [ year 1850; Date.cdate_None ]);
      ("textual date", true, [ year 1850; text "vers 1860" ]);
      ("same year", true, [ year 1850; year 1850 ]);
      ("contradicted dates", true, [ year 1860; year 1850 ]);
    ]

let v =
  [
    ( "update-order",
      [
        test_case "free family order" `Quick test_free_family_order;
      ] );
  ]
