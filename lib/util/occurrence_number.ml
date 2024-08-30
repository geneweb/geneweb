let smallest_free occurrence_numbers =
  let list_occ = Ext_int.Set.elements occurrence_numbers in
  let rec loop cnt1 = function
    | cnt2 :: list -> if cnt1 = cnt2 then loop (cnt1 + 1) list else cnt1
    | [] -> cnt1
  in
  loop 0 list_occ
