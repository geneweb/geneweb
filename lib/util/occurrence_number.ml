let smallest_free occurrence_numbers =
  let rec loop maybe_free_occurrence_number = function
    | occurrence_number :: occurrence_numbers ->
        if maybe_free_occurrence_number = occurrence_number then
          loop (maybe_free_occurrence_number + 1) occurrence_numbers
        else maybe_free_occurrence_number
    | [] -> maybe_free_occurrence_number
  in
  loop 0 (Ext_int.Set.elements occurrence_numbers)
