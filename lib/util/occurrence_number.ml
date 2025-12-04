let is_valid occurrence_number = occurrence_number >= 0

let smallest_free occurrence_numbers =
  let rec loop maybe_free_occurrence_number = function
    | occurrence_number :: occurrence_numbers ->
        if maybe_free_occurrence_number = occurrence_number then
          loop (maybe_free_occurrence_number + 1) occurrence_numbers
        else maybe_free_occurrence_number
    | [] -> maybe_free_occurrence_number
  in
  loop 0 (List.filter is_valid @@ Ext_int.Set.elements occurrence_numbers)
