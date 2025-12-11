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

let from_string s =
  let occurrence_number =
    try Some (Scanf.sscanf s "%d%!" Fun.id)
    with Scanf.Scan_failure _ | Failure _ | End_of_file -> None
  in
  Option.fold ~none:(Error `Not_a_decimal)
    ~some:(fun occurrence_number ->
      if occurrence_number < 0 then Error `Negative_decimal
      else Ok occurrence_number)
    occurrence_number
