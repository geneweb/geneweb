let to_list_map fn a = Array.fold_right (fun x acc -> fn x :: acc) a []

let except v a =
  let rec loop i =
    if i = Array.length a then Array.copy a
    else if a.(i) = v then
      Array.append (Array.sub a 0 i)
        (Array.sub a (i + 1) (Array.length a - i - 1))
    else loop (i + 1)
  in
  loop 0
