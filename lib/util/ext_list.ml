let take l n =
  let rec aux res l n =
    match l with x :: l when n > 0 -> aux (x :: res) l (n - 1) | _ -> res
  in
  List.rev (aux [] l n)

let rec drop l n = match l with _ :: l when n > 0 -> drop l (n - 1) | _ -> l
let sublist l pos len = take (drop l pos) len
