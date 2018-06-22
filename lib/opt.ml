let get ~__LOC__:l = function
  | Some x -> x
  | None -> failwith ("Opt.get :" ^ l)

let iter fn x = match x with Some x -> fn x | None -> ()

let map fn x = match x with Some x -> Some (fn x) | None -> None

let default x = function Some x -> x | _ -> x
