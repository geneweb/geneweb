let get ~__LOC__:l = function
  | Some x -> x
  | None -> failwith ("Opt.get :" ^ l)

let iter fn x = match x with Some x -> fn x | None -> ()

let map fn x = match x with Some x -> Some (fn x) | None -> None

let map_default default fn = function Some x -> fn x | None -> default

let default x = function Some x -> x | _ -> x

let string_default def = function "" -> def () | x -> x

let string_map_default default fn = function "" -> default () | x -> fn x

let of_string = function "" -> None | s -> Some s

let to_string = function None -> "" | Some s -> s
