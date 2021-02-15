type t = Jsonaf.t

let member m = function `Object o -> (try List.assoc m o with Not_found -> `Null) | _ -> assert false

let to_string : t -> string = function
  | `String s -> s
  | `Null -> ""
  | _ -> assert false

let to_int : t -> int = function
  | `Number n -> int_of_string n
  | `Null -> 0
  | _ -> assert false

let to_assoc : t -> (string * t) list = function
  | `Object list -> list
  | _ -> assert false

let to_bool : t -> bool = function
  | `True -> true
  | `False -> false
  | _ -> assert false

let to_opt fn = function `Null -> None | x -> Some (fn x)

let to_list = function `Array l -> l | `Null -> [] | _ -> assert false

let to_list_of fn t = List.map fn @@ to_list t

let get_string name js =
  to_string (member name js)

let get_int name js =
  to_int (member name js)

let get_list name fn js =
  match member name js with
  | `Array l -> List.map fn l
  | `Null -> []
  | _ -> assert false

let get_bool name js =
  to_bool (member name js)

let show = Jsonaf.to_string

let int i = `Number (string_of_int i)

let string s = `String s

let list fn l = `Array (List.map fn l)

let assoc o = `Object o
