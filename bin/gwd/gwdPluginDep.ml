type 'a result = Sorted of 'a list | Cycle of 'a list
type mark = Unmarked | Visiting | Marked

let topological_sort (type a) (t : (a * a list) list) : a result =
  let exception Cycle of a list in
  let marks : (a, mark) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun (n, _) -> Hashtbl.replace marks n Unmarked) t;
  let outgoings : (a, a list) Hashtbl.t = Hashtbl.of_seq @@ List.to_seq t in
  let rec visit path acc n =
    match Hashtbl.find marks n with
    | Marked -> acc
    | Visiting -> raise_notrace (Cycle (n :: path))
    | Unmarked ->
        Hashtbl.replace marks n Visiting;
        let outgoings = Hashtbl.find outgoings n in
        let acc = List.fold_left (visit (n :: path)) acc outgoings in
        Hashtbl.replace marks n Marked;
        n :: acc
  in
  match List.fold_left (fun acc (n, _) -> visit [] acc n) [] t with
  | l -> Sorted (List.rev l)
  | exception Cycle l -> Cycle (List.rev l)
