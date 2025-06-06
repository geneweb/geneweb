type 'a t = { run : Input.t -> ('a * Input.t) option } [@@unboxed]

let[@inline always] run t st = t.run st
let ret a = { run = (fun st -> Some (a, st)) }
let fail = { run = (fun _ -> None) }

let bind t f =
  let run st =
    match t.run st with Some (a, st') -> (f a).run st' | None -> None
  in
  { run }

let choice l =
  let run st =
    let rec loop l =
      match l with
      | [] -> None
      | t :: tx -> ( match t.run st with Some _ as r -> r | None -> loop tx)
    in
    loop l
  in
  { run }

let case l =
  let run st =
    let rec loop l =
      match l with
      | [] -> None
      | (t1, t2) :: tx -> (
          match t1.run st with Some _ -> t2.run st | None -> loop tx)
    in
    loop l
  in
  { run }

let ( let* ) = bind

let ( <|> ) t1 t2 =
  let run st = match t1.run st with Some _ as r -> r | None -> t2.run st in
  { run }

let ( *> ) t1 t2 =
  let* _ = t1 in
  t2

let ( <* ) t1 t2 =
  let* r = t1 in
  let* _ = t2 in
  ret r

let many t =
  let rec loop acc st =
    match t.run st with
    | Some (a, st') -> loop (a :: acc) st'
    | None -> Some (List.rev acc, st)
  in
  { run = loop [] }

let many1 t =
  let rec loop acc st =
    match t.run st with
    | Some (a, st') -> loop (a :: acc) st'
    | None -> Some (List.rev acc, st)
  in
  let* t = t in
  { run = loop [ t ] }

let count t =
  let rec loop acc st =
    match t.run st with
    | Some (_, st') -> loop (acc + 1) st'
    | None -> Some (acc, st)
  in
  { run = loop 0 }

let fix f x =
  let rec run y st = (f y (fun z -> { run = run z })).run st in
  { run = run x }

let until a =
  let run st =
    let buf = Buffer.create 17 in
    let rec loop st =
      match a.run st with
      | Some _ -> Some (Buffer.contents buf, st)
      | None -> (
          match Input.peak st with
          | Some c ->
              Buffer.add_char buf c;
              loop (Input.next st)
          | None -> Some (Buffer.contents buf, st))
    in
    loop st
  in
  { run }

let skip t =
  let run st =
    let rec loop st =
      match t.run st with Some (_, st') -> loop st' | None -> Some ((), st)
    in
    loop st
  in
  { run }

let option a =
  let run st =
    match a.run st with
    | Some (r, st) -> Some (Some r, st)
    | None -> Some (None, st)
  in
  { run }

let char c =
  let run st =
    match Input.peak st with
    | Some c' when Char.equal c c' -> Some (c, Input.next st)
    | Some _ | None -> None
  in
  { run }

let string s =
  let len = String.length s in
  let rec loop i st =
    if i = len then Some (s, st)
    else
      match Input.peak st with
      | Some c when Char.equal s.[i] c -> loop (i + 1) (Input.next st)
      | Some _ | None -> None
  in
  { run = loop 0 }

let digit =
  let run st =
    match Input.peak st with
    | Some c ->
        let cd = Char.code c in
        if 48 <= cd && cd <= 57 then Some (cd - 48, Input.next st) else None
    | None -> None
  in
  { run }

let int =
  let rec loop r st =
    match digit.run st with
    | Some (d, st') -> loop ((r * 10) + d) st'
    | None -> Some (r, st)
  in
  let* r = digit in
  { run = loop r }

let with_loc t =
  let run st =
    let start = Input.offset st in
    match t.run st with
    | Some (r, st') ->
        let stop = Input.offset st' in
        Some ((r, Loc.mk start stop), st')
    | None -> None
  in
  { run }
