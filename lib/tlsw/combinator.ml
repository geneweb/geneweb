type ('a, 'st) res = Ok of 'a * 'st | Fail of (unit -> string) * 'st
type 'a t = { run : Input.t -> ('a, Input.t) res } [@@unboxed]

let[@inline always] run t st = t.run st
let ret a = { run = (fun st -> Ok (a, st)) }

let fail fmt =
  Fmt.kstr (fun s -> { run = (fun st -> Fail ((fun () -> s), st)) }) fmt

let bind t f =
  let run st =
    match t.run st with Ok (a, st') -> (f a).run st' | Fail _ as r -> r
  in
  { run }

let choice l =
  let run st =
    let rec loop l =
      match l with
      | [] -> Fail ((fun () -> "non-exhaustive choice combinator"), st)
      | t :: tx -> ( match t.run st with Ok _ as r -> r | Fail _ -> loop tx)
    in
    loop l
  in
  { run }

let case l =
  let run st =
    let rec loop l =
      match l with
      | [] -> Fail ((fun () -> "non-exhaustive case combinator"), st)
      | (t1, t2) :: tx -> (
          match t1.run st with Ok _ -> t2.run st | Fail _ -> loop tx)
    in
    loop l
  in
  { run }

let ( let* ) = bind

let ( <|> ) t1 t2 =
  let run st = match t1.run st with Ok _ as r -> r | Fail _ -> t2.run st in
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
    | Ok (a, st') -> loop (a :: acc) st'
    | Fail _ -> Ok (List.rev acc, st)
  in
  { run = loop [] }

let many1 t =
  let* x = t in
  let* xs = many t in
  ret (x :: xs)

let count t =
  let rec loop acc st =
    match t.run st with
    | Ok (_, st') -> loop (acc + 1) st'
    | Fail _ -> Ok (acc, st)
  in
  { run = loop 0 }

let until a =
  let run st =
    let buf = Buffer.create 17 in
    let rec loop st =
      match a.run st with
      | Ok _ -> Ok (Buffer.contents buf, st)
      | Fail _ -> (
          match Input.peak st with
          | Some c ->
              Buffer.add_char buf c;
              loop (Input.next st)
          | None -> Ok (Buffer.contents buf, st))
    in
    loop st
  in
  { run }

let skip t =
  let run st =
    let rec loop st =
      match t.run st with Ok (_, st') -> loop st' | Fail _ -> Ok ((), st)
    in
    loop st
  in
  { run }

let option a =
  let run st =
    match a.run st with Ok (r, st) -> Ok (Some r, st) | Fail _ -> Ok (None, st)
  in
  { run }

let char c =
  let run st =
    match Input.peak st with
    | Some c' ->
        let st' = Input.next st in
        if Char.equal c c' then Ok (c, st')
        else Fail ((fun () -> Fmt.str "expected %C, got %C" c c'), st')
    | None -> Fail ((fun () -> "expected input, reached end of file"), st)
  in
  { run }

let string s =
  let len = String.length s in
  let rec loop i st =
    if i = len then Ok (s, st)
    else
      match Input.peak st with
      | Some c ->
          let st' = Input.next st in
          if Char.equal s.[i] c then loop (i + 1) st'
          else Fail ((fun () -> Fmt.str "expected %C, got %C" s.[i] c), st')
      | None -> Fail ((fun () -> "expected input, reached end of file"), st)
  in
  { run = loop 0 }

let digit =
  let run st =
    match Input.peak st with
    | Some c ->
        let st' = Input.next st in
        let cd = Char.code c in
        if 48 <= cd && cd <= 57 then Ok (cd - 48, st')
        else Fail ((fun () -> Fmt.str "expected digit, got %C" c), st')
    | None -> Fail ((fun () -> "expected digit, reached end of file"), st)
  in
  { run }

let int =
  let rec loop r st =
    match digit.run st with
    | Ok (d, st') -> loop ((r * 10) + d) st'
    | Fail _ -> Ok (r, st)
  in
  let* r = digit in
  { run = loop r }

let located t =
  let run st =
    let start = Input.offset st in
    match t.run st with
    | Ok (r, st') ->
        let stop = Input.offset st' in
        let src = Input.to_source st' in
        Ok ((r, Loc.mk src start stop), st')
    | Fail _ as r -> r
  in
  { run }
