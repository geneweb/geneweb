module Loc = Geneweb_loc

type 'a t = { run : Input.t -> ('a, unit -> string) result * Input.t }
[@@unboxed]

let[@inline] run t st = t.run st
let[@inline] ret a = { run = (fun st -> (Ok a, st)) }

let fail fmt =
  Fmt.kstr (fun s -> { run = (fun st -> (Error (fun () -> s), st)) }) fmt

let bind t f =
  let run st =
    let r, st' = t.run st in
    match r with Ok a -> (f a).run st' | Error _ as r -> (r, st')
  in
  { run }

let choice l =
  let run st =
    let rec loop l =
      match l with
      | [] -> (Error (fun () -> "non-exhaustive choice combinator"), st)
      | t :: tx -> (
          let r, st' = t.run st in
          match r with Ok _ as r -> (r, st') | Error _ -> loop tx)
    in
    loop l
  in
  { run }

let case l t =
  let run st =
    let rec loop l =
      match l with
      | [] -> t.run st
      | (t1, t2) :: tx -> (
          let r, _st' = t1.run st in
          match r with Ok _ -> t2.run st | Error _ -> loop tx)
    in
    loop l
  in
  { run }

let case_with_recover ~recover l t =
  let run st =
    let rec loop l =
      match l with
      | [] -> t.run st
      | (t1, t2) :: tx -> (
          let start = Input.offset st in
          let r, st' = t1.run st in
          match r with
          | Ok _ -> (
              let r, st'' = t2.run st in
              match r with
              | Ok _ as r -> (r, st'')
              | Error _ ->
                  let stop = Input.offset st' in
                  let loc = Loc.mk (Input.to_source st) start stop in
                  (Ok (recover loc), st'))
          | Error _ -> loop tx)
    in
    loop l
  in
  { run }

let case ?recover l t =
  match recover with
  | Some recover -> case_with_recover ~recover l t
  | None -> case l t

let case2 l =
  let run st =
    let rec loop l =
      match l with
      | [] -> (Error (fun () -> "non-exhaustive choice combinator"), st)
      | (t1, t2) :: tx -> (
          let r, _st' = t1.run st in
          match r with Ok _ -> t2.run st | Error _ -> loop tx)
    in
    loop l
  in
  { run }

let ( let* ) = bind

let ( <|> ) t1 t2 =
  let run st =
    let r, st' = t1.run st in
    match r with Ok _ -> (r, st') | Error _ -> t2.run st
  in
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
    let r, st' = t.run st in
    match r with
    | Ok a -> loop (a :: acc) st'
    | Error _ -> (Ok (List.rev acc), st)
  in
  { run = loop [] }

let many1 t =
  let* x = t in
  let* xs = many t in
  ret (x :: xs)

let count t =
  let rec loop acc st =
    let r, st' = t.run st in
    match r with Ok _ -> loop (acc + 1) st' | Error _ -> (Ok acc, st)
  in
  { run = loop 0 }

let until t =
  let run st =
    let buf = Buffer.create 17 in
    let rec loop st =
      let r, _st' = t.run st in
      match r with
      | Ok _ -> (Ok (Buffer.contents buf), st)
      | Error _ -> (
          match Input.peak st with
          | Some c ->
              Buffer.add_char buf c;
              loop (Input.next st)
          | None -> (Ok (Buffer.contents buf), st))
    in
    loop st
  in
  { run }

let skip t =
  let run st =
    let rec loop st =
      let r, st' = t.run st in
      match r with Ok _ -> loop st' | Error _ -> (Ok (), st)
    in
    loop st
  in
  { run }

let option t =
  let run st =
    let r, st' = t.run st in
    match r with Ok r -> (Ok (Some r), st') | Error _ -> (Ok None, st)
  in
  { run }

let char c =
  let run st =
    match Input.peak st with
    | Some c' ->
        let st' = Input.next st in
        if Char.equal c c' then (Ok c, st')
        else (Error (fun () -> Fmt.str "expected %C, got %C" c c'), st')
    | None -> (Error (fun () -> "expected input, reached end of file"), st)
  in
  { run }

let string s =
  let len = String.length s in
  let rec loop i st =
    if i = len then (Ok s, st)
    else
      match Input.peak st with
      | Some c ->
          let st' = Input.next st in
          if Char.equal s.[i] c then loop (i + 1) st'
          else (Error (fun () -> Fmt.str "expected %C, got %C" s.[i] c), st')
      | None -> (Error (fun () -> "expected input, reached end of file"), st)
  in
  { run = loop 0 }

let digit =
  let run st =
    match Input.peak st with
    | Some c ->
        let st' = Input.next st in
        let cd = Char.code c in
        if 48 <= cd && cd <= 57 then (Ok (cd - 48), st')
        else (Error (fun () -> Fmt.str "expected digit, got %C" c), st')
    | None -> (Error (fun () -> "expected digit, reached end of file"), st)
  in
  { run }

let int =
  let rec loop acc st =
    let r, st' = digit.run st in
    match r with Ok d -> loop ((acc * 10) + d) st' | Error _ -> (Ok acc, st)
  in
  let* acc = digit in
  { run = loop acc }

let located t =
  let run st =
    let start = Input.offset st in
    let x, st' = t.run st in
    match x with
    | Ok r ->
        let stop = Input.offset st' in
        let src = Input.to_source st' in
        (Ok (r, Loc.mk src start stop), st')
    | Error _ as r -> (r, st')
  in
  { run }
