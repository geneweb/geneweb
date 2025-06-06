type t = { input : string; offset : int }

let of_string s = { input = s; offset = 0 }
let[@inline always] eof { input; offset } = offset >= String.length input
let peak ({ input; offset } as t) = if eof t then None else Some input.[offset]
let[@inline] offset { offset; _ } = offset

let next ({ offset; _ } as t) =
  let offset = if eof t then offset else offset + 1 in
  { t with offset }
