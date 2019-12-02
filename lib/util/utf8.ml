(* TODO: replace with Unidecode.nbc
   when version constraint [= 0.2.0] will be removed *)
(** Return the number of bytes composing the UTF8 character starting with [c] *)
let nbc c =
  if Char.code c < 0x80 then 1
  else if Char.code c < 0xC0 then invalid_arg "nbc"
  else if Char.code c < 0xE0 then 2
  else if Char.code c < 0xF0 then 3
  else if Char.code c < 0xF8 then 4
  else if Char.code c < 0xFC then 5
  else if Char.code c < 0xFE then 6
  else invalid_arg "nbc"

(** [Utf8.next s i] returns the index of the character comming after
    the one which starts at [i].
*)
let next s i =
  i + nbc s.[i]

(** [Utf8.nth s n] returns the index where the [n]-th character
    starts in string [s].
*)
let get s i =
  let rec loop i k =
    if k = 0 then i
    else loop (next s i) (pred k)
  in loop 0 i

(** Return the length (number of characters, not bytes)
    of the given string.
*)
let length s =
  let rec loop i len =
    if i < String.length s
    then loop (next s i) (succ len)
    else len
  in loop 0 0

(** [sub ?pad s start len]
    Return a fresh UTF8-friendly substring of [len] characters, padded if needed.
    Be careful [start] is the index of the byte where to start in [s],
    not the [start-th] UTF8-character.
*)
let sub ?pad str start len =
  let strlen = String.length str in
  let n, i =
    let rec loop n i =
      if n = len || strlen <= i then (n, i)
      else loop (n + 1) (next str i)
    in
    loop 0 start
  in
  if n = len then String.sub str start (i - start)
  else match pad with
    | None -> raise (Invalid_argument "str_sub")
    | Some pad ->
      let bytes = Bytes.make (i - start + len - n) pad in
      Bytes.blit (Bytes.unsafe_of_string str) start bytes 0 (String.length str) ;
      Bytes.unsafe_to_string bytes
