let hexa_string s =
  let s' = Bytes.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    Bytes.set s' (2 * i) "0123456789ABCDEF".[Char.code s.[i] / 16];
    Bytes.set s' ((2 * i) + 1) "0123456789ABCDEF".[Char.code s.[i] mod 16]
  done;
  Bytes.unsafe_to_string s'

let gen_only_printable or_nl s =
  let s' =
    let conv_char i =
      if Char.code s.[i] > 127 then s.[i]
      else
        match s.[i] with
        | ' ' .. '~' | '\160' .. '\255' -> s.[i]
        | '\n' -> if or_nl then '\n' else ' '
        | _ -> ' '
    in
    String.init (String.length s) conv_char
  in
  String.trim s'

let only_printable_or_nl = gen_only_printable true
let only_printable = gen_only_printable false

let nb_char_occ c s =
  let cnt = ref 0 in
  String.iter (fun x -> if x = c then incr cnt) s;
  !cnt

let split_on_char separator str =
  str
  |> String.split_on_char separator
  |> List.map String.trim
  |> List.filter (( <> ) "")

let strip_all_trailing_spaces s =
  let b = Buffer.create (String.length s) in
  let len =
    let rec loop i =
      if i < 0 then 0
      else
        match s.[i] with ' ' | '\t' | '\r' | '\n' -> loop (i - 1) | _ -> i + 1
    in
    loop (String.length s - 1)
  in
  let rec loop i =
    if i = len then Buffer.contents b
    else
      match s.[i] with
      | '\r' -> loop (i + 1)
      | ' ' | '\t' ->
          let rec loop0 j =
            if j = len then Buffer.contents b
            else
              match s.[j] with
              | ' ' | '\t' | '\r' -> loop0 (j + 1)
              | '\n' -> loop j
              | _ ->
                  Buffer.add_char b s.[i];
                  loop (i + 1)
          in
          loop0 (i + 1)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0

module Set = Set.Make (String)
module Map = Map.Make (String)

let tr c1 c2 s =
  match String.rindex_opt s c1 with
  | Some _ ->
      String.init (String.length s) (fun i ->
          let c = String.unsafe_get s i in
          if c = c1 then c2 else c)
  | None -> s

let unsafe_tr c1 c2 s =
  match String.rindex_opt s c1 with
  | Some _ ->
      let bytes = Bytes.unsafe_of_string s in
      for i = 0 to Bytes.length bytes - 1 do
        if Bytes.unsafe_get bytes i = c1 then Bytes.unsafe_set bytes i c2
      done;
      Bytes.unsafe_to_string bytes
  | None -> s

let contains str sub =
  let strlen = String.length str in
  let sublen = String.length sub in
  let rec aux i1 i2 =
    if i1 = sublen then true
    else if i2 = strlen then false
    else if String.unsafe_get str i2 = String.unsafe_get sub i1 then
      aux (i1 + 1) (i2 + 1)
    else false
  in
  let rec loop i =
    if i + sublen <= strlen then aux 0 i || loop (i + 1) else false
  in
  loop 0

let contains_word ~word s =
  let aux () =
    let word = Str.regexp ("\\b" ^ Str.quote word ^ "\\b") in
    try
      ignore (Str.search_forward word s 0);
      true
    with Not_found -> false
  in
  (not (String.equal (String.trim word) String.empty)) && aux ()

let digest s = Digest.string s |> Digest.to_hex

let trim_trailing_spaces s =
  let len = String.length s in
  let len' =
    let rec loop i =
      if i = -1 then 0
      else
        match String.unsafe_get s i with
        | ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1
    in
    loop (len - 1)
  in
  if len' = 0 then "" else if len' = len then s else String.sub s 0 len'
