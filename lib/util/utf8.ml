(* TODO: replace with Unidecode.nbc
   when version constraint [= 0.2.0] will be removed *)
let nbc c =
  if Char.code c < 0x80 then 1
  else if Char.code c < 0xC0 then invalid_arg "nbc"
  else if Char.code c < 0xE0 then 2
  else if Char.code c < 0xF0 then 3
  else if Char.code c < 0xF8 then 4
  else if Char.code c < 0xFC then 5
  else if Char.code c < 0xFE then 6
  else invalid_arg "nbc"

let next s i = i + nbc s.[i]

let get s i =
  let rec loop i k = if k = 0 then i else loop (next s i) (pred k) in
  loop 0 i

let length s =
  let rec loop i len =
    if i < String.length s then loop (next s i) (succ len) else len
  in
  loop 0 0

let sub ?pad str start len =
  let strlen = String.length str in
  let n, i =
    let rec loop n i =
      if n = len || strlen <= i then (n, i) else loop (n + 1) (next str i)
    in
    loop 0 start
  in
  if n = len then String.sub str start (i - start)
  else
    match pad with
    | None -> raise (Invalid_argument "str_sub")
    | Some pad ->
        let bytes = Bytes.make (i - start + len - n) pad in
        Bytes.blit
          (Bytes.unsafe_of_string str)
          start bytes 0 (String.length str);
        Bytes.unsafe_to_string bytes

(**/**)

(* cmap_utf_8 code code comes from
   http://erratique.ch/software/uucp/doc/Uucp.Case.html *)
let cmap_utf_8 cmap s =
  let b = Buffer.create (String.length s * 2) in
  let add_map _ _ u =
    let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
    match cmap u with
    | `Self -> Uutf.Buffer.add_utf_8 b u
    | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
  in
  Uutf.String.fold_utf_8 add_map () s;
  Buffer.contents b

(**/**)

let lowercase s = cmap_utf_8 Uucp.Case.Map.to_lower s
let uppercase s = cmap_utf_8 Uucp.Case.Map.to_upper s

let skip_html_tags s =
  let rec loop i s =
    if i = String.length s then i
    else
      match s.[i] with
      | ' ' -> loop (i + 1) s
      | '<' -> (
          match String.index_from s i '>' with
          | exception Not_found ->
              Printf.eprintf "WARNING: badly formed string %s\n" s;
              0
          | j -> loop (j + 1) s)
      | _ -> i
  in
  loop 0 s

let capitalize_fst s =
  let i = skip_html_tags s in
  let head = String.sub s 0 i in
  let tail = String.sub s i (String.length s - i) in
  let first = ref true in
  let cmap u =
    if !first then (
      first := false;
      Uucp.Case.Map.to_upper u)
    else `Self
  in
  let tail = cmap_utf_8 cmap tail in
  head ^ tail

let capitalize s =
  let first = ref true in
  let cmap u =
    if !first then (
      first := false;
      Uucp.Case.Map.to_upper u)
    else Uucp.Case.Map.to_lower u
  in
  cmap_utf_8 cmap s

module C = struct
  type t = Str of string | Chr of char | Empty

  let unaccent trimmed s i0 len =
    let rec loop i =
      if i < len then
        match
          match Char.code @@ String.unsafe_get s i with
          (* A..Z *)
          | ( 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49
            | 0x4A | 0x4B | 0x4C | 0x4D | 0x4E | 0x4F | 0x50 | 0x51 | 0x52
            | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 | 0x58 | 0x59 | 0x5A ) as c ->
              (Chr (Char.unsafe_chr @@ (c + 32)), i, succ i)
          (* a..z *)
          | ( 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67 | 0x68 | 0x69
            | 0x6A | 0x6B | 0x6C | 0x6D | 0x6E | 0x6F | 0x70 | 0x71 | 0x72
            | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79 | 0x7A
            (* 0..9 *)
            | 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 | 0x36 | 0x37 | 0x38
            | 0x39 ) as c ->
              (Chr (Char.unsafe_chr c), i, succ i)
          (* '-' | ' ' | '\'' *)
          | (0x2D | 0x20 | 0x27) as c ->
              ((if trimmed then Chr (Char.unsafe_chr c) else Empty), i, succ i)
          | _ ->
              Unidecode.decode
                (fun n -> function
                  | "" -> (Empty, i, n)
                  | s -> (Str (String.lowercase_ascii s), i, n))
                (fun n c ->
                  match Char.lowercase_ascii c with
                  | ('a' .. 'z' | '0' .. '9') as c -> (Chr c, i, n)
                  | ('-' | ' ' | '\'') as c ->
                      ((if trimmed then Chr c else Empty), i, n)
                  | _ -> (Empty, i, n))
                (fun n -> (Empty, i, n))
                s i len
        with
        | Empty, _, n -> loop n
        | x, i, n -> (x, i, n)
      else (Empty, i0, len)
    in
    loop i0

  (* See BatUTF8.look source (from batteries) *)
  let cp s i =
    Uchar.of_int
    @@
    let n = Char.code (String.unsafe_get s i) in
    if n < 0x80 then n
    else if n <= 0xdf then
      ((n - 0xc0) lsl 6) lor (0x7f land Char.code (String.unsafe_get s (i + 1)))
    else if n <= 0xef then
      let n' = n - 0xe0 in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = (n' lsl 6) lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      (n' lsl 6) lor (0x7f land m)
    else
      let n' = n - 0xf0 in
      let m = Char.code (String.unsafe_get s (i + 1)) in
      let n' = (n' lsl 6) lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 2)) in
      let n' = (n' lsl 6) lor (0x7f land m) in
      let m = Char.code (String.unsafe_get s (i + 3)) in
      (n' lsl 6) lor (0x7f land m)

  (* compare bytes (UTF-8 charachter) delimited by intevals [i1,j1] and [i2,j2] *)
  let cmp_substring s1 i1 j1 s2 i2 j2 =
    let l1 = j1 - i1 in
    let l2 = j2 - i2 in
    if l1 = 1 && l2 = 1 then
      (* Optimize ASCII characters comparison *)
      Char.compare
        (Char.lowercase_ascii @@ String.get s1 i1)
        (Char.lowercase_ascii @@ String.get s2 i2)
    else
      let c1 = cp s1 i1 in
      let c2 = cp s2 i2 in
      let c1 =
        match Uucp.Case.Fold.fold c1 with `Self -> [ c1 ] | `Uchars us -> us
      in
      let c2 =
        match Uucp.Case.Fold.fold c2 with `Self -> [ c2 ] | `Uchars us -> us
      in
      Stdlib.compare c1 c2

  (* See [Utf8.compare] *)
  let compare n1 n2 =
    let trimmed1 = ref false in
    let trimmed2 = ref false in
    let rec loop i1 i2 =
      if i1 >= String.length n1 && i2 >= String.length n2 then i1 - i2
      else if i1 >= String.length n1 then -1
      else if i2 >= String.length n2 then 1
      else
        let c1, i1, ii1 = unaccent !trimmed1 n1 i1 (String.length n1) in
        let c2, i2, ii2 = unaccent !trimmed2 n2 i2 (String.length n2) in
        let () = trimmed1 := true in
        let () = trimmed2 := true in
        let cmp_aux = function
          | 0 -> (
              match cmp_substring n1 i1 ii1 n2 i2 ii2 with
              | 0 -> loop ii1 ii2
              | x -> x)
          | x -> x
        in
        match (c1, c2) with
        | Str s1, Str s2 -> cmp_aux (String.compare s1 s2)
        | Chr c1, Chr c2 -> cmp_aux (Char.compare c1 c2)
        | Empty, Empty -> cmp_aux 0
        | (Str _ | Chr _), Empty -> -1
        | Empty, (Str _ | Chr _) -> 1
        | Str s1, Chr c2 -> (
            match Char.compare (String.unsafe_get s1 0) c2 with
            | 0 -> 1
            | x -> x)
        | Chr c1, Str s2 -> (
            match Char.compare c1 (String.unsafe_get s2 0) with
            | 0 -> -1
            | x -> x)
    in
    if n1 = n2 then 0 else loop 0 0
end

let compare = C.compare
