module Lexicon_parser = Gwxjg_lexicon_parser
open Jingoo

let fast_concat = function
  | [] -> ""
  | [ s ] -> s
  | l ->
      let b =
        Bytes.create (List.fold_left (fun acc s -> String.length s + acc) 0 l)
      in
      ignore
      @@ List.fold_left
           (fun pos s ->
             let len = String.length s in
             Bytes.unsafe_blit (Bytes.unsafe_of_string s) 0 b pos len;
             pos + len)
           0 l;
      Bytes.unsafe_to_string b

let import_trad ht keyword line =
  let open Jg_types in
  let open Jg_runtime in
  Hashtbl.add ht keyword @@ fun ?(kwargs = []) i ->
  let i = if i < 0 || i >= Array.length line then 0 else i in
  let arg s = List.assoc s kwargs in
  Tstr
    (fast_concat
    @@
    let a = Array.unsafe_get line i in
    let rec loop acc i =
      if i < 0 then acc
      else
        match Array.unsafe_get a i with
        | Lexicon_parser.Str s -> loop (s :: acc) (i - 1)
        | Arg n -> loop (string_of_tvalue (arg n) :: acc) (i - 1)
        | Declension (c, n) ->
            loop ((arg n |> string_of_tvalue |> Mutil.decline c) :: acc) (i - 1)
        | Elision (s1, s2) ->
            let x =
              try unbox_string @@ arg "elision" with Not_found -> List.hd acc
            in
            if
              x <> ""
              && Unidecode.decode
                   (fun _ _ -> false)
                   (fun _ -> function
                     | 'A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o' | 'u'
                       ->
                         true
                     | _ -> false)
                   (fun _ -> false)
                   x 0 (String.length x)
            then loop (s2 :: acc) (i - 1)
            else loop (s1 :: acc) (i - 1)
    in
    loop [] (Array.length a - 1))

let default_lang = "en"

let find_lang lang tr =
  try List.assoc lang tr with Not_found -> List.assoc default_lang tr

let make_lang lexicon len lang =
  let ht = Hashtbl.create len in
  List.iter (fun (key, tr) -> import_trad ht key (find_lang lang tr)) lexicon;
  ht
