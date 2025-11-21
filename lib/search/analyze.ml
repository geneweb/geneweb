module I = Index.Default

let is_separator c = c = ' ' || c = ',' || c = '-'

type 'a loc = { content : 'a; offset : int; len : int }

let flush_buf acc buf offset i =
  match buf with
  | [] | [ _ ] | [ _; _ ] ->
      (* Ignore short tokens. *)
      acc
  | _ ->
      let content = List.rev buf |> List.to_seq |> String.of_seq in
      { content; offset; len = i - offset } :: acc

let tokenize s =
  let len = String.length s in
  let rec loop acc buf offset i =
    if i = len then flush_buf acc buf offset i
    else
      let c = String.get s i in
      if is_separator c then
        let acc = flush_buf acc buf offset i in
        loop acc [] (i + 1) (i + 1)
      else loop acc (c :: buf) offset (i + 1)
  in
  loop [] [] 0 0 |> List.rev

let normalize = String.lowercase_ascii

let preprocess s =
  List.map
    (fun t ->
      let content = normalize t.content in
      { t with content })
    (tokenize s)
