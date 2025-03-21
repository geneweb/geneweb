let escape_aux count blit str =
  let strlen = String.length str in
  let rec loop acc i =
    if i < strlen then loop (acc + count (String.unsafe_get str i)) (i + 1)
    else if acc = strlen then str
    else
      let buf = Bytes.create acc in
      let rec loop istr ibuf =
        if istr = strlen then Bytes.unsafe_to_string buf
        else blit buf ibuf istr loop (String.unsafe_get str istr)
      in
      loop 0 0
  in
  loop 0 0

let escape s =
  escape_aux
    (function '&' | '"' | '\'' | '<' | '>' -> 5 (* "&#xx;" *) | _ -> 1)
    (fun buf ibuf istr loop -> function
      | '&' ->
          Bytes.blit_string "&#38;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '"' ->
          Bytes.blit_string "&#34;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '\'' ->
          Bytes.blit_string "&#39;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '<' ->
          Bytes.blit_string "&#60;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '>' ->
          Bytes.blit_string "&#62;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | c ->
          Bytes.unsafe_set buf ibuf c;
          loop (istr + 1) (ibuf + 1))
    s

(** [escape_attribute str] only escapes double quote and ampersand.
    Since we will return normalized HTML, ['"'] should be the only
    dangerous character here. *)
let escape_attribute =
  escape_aux
    (function '&' | '"' -> 5 (* "&#xx;" *) | _ -> 1)
    (fun buf ibuf istr loop -> function
      | '&' ->
          Bytes.blit_string "&#38;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | '"' ->
          Bytes.blit_string "&#34;" 0 buf ibuf 5;
          loop (istr + 1) (ibuf + 5)
      | c ->
          Bytes.unsafe_set buf ibuf c;
          loop (istr + 1) (ibuf + 1))

let parse_as_body_content s =
  Markup.string s
  |> Markup.parse_html ~context:(`Fragment "body")
  |> Markup.signals

let pretty_print elements =
  elements
  |> Markup.write_html ~escape_text:escape ~escape_attribute
  |> Markup.to_string

let map f s = s |> parse_as_body_content |> Markup.map f |> pretty_print

let is_plain_text s =
  s |> parse_as_body_content
  |> Markup.fold
       (fun is_plain_text_so_far element ->
         is_plain_text_so_far && Ext_markup.is_text element)
       true

let text_content s =
  s |> parse_as_body_content |> Markup.filter Ext_markup.is_text |> pretty_print
