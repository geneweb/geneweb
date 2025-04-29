let parse_as_body_content s =
  Markup.string s
  |> Markup.parse_html ~context:(`Fragment "body")
  |> Markup.signals

let pretty_print ?escape_attribute ?escape_text elements =
  elements
  |> Markup.write_html ?escape_text ?escape_attribute
  |> Markup.to_string

let map ?escape_attribute ?escape_text f s =
  s |> parse_as_body_content |> Markup.map f
  |> pretty_print ?escape_attribute ?escape_text

let text_content s =
  s |> parse_as_body_content
  |> Markup.filter Ext_markup.is_text
  |> pretty_print ~escape_attribute:Fun.id ~escape_text:Fun.id

let is_plain_text s =
  s |> parse_as_body_content
  |> Markup.fold
       (fun is_plain_text_so_far element ->
         is_plain_text_so_far && Ext_markup.is_text element)
       true
