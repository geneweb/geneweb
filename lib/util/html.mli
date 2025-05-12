val map :
  ?context:[< `Document | `Fragment of string ] ->
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  (Markup.signal -> Markup.signal) ->
  string ->
  string

val is_plain_text : string -> bool
val text_content : string -> string
