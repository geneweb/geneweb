type mode = Generic | Text | Font

external compress_raw : int -> int -> string -> string = "ml_brotli_compress"

let int_of_mode = function Generic -> 0 | Text -> 1 | Font -> 2

let compress ?(quality = 4) ?(mode = Text) input =
  compress_raw quality (int_of_mode mode) input
