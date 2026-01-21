type t = Utf8 | Utf16_le | Utf16_be | Utf32_le | Utf32_be | None

let check ic =
  let pos = pos_in ic in
  let result =
    try
      let b0 = input_byte ic in
      let b1 = input_byte ic in
      if b0 = 0xEF && b1 = 0xBB then if input_byte ic = 0xBF then Utf8 else None
      else if b0 = 0xFE && b1 = 0xFF then Utf16_be
      else if b0 = 0xFF && b1 = 0xFE then
        let b2 = input_byte ic in
        let b3 = input_byte ic in
        if b2 = 0x00 && b3 = 0x00 then Utf32_le else Utf16_le
      else if b0 = 0x00 && b1 = 0x00 then
        let b2 = input_byte ic in
        let b3 = input_byte ic in
        if b2 = 0xFE && b3 = 0xFF then Utf32_be else None
      else None
    with End_of_file -> None
  in
  (match result with Utf8 -> () | _ -> seek_in ic pos);
  result

let to_string = function
  | Utf8 -> "UTF-8"
  | Utf16_le -> "UTF-16 LE"
  | Utf16_be -> "UTF-16 BE"
  | Utf32_le -> "UTF-32 LE"
  | Utf32_be -> "UTF-32 BE"
  | None -> ""

let is_unsupported = function
  | Utf16_le | Utf16_be | Utf32_le | Utf32_be -> true
  | Utf8 | None -> false
