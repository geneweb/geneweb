(* $Id: ansel.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

let iso_8859_1_unknown = '\129'
let ansel_unknown = 129

let of_utf_8 s =
  let buf = Buffer.create (String.length s * 2) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      let c = Char.code s.[i] in
      let uchar, next =
        if c < 0x80 then (c, i + 1)
        else if c < 0xC0 then (0xFFFD, i + 1)
        else if c < 0xE0 && i + 1 < len then
          let c1 = Char.code s.[i + 1] in
          (((c land 0x1F) lsl 6) lor (c1 land 0x3F), i + 2)
        else if c < 0xF0 && i + 2 < len then
          let c1 = Char.code s.[i + 1] in
          let c2 = Char.code s.[i + 2] in
          ( ((c land 0x0F) lsl 12) lor ((c1 land 0x3F) lsl 6) lor (c2 land 0x3F),
            i + 3 )
        else (0xFFFD, i + 1)
      in
      (match uchar with
      | u when u < 0x80 -> Buffer.add_char buf (Char.chr u)
      | 0x00A1 -> Buffer.add_char buf '\xC6'
      | 0x00A3 -> Buffer.add_char buf '\xB9'
      | 0x00A9 -> Buffer.add_char buf '\xC3'
      | 0x00AE -> Buffer.add_char buf '\xAA'
      | 0x00B1 -> Buffer.add_char buf '\xAB'
      | 0x00B7 -> Buffer.add_char buf '\xA8'
      | 0x00BF -> Buffer.add_char buf '\xC5'
      | 0x00C6 -> Buffer.add_char buf '\xA5'
      | 0x00D0 -> Buffer.add_char buf '\xA3'
      | 0x00D8 -> Buffer.add_char buf '\xA2'
      | 0x00DE -> Buffer.add_char buf '\xA4'
      | 0x00DF -> Buffer.add_char buf '\xCF'
      | 0x00E6 -> Buffer.add_char buf '\xB5'
      | 0x00F0 -> Buffer.add_char buf '\xBA'
      | 0x00F8 -> Buffer.add_char buf '\xB2'
      | 0x00FE -> Buffer.add_char buf '\xB4'
      | 0x0131 -> Buffer.add_char buf '\xB8'
      | 0x0141 -> Buffer.add_char buf '\xA1'
      | 0x0142 -> Buffer.add_char buf '\xB1'
      | 0x0110 -> Buffer.add_char buf '\xA3'
      | 0x0111 -> Buffer.add_char buf '\xB3'
      | 0x02BB -> Buffer.add_char buf '\xB0'
      | 0x02BC -> Buffer.add_char buf '\xAE'
      | 0x0152 -> Buffer.add_char buf '\xA6'
      | 0x0153 -> Buffer.add_char buf '\xB6'
      | 0x00C0 -> Buffer.add_string buf "\xE1\x41"
      | 0x00C1 -> Buffer.add_string buf "\xE2\x41"
      | 0x00C2 -> Buffer.add_string buf "\xE3\x41"
      | 0x00C3 -> Buffer.add_string buf "\xE4\x41"
      | 0x00C4 -> Buffer.add_string buf "\xE8\x41"
      | 0x00C5 -> Buffer.add_string buf "\xEA\x41"
      | 0x00C7 -> Buffer.add_string buf "\xF0\x43"
      | 0x00C8 -> Buffer.add_string buf "\xE1\x45"
      | 0x00C9 -> Buffer.add_string buf "\xE2\x45"
      | 0x00CA -> Buffer.add_string buf "\xE3\x45"
      | 0x00CB -> Buffer.add_string buf "\xE8\x45"
      | 0x00CC -> Buffer.add_string buf "\xE1\x49"
      | 0x00CD -> Buffer.add_string buf "\xE2\x49"
      | 0x00CE -> Buffer.add_string buf "\xE3\x49"
      | 0x00CF -> Buffer.add_string buf "\xE8\x49"
      | 0x00D1 -> Buffer.add_string buf "\xE4\x4E"
      | 0x00D2 -> Buffer.add_string buf "\xE1\x4F"
      | 0x00D3 -> Buffer.add_string buf "\xE2\x4F"
      | 0x00D4 -> Buffer.add_string buf "\xE3\x4F"
      | 0x00D5 -> Buffer.add_string buf "\xE4\x4F"
      | 0x00D6 -> Buffer.add_string buf "\xE8\x4F"
      | 0x00D9 -> Buffer.add_string buf "\xE1\x55"
      | 0x00DA -> Buffer.add_string buf "\xE2\x55"
      | 0x00DB -> Buffer.add_string buf "\xE3\x55"
      | 0x00DC -> Buffer.add_string buf "\xE8\x55"
      | 0x00DD -> Buffer.add_string buf "\xE2\x59"
      | 0x00E0 -> Buffer.add_string buf "\xE1\x61"
      | 0x00E1 -> Buffer.add_string buf "\xE2\x61"
      | 0x00E2 -> Buffer.add_string buf "\xE3\x61"
      | 0x00E3 -> Buffer.add_string buf "\xE4\x61"
      | 0x00E4 -> Buffer.add_string buf "\xE8\x61"
      | 0x00E5 -> Buffer.add_string buf "\xEA\x61"
      | 0x00E7 -> Buffer.add_string buf "\xF0\x63"
      | 0x00E8 -> Buffer.add_string buf "\xE1\x65"
      | 0x00E9 -> Buffer.add_string buf "\xE2\x65"
      | 0x00EA -> Buffer.add_string buf "\xE3\x65"
      | 0x00EB -> Buffer.add_string buf "\xE8\x65"
      | 0x00EC -> Buffer.add_string buf "\xE1\x69"
      | 0x00ED -> Buffer.add_string buf "\xE2\x69"
      | 0x00EE -> Buffer.add_string buf "\xE3\x69"
      | 0x00EF -> Buffer.add_string buf "\xE8\x69"
      | 0x00F1 -> Buffer.add_string buf "\xE4\x6E"
      | 0x00F2 -> Buffer.add_string buf "\xE1\x6F"
      | 0x00F3 -> Buffer.add_string buf "\xE2\x6F"
      | 0x00F4 -> Buffer.add_string buf "\xE3\x6F"
      | 0x00F5 -> Buffer.add_string buf "\xE4\x6F"
      | 0x00F6 -> Buffer.add_string buf "\xE8\x6F"
      | 0x00F9 -> Buffer.add_string buf "\xE1\x75"
      | 0x00FA -> Buffer.add_string buf "\xE2\x75"
      | 0x00FB -> Buffer.add_string buf "\xE3\x75"
      | 0x00FC -> Buffer.add_string buf "\xE8\x75"
      | 0x00FD -> Buffer.add_string buf "\xE2\x79"
      | 0x00FF -> Buffer.add_string buf "\xE8\x79"
      | 0x0100 -> Buffer.add_string buf "\xE5\x41"
      | 0x0101 -> Buffer.add_string buf "\xE5\x61"
      | 0x0102 -> Buffer.add_string buf "\xE6\x41"
      | 0x0103 -> Buffer.add_string buf "\xE6\x61"
      | 0x0104 -> Buffer.add_string buf "\xF1\x41"
      | 0x0105 -> Buffer.add_string buf "\xF1\x61"
      | 0x0106 -> Buffer.add_string buf "\xE2\x43"
      | 0x0107 -> Buffer.add_string buf "\xE2\x63"
      | 0x0108 -> Buffer.add_string buf "\xE3\x43"
      | 0x0109 -> Buffer.add_string buf "\xE3\x63"
      | 0x010A -> Buffer.add_string buf "\xE7\x43"
      | 0x010B -> Buffer.add_string buf "\xE7\x63"
      | 0x010C -> Buffer.add_string buf "\xE9\x43"
      | 0x010D -> Buffer.add_string buf "\xE9\x63"
      | 0x010E -> Buffer.add_string buf "\xE9\x44"
      | 0x010F -> Buffer.add_string buf "\xE9\x64"
      | 0x0112 -> Buffer.add_string buf "\xE5\x45"
      | 0x0113 -> Buffer.add_string buf "\xE5\x65"
      | 0x0116 -> Buffer.add_string buf "\xE7\x45"
      | 0x0117 -> Buffer.add_string buf "\xE7\x65"
      | 0x0118 -> Buffer.add_string buf "\xF1\x45"
      | 0x0119 -> Buffer.add_string buf "\xF1\x65"
      | 0x011A -> Buffer.add_string buf "\xE9\x45"
      | 0x011B -> Buffer.add_string buf "\xE9\x65"
      | 0x011C -> Buffer.add_string buf "\xE3\x47"
      | 0x011D -> Buffer.add_string buf "\xE3\x67"
      | 0x011E -> Buffer.add_string buf "\xE6\x47"
      | 0x011F -> Buffer.add_string buf "\xE6\x67"
      | 0x0120 -> Buffer.add_string buf "\xE7\x47"
      | 0x0121 -> Buffer.add_string buf "\xE7\x67"
      | 0x0122 -> Buffer.add_string buf "\xF0\x47"
      | 0x0123 -> Buffer.add_string buf "\xF0\x67"
      | 0x0124 -> Buffer.add_string buf "\xE3\x48"
      | 0x0125 -> Buffer.add_string buf "\xE3\x68"
      | 0x0128 -> Buffer.add_string buf "\xE4\x49"
      | 0x0129 -> Buffer.add_string buf "\xE4\x69"
      | 0x012A -> Buffer.add_string buf "\xE5\x49"
      | 0x012B -> Buffer.add_string buf "\xE5\x69"
      | 0x012C -> Buffer.add_string buf "\xE6\x49"
      | 0x012D -> Buffer.add_string buf "\xE6\x69"
      | 0x012E -> Buffer.add_string buf "\xF1\x49"
      | 0x012F -> Buffer.add_string buf "\xF1\x69"
      | 0x0134 -> Buffer.add_string buf "\xE3\x4A"
      | 0x0135 -> Buffer.add_string buf "\xE3\x6A"
      | 0x0136 -> Buffer.add_string buf "\xF0\x4B"
      | 0x0137 -> Buffer.add_string buf "\xF0\x6B"
      | 0x0139 -> Buffer.add_string buf "\xE2\x4C"
      | 0x013A -> Buffer.add_string buf "\xE2\x6C"
      | 0x013B -> Buffer.add_string buf "\xF0\x4C"
      | 0x013C -> Buffer.add_string buf "\xF0\x6C"
      | 0x013D -> Buffer.add_string buf "\xE9\x4C"
      | 0x013E -> Buffer.add_string buf "\xE9\x6C"
      | 0x0143 -> Buffer.add_string buf "\xE2\x4E"
      | 0x0144 -> Buffer.add_string buf "\xE2\x6E"
      | 0x0145 -> Buffer.add_string buf "\xF0\x4E"
      | 0x0146 -> Buffer.add_string buf "\xF0\x6E"
      | 0x0147 -> Buffer.add_string buf "\xE9\x4E"
      | 0x0148 -> Buffer.add_string buf "\xE9\x6E"
      | 0x014C -> Buffer.add_string buf "\xE5\x4F"
      | 0x014D -> Buffer.add_string buf "\xE5\x6F"
      | 0x014E -> Buffer.add_string buf "\xE6\x4F"
      | 0x014F -> Buffer.add_string buf "\xE6\x6F"
      | 0x0150 -> Buffer.add_string buf "\xEE\x4F"
      | 0x0151 -> Buffer.add_string buf "\xEE\x6F"
      | 0x0154 -> Buffer.add_string buf "\xE2\x52"
      | 0x0155 -> Buffer.add_string buf "\xE2\x72"
      | 0x0156 -> Buffer.add_string buf "\xF0\x52"
      | 0x0157 -> Buffer.add_string buf "\xF0\x72"
      | 0x0158 -> Buffer.add_string buf "\xE9\x52"
      | 0x0159 -> Buffer.add_string buf "\xE9\x72"
      | 0x015A -> Buffer.add_string buf "\xE2\x53"
      | 0x015B -> Buffer.add_string buf "\xE2\x73"
      | 0x015C -> Buffer.add_string buf "\xE3\x53"
      | 0x015D -> Buffer.add_string buf "\xE3\x73"
      | 0x015E -> Buffer.add_string buf "\xF0\x53"
      | 0x015F -> Buffer.add_string buf "\xF0\x73"
      | 0x0160 -> Buffer.add_string buf "\xE9\x53"
      | 0x0161 -> Buffer.add_string buf "\xE9\x73"
      | 0x0162 -> Buffer.add_string buf "\xF0\x54"
      | 0x0163 -> Buffer.add_string buf "\xF0\x74"
      | 0x0164 -> Buffer.add_string buf "\xE9\x54"
      | 0x0165 -> Buffer.add_string buf "\xE9\x74"
      | 0x0168 -> Buffer.add_string buf "\xE4\x55"
      | 0x0169 -> Buffer.add_string buf "\xE4\x75"
      | 0x016A -> Buffer.add_string buf "\xE5\x55"
      | 0x016B -> Buffer.add_string buf "\xE5\x75"
      | 0x016C -> Buffer.add_string buf "\xE6\x55"
      | 0x016D -> Buffer.add_string buf "\xE6\x75"
      | 0x016E -> Buffer.add_string buf "\xEA\x55"
      | 0x016F -> Buffer.add_string buf "\xEA\x75"
      | 0x0170 -> Buffer.add_string buf "\xEE\x55"
      | 0x0171 -> Buffer.add_string buf "\xEE\x75"
      | 0x0172 -> Buffer.add_string buf "\xF1\x55"
      | 0x0173 -> Buffer.add_string buf "\xF1\x75"
      | 0x0174 -> Buffer.add_string buf "\xE3\x57"
      | 0x0175 -> Buffer.add_string buf "\xE3\x77"
      | 0x0176 -> Buffer.add_string buf "\xE3\x59"
      | 0x0177 -> Buffer.add_string buf "\xE3\x79"
      | 0x0178 -> Buffer.add_string buf "\xE8\x59"
      | 0x0179 -> Buffer.add_string buf "\xE2\x5A"
      | 0x017A -> Buffer.add_string buf "\xE2\x7A"
      | 0x017B -> Buffer.add_string buf "\xE7\x5A"
      | 0x017C -> Buffer.add_string buf "\xE7\x7A"
      | 0x017D -> Buffer.add_string buf "\xE9\x5A"
      | 0x017E -> Buffer.add_string buf "\xE9\x7A"
      | 0x0218 -> Buffer.add_string buf "\xF0\x53"
      | 0x0219 -> Buffer.add_string buf "\xF0\x73"
      | 0x021A -> Buffer.add_string buf "\xF0\x54"
      | 0x021B -> Buffer.add_string buf "\xF0\x74"
      | _ -> Buffer.add_char buf (Char.chr ansel_unknown));
      loop next
  in
  loop 0

let to_utf_8 s =
  let buf = Buffer.create (String.length s * 2) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      let c = Char.code s.[i] in
      if c < 0x80 then (
        Buffer.add_char buf s.[i];
        loop (i + 1))
      else if c >= 0xE0 && c <= 0xF9 && i + 1 < len then
        let base = Char.code s.[i + 1] in
        let utf8 =
          match c with
          | 0xE1 -> (
              match base with
              | 0x41 -> "\xC3\x80"
              | 0x45 -> "\xC3\x88"
              | 0x49 -> "\xC3\x8C"
              | 0x4F -> "\xC3\x92"
              | 0x55 -> "\xC3\x99"
              | 0x61 -> "\xC3\xA0"
              | 0x65 -> "\xC3\xA8"
              | 0x69 -> "\xC3\xAC"
              | 0x6F -> "\xC3\xB2"
              | 0x75 -> "\xC3\xB9"
              | _ -> "")
          | 0xE2 -> (
              match base with
              | 0x41 -> "\xC3\x81"
              | 0x43 -> "\xC4\x86"
              | 0x45 -> "\xC3\x89"
              | 0x49 -> "\xC3\x8D"
              | 0x4C -> "\xC4\xB9"
              | 0x4E -> "\xC5\x83"
              | 0x4F -> "\xC3\x93"
              | 0x52 -> "\xC5\x94"
              | 0x53 -> "\xC5\x9A"
              | 0x55 -> "\xC3\x9A"
              | 0x59 -> "\xC3\x9D"
              | 0x5A -> "\xC5\xB9"
              | 0x61 -> "\xC3\xA1"
              | 0x63 -> "\xC4\x87"
              | 0x65 -> "\xC3\xA9"
              | 0x69 -> "\xC3\xAD"
              | 0x6C -> "\xC4\xBA"
              | 0x6E -> "\xC5\x84"
              | 0x6F -> "\xC3\xB3"
              | 0x72 -> "\xC5\x95"
              | 0x73 -> "\xC5\x9B"
              | 0x75 -> "\xC3\xBA"
              | 0x79 -> "\xC3\xBD"
              | 0x7A -> "\xC5\xBA"
              | _ -> "")
          | 0xE3 -> (
              match base with
              | 0x41 -> "\xC3\x82"
              | 0x43 -> "\xC4\x88"
              | 0x45 -> "\xC3\x8A"
              | 0x47 -> "\xC4\x9C"
              | 0x48 -> "\xC4\xA4"
              | 0x49 -> "\xC3\x8E"
              | 0x4A -> "\xC4\xB4"
              | 0x4F -> "\xC3\x94"
              | 0x53 -> "\xC5\x9C"
              | 0x55 -> "\xC3\x9B"
              | 0x57 -> "\xC5\xB4"
              | 0x59 -> "\xC5\xB6"
              | 0x61 -> "\xC3\xA2"
              | 0x63 -> "\xC4\x89"
              | 0x65 -> "\xC3\xAA"
              | 0x67 -> "\xC4\x9D"
              | 0x68 -> "\xC4\xA5"
              | 0x69 -> "\xC3\xAE"
              | 0x6A -> "\xC4\xB5"
              | 0x6F -> "\xC3\xB4"
              | 0x73 -> "\xC5\x9D"
              | 0x75 -> "\xC3\xBB"
              | 0x77 -> "\xC5\xB5"
              | 0x79 -> "\xC5\xB7"
              | _ -> "")
          | 0xE4 -> (
              match base with
              | 0x41 -> "\xC3\x83"
              | 0x49 -> "\xC4\xA8"
              | 0x4E -> "\xC3\x91"
              | 0x4F -> "\xC3\x95"
              | 0x55 -> "\xC5\xA8"
              | 0x61 -> "\xC3\xA3"
              | 0x69 -> "\xC4\xA9"
              | 0x6E -> "\xC3\xB1"
              | 0x6F -> "\xC3\xB5"
              | 0x75 -> "\xC5\xA9"
              | _ -> "")
          | 0xE5 -> (
              match base with
              | 0x41 -> "\xC4\x80"
              | 0x45 -> "\xC4\x92"
              | 0x49 -> "\xC4\xAA"
              | 0x4F -> "\xC5\x8C"
              | 0x55 -> "\xC5\xAA"
              | 0x61 -> "\xC4\x81"
              | 0x65 -> "\xC4\x93"
              | 0x69 -> "\xC4\xAB"
              | 0x6F -> "\xC5\x8D"
              | 0x75 -> "\xC5\xAB"
              | _ -> "")
          | 0xE6 -> (
              match base with
              | 0x41 -> "\xC4\x82"
              | 0x47 -> "\xC4\x9E"
              | 0x49 -> "\xC4\xAC"
              | 0x4F -> "\xC5\x8E"
              | 0x55 -> "\xC5\xAC"
              | 0x61 -> "\xC4\x83"
              | 0x67 -> "\xC4\x9F"
              | 0x69 -> "\xC4\xAD"
              | 0x6F -> "\xC5\x8F"
              | 0x75 -> "\xC5\xAD"
              | _ -> "")
          | 0xE7 -> (
              match base with
              | 0x43 -> "\xC4\x8A"
              | 0x45 -> "\xC4\x96"
              | 0x47 -> "\xC4\xA0"
              | 0x5A -> "\xC5\xBB"
              | 0x63 -> "\xC4\x8B"
              | 0x65 -> "\xC4\x97"
              | 0x67 -> "\xC4\xA1"
              | 0x7A -> "\xC5\xBC"
              | _ -> "")
          | 0xE8 -> (
              match base with
              | 0x41 -> "\xC3\x84"
              | 0x45 -> "\xC3\x8B"
              | 0x49 -> "\xC3\x8F"
              | 0x4F -> "\xC3\x96"
              | 0x55 -> "\xC3\x9C"
              | 0x59 -> "\xC5\xB8"
              | 0x61 -> "\xC3\xA4"
              | 0x65 -> "\xC3\xAB"
              | 0x69 -> "\xC3\xAF"
              | 0x6F -> "\xC3\xB6"
              | 0x75 -> "\xC3\xBC"
              | 0x79 -> "\xC3\xBF"
              | _ -> "")
          | 0xE9 -> (
              match base with
              | 0x43 -> "\xC4\x8C"
              | 0x44 -> "\xC4\x8E"
              | 0x45 -> "\xC4\x9A"
              | 0x4C -> "\xC4\xBD"
              | 0x4E -> "\xC5\x87"
              | 0x52 -> "\xC5\x98"
              | 0x53 -> "\xC5\xA0"
              | 0x54 -> "\xC5\xA4"
              | 0x5A -> "\xC5\xBD"
              | 0x63 -> "\xC4\x8D"
              | 0x64 -> "\xC4\x8F"
              | 0x65 -> "\xC4\x9B"
              | 0x6C -> "\xC4\xBE"
              | 0x6E -> "\xC5\x88"
              | 0x72 -> "\xC5\x99"
              | 0x73 -> "\xC5\xA1"
              | 0x74 -> "\xC5\xA5"
              | 0x7A -> "\xC5\xBE"
              | _ -> "")
          | 0xEA -> (
              match base with
              | 0x41 -> "\xC3\x85"
              | 0x55 -> "\xC5\xAE"
              | 0x61 -> "\xC3\xA5"
              | 0x75 -> "\xC5\xAF"
              | _ -> "")
          | 0xEE -> (
              match base with
              | 0x4F -> "\xC5\x90"
              | 0x55 -> "\xC5\xB0"
              | 0x6F -> "\xC5\x91"
              | 0x75 -> "\xC5\xB1"
              | _ -> "")
          | 0xF0 -> (
              match base with
              | 0x43 -> "\xC3\x87"
              | 0x47 -> "\xC4\xA2"
              | 0x4B -> "\xC4\xB6"
              | 0x4C -> "\xC4\xBB"
              | 0x4E -> "\xC5\x85"
              | 0x52 -> "\xC5\x96"
              | 0x53 -> "\xC5\x9E"
              | 0x54 -> "\xC5\xA2"
              | 0x63 -> "\xC3\xA7"
              | 0x67 -> "\xC4\xA3"
              | 0x6B -> "\xC4\xB7"
              | 0x6C -> "\xC4\xBC"
              | 0x6E -> "\xC5\x86"
              | 0x72 -> "\xC5\x97"
              | 0x73 -> "\xC5\x9F"
              | 0x74 -> "\xC5\xA3"
              | _ -> "")
          | 0xF1 -> (
              match base with
              | 0x41 -> "\xC4\x84"
              | 0x45 -> "\xC4\x98"
              | 0x49 -> "\xC4\xAE"
              | 0x55 -> "\xC5\xB2"
              | 0x61 -> "\xC4\x85"
              | 0x65 -> "\xC4\x99"
              | 0x69 -> "\xC4\xAF"
              | 0x75 -> "\xC5\xB3"
              | _ -> "")
          | _ -> ""
        in
        if utf8 = "" then (
          Buffer.add_char buf s.[i];
          loop (i + 1))
        else (
          Buffer.add_string buf utf8;
          loop (i + 2))
      else
        let utf8 =
          match c with
          | 0xA1 -> "\xC5\x81"
          | 0xA2 -> "\xC3\x98"
          | 0xA3 -> "\xC3\x90"
          | 0xA4 -> "\xC3\x9E"
          | 0xA5 -> "\xC3\x86"
          | 0xA6 -> "\xC5\x92"
          | 0xA8 -> "\xC2\xB7"
          | 0xAA -> "\xC2\xAE"
          | 0xAB -> "\xC2\xB1"
          | 0xAE -> "\xCA\xBC"
          | 0xB0 -> "\xCA\xBB"
          | 0xB1 -> "\xC5\x82"
          | 0xB2 -> "\xC3\xB8"
          | 0xB3 -> "\xC4\x91"
          | 0xB4 -> "\xC3\xBE"
          | 0xB5 -> "\xC3\xA6"
          | 0xB6 -> "\xC5\x93"
          | 0xB8 -> "\xC4\xB1"
          | 0xB9 -> "\xC2\xA3"
          | 0xBA -> "\xC3\xB0"
          | 0xC3 -> "\xC2\xA9"
          | 0xC5 -> "\xC2\xBF"
          | 0xC6 -> "\xC2\xA1"
          | 0xCF -> "\xC3\x9F"
          | _ -> ""
        in
        if utf8 = "" then (
          Buffer.add_char buf s.[i];
          loop (i + 1))
        else (
          Buffer.add_string buf utf8;
          loop (i + 1))
  in
  loop 0

let no_accent = function
  | '\224' .. '\229' -> 'a'
  | '\162' | '\231' -> 'c'
  | '\232' .. '\235' -> 'e'
  | '\236' .. '\239' -> 'i'
  | '\241' -> 'n'
  | '\242' .. '\246' -> 'o'
  | '\249' .. '\252' -> 'u'
  | '\253' | '\255' -> 'y'
  | '\192' .. '\197' -> 'A'
  | '\199' -> 'C'
  | '\200' .. '\203' -> 'E'
  | '\204' .. '\207' -> 'I'
  | '\209' -> 'N'
  | '\210' .. '\214' -> 'O'
  | '\217' .. '\220' -> 'U'
  | '\221' -> 'Y'
  | '\168' | '\176' | '\180' | '\184' | '\186' -> ' '
  | '\171' -> '<'
  | '\187' -> '>'
  | c -> c

let accent_code =
  (* and 1-to-1 conversions *)
  function
  | '\192' | '\200' | '\204' | '\210' | '\217' | '\224' | '\232' | '\236'
  | '\242' | '\249' ->
      225
  | '\193' | '\201' | '\205' | '\211' | '\218' | '\221' | '\180' | '\225'
  | '\233' | '\237' | '\243' | '\250' | '\253' ->
      226
  | '\194' | '\202' | '\206' | '\212' | '\219' | '\226' | '\234' | '\238'
  | '\244' | '\251' ->
      227
  | '\195' | '\209' | '\213' | '\227' | '\241' | '\245' -> 228
  | '\196' | '\203' | '\207' | '\214' | '\220' | '\168' | '\228' | '\235'
  | '\239' | '\246' | '\252' | '\255' ->
      232
  | '\197' | '\229' | '\176' | '\186' -> 234
  | '\199' | '\231' | '\184' -> 240
  | '\161' -> 198
  | '\162' -> 252
  | '\163' -> 185
  | '\164' -> 0x6f
  | '\165' -> 0x59
  | '\166' -> 0x7c
  | '\169' -> 195
  | '\170' -> 0x61
  | '\171' -> 0x3c
  | '\173' -> 0x2d
  | '\174' -> 170
  | '\177' -> 171
  | '\178' -> 0x32
  | '\179' -> 0x33
  | '\183' -> 168
  | '\185' -> 0x31
  | '\187' -> 0x3e
  | '\191' -> 197
  | '\198' -> 165
  | '\230' -> 181
  | '\208' -> 163
  | '\240' -> 179
  | '\216' -> 162
  | '\248' -> 178
  | '\222' -> 164
  | '\254' -> 180
  | '\223' -> 207
  | c when c >= '\161' -> ansel_unknown
  | _ -> 0

let of_iso_8859_1 s =
  let len, identical =
    let rec loop i len identical =
      if i = String.length s then (len, identical)
      else
        let a = accent_code s.[i] in
        if a = 0 then loop (i + 1) (len + 1) identical
        else
          let n = no_accent s.[i] in
          if n = s.[i] then loop (i + 1) (len + 1) false
          else loop (i + 1) (len + 2) false
    in
    loop 0 0 true
  in
  if identical then s
  else
    let s' = Bytes.create len in
    let rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else
        let i' =
          let a = accent_code s.[i] in
          if a > 0 then (
            Bytes.set s' i' (Char.chr a);
            let n = no_accent s.[i] in
            if n = s.[i] then i'
            else (
              Bytes.set s' (i' + 1) n;
              i' + 1))
          else (
            Bytes.set s' i' s.[i];
            i')
        in
        loop (i + 1) (i' + 1)
    in
    loop 0 0

let grave = function
  | 'a' -> '\224'
  | 'e' -> '\232'
  | 'i' -> '\236'
  | 'o' -> '\242'
  | 'u' -> '\249'
  | 'A' -> '\192'
  | 'E' -> '\200'
  | 'I' -> '\204'
  | 'O' -> '\210'
  | 'U' -> '\217'
  | ' ' -> '`'
  | x -> x

let acute = function
  | 'a' -> '\225'
  | 'e' -> '\233'
  | 'i' -> '\237'
  | 'o' -> '\243'
  | 'u' -> '\250'
  | 'y' -> '\253'
  | 'A' -> '\193'
  | 'E' -> '\201'
  | 'I' -> '\205'
  | 'O' -> '\211'
  | 'U' -> '\218'
  | 'Y' -> '\221'
  | ' ' -> '\180'
  | x -> x

let circum = function
  | 'a' -> '\226'
  | 'e' -> '\234'
  | 'i' -> '\238'
  | 'o' -> '\244'
  | 'u' -> '\251'
  | 'A' -> '\194'
  | 'E' -> '\202'
  | 'I' -> '\206'
  | 'O' -> '\212'
  | 'U' -> '\219'
  | ' ' -> '^'
  | x -> x

let uml = function
  | 'a' -> '\228'
  | 'e' -> '\235'
  | 'i' -> '\239'
  | 'o' -> '\246'
  | 'u' -> '\252'
  | 'y' -> '\255'
  | 'A' -> '\196'
  | 'E' -> '\203'
  | 'I' -> '\207'
  | 'O' -> '\214'
  | 'U' -> '\220'
  | ' ' -> '\168'
  | x -> x

let circle = function 'a' -> '\229' | 'A' -> '\197' | ' ' -> '\176' | x -> x

let tilde = function
  | 'a' -> '\227'
  | 'n' -> '\241'
  | 'o' -> '\245'
  | 'A' -> '\195'
  | 'N' -> '\209'
  | 'O' -> '\213'
  | ' ' -> '~'
  | x -> x

let cedil = function 'c' -> '\231' | 'C' -> '\199' | ' ' -> '\184' | x -> x

let slash = function
  | 'C' | 'c' -> '\162'
  | 'O' -> '\216'
  | 'o' -> '\248'
  | ' ' -> '/'
  | x -> x

let to_iso_8859_1 s =
  let len, identical =
    let rec loop i len identical =
      if i = String.length s then (len, identical)
      else if i = String.length s - 1 then (len + 1, identical)
      else
        match Char.code s.[i] with
        | 166 | 172 | 173 | 182 | 188 | 189 -> loop (i + 1) (len + 2) false
        | c when c >= 224 -> loop (i + 2) (len + 1) false
        | c when c >= 161 -> loop (i + 1) (len + 1) false
        | _ -> loop (i + 1) (len + 1) identical
    in
    loop 0 0 true
  in
  if identical then s
  else
    let s' = Bytes.create len in
    let rec loop i i' =
      if i = String.length s then Bytes.unsafe_to_string s'
      else if i = String.length s - 1 then (
        Bytes.set s' i' s.[i];
        Bytes.unsafe_to_string s')
      else
        match Char.code s.[i] with
        | (166 | 172 | 173 | 182 | 188 | 189) as c ->
            let c', c'' =
              match c with
              | 166 -> ('O', 'E')
              | 172 -> ('O', '\180')
              | 173 -> ('U', '\180')
              | 182 -> ('o', 'e')
              | 188 -> ('o', '\180')
              | 189 -> ('u', '\180')
              | _ -> (iso_8859_1_unknown, iso_8859_1_unknown)
            in
            Bytes.set s' i' c';
            Bytes.set s' (i' + 1) c'';
            loop (i + 1) (i' + 2)
        | c when c >= 224 ->
            let c' = s.[i + 1] in
            let c' =
              match c with
              | 224 | 226 | 235 | 237 | 254 -> acute c'
              | 225 | 236 -> grave c'
              | 227 | 250 -> circum c'
              | 228 | 230 | 233 -> tilde c'
              | 232 | 238 -> uml c'
              | 231 | 234 -> circle c'
              | 240 | 241 | 242 | 243 | 244 | 247 | 248 | 249 -> cedil c'
              | 252 -> slash c'
              | _ -> c'
            in
            Bytes.set s' i' c';
            loop (i + 2) (i' + 1)
        | c ->
            let c' =
              match c with
              | 161 -> 'L'
              | 162 -> '\216'
              | 163 -> '\208'
              | 164 -> '\222'
              | 165 -> '\198'
              | 167 | 174 | 176 -> '\180'
              | 168 -> '\183'
              | 169 -> 'b'
              | 170 -> '\174'
              | 171 -> '\177'
              | 177 | 193 -> 'l'
              | 178 -> '\248'
              | 179 | 186 -> '\240'
              | 180 -> '\254'
              | 181 -> '\230'
              | 183 -> '"'
              | 184 -> 'i'
              | 185 -> '\163'
              | 190 | 191 -> iso_8859_1_unknown
              | 192 -> '\176'
              | 194 -> 'P'
              | 195 -> '\169'
              | 196 -> '#'
              | 197 -> '\191'
              | 198 -> '\161'
              | 205 -> '\101'
              | 206 -> '\111'
              | 207 -> '\223'
              | _ -> Char.chr c
            in
            Bytes.set s' i' c';
            loop (i + 1) (i' + 1)
    in
    loop 0 0
