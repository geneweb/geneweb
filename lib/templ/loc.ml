type source = [ `File of string | `In_channel of in_channel | `Raw of string ]

let equal_source src1 src2 =
  match (src1, src2) with
  | `File f1, `File f2 -> String.equal f1 f2
  | `File _, _ | _, `File _ -> false
  | `In_channel ic1, `In_channel ic2 -> ic1 = ic2
  | `In_channel _, _ | _, `In_channel _ -> false
  | `Raw s1, `Raw s2 -> String.equal s1 s2

let pp_source ppf src =
  match src with
  | `File f -> Fmt.string ppf f
  | `In_channel _ -> Fmt.pf ppf "<in_channel>"
  | `Raw _ -> Fmt.pf ppf "<raw>"

type t = { src : source; start : int; stop : int }

let dummy = { src = `File "<dummy>"; start = -1; stop = -1 }

let of_lexbuf lexbuf =
  let f = (Lexing.lexeme_start_p lexbuf).pos_fname in
  let start = Lexing.lexeme_start lexbuf in
  let stop = Lexing.lexeme_start lexbuf in
  { src = `File f; start; stop }

let equal { src = s11; start = s12; stop = s13 }
    { src = s21; start = s22; stop = s23 } =
  s12 = s22 && s13 = s23 && equal_source s11 s21

let pp ppf ({ src; start; stop } as t) =
  if t == dummy then Fmt.pf ppf "<dummy>"
  else Fmt.pf ppf "%a:%d:%d" pp_source src start stop

let pp_with_input ppf ({ src; start; stop } as t) =
  if t == dummy then Fmt.pf ppf "No location implemented"
  else
    let input =
      match src with
      | `File f -> Pp_loc.Input.file f
      | `In_channel ic -> Pp_loc.Input.in_channel ic
      | `Raw s -> Pp_loc.Input.string s
    in
    let start = Pp_loc.Position.of_offset start in
    let stop = Pp_loc.Position.of_offset stop in
    Pp_loc.pp ~input ppf [ (start, stop) ]
