module Compat = Geneweb_compat

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
let[@inline] is_dummy t = t == dummy

let[@inline always] mk src start stop =
  assert (0 <= start && start <= stop);
  { src; start; stop }

let equal { src = s11; start = s12; stop = s13 }
    { src = s21; start = s22; stop = s23 } =
  s12 = s22 && s13 = s23 && equal_source s11 s21

(* Compute the number of colomn starting at 1. *)
let[@inline] compute_cnum (p : Lexing.position) = p.pos_cnum - p.pos_bol + 1

let pp_lexing_position ppf (p : Lexing.position) =
  Fmt.pf ppf "(%d, %d)" p.pos_lnum (compute_cnum p)

let with_pp_loc_input src k =
  match src with
  | `File f ->
      (* [Pp_loc.Input.file] opens templates in binary mode, but the Parser module
          open them in text mode. On Windows, text mode normalizes line endings
          to the UNIX format. We must use the same mode to print correct line
          and column numbers. *)
      Compat.In_channel.with_open_text f @@ fun ic ->
      k (Pp_loc.Input.in_channel ic)
  | `Raw s -> k (Pp_loc.Input.string s)
  | `In_channel ic ->
      assert (not @@ Compat.In_channel.is_binary_mode ic);
      k (Pp_loc.Input.in_channel ic)

let pp ppf ({ src; start; stop } as t) =
  if is_dummy t then Fmt.pf ppf "<dummy>"
  else
    with_pp_loc_input src @@ fun input ->
    let start = Pp_loc.Position.(to_lexing input @@ of_offset start) in
    let stop = Pp_loc.Position.(to_lexing input @@ of_offset stop) in
    Fmt.pf ppf "%a:%a:%a" pp_source src pp_lexing_position start
      pp_lexing_position stop

let pp_quote ppf ({ src; start; stop } as t) =
  if is_dummy t then Fmt.nop ppf ()
  else
    with_pp_loc_input src @@ fun input ->
    Pp_loc.pp ~input ppf [ Pp_loc.Position.(of_offset start, of_offset stop) ]

let pp_with_source ppf t = Fmt.pf ppf "%a:@ %a" pp t pp_quote t
