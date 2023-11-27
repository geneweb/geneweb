
let starting_char no_num s =
  match s.[0] with
  (*'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' *)
  | 'a' .. 'z' | 'A' .. 'Z' | '\xE0' .. '\xFD' | '\xC0' .. '\xDD' -> true
  | '0' .. '9' -> not no_num
  | '?' -> if s = "?" then true else false
  | _ -> false

let is_printable = function '\000' .. '\031' -> false | _ -> true

let gen_correct_string no_num no_colon s =
  let s = String.trim s in
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if len = 0 && not (starting_char no_num s) then
      loop i (Buff.store len '_')
    else
      match s.[i] with
      | ' ' | '\n' | '\t' ->
        if i = String.length s - 1 then Buff.get len
        else loop (i + 1) (Buff.store len '_')
      | '_' | '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | ':' when no_colon ->
        let len = Buff.store len '\\' in
        loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | c ->
        let c = if is_printable c then c else '_' in
        loop (i + 1) (Buff.store len c)
  in
  loop 0 0

let soy y = if y = 0 then "-0" else string_of_int y

let string_of_date_dmy d =
  let add_prec_f = Date.(function
      | About -> fun ds -> "~" ^ ds
      | Maybe -> fun ds -> "?" ^ ds
      | Before -> fun ds -> "<" ^ ds
      | After -> fun ds -> ">" ^ ds
      | Sure -> fun ds -> ds
      | YearInt d2 -> fun ds ->
        ds ^ if d2.month2 = 0 then Printf.sprintf "..%s" (soy d2.year2)
        else if d2.day2 = 0 then
          Printf.sprintf "..%d/%s" d2.month2 (soy d2.year2)
        else
          Printf.sprintf "..%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
      | OrYear d2 -> fun ds ->
        ds ^ if d2.month2 = 0 then Printf.sprintf "|%s" (soy d2.year2)
        else if d2.day2 = 0 then
          Printf.sprintf "|%d/%s" d2.month2 (soy d2.year2)
        else
          Printf.sprintf "|%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
    )
  in
  let ds =
    let open Date in
    if d.month = 0 then soy d.year
    else if d.day = 0 then Printf.sprintf "%d/%s" d.month (soy d.year)
    else Printf.sprintf "%d/%d/%s" d.day d.month (soy d.year)
  in
  let ds = add_prec_f d.Date.prec ds in
  ds

let date_to_string' no_colon = function
  | Date.Dgreg (d, Dgregorian) ->
    string_of_date_dmy d
  | Dgreg (d, Djulian) ->
    let d = Date.convert ~from:Dgregorian ~to_:Djulian d in
    let ds = string_of_date_dmy d in
    Printf.sprintf "%s%s" ds "J"
  | Dgreg (d, Dfrench) ->
    let d = Date.convert ~from:Dgregorian ~to_:Dfrench d in
    let ds = string_of_date_dmy d in
    Printf.sprintf "%s%s" ds "F"
  | Dgreg (d, Dhebrew) ->
    let d = Date.convert ~from:Dgregorian ~to_:Dhebrew d in
    let ds = string_of_date_dmy d in
    Printf.sprintf "%s%s" ds "H"
  | Dtext t ->
    (* Dans le cas d'une date texte pour un titre, on échappe les ':' *)
    let t = gen_correct_string false no_colon t in
    Printf.sprintf "0(%s)" t

let date_to_string d = match Date.od_of_cdate d with
  | Some d -> date_to_string' false d
  | None -> "0"

let title_date_to_string d = match Date.od_of_cdate d with
  | Some d -> date_to_string' true d
  | None -> "0"
