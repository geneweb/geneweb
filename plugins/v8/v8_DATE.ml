open Geneweb
open Config
open Def

(* open Angstrom
 * 
 * let whitespace = take_while begin function
 *     | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
 *     | _ -> false
 *   end
 * 
 * let integer =
 *   take_while1 begin function
 *     | '0'..'9' -> true
 *     | _ -> false
 *   end
 * 
 * let sign = take_while begin function
 *     | '+' | '-' -> true
 *     | _ -> false
 *   end *)

let cut_space x =
  let len = String.length x in
  if len = 0 then x
  else if x = " " then ""
  else
    let start = if x.[0] = ' ' then 1 else 0 in
    let stop = if x.[len-1] = ' ' then len - 1 else len in
    if start = 0 && stop = len then x else String.sub x start (stop - start)


let month_txt =
  [| "JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN"; "JUL"; "AUG"; "SEP"; "OCT";
     "NOV"; "DEC" |]

let french_txt =
  [| "VEND"; "BRUM"; "FRIM"; "NIVO"; "PLUV"; "VENT"; "GERM"; "FLOR"; "PRAI";
     "MESS"; "THER"; "FRUC"; "COMP" |]

let hebrew_txt =
  [| "TSH"; "CSH"; "KSL"; "TVT"; "SHV"; "ADR"; "ADS"; "NSN"; "IYR"; "SVN";
     "TMZ"; "AAV"; "ELL" |]

let parse_date =
  let open Re in
  let dmy =
    seq [ opt ( alt [ seq [ opt (seq [ group (rep1 digit) ; rep space ; char '/' ; rep space ])
                          ; group (rep1 digit)
                          ; rep space
                          ; char '/'
                          ; rep space
                          ]
                    ; seq [ opt (seq [ group (rep1 digit) ; rep space ])
                          ; group (rep1 alpha)
                          ; rep space
                          ]
                    ])
        ; group (seq [ opt (char '-') ; rep1 digit ])
        ]
  in
  let regexp =
    seq [ bos
        ; rep space
        ; opt (group (alt [ char '~' ; char '?' ; char '>' ; char '<' ]) )
        ; rep space
        ; dmy
        ; rep space
        ; opt (seq [ group (alt [ str ".." ; char '|' ]) ; rep space ; dmy ]) 
        ; rep space
        ; opt (seq [ group (alt [ char 'G' ; char 'J' ; char 'F' ; char 'H' ])
                   ; rep space
                   ])
        ; rep space
        ; eos
        ]
    |> compile
  in
  let module G = Group in
  let dmy s c g o k =
    let int o =
    (*   let i = G.start g o in
     *   let rec loop acc j k =
     *     if j < i then acc
     *     else match String.unsafe_get s j with
     *       | '-' -> - acc
     *       | '0' -> loop acc (j - 1) (k * 10)
     *       |  c  -> loop (acc + (int_of_char c - 48) * k)  (j - 1) (k * 10)
     *   in loop 0 (G.stop g o) 1
     * in *)
      G.get g o |> int_of_string in
    let y = int (o + 4) in
    if G.test g (o + 1) then
      let m = int (o + 1) in
      let d = if G.test g o then int o else 0 in
      k d m y
    else if G.test g (o + 3) then
      let a =
        match c with
        | Def.Dgregorian | Def.Djulian -> month_txt
        | Def.Dfrench -> french_txt
        | Def.Dhebrew -> hebrew_txt
      in
      let find s =
        let rec find i =
          if i < 0 then
            0
          else if Array.unsafe_get a i = s then
            i + 1
          else
            find (i - 1)
        in find (Array.length a - 1)
      in
      let m = find (G.get g (o + 3)) in
      let d = if G.test g (o + 2) then int (o + 2) else 0 in
      k d m y
    else k 0 0 y
  in
  fun s ->
    let g = exec regexp s in
    let calendar =
      if G.test g 13 then
        match String.unsafe_get s (G.start g 13) with
        | 'J' -> Djulian
        | 'F' -> Dfrench
        | 'H' -> Dhebrew
        | 'G' -> Dgregorian
        | _ -> assert false
      else Dgregorian
    in
    let prec =
      if G.test g 7 then
        match String.unsafe_get s (G.start g 7) with
        | '.' ->
          YearInt (dmy s calendar g 8 @@ fun day2 month2 year2 -> { day2 ; month2 ; year2 ; delta2 = 0 })
        | '|' ->
          OrYear (dmy s calendar g 8 @@ fun day2 month2 year2 -> { day2 ; month2 ; year2 ; delta2 = 0 })
        | _ -> assert false
      else if G.test g 1 then
        match String.unsafe_get s (G.start g 1) with
        | '~' -> About
        | '?' -> Maybe
        | '>' -> After
        | '<' -> Before
        | _ -> assert false
      else Sure
    in
    dmy s calendar g 2 @@ fun day month year ->
    let d = { day ; month ; year ; delta = 0 ; prec } in
    match calendar with
    | Dgregorian -> Dgreg (d, Dgregorian)
    | Djulian -> Dgreg (Calendar.gregorian_of_julian d, Djulian)
    | Dfrench -> Dgreg (Calendar.gregorian_of_french d, Dfrench)
    | Dhebrew -> Dgreg (Calendar.gregorian_of_hebrew d, Dhebrew)

let repr_of_date = function
  | Dtext s -> s
  | Dgreg (d, c) ->
    let d = match c with
      | Dgregorian -> d
      | Djulian -> Calendar.julian_of_gregorian d
      | Dfrench -> Calendar.french_of_gregorian d
      | Dhebrew -> Calendar.hebrew_of_gregorian d
    in
    let a =
      match c with
      | Dgregorian | Djulian -> month_txt
      | Dfrench -> french_txt
      | Dhebrew -> hebrew_txt
    in
    let dmy d m y =
      string_of_int y
      |> begin fun s ->
        if m = 0 then
          s
        else
          Array.get a (m - 1) ^ " " ^ s
          |> begin fun s ->
            if d = 0 then s
            else string_of_int d ^ " " ^ s
          end
      end
    in
    let s =
      match d.prec with
      | Sure -> dmy d.day d.month d.year
      | Maybe -> "? " ^ dmy d.day d.month d.year
      | About -> "~ " ^ dmy d.day d.month d.year
      | Before -> "< " ^ dmy d.day d.month d.year
      | After -> "> " ^ dmy d.day d.month d.year
      | OrYear d2 -> dmy d.day d.month d.year ^ " | " ^ dmy d2.day2 d2.month2 d2.year2
      | YearInt d2 -> dmy d.day d.month d.year ^ " .. " ^ dmy d2.day2 d2.month2 d2.year2
    in
    match c with
    | Dgregorian -> s
    | Djulian -> s ^ " J"
    | Dfrench -> s ^ " F"
    | Dhebrew -> s ^ " H"    

let date_of_string s i =
  let champ i =
    let (neg, i) =
      if i < String.length s && s.[i] = '-' then true, i + 1 else false, i
    in
    let rec loop i n =
      if i = String.length s then (if neg then -n else n), i
      else
        match s.[i] with
          '0'..'9' as c ->
            loop (succ i) (10 * n + Char.code c - Char.code '0')
        | _ -> (if neg then -n else n), i
    in
    loop i 0
  in
  let skip_slash i =
    if i < String.length s && s.[i] = '/' then Some (succ i) else None
  in
  let (precision, i) =
    match s.[i] with
    | '~' -> About, succ i
    | '?' -> Maybe, succ i
    | '>' -> After, succ i
    | '<' -> Before, succ i
    | _ -> Sure, i
  in
  let (undefined, year, i) =
    let (year, j) = champ i in
    if j = i + 1 && s.[i] = '0' then true, year, j else false, year, j
  in
  let error n = failwith (Printf.sprintf "date_of_string%d %s" n s) in
  let dmy2 year2 i =
    match skip_slash i with
      Some i ->
        let month2 = year2 in
        let (year2, i) = champ i in
        begin match skip_slash i with
          Some i ->
            let day2 = month2 in
            let month2 = year2 in
            let (year2, i) = champ i in
            if month2 < 1 || month2 > 13 then error 2
            else if day2 < 1 || day2 > 31 then error 3
            else (day2, month2, year2), i
        | None ->
            if month2 < 1 || month2 > 13 then error 4
            else (0, month2, year2), i
        end
    | None -> (0, 0, year2), i
  in
  let date =
    match skip_slash i with
      Some i ->
        let month = year in
        let (year, i) = champ i in
        begin match skip_slash i with
          Some i ->
            let day = month in
            let month = year in
            let (year, i) = champ i in
            (*
                        if year = 0 then if i = String.length s then None else error 1
                        else
            *)
            if month < 1 || month > 13 then error 2
            else if day < 1 || day > 31 then error 3
            else
              let d =
                {day = day; month = month; year = year; prec = precision;
                 delta = 0}
              in
              Some (Dgreg (d, Dgregorian), i)
        | None ->
            if year = 0 then None
            else if month < 1 || month > 13 then error 4
            else
              let d =
                {day = 0; month = month; year = year; prec = precision;
                 delta = 0}
              in
              Some (Dgreg (d, Dgregorian), i)
        end
    | None ->
        if undefined then
          if i = String.length s then None
          else if s.[i] = '(' && s.[String.length s - 1] = ')' then
            let txt = String.sub s (i + 1) (String.length s - i - 2) in
            let txt = cut_space txt in Some (Dtext txt, String.length s)
          else failwith ("date_of_string " ^ s)
        else
          let d =
            {day = 0; month = 0; year = year; prec = precision; delta = 0}
          in
          Some (Dgreg (d, Dgregorian), i)
  in
  let date =
    match date with
      Some ((Dgreg (d, cal) as dt), i) ->
        if i = String.length s then Some (dt, i)
        else if s.[i] = '|' then
          let (year2, i) = champ (succ i) in
          let ((day2, month2, year2), i) = dmy2 year2 i in
          let dmy2 =
            {day2 = day2; month2 = month2; year2 = year2; delta2 = 0}
          in
          Some (Dgreg ({d with prec = OrYear dmy2}, cal), i)
        else if i + 1 < String.length s && s.[i] = '.' && s.[i+1] = '.' then
          let (year2, i) = champ (i + 2) in
          let ((day2, month2, year2), i) = dmy2 year2 i in
          let dmy2 =
            {day2 = day2; month2 = month2; year2 = year2; delta2 = 0}
          in
          Some (Dgreg ({d with prec = YearInt dmy2}, cal), i)
        else Some (dt, i)
    | Some ((Dtext _ as dt), i) -> Some (dt, i)
    | None -> None
  in
  let date =
    match date with
      Some (Dgreg (d, _), i) ->
        if i = String.length s then Some (Dgreg (d, Dgregorian), i)
        else
          begin match s.[i] with
            'G' -> Some (Dgreg (d, Dgregorian), i + 1)
          | 'J' ->
              let d = Calendar.gregorian_of_julian d in
              Some (Dgreg (d, Djulian), i + 1)
          | 'F' ->
              let d = Calendar.gregorian_of_french d in
              Some (Dgreg (d, Dfrench), i + 1)
          | 'H' ->
              let d = Calendar.gregorian_of_hebrew d in
              Some (Dgreg (d, Dhebrew), i + 1)
          | _ -> Some (Dgreg (d, Dgregorian), i)
          end
    | d -> d
  in
  match date with
    Some (dt, i) -> if i = String.length s then Some dt else error 5
  | None -> None


let test =
  [ "~1024"
  ; "01/01/1024..01/12/1982"
  ; "07/1982"
  ; "02/07/1982F"
  ]

let () =
  Gc.compact () ;
  Mutil.bench __LOC__ @@ fun () -> for i = 0 to 100000 do List.iter (fun s -> ignore @@ date_of_string s 0) test done

let () =
  Gc.compact () ;
  Mutil.bench __LOC__ @@ fun () -> for i = 0 to 100000 do List.iter (fun s -> ignore @@ parse_date s) test done

let () =
  List.iter (fun s -> s ^ ": " ^ (parse_date s |> repr_of_date) |> print_endline) test
  
