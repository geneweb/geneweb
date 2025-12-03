type key = { pk_first_name : string; pk_surname : string; pk_occ : int }
(** Key to refer a person's definition *)

(** Represents a person in .gw file. It could be either reference to a person
    (only key elements provided) or definition (all information provided). *)
type somebody =
  | Undefined of key  (** Reference to person *)
  | Defined of (Gwdb.iper, Gwdb.iper, string) Def.gen_person
      (** Person's definition *)

type 'a assumption = Weak of 'a | Strong of 'a

(** Blocks that could appear in .gw file. *)
type gw_syntax =
  | Family of
      somebody Def.gen_couple
      * Def.sex assumption
      * Def.sex assumption
      * (somebody * Def.sex assumption) list
      * (string Def.gen_fam_event_name
        * Def.cdate
        * string
        * string
        * string
        * string
        * (somebody * Def.sex assumption * Def.witness_kind * string) list)
        list
      * ( (Gwdb.iper, Gwdb.iper, string) Def.gen_person,
          Gwdb.ifam,
          string )
        Def.gen_family
      * (Gwdb.iper, Gwdb.iper, string) Def.gen_person Def.gen_descend
      (** Family definition block. Contains:
      - Family couple (father's and mother's definition/reference)
      - Father's sex
      - Mother's sex
      - List of witnesses definition/reference with their sex.
      - List of information about every family event (name, date,
        place, reason, source, notes and witnesses)
      - Family definition
      - Children (descendants) *)
  | Notes of key * string
      (** Block that defines personal notes. First element represents
      reference to person. Second is note's content. *)
  | Relations of
      somebody * Def.sex assumption * (somebody, string) Def.gen_relation list
      (** Block that defines relations of a person with someone outisde of
      family block (like foster parents) (field {i rparents}). Contains:
      - Concerned person definition/reference
      - Sex of person
      - List of his relations. *)
  | Pevent of
      somebody
      * Def.sex assumption
      * (string Def.gen_pers_event_name
        * Def.cdate
        * string
        * string
        * string
        * string
        * (somebody * Def.sex assumption * Def.witness_kind * string) list)
        list
      (** Block that defines events of a person. Specific to gwplus format. Contains:
      - Concerned person's definition/reference
      - Sex of person
      - List of information about every personal event (name, date,
      place, reason, source, notes and witnesses)*)
  | Bnotes of string * string
      (** Block that defines database notes and extended pages.
      First string represents name of extended page ("" for
      database notes, only one per file). Second is note's
      or page's content. *)
  | Wnotes of string * string
      (** Block that defines wizard notes. First string represents
      First string represents wizard's id. Second is note's content. *)

let log_error ~filename ~state message =
  Printf.printf "File \"%s\", line %d:\n" filename state.State.line_cnt;
  Printf.printf "Error: %s\n" message;
  flush stdout

let ensure_end_of_line ~filename ~state fields =
  if fields = [] then Ok ()
  else
    let message =
      Printf.sprintf "Ignored fields: %s" (String.concat " " fields)
    in
    if state.State.no_fail then (
      log_error ~filename ~state message;
      Ok ())
    else Error message

(** {i .gw} file encoding *)
type encoding = E_utf_8 | E_iso_8859_1

let make_strong_assumption v = Strong v
let make_weak_assumption v = Weak v

(** .gwo file header *)
let magic_gwo = "GnWo000o"

(** Checks a .gwo header and prints fails if header is absent or not compatible. *)
let check_magic fname ic =
  let b = really_input_string ic (String.length magic_gwo) in
  if b <> magic_gwo then
    if String.sub magic_gwo 0 4 = String.sub b 0 4 then
      failwith ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
    else
      failwith
        ("\"" ^ fname
       ^ "\" is not a GeneWeb object file, or it is a very old version")

(** [copy_decode s i1 i2] decode the word delimited by [i1] and [i2] inside [s]
    by remplacing "\\" -> '\' and '_' -> ' ' *)
let copy_decode s i1 i2 =
  let len =
    let rec loop len i =
      if i >= i2 then len
      else if i = i2 - 1 then len + 1
      else if s.[i] = '\\' then loop (len + 1) (i + 2)
      else loop (len + 1) (i + 1)
    in
    loop 0 i1
  in
  let rec loop_copy t i j =
    if i >= i2 then Bytes.unsafe_to_string t
    else if i = i2 - 1 && s.[i] <> '_' then (
      Bytes.set t j s.[i];
      Bytes.unsafe_to_string t)
    else
      let c, i =
        match s.[i] with
        | '_' -> (' ', i)
        | '\\' -> (s.[i + 1], i + 1)
        | x -> (x, i)
      in
      Bytes.set t j c;
      loop_copy t (succ i) (succ j)
  in
  loop_copy (Bytes.create len) i1 0

(** Return list of words inside the [str] *)
let fields str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
      | ' ' | '\t' ->
          if beg = i then loop (succ beg) (succ i)
          else copy_decode str beg i :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [ copy_decode str beg i ]
  in
  loop 0 0

(** Removes spaces at the begining an at the end of string. *)
let cut_space x =
  let len = String.length x in
  if len = 0 then x
  else if x = " " then ""
  else
    let start = if x.[0] = ' ' then 1 else 0 in
    let stop = if x.[len - 1] = ' ' then len - 1 else len in
    if start = 0 && stop = len then x else String.sub x start (stop - start)

(** Returns field if its label [lab] is first element of [l] *)
let get_field lab l =
  match l with
  | lab1 :: x :: l' when lab1 = lab -> (cut_space x, l')
  | _ -> ("", l)

(** Parses [Def.date] from string that starts at pos [i]
    inside [s] *)
let date_of_string s i =
  let champ i =
    let neg, i =
      if i < String.length s && s.[i] = '-' then (true, i + 1) else (false, i)
    in
    let rec loop i n =
      if i = String.length s then ((if neg then -n else n), i)
      else
        match s.[i] with
        | '0' .. '9' as c ->
            loop (succ i) ((10 * n) + Char.code c - Char.code '0')
        | _ -> ((if neg then -n else n), i)
    in
    loop i 0
  in
  let skip_slash i =
    if i < String.length s && s.[i] = '/' then Some (succ i) else None
  in
  let precision, i =
    match s.[i] with
    | '~' -> (Date.About, succ i)
    | '?' -> (Maybe, succ i)
    | '>' -> (After, succ i)
    | '<' -> (Before, succ i)
    | _ -> (Sure, i)
  in
  let undefined, year, i =
    let year, j = champ i in
    if j = i + 1 && s.[i] = '0' then (true, year, j) else (false, year, j)
  in
  let error n = failwith (Printf.sprintf "date_of_string%d %s" n s) in
  let dmy2 year2 i =
    match skip_slash i with
    | None -> ((0, 0, year2), i)
    | Some i -> (
        let month2 = year2 in
        let year2, i = champ i in
        match skip_slash i with
        | Some i ->
            let day2 = month2 in
            let month2 = year2 in
            let year2, i = champ i in
            if month2 < 1 || month2 > 13 then error 2
            else if day2 < 1 || day2 > 31 then error 3
            else ((day2, month2, year2), i)
        | None ->
            if month2 < 1 || month2 > 13 then error 4
            else ((0, month2, year2), i))
  in
  let date =
    match skip_slash i with
    | Some i -> (
        let month = year in
        let year, i = champ i in
        match skip_slash i with
        | Some i ->
            let day = month in
            let month = year in
            let year, i = champ i in
            (*
                        if year = 0 then if i = String.length s then None else error 1
                        else
            *)
            if month < 1 || month > 13 then error 2
            else if day < 1 || day > 31 then error 3
            else
              let d = Date.{ day; month; year; prec = precision; delta = 0 } in
              Some (Date.Dgreg (d, Dgregorian), i)
        | None ->
            if year = 0 then None
            else if month < 1 || month > 13 then error 4
            else
              let d =
                Date.{ day = 0; month; year; prec = precision; delta = 0 }
              in
              Some (Dgreg (d, Dgregorian), i))
    | None ->
        if undefined then
          if i = String.length s then None
          else if s.[i] = '(' && s.[String.length s - 1] = ')' then
            let txt = String.sub s (i + 1) (String.length s - i - 2) in
            let txt = cut_space txt in
            Some (Dtext txt, String.length s)
          else failwith ("date_of_string " ^ s)
        else
          let d =
            Date.{ day = 0; month = 0; year; prec = precision; delta = 0 }
          in
          Some (Dgreg (d, Dgregorian), i)
  in
  let date =
    match date with
    | Some ((Dgreg (d, cal) as dt), i) ->
        if i = String.length s then Some (dt, i)
        else if s.[i] = '|' then
          let year2, i = champ (succ i) in
          let (day2, month2, year2), i = dmy2 year2 i in
          let dmy2 = Date.{ day2; month2; year2; delta2 = 0 } in
          Some (Dgreg ({ d with prec = OrYear dmy2 }, cal), i)
        else if i + 1 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
          let year2, i = champ (i + 2) in
          let (day2, month2, year2), i = dmy2 year2 i in
          let dmy2 = Date.{ day2; month2; year2; delta2 = 0 } in
          Some (Dgreg ({ d with prec = YearInt dmy2 }, cal), i)
        else Some (dt, i)
    | Some ((Dtext _ as dt), i) -> Some (dt, i)
    | None -> None
  in
  let date =
    match date with
    | Some (Dgreg (d, _), i) -> (
        if i = String.length s then Some (Date.Dgreg (d, Dgregorian), i)
        else
          match s.[i] with
          | 'G' -> Some (Dgreg (d, Dgregorian), i + 1)
          | 'J' ->
              let d = Date.convert ~from:Djulian ~to_:Dgregorian d in
              Some (Dgreg (d, Djulian), i + 1)
          | 'F' ->
              let d = Date.convert ~from:Dfrench ~to_:Dgregorian d in
              Some (Dgreg (d, Dfrench), i + 1)
          | 'H' ->
              let d = Date.convert ~from:Dhebrew ~to_:Dgregorian d in
              Some (Dgreg (d, Dhebrew), i + 1)
          | 'I' ->
              let d = Date.convert ~from:Dislamic ~to_:Dgregorian d in
              Some (Dgreg (d, Dislamic), i + 1)
          | _ -> Some (Dgreg (d, Dgregorian), i))
    | d -> d
  in
  match date with
  | Some (dt, i) -> if i = String.length s then Some dt else error 5
  | None -> None

(** Read line from input channel. *)
let input_line0 state ic =
  let line = input_line ic in
  state.State.line_cnt <- succ state.State.line_cnt;
  if String.length line > 0 && line.[String.length line - 1] = '\r' then
    String.sub line 0 (String.length line - 1)
  else line

(** Read a line and convert it to [encoding]. *)
let input_a_line state (ic, encoding) =
  let line = input_line0 state ic in
  match encoding with
  | E_utf_8 -> line
  | E_iso_8859_1 -> Utf8.utf_8_of_iso_8859_1 line

(** Read a line. If line is empty or only contains a comment, then read next line  *)
let rec input_real_line state ic =
  let x = input_a_line state ic in
  if x = "" || x.[0] = '#' then input_real_line state ic else x

(** Parses person's birth date if it is present. *)
let get_optional_birthdate l =
  match l with
  | x :: l' -> (
      let i = 0 in
      if x.[i] = '!' then (None, l)
      else
        match x.[i] with
        | '~' | '?' | '<' | '>' | '-' | '0' .. '9' ->
            let d = date_of_string x i in
            (Some d, l')
        | _ -> (None, l))
  | _ -> (None, l)

(** Parses person's baptism date if it is present. *)
let get_optional_baptdate l =
  match l with
  | x :: l' ->
      let i = 0 in
      if x.[i] = '!' then
        let i = succ i in
        match x.[i] with
        | '~' | '?' | '<' | '>' | '-' | '0' .. '9' ->
            let d = date_of_string x i in
            (Some d, l')
        | _ -> (None, l)
      else (None, l)
  | _ -> (None, l)

(** Parse death information if present. *)
let get_optional_deathdate l =
  match l with
  | "?" :: l' -> (Some Def.DontKnowIfDead, l')
  | "mj" :: l' -> (Some DeadYoung, l')
  | "od" :: l' -> (Some OfCourseDead, l')
  | x :: l' ->
      let i = 0 in
      let dr, i =
        match x.[i] with
        | 'k' -> (Def.Killed, i + 1)
        | 'm' -> (Murdered, i + 1)
        | 'e' -> (Executed, i + 1)
        | 's' -> (Disappeared, i + 1)
        | _ -> (Unspecified, i)
      in
      if i < String.length x then
        match x.[i] with
        | '~' | '?' | '>' | '<' | '-' | '0' .. '9' ->
            let d =
              match date_of_string x i with
              | None -> Def.DeadDontKnowWhen
              | Some d -> Death (dr, Date.cdate_of_date d)
            in
            (Some d, l')
        | _ -> (None, l)
      else (None, l)
  | _ -> (None, l)

(** Parse burial information if present. *)
let get_burial l =
  match l with
  | "#buri" :: l -> (
      match l with
      | x :: l' ->
          let i = 0 in
          let od, l =
            match x.[i] with
            | '~' | '?' | '>' | '<' | '-' | '0' .. '9' ->
                (date_of_string x i, l')
            | _ -> (None, l)
          in
          (Def.Buried (Date.cdate_of_od od), l)
      | [] -> (Buried Date.cdate_None, l))
  | "#crem" :: l -> (
      match l with
      | x :: l' ->
          let i = 0 in
          let od, l =
            match x.[i] with
            | '~' | '?' | '>' | '<' | '-' | '0' .. '9' ->
                (date_of_string x i, l')
            | _ -> (None, l)
          in
          (Cremated (Date.cdate_of_od od), l)
      | [] -> (Cremated Date.cdate_None, l))
  | _ -> (UnknownBurial, l)

(** Parse sex of person *)
let get_optional_sexe = function
  | "h" :: l -> (Def.Male, l)
  | "f" :: l -> (Female, l)
  | l -> (Neuter, l)

(** Parses int that starts at the position [i] inside [x].
    Raises [Not_found] if integer isn't found. *)
let make_int x =
  let rec loop found n i =
    if i = String.length x then if found then n else raise Not_found
    else
      match x.[i] with
      | '0' .. '9' as c ->
          loop true ((10 * n) + Char.code c - Char.code '0') (succ i)
      | _ -> raise Not_found
  in
  loop false 0

(** Parses person's first name and occurence number.
    Occurence number is 0 if not present. *)
let get_fst_name str l =
  match l with
  | x :: l' -> (
      match x.[0] with
      (*'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' *)
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '\xE0' .. '\xFF'
      | '\xC0' .. '\xDD'
      | '['
      | '0' .. '9'
      | '?' | ' ' ->
          let x = cut_space x in
          let x, occ =
            match String.rindex_opt x '.' with
            | Some i -> (
                try (String.sub x 0 i, make_int x (succ i))
                with Not_found -> (x, 0))
            | None -> (x, 0)
          in
          Ok (x, occ, l')
      | _ -> Error str)
  | _ -> Error str

(** Parses person's first name aliases if they are present *)
let rec get_fst_names_aliases str l =
  match l with
  | x :: l' ->
      if x.[0] = '{' && x.[String.length x - 1] = '}' then
        let n = String.sub x 1 (String.length x - 2) in
        let nl, l = get_fst_names_aliases str l' in
        (cut_space n :: nl, l)
      else ([], l)
  | [] -> ([], l)

(** Parses person's surname aliases if they are present *)
let rec get_surnames_aliases str l =
  match l with
  | "#salias" :: x :: l' ->
      let nl, l = get_surnames_aliases str l' in
      (cut_space x :: nl, l)
  | _ -> ([], l)

(** Parses person's qualifiers if they are present *)
let rec get_qualifiers str l =
  match l with
  | "#nick" :: x :: l' ->
      let nl, l = get_qualifiers str l' in
      (cut_space x :: nl, l)
  | _ -> ([], l)

(** Parses person's aliases if they are present *)
let rec get_aliases str l =
  match l with
  | "#alias" :: x :: l' ->
      let nl, l = get_aliases str l' in
      (cut_space x :: nl, l)
  | _ -> ([], l)

(** [get_name l] parses a last name. Looks up first element of the list and returns a
    [(name,rest)] couple where [name] is a person's last name and [rest] is a tail of
    the list. If first element is [#nick], [#alias] start with '{' returns empty string
    and list unchanged. *)
let get_name l =
  match l with
  | "#nick" :: _ | "#alias" :: _ -> ("", l)
  | x :: l' -> (
      match x.[0] with
      | '{' -> ("", l)
      (*'a'..'z' | 'A'..'Z' | 'à'..'ÿ' | 'À'..'Ý' *)
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '\xE0' .. '\xFF'
      | '\xC0' .. '\xDD'
      | '0' .. '9'
      | '?' | ' ' ->
          (cut_space x, l')
      | _ -> ("", l))
  | _ -> ("", l)

(** Parses person's public name if present *)
let get_pub_name l =
  match l with
  | x :: l' ->
      if x.[0] = '(' && x.[String.length x - 1] = ')' then
        let a = String.sub x 1 (String.length x - 2) in
        (cut_space a, l')
      else ("", l)
  | _ -> ("", l)

(** Parses person's image path if present *)
let get_image state l =
  match l with
  | ("#image" | "#photo") :: x :: l' ->
      if state.State.no_picture then ("", l') else (cut_space x, l')
  | _ -> ("", l)

(** Parses person's occupation if present *)
let get_occu l =
  match l with "#occu" :: x :: l' -> (cut_space x, l') | _ -> ("", l)

(** Parses person's source if present *)
let get_sources l =
  match l with "#src" :: x :: l' -> (cut_space x, l') | _ -> ("", l)

(** Parses person's acces rights *)
let get_access l =
  match l with
  | "#apubl" :: l' -> (Def.Public, l')
  | "#apriv" :: l' -> (Private, l')
  | "#semipub" :: l' -> (IfTitles, l')
  | _ -> (IfTitles, l)

(** Create [gen_title] from string *)
let scan_title t =
  let next_field i =
    let rec loop s i =
      if i < String.length t then
        match t.[i] with
        | ':' -> (s, i + 1)
        | '\\' -> loop (s ^ String.make 1 t.[i + 1]) (i + 2)
        | c -> loop (s ^ String.make 1 c) (i + 1)
      else (s, i)
    in
    loop "" i
  in
  let i = 0 in
  let name, i =
    let s, i = next_field i in
    if i = String.length t then failwith t else (s, i)
  in
  let name =
    match name with "" -> Def.Tnone | "*" -> Tmain | _ -> Tname name
  in
  let title, i =
    let s, i = next_field i in
    if t.[i - 1] <> ':' then failwith t else (s, i)
  in
  let place, i = next_field i in
  let date_start, i =
    let d, i = next_field i in
    ((if d = "" then None else date_of_string d 0), i)
  in
  let date_end, i =
    let d, i = next_field i in
    ((if d = "" then None else date_of_string d 0), i)
  in
  let nth, i =
    let d, i = next_field i in
    ((if d = "" then 0 else int_of_string d), i)
  in
  if i <> String.length t then failwith t
  else
    {
      Def.t_name = name;
      t_ident = title;
      t_place = place;
      t_date_start = Date.cdate_of_od date_start;
      t_date_end = Date.cdate_of_od date_end;
      t_nth = nth;
    }

(** Parses list of titles ([gen_title]) if they are present. *)
let rec get_titles str l =
  match l with
  | x :: l' ->
      if x.[0] = '[' && x.[String.length x - 1] = ']' then
        let t = String.sub x 1 (String.length x - 2) in
        let t = scan_title t in
        let al, l' = get_titles str l' in
        ((if t.t_ident = "" then al else t :: al), l')
      else ([], l)
  | _ -> ([], l)

(** Parses person's event name *)
let get_pevent_name str l =
  match l with
  | "#birt" :: l' -> (Def.Epers_Birth, l')
  | "#bapt" :: l' -> (Epers_Baptism, l')
  | "#deat" :: l' -> (Epers_Death, l')
  | "#buri" :: l' -> (Epers_Burial, l')
  | "#crem" :: l' -> (Epers_Cremation, l')
  | "#acco" :: l' -> (Epers_Accomplishment, l')
  | "#acqu" :: l' -> (Epers_Acquisition, l')
  | "#adhe" :: l' -> (Epers_Adhesion, l')
  | "#awar" :: l' -> (Epers_Decoration, l')
  | "#bapl" :: l' -> (Epers_BaptismLDS, l')
  | "#barm" :: l' -> (Epers_BarMitzvah, l')
  | "#basm" :: l' -> (Epers_BatMitzvah, l')
  | "#bles" :: l' -> (Epers_Benediction, l')
  | "#cens" :: l' -> (Epers_Recensement, l')
  | "#chgn" :: l' -> (Epers_ChangeName, l')
  | "#circ" :: l' -> (Epers_Circumcision, l')
  | "#conf" :: l' -> (Epers_Confirmation, l')
  | "#conl" :: l' -> (Epers_ConfirmationLDS, l')
  | "#degr" :: l' -> (Epers_Diploma, l')
  | "#demm" :: l' -> (Epers_DemobilisationMilitaire, l')
  | "#dist" :: l' -> (Epers_Distinction, l')
  | "#dotl" :: l' -> (Epers_DotationLDS, l')
  | "#educ" :: l' -> (Epers_Education, l')
  | "#elec" :: l' -> (Epers_Election, l')
  | "#emig" :: l' -> (Epers_Emigration, l')
  | "#endl" :: l' -> (Epers_Dotation, l')
  | "#exco" :: l' -> (Epers_Excommunication, l')
  | "#fcom" :: l' -> (Epers_FirstCommunion, l')
  | "#flkl" :: l' -> (Epers_FamilyLinkLDS, l')
  | "#fune" :: l' -> (Epers_Funeral, l')
  | "#grad" :: l' -> (Epers_Graduate, l')
  | "#hosp" :: l' -> (Epers_Hospitalisation, l')
  | "#illn" :: l' -> (Epers_Illness, l')
  | "#immi" :: l' -> (Epers_Immigration, l')
  | "#lpas" :: l' -> (Epers_ListePassenger, l')
  | "#mdis" :: l' -> (Epers_MilitaryDistinction, l')
  | "#mobm" :: l' -> (Epers_MobilisationMilitaire, l')
  | "#mpro" :: l' -> (Epers_MilitaryPromotion, l')
  | "#mser" :: l' -> (Epers_MilitaryService, l')
  | "#natu" :: l' -> (Epers_Naturalisation, l')
  | "#occu" :: l' -> (Epers_Occupation, l')
  | "#ordn" :: l' -> (Epers_Ordination, l')
  | "#prop" :: l' -> (Epers_Property, l')
  | "#resi" :: l' -> (Epers_Residence, l')
  | "#reti" :: l' -> (Epers_Retired, l')
  | "#slgc" :: l' -> (Epers_ScellentChildLDS, l')
  | "#slgp" :: l' -> (Epers_ScellentParentLDS, l')
  | "#slgs" :: l' -> (Epers_ScellentSpouseLDS, l')
  | "#vteb" :: l' -> (Epers_VenteBien, l')
  | "#will" :: l' -> (Epers_Will, l')
  | s :: l' ->
      if s.[0] = '#' then (Epers_Name (String.sub s 1 (String.length s - 1)), l')
      else failwith str
  | _ -> failwith str

(** Parses family event name *)
let get_fevent_name str l =
  match l with
  | "#marr" :: l' -> (Def.Efam_Marriage, l')
  | "#nmar" :: l' -> (Efam_NoMarriage, l')
  | "#nmen" :: l' -> (Efam_NoMention, l')
  | "#enga" :: l' -> (Efam_Engage, l')
  | "#div" :: l' -> (Efam_Divorce, l')
  | "#sep" :: l' -> (Efam_Separated, l')
  | "#anul" :: l' -> (Efam_Annulation, l')
  | "#marb" :: l' -> (Efam_MarriageBann, l')
  | "#marc" :: l' -> (Efam_MarriageContract, l')
  | "#marl" :: l' -> (Efam_MarriageLicense, l')
  | "#pacs" :: l' -> (Efam_PACS, l')
  | "#resi" :: l' -> (Efam_Residence, l')
  | s :: l' ->
      if s.[0] = '#' then (Efam_Name (String.sub s 1 (String.length s - 1)), l')
      else failwith str
  | _ -> failwith str

(** Parses event date if it is present. *)
let get_optional_event_date l =
  match l with
  | x :: l' -> (
      let i = 0 in
      if x.[i] = '!' then (None, l)
      else
        match x.[i] with
        | '~' | '?' | '<' | '>' | '-' | '0' .. '9' ->
            let d = date_of_string x i in
            (Some d, l')
        | _ -> (None, l))
  | _ -> (None, l)

(** Parse witness kind *)
let get_event_witness_kind l =
  match l with
  | "#godp" :: l' -> (Def.Witness_GodParent, l')
  | "#offi" :: l' -> (Witness_CivilOfficer, l')
  | "#reli" :: l' -> (Witness_ReligiousOfficer, l')
  | "#info" :: l' -> (Witness_Informant, l')
  | "#atte" :: l' -> (Witness_Attending, l')
  | "#ment" :: l' -> (Witness_Mentioned, l')
  | "#othe" :: l' -> (Witness_Other, l')
  | _ -> (Witness, l)

(** Parses the line containing an information about relationship between parents within family
    and returns [((relk, fath_sex, moth_sex), mar, place, note, src, divorce, rest)].
    [relk] i a relation kind between parents ([Def.relation_kind]), [fath_sex] and [moth_sex]
    is a sex of each parent, [mar] is a optional mariage date (if married), [place] is a
    marriage place if present, [note] is a mariage note if present, [src] is a mariage source
    if present, [divorce] is a divorce status [Def.divorce], [rest] is the rest of the line to
    parse
*)
let get_mar_date str = function
  | x :: l ->
      let mar, l =
        match x.[0] with
        | '+' ->
            ( (if String.length x > 1 then Date.cdate_of_od (date_of_string x 1)
              else Date.cdate_None),
              l )
        | _ -> failwith str
      in
      let relation, l =
        let decode_sex v c l =
          let decode_sex i =
            match c.[i] with
            | 'm' -> make_strong_assumption Def.Male
            | 'f' -> make_strong_assumption Def.Female
            | '?' -> make_strong_assumption Def.Neuter
            | _ -> failwith __LOC__
          in
          try ((v, decode_sex 0, decode_sex 1), l)
          with _ ->
            ( (v, make_weak_assumption Def.Male, make_weak_assumption Def.Female),
              c :: l )
        in
        match l with
        | "#nm" :: l ->
            ( ( Def.NotMarried,
                make_weak_assumption Def.Male,
                make_weak_assumption Def.Female ),
              l )
        | "#eng" :: l ->
            ( ( Engaged,
                make_weak_assumption Def.Male,
                make_weak_assumption Def.Female ),
              l )
        | "#noment" :: c :: l when String.length c = 2 ->
            decode_sex Def.NoMention c l
        | "#noment" :: l ->
            ( ( NoMention,
                make_weak_assumption Def.Male,
                make_weak_assumption Def.Female ),
              l )
        | "#nsck" :: c :: l when String.length c = 2 ->
            decode_sex Def.NoSexesCheckNotMarried c l
        | "#nsckm" :: c :: l when String.length c = 2 ->
            decode_sex Def.NoSexesCheckMarried c l
        | "#banns" :: c :: l when String.length c = 2 ->
            decode_sex Def.MarriageBann c l
        | "#contract" :: c :: l when String.length c = 2 ->
            decode_sex Def.MarriageContract c l
        | "#license" :: c :: l when String.length c = 2 ->
            decode_sex Def.MarriageLicense c l
        | "#pacs" :: c :: l when String.length c = 2 -> decode_sex Def.Pacs c l
        | "#residence" :: c :: l when String.length c = 2 ->
            decode_sex Def.Residence c l
        | _ ->
            ( ( Married,
                make_weak_assumption Def.Male,
                make_weak_assumption Def.Female ),
              l )
      in
      let place, l = get_field "#mp" l in
      let note, l = get_field "#mn" l in
      let src, l = get_field "#ms" l in
      let divorce, l =
        match l with
        | x :: l when x.[0] = '-' ->
            if String.length x > 1 then
              (Def.Divorced (Date.cdate_of_od (date_of_string x 1)), l)
            else (Divorced Date.cdate_None, l)
        | "#sep" :: l -> (Separated, l)
        | _ -> (NotDivorced, l)
      in
      (relation, mar, place, note, src, divorce, l)
  | [] -> failwith str

(** Read and return a line with list of words that appears on this line. If
    reading raises [Enf_of_file] returns [None] *)
let read_line state ic =
  try
    let str = input_real_line state ic in
    Some (str, fields str)
  with End_of_file -> None

(** Create a dummy [gen_person]. *)
let create_person () =
  { (Mutil.empty_person "" "") with key_index = Gwdb.dummy_iper }

(** Person is unknown (bogus definition) *)
let bogus_def p n = p = "?" || n = "?"

(** Parse the line and create person's [gen_person] definition.
    Doesn't modify following personal information:
    - Key
    - Parents
    - Related persons
    - Events
    - Notes
    If can't parse person's sources use [comm_psources] instead.
    If can't parse bithdate use [comm_birth_place] instead. *)
let set_infos state fn sn occ sex comm_psources comm_birth_place str u l =
  let first_names_aliases, l = get_fst_names_aliases str l in
  let surnames_aliases, l = get_surnames_aliases str l in
  let public_name, l = get_pub_name l in
  let image, l = get_image state l in
  let qualifiers, l = get_qualifiers str l in
  let aliases, l = get_aliases str l in
  let titles, l = get_titles str l in
  let access, l = get_access l in
  let occupation, l = get_occu l in
  let psources, l = get_sources l in
  let naissance, l = get_optional_birthdate l in
  let birth_place, l = get_field "#bp" l in
  let birth_note, l = get_field "#bn" l in
  let birth_src, l = get_field "#bs" l in
  let baptism, l = get_optional_baptdate l in
  let baptism_place, l =
    let pp, l = get_field "#pp" l in
    (* if no baptism place then it's equals to birth place *)
    if pp = "" then get_field "#bp" l else (pp, l)
  in
  let bapt_note, l = get_field "#pn" l in
  let bapt_src, l = get_field "#ps" l in
  let mort, l = get_optional_deathdate l in
  let death_place, l = get_field "#dp" l in
  let death_note, l = get_field "#dn" l in
  let death_src, l = get_field "#ds" l in
  let mort =
    match (naissance, mort) with
    | None, _ | _, Some _ | Some None, _ -> (
        match mort with Some m -> m | None -> DontKnowIfDead)
    | Some _, None -> NotDead
  in
  let naissance =
    match naissance with
    | None -> Date.cdate_None
    | Some x -> Date.cdate_of_od x
  in
  let baptism =
    match baptism with None -> Date.cdate_None | Some x -> Date.cdate_of_od x
  in
  let burial, l = get_burial l in
  let burial_place, l = get_field "#rp" l in
  let burial_note, l = get_field "#rn" l in
  let burial_src, l = get_field "#rs" l in
  let u =
    {
      Def.first_name = fn;
      surname = sn;
      occ;
      rparents = u.Def.rparents;
      related = u.related;
      sex;
      notes = u.notes;
      key_index = u.key_index;
      first_names_aliases;
      surnames_aliases;
      public_name;
      image;
      qualifiers;
      aliases;
      titles;
      access;
      occupation;
      psources = (if psources <> "" then psources else comm_psources);
      birth = naissance;
      birth_place =
        (if birth_place <> "" then birth_place else comm_birth_place);
      birth_note;
      birth_src;
      baptism;
      baptism_place;
      baptism_note = bapt_note;
      baptism_src = bapt_src;
      death = mort;
      death_place;
      death_note;
      death_src;
      burial;
      burial_place;
      burial_note;
      burial_src;
      pevents = u.pevents;
    }
  in
  (u, l)

(** Parses the line containing a parent and returns [(somebody,np,rest)]. [somebody] is either [Defined p] if
    person's definiton was parsed ([p] regroups all personal information) either [Undefined k] if a reference
    to a person already defined was parsed ([k] is a key to find corresponding definition). [np] is a person's
    surname. [rest] is a rest of line to parse. Could be used to parse familial witnesses. *)
let parse_parent state str l =
  (* last name *)
  let np, l = get_name l in
  Result.map
    (fun (* first name and occurence number *)
           (pp, op, l) ->
      (* person is not defined as a child elsewhere (is defined here) *)
      let defined =
        if bogus_def pp np then true
        else
          match l with
          | [] -> false
          | s :: _ when s.[0] = '+' -> false
          | _ -> true
      in
      if not defined then
        let key = { pk_first_name = pp; pk_surname = np; pk_occ = op } in
        (Undefined key, np, l)
      else
        let u = create_person () in
        let u, l = set_infos state pp np op u.sex "" "" str u l in
        (Defined u, np, l))
    (get_fst_name str l)

(** Parses the line containing a children and returns a person [gen_person] containing
    all extracted information. If a children definition doesn't provide
    surname then father's surname is used. ALso if it doesn't provide a children's
    birth place and source then it uses information provided by family definiton. *)
let parse_child state str surname sex csrc cbp l =
  let u = create_person () in
  Result.map
    (fun (prenom, occ, l) ->
      let nom, l =
        match l with
        | "?" :: _ -> get_name l
        | x :: _ -> (
            match x.[0] with
            | '<' | '>' | '!' | '~' | '?' | '-' | '0' .. '9' | '{' | '#' ->
                (surname, l)
            | '(' | '[' -> ((if prenom = "" then "" else surname), l)
            | _ -> get_name l)
        | _ -> (surname, [])
      in
      set_infos state prenom nom occ sex csrc cbp str u l)
    (get_fst_name str l)

(** Parse relation type [Def.gen_relation] with a person outside of family block
    (foster parents, god parent, etc.). *)
let get_relation ~filename state str =
  let ( >>= ) = Result.bind in
  let ensure_end_of_line fields = ensure_end_of_line ~filename ~state fields in
  function
  | "-" :: x :: l -> (
      (match x with
      | "adop" | "adop:" -> Ok Def.Adoption
      | "reco" | "reco:" -> Ok Recognition
      | "cand" | "cand:" -> Ok CandidateParent
      | "godp" | "godp:" -> Ok GodParent
      | "fost" | "fost:" -> Ok FosterParent
      | _ -> Error str)
      >>= fun rtyp ->
      if String.length x = 5 && x.[4] = ':' then
        parse_parent state str l >>= fun (fk, _, l) ->
        (match l with "+" :: l -> Ok l | _ -> Error str) >>= fun l ->
        parse_parent state str l >>= fun (mk, _, l) ->
        Result.map
          (fun () ->
            {
              Def.r_type = rtyp;
              r_fath = Some fk;
              r_moth = Some mk;
              r_sources = "";
            })
          (ensure_end_of_line l)
      else
        match l with
        | "fath:" :: l ->
            parse_parent state str l >>= fun (fk, _, l) ->
            Result.map
              (fun () ->
                {
                  Def.r_type = rtyp;
                  r_fath = Some fk;
                  r_moth = None;
                  r_sources = "";
                })
              (ensure_end_of_line l)
        | "moth:" :: l ->
            parse_parent state str l >>= fun (mk, _, l) ->
            Result.map
              (fun () ->
                {
                  Def.r_type = rtyp;
                  r_fath = None;
                  r_moth = Some mk;
                  r_sources = "";
                })
              (ensure_end_of_line l)
        | _ -> Error str)
  | _ -> Error str

(** Read notes of a person inside [note] block across multiple lines and
    concat them. *)
let read_notes state ic =
  let notes =
    try
      let rec loop = function
        | "end notes" -> ""
        | l -> l ^ "\n" ^ loop (input_a_line state ic)
      in
      loop (input_a_line state ic)
    with End_of_file -> failwith "end of file"
  in
  Ext_string.strip_all_trailing_spaces notes

(* from version 5.00 *)

(** Read database notes across multiple lines and concat them. Stop reading when
    encounter [end_text] *)
let read_notes_db state ic end_txt =
  let notes =
    try
      let rec loop s =
        if s = end_txt then ""
        else
          let len = String.length s in
          let s =
            if len > 2 && s.[0] = ' ' && s.[1] = ' ' then
              String.sub s 2 (len - 2)
            else s
          in
          s ^ "\n" ^ loop (input_a_line state ic)
      in
      loop (input_a_line state ic)
    with End_of_file -> failwith "end of file"
  in
  Ext_string.strip_all_trailing_spaces notes

(** Parsing status of .gw block  *)
type 'a read_family =
  | F_some of 'a  (** Read block inside .gw file *)
  | F_enc_utf_8  (** Read block that defines that file use utf-8 encoding *)
  | F_gw_plus  (** Read block that defines that the file uses gwplus syntax *)
  | F_none  (** Read end of the file *)

(** Read succesive lines starting with `tag` and concat them. *)
let aux_loop_note state tag line ic =
  let tag_len = String.length tag in
  let rec loop acc str =
    match fields str with
    | s :: tl when s = tag ->
        let note =
          if tl = [] then ""
          else String.sub str (tag_len + 1) (String.length str - tag_len - 1)
        in
        loop (note :: acc) (input_a_line state ic)
    | _l -> (acc, str)
  in
  let acc, line = loop [] line in
  let note =
    String.concat "\n" (List.rev @@ ("" :: acc))
    |> Ext_string.strip_all_trailing_spaces
  in
  (note, line)

(** Parse note (succesive lines starting with "note") *)
let loop_note state = aux_loop_note state "note"

(** Parse wintess note (succesive lines starting with "wnote") *)
let loop_witness_note state = aux_loop_note state "wnote"

(** Parse comment (succesive lines starting with "comm") *)
let loop_comment state = aux_loop_note state "comm"

(** Parse witnesses across the lines and returns list of [(wit,wsex,wk,wnote)]
    where wit is a witness definition/reference, [wsex] is a sex of witness
    , [wk] is a kind of witness relationship to the family, [wnote] is a witness note. *)
let loop_witn ~filename state line ic =
  let rec loop_witn acc str =
    let ( >>= ) = Result.bind in
    match fields str with
    | ("wit" | "wit:") :: l ->
        let sex, l =
          (* TODO factorize sex parsing? *)
          match l with
          | "m:" :: l -> (make_strong_assumption Def.Male, l)
          | "f:" :: l -> (make_strong_assumption Def.Female, l)
          | l -> (make_weak_assumption Def.Neuter, l)
        in
        let wkind, l = get_event_witness_kind l in
        parse_parent state str l >>= fun (wit, _, l) ->
        ensure_end_of_line ~filename ~state l >>= fun () ->
        (* read witness note which starts on a new line *)
        let wnote, str = loop_witness_note state (input_a_line state ic) ic in
        loop_witn ((wit, sex, wkind, wnote) :: acc) str
    | _ -> Ok (List.rev acc, str)
  in
  loop_witn [] line

(** Read and parse a gw file block from [ic]. Returns also next line if it's
    not the end of the file. *)
let read_family state ic fname =
  let ( >>= ) = Result.bind in
  let ensure_end_of_line fields =
    ensure_end_of_line ~filename:fname ~state fields
  in
  function
  (* Block that defines that file use utf-8 encoding *)
  | Some (_, [ "encoding:"; "utf-8" ]) -> Ok F_enc_utf_8
  (* Block that defines that the file uses gwplus syntax *)
  | Some (_, [ "gwplus" ]) -> Ok F_gw_plus
  (* Family block *)
  | Some (str, "fam" :: l) -> (
      (* read father *)
      parse_parent state str l
      >>= fun (fath_key, surname, l) ->
      (* read relation between parents *)
      let relation_ss, marriage, marr_place, marr_note, marr_src, divorce, l =
        get_mar_date str l
      in
      let relation, fath_sex, moth_sex = relation_ss in
      (* read mother *)
      parse_parent state str l >>= fun (moth_key, _, l) ->
      ensure_end_of_line l >>= fun () ->
      let line = read_line state ic in
      (* read list of witnesses with their sex (if exists) *)
      let rec loop = function
        (* TODO duplicate of loop_witn ?*)
        | Some (str, ("wit" | "wit:") :: l) ->
            let sex, l =
              match l with
              | "m:" :: l -> (make_strong_assumption Def.Male, l)
              | "f:" :: l -> (make_strong_assumption Def.Female, l)
              | l -> (make_weak_assumption Def.Neuter, l)
            in
            parse_parent state str l >>= fun (wk, _, l) ->
            ensure_end_of_line l >>= fun () ->
            Result.map
              (fun (witn, line) -> ((wk, sex) :: witn, line))
              (loop (read_line state ic))
        | line -> Ok ([], line)
      in
      loop line >>= fun (witn, line) ->
      (* read familial source if present *)
      (match line with
      | Some (_, [ "src"; x ]) -> Ok (cut_space x, read_line state ic)
      | Some (str, "src" :: _) -> Error str
      | _ -> Ok ("", line))
      >>= fun (fsrc, line) ->
      (* read common children source if present *)
      (match line with
      | Some (_, [ "csrc"; x ]) -> Ok (cut_space x, read_line state ic)
      | Some (str, "csrc" :: _) -> Error str
      | _ -> Ok ("", line))
      >>= fun (csrc, line) ->
      (* read common children birth place if present *)
      (match line with
      | Some (_, [ "cbp"; x ]) -> Ok (cut_space x, read_line state ic)
      | Some (str, "cbp" :: _) -> Error str
      | _ -> Ok ("", line))
      >>= fun (cbp, line) ->
      (* create a couple *)
      let co = Adef.couple fath_key moth_key in
      (* read a family comments *)
      let comm, line =
        match line with
        | Some (str, "comm" :: _) ->
            let comm, next_line = loop_comment state str ic in

            (* duplicate of input_real_line but starting with line [s] *)
            let rec get_next_real_line s =
              if s = "" || s.[0] = '#' then
                get_next_real_line (input_a_line state ic)
              else s
            in

            let next_line = get_next_real_line next_line in
            (comm, Some (next_line, fields next_line))
        | _ -> ("", line)
      in
      (* read family events *)
      (match line with
      | Some (_, "fevt" :: _) ->
          let fevents_and_line =
            let rec loop fevents = function
              | "end fevt" -> Ok (fevents, read_line state ic)
              | x ->
                  let str, l = (x, fields x) in
                  (* On récupère le nom, date, lieu, source, cause *)
                  let name, l = get_fevent_name str l in
                  let date, l = get_optional_event_date l in
                  let place, l = get_field "#p" l in
                  let cause, l = get_field "#c" l in
                  let src, l = get_field "#s" l in
                  let date =
                    match date with
                    | None -> Date.cdate_None
                    | Some x -> Date.cdate_of_od x
                  in
                  ensure_end_of_line l >>= fun () ->
                  (* On récupère les témoins *)
                  loop_witn ~filename:fname state (input_a_line state ic) ic
                  >>= fun (witn, line) ->
                  (* On récupère les notes *)
                  let notes, line = loop_note state line ic in
                  let evt = (name, date, place, cause, src, notes, witn) in
                  loop (evt :: fevents) line
            in
            loop [] (input_a_line state ic)
          in
          Result.map
            (fun (fevents, line) -> (List.rev fevents, line))
            fevents_and_line
      | _ -> Ok ([], line))
      >>= fun (fevents, line) ->
      match line with
      (* have children *)
      | Some (_, [ "beg" ]) ->
          let cles_enfants =
            let rec loop children =
              match read_line state ic with
              | Some (str, "-" :: l) ->
                  let sex, l = get_optional_sexe l in
                  parse_child state str surname sex csrc cbp l
                  >>= fun (child, l) ->
                  ensure_end_of_line l >>= fun () -> loop (child :: children)
              | Some (_, [ "end" ]) -> Ok children
              | Some (str, _) -> Error str
              | _ -> Error "eof"
            in
            Result.map List.rev (loop [])
          in
          (* create a family definition (without witnesses, events and family index) *)
          let fo =
            {
              Def.marriage;
              marriage_place = marr_place;
              marriage_note = marr_note;
              marriage_src = marr_src;
              witnesses = [||];
              relation;
              divorce;
              fevents = [];
              comment = comm;
              origin_file = fname;
              fsources = fsrc;
              fam_index = Gwdb.dummy_ifam;
            }
          in
          let deo =
            Result.map
              (fun cles_enfants ->
                { Def.children = Array.of_list cles_enfants })
              cles_enfants
          in
          Result.map
            (fun deo ->
              F_some
                ( Family (co, fath_sex, moth_sex, witn, fevents, fo, deo),
                  read_line state ic ))
            deo
      (* no children *)
      | line ->
          let fo =
            {
              Def.marriage;
              marriage_place = marr_place;
              marriage_note = marr_note;
              marriage_src = marr_src;
              witnesses = [||];
              relation;
              divorce;
              fevents = [];
              comment = comm;
              origin_file = fname;
              fsources = fsrc;
              fam_index = Gwdb.dummy_ifam;
            }
          in
          let deo = { Def.children = [||] } in
          Ok
            (F_some
               (Family (co, fath_sex, moth_sex, witn, fevents, fo, deo), line)))
  (* Database notes block *)
  | Some (_, [ "notes-db" ]) ->
      let notes = read_notes_db state ic "end notes-db" in
      Ok (F_some (Bnotes ("", notes), read_line state ic))
  (* Extended page block *)
  | Some (str, [ "page-ext"; _ ]) ->
      let page_title =
        let len = String.length "page-ext" + 1 in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db state ic "end page-ext" in
      Ok (F_some (Bnotes (page_title, notes), read_line state ic))
  (* Used before version 5.00. Notes block *)
  | Some (_, [ "notes" ]) ->
      let notes = read_notes state ic in
      Ok (F_some (Bnotes ("", notes), read_line state ic))
  (* Notes block *)
  | Some (str, "notes" :: l) -> (
      let surname, l = get_name l in
      get_fst_name str l >>= fun (first_name, occ, l) ->
      ensure_end_of_line l >>= fun () ->
      match read_line state ic with
      | Some (_, [ "beg" ]) ->
          let notes = read_notes state ic in
          let key =
            { pk_first_name = first_name; pk_surname = surname; pk_occ = occ }
          in
          Ok (F_some (Notes (key, notes), read_line state ic))
      | Some (str, _) -> Error str
      | None -> Error "end of file")
  (* Wizard note block *)
  | Some (str, "wizard-note" :: _) ->
      let wizid =
        let len = String.length "wizard-note " in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db state ic "end wizard-note" in
      Ok (F_some (Wnotes (wizid, notes), read_line state ic))
  (* Personal relation block *)
  | Some (str, "rel" :: l) -> (
      (* get considered person *)
      parse_parent state str l >>= fun (sb, _, l) ->
      let sex, l =
        match l with
        | "#h" :: l -> (make_strong_assumption Def.Male, l)
        | "#f" :: l -> (make_strong_assumption Def.Female, l)
        | l -> (make_weak_assumption Def.Neuter, l)
      in
      ensure_end_of_line l >>= fun () ->
      match read_line state ic with
      (* Read list of relations *)
      | Some (_, [ "beg" ]) ->
          let rl =
            try
              let rec loop = function
                | "end" -> Ok []
                | x ->
                    loop (input_a_line state ic) >>= fun relations ->
                    Result.map
                      (fun relation -> relation :: relations)
                      (get_relation ~filename:fname state x (fields x))
              in
              loop (input_a_line state ic)
            with End_of_file -> Error "missing end rel"
          in
          Result.map
            (fun rl -> F_some (Relations (sb, sex, rl), read_line state ic))
            rl
      | Some (str, _) -> Error str
      | None -> Error "end of file")
  (* Person's events block *)
  | Some (str, "pevt" :: l) ->
      (* get considered person *)
      parse_parent state str l >>= fun (sb, _, l) ->
      ensure_end_of_line l >>= fun () ->
      let pevents =
        let rec loop pevents = function
          | "end pevt" -> Ok pevents
          | x ->
              let str, l = (x, fields x) in
              (* On récupère le nom, date, lieu, source, cause *)
              let name, l = get_pevent_name str l in
              let date, l = get_optional_event_date l in
              let place, l = get_field "#p" l in
              let cause, l = get_field "#c" l in
              let src, l = get_field "#s" l in
              let date =
                match date with
                | None -> Date.cdate_None
                | Some x -> Date.cdate_of_od x
              in
              ensure_end_of_line l >>= fun () ->
              (* On récupère les témoins *)
              loop_witn ~filename:fname state (input_a_line state ic) ic
              >>= fun (witn, line) ->
              (* On récupère les notes *)
              let notes, line = loop_note state line ic in
              let evt = (name, date, place, cause, src, notes, witn) in
              loop (evt :: pevents) line
        in
        loop [] (input_a_line state ic)
      in
      let pevents = Result.map List.rev pevents in
      Result.map
        (fun pevents ->
          F_some
            ( Pevent (sb, make_weak_assumption Def.Neuter, pevents),
              read_line state ic ))
        pevents
  | Some (str, _) -> Error str
  (* End of the file *)
  | None -> Ok F_none

(** Compile .gw file and save result to corresponding .gwo *)
let comp_families state x =
  let out_file = Filename.chop_suffix x ".gw" ^ ".gwo" in
  state.State.line_cnt <- 0;
  let oc = open_out_bin out_file in
  (try
     let ic = open_in x in
     (* write header *)
     output_string oc magic_gwo;
     (* write source filename *)
     output_value oc (x : string);
     let rec loop ~data_elements line encoding =
       match read_family state (ic, encoding) x line with
       | Ok (F_some (family, line)) ->
           loop ~data_elements:(family :: data_elements) line encoding
       | Ok F_enc_utf_8 ->
           loop ~data_elements (read_line state (ic, E_utf_8)) E_utf_8
       | Ok F_gw_plus ->
           state.State.create_all_keys <- true;
           loop ~data_elements (read_line state (ic, encoding)) encoding
       | Ok F_none -> data_elements
       | Error str ->
           if state.State.no_fail then (
             log_error ~filename:x ~state str;
             loop ~data_elements (read_line state (ic, encoding)) encoding)
           else failwith str
     in
     let data_elements =
       loop ~data_elements:[] (read_line state (ic, E_iso_8859_1)) E_iso_8859_1
     in
     List.iter
       (fun data_element -> output_value oc (data_element : gw_syntax))
       (List.rev data_elements);
     close_in ic
   with e ->
     close_out oc;
     Files.rm out_file;
     raise e);
  close_out oc

let value_of_assumption = function Strong value | Weak value -> value
