(* Copyright (c) 1998-2007 INRIA *)

open Def
module Driver = Geneweb_db.Driver

let magic_gwo = "GnWo000o"

(* Option qui force a créer les clés des individus. De fait, *)
(* si la clé est incomplète, on l'enregistre tout de même.  *)
let create_all_keys = ref false
let line_cnt = ref 0
let no_fail = ref false
let no_picture = ref false
let rgpd_dir = ref "None"
let rgpd = ref false
let verbose = ref false
let semi_pub_cnt = ref 0
let out_file = ref (Filename.concat Filename.current_dir_name "a")

type key = { pk_first_name : string; pk_surname : string; pk_occ : int }

type somebody =
  | Undefined of key (* Reference to person *)
  | Defined of
      (Driver.iper, Driver.iper, string) gen_person (* Person's definition *)

type gw_syntax =
  | Family of
      somebody gen_couple
      * sex
      * sex
      * (somebody * sex) list
      * (string gen_fam_event_name
        * cdate
        * string
        * string
        * string
        * string
        * (somebody * sex * witness_kind) list)
        list
      * ( (Driver.iper, Driver.iper, string) gen_person,
          Driver.ifam,
          string )
        gen_family
      * (Driver.iper, Driver.iper, string) gen_person gen_descend
      (** Family definition block. Contains:
          - Family couple (father's and mother's definition/reference)
          - Father's sex
          - Mother's sex
          - List of witnesses definition/reference with their sex.
          - List of information about every family event (name, date, place,
            reason, source, notes and witnesses)
          - Family definition
          - Children (descendants) *)
  | Notes of key * string
    (* Block that defines personal notes. First element represents
       reference to person. Second is note's content. *)
  | Relations of somebody * sex * (somebody, string) gen_relation list
    (* Block that defines relations of a person with someone outisde of
       family block (like foster parents) (field {i rparents}). Contains:
       - Concerned person definition/reference
       - Sex of person
       - List of his relations. *)
  | Pevent of
      somebody
      * sex
      * (string gen_pers_event_name
        * cdate
        * string
        * string
        * string
        * string
        * (somebody * sex * witness_kind) list)
        list
      (** Block that defines events of a person. Specific to gwplus format.
          Contains:
          - Concerned person's definition/reference
          - Sex of person
          - List of information about every personal event (name, date, place,
            reason, source, notes and witnesses)*)
  | Bnotes of string * string
    (* Block that defines database notes and extended pages.
       First string represents name of extended page ("" for
       database notes, only one for file). Second is note's
       or page's content. *)
  | Wnotes of string * string
(* Block that defines wizard notes. First string represents
   First string represents wizard's id. Second is note's content. *)

(* {i .gw} file encoding *)
type encoding = E_utf_8 | E_iso_8859_1

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

(** Parses [Def.date] from string that starts at pos [i] inside [s] *)
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
    | '~' -> (About, succ i)
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
    | None -> ((0, 0, year2), i)
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
              let d = { day; month; year; prec = precision; delta = 0 } in
              Some (Dgreg (d, Dgregorian), i)
        | None ->
            if year = 0 then None
            else if month < 1 || month > 13 then error 4
            else
              let d = { day = 0; month; year; prec = precision; delta = 0 } in
              Some (Dgreg (d, Dgregorian), i))
    | None ->
        if undefined then
          if i = String.length s then None
          else if s.[i] = '(' && s.[String.length s - 1] = ')' then
            let txt = String.sub s (i + 1) (String.length s - i - 2) in
            let txt = cut_space txt in
            let txt = copy_decode txt 0 (String.length txt) in
            Some (Dtext txt, String.length s)
          else failwith ("date_of_string " ^ s)
        else
          let d = { day = 0; month = 0; year; prec = precision; delta = 0 } in
          Some (Dgreg (d, Dgregorian), i)
  in
  let date =
    match date with
    | Some ((Dgreg (d, cal) as dt), i) ->
        if i = String.length s then Some (dt, i)
        else if s.[i] = '|' then
          let year2, i = champ (succ i) in
          let (day2, month2, year2), i = dmy2 year2 i in
          let dmy2 = { day2; month2; year2; delta2 = 0 } in
          Some (Dgreg ({ d with prec = OrYear dmy2 }, cal), i)
        else if i + 1 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
          let year2, i = champ (i + 2) in
          let (day2, month2, year2), i = dmy2 year2 i in
          let dmy2 = { day2; month2; year2; delta2 = 0 } in
          Some (Dgreg ({ d with prec = YearInt dmy2 }, cal), i)
        else Some (dt, i)
    | Some ((Dtext _ as dt), i) -> Some (dt, i)
    | None -> None
  in
  let date =
    match date with
    | Some (Dgreg (d, _), i) -> (
        if i = String.length s then Some (Dgreg (d, Dgregorian), i)
        else
          match s.[i] with
          | 'G' -> Some (Dgreg (d, Dgregorian), i + 1)
          | 'J' ->
              let d = Calendar.gregorian_of_julian d in
              Some (Dgreg (d, Djulian), i + 1)
          | 'F' ->
              let d = Calendar.gregorian_of_french d in
              Some (Dgreg (d, Dfrench), i + 1)
          | 'H' ->
              let d = Calendar.gregorian_of_hebrew d in
              Some (Dgreg (d, Dhebrew), i + 1)
          | _ -> Some (Dgreg (d, Dgregorian), i))
    | d -> d
  in
  match date with
  | Some (dt, i) -> if i = String.length s then Some dt else error 5
  | None -> None

(** Read line from input channel. *)
let input_line0 ic =
  let line = input_line ic in
  incr line_cnt;
  if String.length line > 0 && line.[String.length line - 1] = '\r' then
    String.sub line 0 (String.length line - 1)
  else line

(** Read a line and convert it to [encoding]. *)
let input_a_line (ic, encoding) =
  let line = input_line0 ic in
  match encoding with
  | E_utf_8 -> line
  | E_iso_8859_1 -> Mutil.utf_8_of_iso_8859_1 line

(** Read a line. If line is empty or only contains a comment, then read next
    line *)
let rec input_real_line ic =
  let x = input_a_line ic in
  if x = "" || x.[0] = '#' then input_real_line ic else x

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
  | "?" :: l' -> (Some DontKnowIfDead, l')
  | "mj" :: l' -> (Some DeadYoung, l')
  | "od" :: l' -> (Some OfCourseDead, l')
  | x :: l' ->
      let i = 0 in
      let dr, i =
        match x.[i] with
        | 'k' -> (Killed, i + 1)
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
              | None -> DeadDontKnowWhen
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
          (Buried (Date.cdate_of_od od), l)
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
  | "h" :: l -> (Male, l)
  | "f" :: l -> (Female, l)
  | l -> (Neuter, l)

(** Parses int that starts at the position [i] inside [x]. Raises [Not_found] if
    integer isn't found. *)
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

(** Parses person's first name and occurence number. Occurence number is 0 if
    not present. *)
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
          (x, occ, l')
      | _ -> failwith str)
  | _ -> failwith str

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

(** [get_name l] parses a last name. Looks up first element of the list and
    returns a [(name,rest)] couple where [name] is a person's last name and
    [rest] is a tail of the list. If first element is [#nick], [#alias] start
    with '\{' returns empty string and list unchanged. *)
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
let get_image l =
  match l with
  | ("#image" | "#photo") :: x :: l' ->
      if !no_picture then ("", l') else (cut_space x, l')
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
  | "#apubl" :: l' -> (Public, l')
  | "#apriv" :: l' -> (Private, l')
  | "#afriend" :: l' -> (SemiPublic, l') (* for retro compatibility *)
  | "#semipub" :: l' -> (SemiPublic, l')
  | _ -> (IfTitles, l)

(* copied from Some *)
let name_unaccent_lower s =
  let rec copy i len =
    if i = String.length s then Buff.get len
    else
      let t, j = Name.unaccent_utf_8 true s i in
      copy j (Buff.mstore len t)
  in
  copy 0 0

(* read .auth file and build a consent_list of keys *)
let auth_access fn sn oc l =
  let access, l = get_access l in
  let fns = name_unaccent_lower fn |> Mutil.tr ' ' '_' in
  let sns = name_unaccent_lower sn |> Mutil.tr ' ' '_' in
  let frs = if access = SemiPublic then "SemiPublic" else "Other" in
  let bname = Filename.basename !out_file |> Filename.remove_extension in
  let gwf_file =
    if Geneweb.GWPARAM.is_reorg_base bname then
      Geneweb.GWPARAM.config_reorg bname
    else Geneweb.GWPARAM.config_legacy bname
  in
  let auth_file_name =
    try
      Secure.with_open_in_text gwf_file (fun ic ->
          let rec loop () =
            match input_line ic with
            | exception End_of_file -> None
            | line when Geneweb.Util.start_with line 0 "friend_passwd_file" -> (
                match Geneweb.Util.extract_value '=' line with
                | exception Not_found -> None
                | passwd_file -> Some passwd_file)
            | _ -> loop ()
          in
          loop ())
    with Sys_error _ -> None
  in

  let consent_htbl =
    let ht = Hashtbl.create 10 in
    match auth_file_name with
    | Some file_name -> (
        let friend_passwd_file =
          Filename.concat (Secure.base_dir ()) file_name
        in
        try
          Secure.with_open_in_text friend_passwd_file (fun ic ->
              let rec loop ht =
                match input_line ic |> name_unaccent_lower with
                | exception End_of_file -> ht
                | line -> (
                    (* ident:passwd:name[|key]:comment *)
                    let parts = String.split_on_char ':' line in
                    let username =
                      try List.nth parts 2 with Failure _ -> ""
                    in
                    match Geneweb.Util.extract_value '|' username with
                    | exception Not_found -> loop ht
                    | key ->
                        Hashtbl.add ht key key;
                        loop ht)
              in
              loop ht)
        with Sys_error _ ->
          if Sys.file_exists friend_passwd_file then
            Printf.eprintf "Warning: error reading %s\n" friend_passwd_file;
          ht)
    | None -> ht
  in

  let is_consent =
    Hashtbl.mem consent_htbl (Format.sprintf "%s.%d+%s" fns oc sns)
  in
  if access = Public then (Public, l)
  else if is_consent then (
    incr semi_pub_cnt;
    if !verbose then Printf.eprintf "Set to %s %s.%d %s\n" frs fns oc sns;
    (SemiPublic, l))
  else (access, l)

(** test presence of a file fn.occ.sn.pdf in rgpd_dirs *)
let rgpd_access fn sn occ l =
  let access, l = get_access l in
  let fns = name_unaccent_lower fn in
  let sns = name_unaccent_lower sn in
  let ocs = string_of_int occ in
  let access, l =
    let rgpd_file = Filename.concat !rgpd_dir (fns ^ "." ^ ocs ^ "." ^ sns) in
    (* if Public, stay Public *)
    if access = Public then (Public, l)
      (* if the files exist, set to SemiPublic *)
    else if Sys.file_exists (rgpd_file ^ ".pdf") then (SemiPublic, l)
      (* if not and person was SemiPublic, then it becomes Private *)
    else if access = SemiPublic then (Private, l)
      (* otherwise keep the current value *)
    else (access, l)
  in
  if access = SemiPublic then incr semi_pub_cnt;
  (access, l)

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
  let name = match name with "" -> Tnone | "*" -> Tmain | _ -> Tname name in
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
      t_name = name;
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
  | "#birt" :: l' -> (Epers_Birth, l')
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
  | "#marr" :: l' -> (Efam_Marriage, l')
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
  | "#godp" :: l' -> (Witness_GodParent, l')
  | "#offi" :: l' -> (Witness_CivilOfficer, l')
  | "#reli" :: l' -> (Witness_ReligiousOfficer, l')
  | "#info" :: l' -> (Witness_Informant, l')
  | "#atte" :: l' -> (Witness_Attending, l')
  | "#ment" :: l' -> (Witness_Mentioned, l')
  | "#othe" :: l' -> (Witness_Other, l')
  | _ -> (Witness, l)

(** Parses the line containing an information about relationship between parents
    within family and returns
    [((relk, fath_sex, moth_sex), mar, place, note, src, divorce, rest)]. [relk]
    i a relation kind between parents ([Def.relation_kind]), [fath_sex] and
    [moth_sex] is a sex of each parent, [mar] is a optional mariage date (if
    married), [place] is a marriage place if present, [note] is a mariage note
    if present, [src] is a mariage source if present, [divorce] is a divorce
    status [Def.divorce], [rest] is the rest of the line to parse *)
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
            | 'm' -> Male
            | 'f' -> Female
            | '?' -> Neuter
            | _ -> failwith __LOC__
          in
          try ((v, decode_sex 0, decode_sex 1), l)
          with _ -> ((v, Male, Female), c :: l)
        in
        match l with
        | "#nm" :: l' -> ((NotMarried, Male, Female), l')
        | "#eng" :: l' -> ((Engaged, Male, Female), l')
        | "#noment" :: c :: l' when String.length c = 2 ->
            decode_sex NoMention c l'
        | "#noment" :: l' -> ((NoMention, Male, Female), l')
        | "#nsck" :: c :: l' when String.length c = 2 ->
            decode_sex NoSexesCheckNotMarried c l'
        | "#nsckm" :: c :: l' when String.length c = 2 ->
            decode_sex NoSexesCheckMarried c l'
        | "#banns" :: c :: l' when String.length c = 2 ->
            decode_sex MarriageBann c l'
        | "#contract" :: c :: l' when String.length c = 2 ->
            decode_sex MarriageContract c l'
        | "#license" :: c :: l' when String.length c = 2 ->
            decode_sex MarriageLicense c l'
        | "#pacs" :: c :: l' when String.length c = 2 -> decode_sex Pacs c l'
        | "#residence" :: c :: l' when String.length c = 2 ->
            decode_sex Residence c l'
        | _ -> ((Married, Male, Female), l)
      in
      let place, l = get_field "#mp" l in
      let note, l = get_field "#mn" l in
      let src, l = get_field "#ms" l in
      (* we should have an event *)
      let divorce, l =
        match l with
        | x :: l' when x.[0] = '-' ->
            if String.length x > 1 then
              (Divorced (Date.cdate_of_od (date_of_string x 1)), l')
            else (Divorced Date.cdate_None, l')
        | "#sep" :: l' -> (Separated Date.cdate_None, l')
        | _ -> (NotDivorced, l)
      in
      (relation, mar, place, note, src, divorce, l)
  | [] -> failwith str

(** Read and return a line with list of words that appears on this line. If
    reading raises [Enf_of_file] returns [None] *)
let read_line ic =
  try
    let str = input_real_line ic in
    Some (str, fields str)
  with End_of_file -> None

(** Create a dummy [gen_person]. *)
let create_person () =
  { (Mutil.empty_person "" "") with key_index = Driver.Iper.dummy }

(** Person is unknown (bogus definition) *)
let bogus_def p n = p = "?" || n = "?"

(** Parse the line and create person's [gen_person] definition. Doesn't modify
    following personal information:
    - Key
    - Parents
    - Related persons
    - Events
    - Notes If can't parse person's sources use [comm_psources] instead. If
      can't parse bithdate use [comm_birth_place] instead. *)
let set_infos fn sn occ sex comm_psources comm_birth_place str u l =
  let first_names_aliases, l = get_fst_names_aliases str l in
  let surnames_aliases, l = get_surnames_aliases str l in
  let public_name, l = get_pub_name l in
  let image, l = get_image l in
  let qualifiers, l = get_qualifiers str l in
  let aliases, l = get_aliases str l in
  let titles, l = get_titles str l in
  let access, l =
    if !rgpd then rgpd_access fn sn occ l else auth_access fn sn occ l
  in
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
      first_name = fn;
      surname = sn;
      occ;
      rparents = u.rparents;
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

(** Parses the line containing a parent and returns [(somebody,np,rest)].
    [somebody] is either [Defined p] if person's definiton was parsed ([p]
    regroups all personal information) either [Undefined k] if a reference to a
    person already defined was parsed ([k] is a key to find corresponding
    definition). [np] is a person's surname. [rest] is a rest of line to parse.
    Could be used to parse familial witnesses. *)
let parse_parent str l =
  (* last name *)
  let np, l = get_name l in
  (* first name and occurence number *)
  let pp, op, l = get_fst_name str l in
  (* person is not defined as a child elsewhere (is defined here) *)
  let defined =
    if bogus_def pp np then true
    else
      match l with [] -> false | s :: _ when s.[0] = '+' -> false | _ -> true
  in
  if not defined then
    let key = { pk_first_name = pp; pk_surname = np; pk_occ = op } in
    (Undefined key, np, l)
  else
    let u = create_person () in
    let u, l = set_infos pp np op u.sex "" "" str u l in
    (Defined u, np, l)

(** Parses the line containing a children and returns a person [gen_person]
    containing all extracted information. If a children definition doesn't
    provide surname then father's surname is used. ALso if it doesn't provide a
    children's birth place and source then it uses information provided by
    family definiton. *)
let parse_child str surname sex csrc cbp l =
  let u = create_person () in
  let prenom, occ, l = get_fst_name str l in
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
  set_infos prenom nom occ sex csrc cbp str u l

(** Parse relation type [Def.gen_relation] with a person outside of family block
    (foster parents, god parent, etc.). *)
let get_relation str = function
  | "-" :: x :: l -> (
      let rtyp =
        match x with
        | "adop" | "adop:" -> Adoption
        | "reco" | "reco:" -> Recognition
        | "cand" | "cand:" -> CandidateParent
        | "godp" | "godp:" -> GodParent
        | "fost" | "fost:" -> FosterParent
        | _ -> failwith str
      in
      if String.length x = 5 && x.[4] = ':' then (
        let fk, _, l = parse_parent str l in
        let l = match l with "+" :: l -> l | _ -> failwith str in
        let mk, _, l = parse_parent str l in
        if l <> [] then failwith str;
        { r_type = rtyp; r_fath = Some fk; r_moth = Some mk; r_sources = "" })
      else
        match l with
        | "fath:" :: l ->
            let fk, _, l = parse_parent str l in
            if l <> [] then failwith str;
            { r_type = rtyp; r_fath = Some fk; r_moth = None; r_sources = "" }
        | "moth:" :: l ->
            let mk, _, l = parse_parent str l in
            if l <> [] then failwith str;
            { r_type = rtyp; r_fath = None; r_moth = Some mk; r_sources = "" }
        | _ -> failwith str)
  | _ -> failwith str

(** Read notes of a person inside [note] block across multiple lines and concat
    them. *)
let read_notes ic =
  let notes =
    try
      let rec loop = function
        | "end notes" -> ""
        | l -> l ^ "\n" ^ loop (input_a_line ic)
      in
      loop (input_a_line ic)
    with End_of_file -> failwith "end of file"
  in
  Mutil.strip_all_trailing_spaces notes

(* from version 5.00 *)

(** Read database notes across multiple lines and concat them. Stop reading when
    encounter [end_text] *)
let read_notes_db ic end_txt =
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
          s ^ "\n" ^ loop (input_a_line ic)
      in
      loop (input_a_line ic)
    with End_of_file -> failwith "end of file"
  in
  Mutil.strip_all_trailing_spaces notes

(** Parsing status of .gw block *)
type 'a read_family =
  | F_some of 'a  (** Read block inside .gw file *)
  | F_enc_utf_8  (** Read block that defines that file use utf-8 encoding *)
  | F_gw_plus  (** Read block that defines that the file uses gwplus syntax *)
  | F_none  (** Read end of the file *)
  | F_fail of string  (** Exception while reading *)

(** Read succesive family note lines and concat it. *)
let loop_note line ic =
  let rec loop_note acc str =
    match fields str with
    | "note" :: tl ->
        let note =
          if tl = [] then ""
          else
            String.sub str
              (String.length "note" + 1)
              (String.length str - String.length "note" - 1)
        in
        loop_note (note :: acc) (input_a_line ic)
    | _ -> (String.concat "\n" (List.rev @@ ("" :: acc)), str)
  in
  loop_note [] line

(** Parse witnesses across the lines and returns list of [(wit,wsex,wk)] where
    wit is a witness definition/reference, [wsex] is a sex of witness and [wk]
    is a kind of witness relationship to the family. *)
let loop_witn line ic =
  let rec loop_witn acc str =
    match fields str with
    | ("wit" | "wit:") :: l ->
        let sex, l =
          match l with
          | "m:" :: l -> (Male, l)
          | "f:" :: l -> (Female, l)
          | l -> (Neuter, l)
        in
        let wkind, l = get_event_witness_kind l in
        let wk, _, l = parse_parent str l in
        if l <> [] then failwith str;
        loop_witn ((wk, sex, wkind) :: acc) (input_a_line ic)
    | _ -> (List.rev acc, str)
  in
  loop_witn [] line

(** Read and parse a gw file block from [ic]. Returns also next line if it's not
    the end of the file. *)
let read_family ic fname = function
  (* Block that defines that file use utf-8 encoding *)
  | Some (_, [ "encoding:"; "utf-8" ]) -> F_enc_utf_8
  (* Block that defines that the file uses gwplus syntax *)
  | Some (_, [ "gwplus" ]) -> F_gw_plus
  (* Family block *)
  | Some (str, "fam" :: l) -> (
      (* read father *)
      let fath_key, surname, l = parse_parent str l in
      (* read relation between parents *)
      let relation_ss, marriage, marr_place, marr_note, marr_src, divorce, l =
        get_mar_date str l
      in
      let relation, fath_sex, moth_sex = relation_ss in
      (* read mother *)
      let moth_key, _, l = parse_parent str l in
      if l <> [] then failwith str;
      let line = read_line ic in
      (* read list of witnesses with their sex (if exists) *)
      let witn, line =
        let rec loop = function
          | Some (str, ("wit" | "wit:") :: l) ->
              let sex, l =
                match l with
                | "m:" :: l -> (Male, l)
                | "f:" :: l -> (Female, l)
                | l -> (Neuter, l)
              in
              let wk, _, l = parse_parent str l in
              if l <> [] then failwith str;
              let witn, line = loop (read_line ic) in
              ((wk, sex) :: witn, line)
          | line -> ([], line)
        in
        loop line
      in
      (* read familial source if present *)
      let fsrc, line =
        match line with
        | Some (_, [ "src"; x ]) -> (cut_space x, read_line ic)
        | Some (str, "src" :: _) -> failwith str
        | _ -> ("", line)
      in
      (* read common children source if present *)
      let csrc, line =
        match line with
        | Some (_, [ "csrc"; x ]) -> (cut_space x, read_line ic)
        | Some (str, "csrc" :: _) -> failwith str
        | _ -> ("", line)
      in
      (* read common children birth place if present *)
      let cbp, line =
        match line with
        | Some (_, [ "cbp"; x ]) -> (cut_space x, read_line ic)
        | Some (str, "cbp" :: _) -> failwith str
        | _ -> ("", line)
      in
      (* create a couple *)
      let co = Adef.couple fath_key moth_key in
      (* read a family comments *)
      let comm, line =
        match line with
        | Some (str, "comm" :: _) ->
            let comm = String.sub str 5 (String.length str - 5) in
            (comm, read_line ic)
        | _ -> ("", line)
      in
      (* read family events *)
      let fevents, line =
        match line with
        | Some (_, "fevt" :: _) ->
            let fevents, line =
              let rec loop fevents = function
                | "end fevt" -> (fevents, read_line ic)
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
                    if l <> [] then failwith str;
                    (* On récupère les témoins *)
                    let witn, line = loop_witn (input_a_line ic) ic in
                    (* On récupère les notes *)
                    let notes, line = loop_note line ic in
                    let notes = Mutil.strip_all_trailing_spaces notes in
                    let evt = (name, date, place, cause, src, notes, witn) in
                    loop (evt :: fevents) line
              in
              loop [] (input_a_line ic)
            in
            (List.rev fevents, line)
        | _ -> ([], line)
      in
      match line with
      (* have children *)
      | Some (_, [ "beg" ]) ->
          let cles_enfants =
            let rec loop children =
              match read_line ic with
              | Some (str, "-" :: l) ->
                  let sex, l = get_optional_sexe l in
                  let child, l = parse_child str surname sex csrc cbp l in
                  if l <> [] then failwith str else loop (child :: children)
              | Some (_, [ "end" ]) -> children
              | Some (str, _) -> failwith str
              | _ -> failwith "eof"
            in
            List.rev (loop [])
          in
          (* create a family definition (without witnesses, events and family index) *)
          let fo =
            {
              marriage;
              marriage_place = marr_place;
              marriage_note = marr_note;
              marriage_src = marr_src;
              witnesses = [||];
              relation;
              divorce;
              fevents = [];
              comment = comm;
              origin_file = Filename.basename fname;
              fsources = fsrc;
              fam_index = Driver.Ifam.dummy;
            }
          in
          let deo = { children = Array.of_list cles_enfants } in
          F_some
            ( Family (co, fath_sex, moth_sex, witn, fevents, fo, deo),
              read_line ic )
      (* no children *)
      | line ->
          let fo =
            {
              marriage;
              marriage_place = marr_place;
              marriage_note = marr_note;
              marriage_src = marr_src;
              witnesses = [||];
              relation;
              divorce;
              fevents = [];
              comment = comm;
              origin_file = Filename.basename fname;
              fsources = fsrc;
              fam_index = Driver.Ifam.dummy;
            }
          in
          let deo = { children = [||] } in
          F_some (Family (co, fath_sex, moth_sex, witn, fevents, fo, deo), line)
      )
  (* Database notes block *)
  | Some (_, [ "notes-db" ]) ->
      let notes = read_notes_db ic "end notes-db" in
      F_some (Bnotes ("", notes), read_line ic)
  (* Extended page block *)
  | Some (str, [ "page-ext"; _ ]) ->
      let p =
        let len = String.length "page-ext" + 1 in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db ic "end page-ext" in
      F_some (Bnotes (p, notes), read_line ic)
  (* Used before version 5.00. Notes block *)
  | Some (_, [ "notes" ]) ->
      let notes = read_notes ic in
      F_some (Bnotes ("", notes), read_line ic)
  (* Notes block *)
  | Some (str, "notes" :: l) -> (
      let surname, l = get_name l in
      let first_name, occ, l = get_fst_name str l in
      if l <> [] then failwith "str"
      else
        match read_line ic with
        | Some (_, [ "beg" ]) ->
            let notes = read_notes ic in
            let key =
              { pk_first_name = first_name; pk_surname = surname; pk_occ = occ }
            in
            F_some (Notes (key, notes), read_line ic)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file")
  (* Wizard note block *)
  | Some (str, "wizard-note" :: _) ->
      let wizid =
        let len = String.length "wizard-note " in
        String.sub str len (String.length str - len)
      in
      let notes = read_notes_db ic "end wizard-note" in
      F_some (Wnotes (wizid, notes), read_line ic)
  (* Personal relation block *)
  | Some (str, "rel" :: l) -> (
      (* get considered person *)
      let sb, _, l = parse_parent str l in

      let sex, l =
        match l with
        | "#h" :: l -> (Male, l)
        | "#f" :: l -> (Female, l)
        | l -> (Neuter, l)
      in
      if l <> [] then failwith "str"
      else
        match read_line ic with
        (* Read list of relations *)
        | Some (_, [ "beg" ]) ->
            let rl =
              try
                let rec loop = function
                  | "end" -> []
                  | x -> get_relation x (fields x) :: loop (input_a_line ic)
                in
                loop (input_a_line ic)
              with End_of_file -> failwith "missing end rel"
            in
            F_some (Relations (sb, sex, rl), read_line ic)
        | Some (str, _) -> failwith str
        | None -> failwith "end of file")
  (* Person's events block *)
  | Some (str, "pevt" :: l) ->
      (* get considered person *)
      let sb, _, l = parse_parent str l in
      if l <> [] then failwith str
      else
        let pevents =
          let rec loop pevents = function
            | "end pevt" -> pevents
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
                if l <> [] then failwith str;
                (* On récupère les témoins *)
                let witn, line = loop_witn (input_a_line ic) ic in
                (* On récupère les notes *)
                let notes, line = loop_note line ic in
                let notes = Mutil.strip_all_trailing_spaces notes in
                let evt = (name, date, place, cause, src, notes, witn) in
                loop (evt :: pevents) line
          in
          loop [] (input_a_line ic)
        in
        let pevents = List.rev pevents in
        F_some (Pevent (sb, Neuter, pevents), read_line ic)
  | Some (str, _) -> failwith str
  (* End of the file *)
  | None -> F_none

(** Read and return a block of .gw file. If [!no_fail] is disabled raises
    [Failure] exception. *)
let read_family_1 ic fname line =
  if !no_fail then
    try read_family ic fname line with Failure str -> F_fail str
  else read_family ic fname line

(** Compile .gw file and save result to corresponding .gwo *)
let comp_families x =
  let out_file = Filename.chop_suffix x ".gw" ^ ".gwo" in
  line_cnt := 0;
  let oc = open_out_bin out_file in
  (try
     let ic = open_in x in
     (* write header *)
     output_string oc magic_gwo;
     (* write source filename *)
     output_value oc (x : string);
     let rec loop line encoding =
       match read_family_1 (ic, encoding) x line with
       | F_some (family, line) ->
           output_value oc (family : gw_syntax);
           loop line encoding
       | F_enc_utf_8 -> loop (read_line (ic, E_utf_8)) E_utf_8
       | F_gw_plus ->
           create_all_keys := true;
           loop (read_line (ic, encoding)) encoding
       | F_none -> ()
       | F_fail str ->
           Printf.printf "File \"%s\", line %d:\n" x !line_cnt;
           Printf.printf "Error: %s\n" str;
           flush stdout;
           loop (read_line (ic, encoding)) encoding
     in
     loop (read_line (ic, E_iso_8859_1)) E_iso_8859_1;
     close_in ic
   with e ->
     close_out oc;
     Mutil.rm out_file;
     raise e);
  close_out oc
