(* Copyright (c) 1998-2007 INRIA *)

(* La liste des caractères interdits *)
let forbidden_char = [ ':'; '@'; '#'; '='; '$' ]

(* Name.lower *)

let unaccent_utf_8 lower s i =
  let fns =
    if lower then fun n s -> (String.lowercase_ascii s, n) else fun n s -> (s, n)
  in
  let fnc =
    if lower then fun n c -> (String.make 1 @@ Char.lowercase_ascii c, n)
    else fun n c -> (String.make 1 c, n)
  in
  let s, n =
    Unidecode.decode fns fnc
      (fun n -> (String.sub s i (n - i), n))
      s i (String.length s)
  in
  if lower then (String.lowercase_ascii s, n) else (s, n)

let next_chars_if_equiv s i t j =
  if i >= String.length s || j >= String.length t then None
  else
    let s1, i1 = unaccent_utf_8 true s i in
    let t1, j1 = unaccent_utf_8 true t j in
    if s1 = t1 then Some (i1, j1) else None

(* Name.lower:
    - uppercase -> lowercase
    - no accents
    - chars no letters and no numbers (except '.') => spaces (stripped)
     Key comparison (first name, surname, number) applies "lower" equality
     on first names and surnames *)
let lower s =
  let rec copy special i len =
    if i = String.length s then Buff.get len
    else if Char.code s.[i] < 0x80 then
      match s.[i] with
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.') as c ->
          let len = if special then Buff.store len ' ' else len in
          let c = Char.lowercase_ascii c in
          copy false (i + 1) (Buff.store len c)
      | _ -> copy (len <> 0) (i + 1) len
    else
      let len = if special then Buff.store len ' ' else len in
      let t, j = unaccent_utf_8 true s i in
      copy false j (Buff.mstore len t)
  in
  copy false 0 0

let title s =
  let t = ref true in
  let cmap u =
    let r = if !t then Uucp.Case.Map.to_upper u else Uucp.Case.Map.to_lower u in
    t := not (Uucp.Alpha.is_alphabetic u);
    r
  in
  Utf8.cmap_utf_8 cmap s

(* Name.abbrev *)

(* List of abbreviations. If abbreviation is mapped to [Some s] should be remplaced by
   [s]. If mapped to None, it should be removed from name. *)
let abbrev_list =
  [
    ("a", None);
    ("af", None);
    ("d", None);
    ("de", None);
    ("di", None);
    ("ier", Some "i");
    ("of", None);
    ("saint", Some "st");
    ("sainte", Some "ste");
    ("van", None);
    ("von", None);
    ("zu", None);
    ("zur", None);
  ]

(* Checks if the word starting at [i] in [s] is [p]. *)
let is_word s i p =
  let rec is_word s i p ip =
    if ip = String.length p then
      if i = String.length s then true else if s.[i] = ' ' then true else false
    else if i = String.length s then false
    else if s.[i] = p.[ip] then is_word s (i + 1) p (ip + 1)
    else false
  in
  is_word s i p 0

(* Checks if word that starts at position [i] in [s] is one of abbreviation *)
let rec search_abbrev s i = function
  | (w, a) :: pl ->
      if is_word s i w then Some (String.length w, a) else search_abbrev s i pl
  | [] -> None

(* Name.abbrev: suppress lowercase particles, shorten "saint" into "st" *)
let abbrev s =
  let rec copy can_start_abbrev i len =
    if i >= String.length s then Buff.get len
    else
      match s.[i] with
      | ' ' -> copy true (i + 1) (Buff.store len ' ')
      | c ->
          if can_start_abbrev then
            match search_abbrev s i abbrev_list with
            | None -> copy false (i + 1) (Buff.store len c)
            | Some (n, Some a) -> copy false (i + n) (Buff.mstore len a)
            | Some (n, None) -> copy true (i + n + 1) len
          else copy false (i + 1) (Buff.store len c)
  in
  copy true 0 0

(* Name.strip *)

(* Name.strip_c = name without the charater c given as parameter *)
let strip_c s c =
  let rec copy i len =
    if i = String.length s then Buff.get len
    else if s.[i] = c then copy (i + 1) len
    else copy (i + 1) (Buff.store len s.[i])
  in
  copy 0 0

let strip s = strip_c s ' '

(* String without any forbidden caracters defined in forbidden_char *)
(* ******************************************************************** *)
(*  [Fonc] purge : string -> string                                     *)

(* ******************************************************************** *)

(** [Description] : Supprime tous les caractères interdits (défini par
    forbidden_char) présents dans la chaine passée en argument. [Args] :
    - s : string que l'on veut purger [Retour] :
    - string : retourne la chaîne délestée des caractères interdits [Rem] :
      Exporté en clair hors de ce module. *)
let purge s = List.fold_left strip_c s forbidden_char

(* Name.crush *)

(* If string starting from [i] contains roman number then returns the next position,
   else returns None. *)
let roman_number s i =
  let rec loop i =
    if i = String.length s then Some i
    else if s.[i] = ' ' then Some i
    else match s.[i] with 'i' | 'v' | 'x' | 'l' -> loop (i + 1) | _ -> None
  in
  if i = 0 || s.[i - 1] = ' ' then loop i else None

(* Name.crush, a custom sonnex/soundex-like phonetic algorithm:
     - no spaces
     - roman numbers are keeped
     - vowels are suppressed, except in words starting with a vowel,
       where this vowel is converted into "e"
     - "k" and "q" replaced by "c"
     - "y" replaced by "i"
     - "z" replaced by "s"
     - "ph" replaced by "f"
     - others "h" deleted
     - s at end of words are deleted
     - no double lowercase consons *)
let crush s =
  let rec copy i len first_vowel =
    if i = String.length s then Buff.get len
    else if s.[i] = ' ' then copy (i + 1) len true
    else
      match roman_number s i with
      | Some j ->
          let rec loop i len =
            if i = j then copy j len true
            else loop (i + 1) (Buff.store len s.[i])
          in
          loop i len
      | _ -> (
          match s.[i] with
          | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->
              let len = if first_vowel then Buff.store len 'e' else len in
              copy (i + 1) len false
          | 'h' ->
              let len =
                if i > 0 && s.[i - 1] = 'p' then Buff.store (len - 1) 'f'
                else len
              in
              copy (i + 1) len first_vowel
          | ('s' | 'z') when i = String.length s - 1 || s.[i + 1] = ' ' ->
              let len =
                let rec loop i len =
                  if
                    i > 0 && len > 0
                    && s.[i] = Bytes.get !Buff.buff len
                    && (s.[i] = 's' || s.[i] = 'z')
                  then loop (i - 1) (len - 1)
                  else len + 1
                in
                loop (i - 1) (len - 1)
              in
              copy (i + 1) len false
          | 's' when i = String.length s - 1 || s.[i + 1] = ' ' ->
              copy (i + 1) len false
          | c ->
              if i > 0 && s.[i - 1] = c then copy (i + 1) len false
              else
                let c = match c with 'k' | 'q' -> 'c' | 'z' -> 's' | c -> c in
                copy (i + 1) (Buff.store len c) false)
  in
  copy 0 0 true

(* strip_lower *)

(* strip_lower = strip o lower, as first comparison of names.
   First names and Surnames comparison is strip_lower equality. *)
let strip_lower s = strip (lower s)

(* crush_lower *)

(* crush_lower = crush o abbrev o lower, as second comparison of names.
   In index by names, the "names" are crush_lowers *)
let crush_lower s = crush (abbrev (lower s))

(* concat two strings using Bytes module *)
let concat_aux fn l1 sn l2 =
  let b = Bytes.create (l1 + l2 + 1) in
  Bytes.blit_string fn 0 b 0 l1;
  Bytes.blit_string sn 0 b (l1 + 1) l2;
  Bytes.unsafe_set b l1 ' ';
  Bytes.unsafe_to_string b

let concat fn sn = concat_aux fn (String.length fn) sn (String.length sn)
let contains_forbidden_char s = List.exists (String.contains s) forbidden_char

(* Copy/paste from String.split_on_char adapted to our needs *)
let split_sname_callback fn s =
  let open String in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if match unsafe_get s i with ' ' | '-' -> true | _ -> false then (
      fn (i + 1) (!j - i - 1);
      j := i)
  done;
  fn 0 !j

(* Copy/paste from String.split_on_char adapted to our needs *)
let split_fname_callback fn s =
  let open String in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = ' ' then (
      fn (i + 1) (!j - i - 1);
      j := i)
  done;
  fn 0 !j

let split_sname s =
  let r = ref [] in
  split_sname_callback (fun i j -> r := String.sub s i j :: !r) s;
  !r

let split_fname s =
  let r = ref [] in
  split_fname_callback (fun i j -> r := String.sub s i j :: !r) s;
  !r
