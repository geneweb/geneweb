module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

type span = { text : string; pos : int; len : int }

type segment = {
  wizard : span option;
  source : span option;
  date : span option;
}

type known = { names : (string, unit) Hashtbl.t; lengths : int list }

let known_names k = Hashtbl.fold (fun n () acc -> n :: acc) k.names []
let is_space c = c = ' ' || c = '\t'

let starts_at s i p =
  let n = String.length p in
  i + n <= String.length s && String.sub s i n = p

let trim_span s a b =
  let a = ref a and b = ref b in
  while !a < !b && is_space s.[!a] do
    incr a
  done;
  while !b > !a && is_space s.[!b - 1] do
    decr b
  done;
  (!a, !b)

let span s a b = { text = String.sub s a (b - a); pos = a; len = b - a }

(* ---- dates ---- *)

let date =
  let open Re in
  let sp = rep (set " \t") in
  let sep = seq [ sp; opt (set "./-"); sp ] in
  let d12 = repn digit 1 (Some 2) in
  let year = alt [ repn digit 4 (Some 4); repn digit 2 (Some 2) ] in
  let roman =
    no_case
      (alt
         (List.map str
            [
              "viii";
              "vii";
              "iii";
              "xii";
              "ii";
              "iv";
              "vi";
              "ix";
              "xi";
              "i";
              "v";
              "x";
            ]))
  in
  let month =
    no_case
      (alt
         (List.map str
            [
              "janvier";
              "janv";
              "février";
              "fevrier";
              "févr";
              "fevr";
              "fév";
              "fev";
              "mars";
              "avril";
              "avr";
              "mai";
              "juin";
              "juillet";
              "juil";
              "août";
              "aout";
              "septembre";
              "sept";
              "octobre";
              "oct";
              "novembre";
              "nov";
              "décembre";
              "decembre";
              "déc";
              "dec";
            ]))
  in
  let ds = set "./-" in
  let date =
    alt
      [
        seq [ d12; sep; roman; sep; year ];
        seq [ d12; sp; ds; sp; d12; sp; ds; sp; year ];
        seq
          [
            d12;
            opt (str "er");
            rep1 (set " \t");
            month;
            opt (char '.');
            rep1 (set " \t");
            year;
          ];
        seq [ roman; rep1 (set " \t"); year ];
        seq [ month; opt (char '.'); rep1 (set " \t"); year ];
        seq
          [
            repn digit 4 (Some 4);
            char '-';
            repn digit 2 (Some 2);
            char '-';
            repn digit 2 (Some 2);
          ];
      ]
  in
  date

(* a date at the end of a string *)
let date_re =
  let open Re in
  let sp = rep (set " \t") in
  compile (longest (seq [ group date; sp; opt (char '.'); sp; eos ]))

(* a date at the start of a string *)
let date_start_re = Re.(compile (longest (seq [ bos; group date ])))

(* the date at the end of s.[a..b) *)
let date_at s a b =
  let sub = String.sub s a (b - a) in
  let rec loop pos =
    if pos >= String.length sub then None
    else
      match Re.exec_opt ~pos date_re sub with
      | None -> None
      | Some g ->
          let start, stop = Re.Group.offset g 1 in
          if start = 0 || String.contains " \t,)" sub.[start - 1] then
            Some (a + start, a + stop)
          else loop (start + 1)
  in
  loop 0

(* ---- known wizards ---- *)

let signature_end s i =
  if starts_at s i "[[w:" then
    let rec loop j =
      if j + 1 >= String.length s then None
      else if s.[j] = ']' && s.[j + 1] = ']' then Some (j + 2)
      else loop (j + 1)
    in
    loop (i + 4)
  else None

(* the text before the first "(" of [s], without a date ending it
   ("wizard date (source)"), if it may be a wizard: at most 4 words, no
   digit *)
let plain_prefix s =
  match String.index_opt s '(' with
  | Some j when j > 0 && j <= 42 ->
      let a, b = trim_span s 0 j in
      let b =
        match date_at s a b with
        | Some (d, _) when d > a -> snd (trim_span s a d)
        | _ -> b
      in
      let p = String.sub s a (b - a) in
      if
        String.length p >= 2
        && String.length p <= 40
        && (not (String.exists (fun c -> String.contains "[],;:" c) p))
        && (not (String.exists (fun c -> c >= '0' && c <= '9') p))
        && List.length (List.filter (( <> ) "") (String.split_on_char ' ' p))
           <= 4
      then Some p
      else None
  | _ -> None

let known_of_strings ?(threshold = 20) strings =
  let names = Hashtbl.create 1024 in
  let prefixes = Hashtbl.create 4096 in
  List.iter
    (fun s ->
      (* idents of the signatures *)
      let rec loop i =
        match String.index_from_opt s i '[' with
        | None -> ()
        | Some i -> (
            match signature_end s i with
            | Some j ->
                let a, b = trim_span s (i + 4) (j - 2) in
                if b > a then Hashtbl.replace names (String.sub s a (b - a)) ();
                loop j
            | None -> loop (i + 1))
      in
      loop 0;
      if not (starts_at s 0 "[[w:") then
        Option.iter
          (fun p ->
            Hashtbl.replace prefixes p
              (1 + Option.value ~default:0 (Hashtbl.find_opt prefixes p)))
          (plain_prefix s))
    strings;
  Hashtbl.iter
    (fun p n -> if n >= threshold then Hashtbl.replace names p ())
    prefixes;
  let lengths =
    Hashtbl.fold (fun n () acc -> String.length n :: acc) names []
    |> List.sort_uniq (fun a b -> compare b a)
  in
  { names; lengths }

(* end of the wizard starting at [i], if any *)
let wizard_at k s i =
  match signature_end s i with
  | Some j -> Some j
  | None ->
      let n = String.length s in
      List.find_map
        (fun l ->
          if
            i + l <= n
            && Hashtbl.mem k.names (String.sub s i l)
            && (i + l = n || String.contains " (," s.[i + l])
          then Some (i + l)
          else None)
        k.lengths

(* ---- parsing ---- *)

(* the segments of [s]: cut at the commas, outside parentheses and [[ ]],
   followed by a wizard *)
let cuts k s =
  let n = String.length s in
  let rec loop i depth start acc =
    if i >= n then List.rev ((start, n) :: acc)
    else
      match s.[i] with
      | '(' -> loop (i + 1) (depth + 1) start acc
      | ')' -> loop (i + 1) (max 0 (depth - 1)) start acc
      | '[' when i + 1 < n && s.[i + 1] = '[' ->
          loop (i + 2) (depth + 1) start acc
      | ']' when i + 1 < n && s.[i + 1] = ']' ->
          loop (i + 2) (max 0 (depth - 1)) start acc
      | ',' when depth = 0 ->
          let j = ref (i + 1) in
          while !j < n && is_space s.[!j] do
            incr j
          done;
          if wizard_at k s !j <> None then
            loop (i + 1) depth (i + 1) ((start, i) :: acc)
          else loop (i + 1) depth start acc
      | _ -> loop (i + 1) depth start acc
  in
  loop 0 0 0 []

(* the position of the ")" closing the "(" at [i], if before [b] *)
let closing s i b =
  let rec loop j depth =
    if j >= b then None
    else
      match s.[j] with
      | '(' -> loop (j + 1) (depth + 1)
      | ')' -> if depth = 1 then Some j else loop (j + 1) (depth - 1)
      | _ -> loop (j + 1) depth
  in
  loop i 0

let parse_segment k s (a, b) =
  let a, b = trim_span s a b in
  if a >= b then None
  else
    let wizard, i =
      match wizard_at k s a with
      | Some j when j <= b -> (Some (span s a j), j)
      | _ -> (None, a)
    in
    let i =
      let i = ref i in
      while !i < b && (is_space s.[!i] || s.[!i] = ',' || s.[!i] = ':') do
        incr i
      done;
      !i
    in
    (* "wizard date (source)" or "wizard (source) date" *)
    let lead =
      if wizard = None || i >= b then None
      else
        match Re.exec_opt date_start_re (String.sub s i (b - i)) with
        | Some g ->
            let _, stop = Re.Group.offset g 1 in
            let j = i + stop in
            if j < b && (is_space s.[j] || s.[j] = '(') then Some (i, j)
            else None
        | None -> None
    in
    let date, i, e =
      match lead with
      | Some (d1, d2) ->
          let i = ref d2 in
          while !i < b && is_space s.[!i] do
            incr i
          done;
          (Some (span s d1 d2), !i, b)
      | None -> (
          match if i < b then date_at s i b else None with
          | Some (d1, d2) -> (Some (span s d1 d2), i, d1)
          | None -> (None, i, b))
    in
    let i, e =
      let i, e = trim_span s i e in
      let e = ref e in
      while !e > i && (s.[!e - 1] = ',' || is_space s.[!e - 1]) do
        decr e
      done;
      (i, !e)
    in
    let source =
      if i >= e then None
      else if s.[i] = '(' && closing s i e = Some (e - 1) then
        let a, b = trim_span s (i + 1) (e - 1) in
        if a < b then Some (span s a b) else None
      else Some (span s i e)
    in
    if wizard = None && source = None && date = None then None
    else Some { wizard; source; date }

let parse k s = List.filter_map (parse_segment k s) (cuts k s)

(* ---- keys and clusters ---- *)

type part = Wizard | Source

let part_of_string = function
  | "w" -> Some Wizard
  | "s" -> Some Source
  | _ -> None

let string_of_part = function Wizard -> "w" | Source -> "s"

let spans part segs =
  List.filter_map
    (fun seg -> match part with Wizard -> seg.wizard | Source -> seg.source)
    segs

let words s =
  String.split_on_char ' ' (Name.lower s)
  |> List.concat_map (String.split_on_char '.')
  |> List.filter (( <> ) "")

let wizard_key s =
  let s =
    if starts_at s 0 "[[w:" && String.length s >= 6 then
      String.sub s 4 (String.length s - 6)
    else s
  in
  match words s with
  | [] -> ""
  | [ w ] -> w
  | w :: l -> String.make 1 w.[0] ^ String.concat "" l

let source_key s = String.concat "" (words s)
let key = function Wizard -> wizard_key | Source -> source_key

type spelling = { text : string; strings : int; uses : int }

type cluster = {
  part : part;
  ckey : string;
  spellings : spelling list;
  total : int;
}

type data = { known : known; strings : (Driver.istr * string * int) list }

let of_strings strings =
  { known = known_of_strings (List.map (fun (_, s, _) -> s) strings); strings }

let load conf base =
  let u = PlaceRename.usage ~field:PlaceRename.Source base in
  ignore conf;
  let strings = ref [] in
  let seen = Hashtbl.create 4096 in
  let add i =
    if (not (Driver.Istr.is_empty i)) && not (Hashtbl.mem seen i) then (
      Hashtbl.add seen i ();
      strings := (i, Driver.sou base i, PlaceRename.uses u i) :: !strings)
  in
  Collection.iter
    (fun ip ->
      let p = Driver.poi base ip in
      List.iter add
        (Driver.get_psources p :: Driver.get_birth_src p
       :: Driver.get_baptism_src p :: Driver.get_death_src p
       :: Driver.get_burial_src p
        :: List.map (fun e -> e.Def.epers_src) (Driver.get_pevents p)))
    (Driver.ipers base);
  Collection.iter
    (fun ifam ->
      let f = Driver.foi base ifam in
      List.iter add
        (Driver.get_fsources f :: Driver.get_marriage_src f
        :: List.map (fun e -> e.Def.efam_src) (Driver.get_fevents f)))
    (Driver.ifams base);
  of_strings !strings

(* the distinct spellings of [part] in each string *)
let iter_spellings d part f =
  List.iter
    (fun ((_, raw, _) as x) ->
      parse d.known raw |> spans part
      |> List.map (fun (sp : span) -> sp.text)
      |> List.sort_uniq compare
      |> List.iter (fun t -> f x t))
    d.strings

let clusters d part =
  let h = Hashtbl.create 4096 in
  iter_spellings d part (fun (_, _, uses) t ->
      let k = key part t in
      if k <> "" then
        let sp =
          match Hashtbl.find_opt h k with
          | Some sp -> sp
          | None ->
              let sp = Hashtbl.create 4 in
              Hashtbl.add h k sp;
              sp
        in
        let n, u = Option.value ~default:(0, 0) (Hashtbl.find_opt sp t) in
        Hashtbl.replace sp t (n + 1, u + uses));
  Hashtbl.fold
    (fun ckey sp acc ->
      if Hashtbl.length sp < 2 then acc
      else
        let spellings =
          Hashtbl.fold
            (fun text (strings, uses) l -> { text; strings; uses } :: l)
            sp []
          |> List.sort (fun a b -> compare (b.uses, a.text) (a.uses, b.text))
        in
        let total = List.fold_left (fun n sp -> n + sp.uses) 0 spellings in
        { part; ckey; spellings; total } :: acc)
    h []
  |> List.sort (fun a b -> compare (b.total, a.ckey) (a.total, b.ckey))

let samples d part spellings n =
  let h = Hashtbl.create 16 in
  List.iter (fun t -> Hashtbl.replace h t []) spellings;
  List.iter
    (fun (_, raw, _) ->
      let sps = parse d.known raw |> spans part in
      List.iter
        (fun t ->
          match Hashtbl.find_opt h t with
          | Some l when List.length l < n -> (
              match List.filter (fun (sp : span) -> sp.text = t) sps with
              | [] -> ()
              | l' -> Hashtbl.replace h t ((raw, l') :: l))
          | _ -> ())
        spellings)
    d.strings;
  List.map (fun t -> (t, List.rev (Hashtbl.find h t))) spellings

let replace_spans raw (spans : span list) new_term =
  List.sort (fun (a : span) (b : span) -> compare b.pos a.pos) spans
  |> List.fold_left
       (fun s (sp : span) ->
         String.sub s 0 sp.pos ^ new_term
         ^ String.sub s (sp.pos + sp.len) (String.length s - sp.pos - sp.len))
       raw

let plan base d part ~terms ~new_term =
  let new_term = String.trim new_term in
  let items =
    List.filter_map
      (fun (istr, raw, _) ->
        match
          parse d.known raw |> spans part
          |> List.filter (fun (sp : span) -> List.mem sp.text terms)
        with
        | [] -> None
        | sps ->
            let new_s =
              Mutil.normalize_utf_8 (replace_spans raw sps new_term)
            in
            if new_s = raw then None
            else
              Some
                ( {
                    PlaceRename.istr;
                    old_s = raw;
                    new_s;
                    depths = [];
                    kind = PlaceRename.New_place;
                  },
                  sps ))
      d.strings
  in
  let spans_of = Hashtbl.create 64 in
  List.iter
    (fun ((it : PlaceRename.item), sps) -> Hashtbl.replace spans_of it.istr sps)
    items;
  PlaceRename.classify base (List.map fst items)
  |> List.map (fun (it : PlaceRename.item) ->
      (it, Option.value ~default:[] (Hashtbl.find_opt spans_of it.istr)))
