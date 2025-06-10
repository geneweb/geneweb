(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Def
open Gwexport
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

let old_gw = ref false
let only_file = ref ""
let out_dir = ref ""
let raw_output = ref false
let sep_limit = ref 21
let separate_list = ref []
let dummy = ref [ "" ]
let all_files = ref false

(* Returns true if `old_gw` is `true` and there exist an event associated to a
   person that:
   * is either a birth, baptism, death, burial or a cremation and is associated to
     a note or a witness;
   * is any other event.
   Otherwise, returns false *)
let put_events_in_notes base p =
  (* Si on est en mode old_gw, on mets tous les évènements *)
  (* dans les notes.                                       *)
  if !old_gw then
    let rec loop pevents =
      match pevents with
      | [] -> false
      | evt :: events -> (
          match evt.epers_name with
          | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
          | Epers_Cremation ->
              if
                Driver.sou base evt.epers_note <> ""
                || evt.epers_witnesses <> [||]
              then true
              else loop events
          | _ -> true)
    in
    loop (Driver.get_pevents p)
  else false

let (ht_dup_occ : (Geneweb_db.Driver.iper, int) Hashtbl.t) =
  Hashtbl.create 20001

let (ht_orig_occ : (string, int list) Hashtbl.t) = Hashtbl.create 20001

let prepare_free_occ ?(select = fun _ -> true) base =
  (* Parce qu'on est obligé ... *)
  let sn = "?" in
  let fn = "?" in
  let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
  Hashtbl.add ht_orig_occ key [ 0 ];
  Collection.iter
    (fun ip ->
      if select ip then
        let p = Driver.poi base ip in
        let sn = Driver.sou base (Driver.get_surname p) in
        let fn = Driver.sou base (Driver.get_first_name p) in
        if sn = "?" && fn = "?" then ()
        else
          let fn = Name.lower fn in
          let sn = Name.lower sn in
          if fn = "" || sn = "" then
            let key = fn ^ " #@# " ^ sn in
            let occ = Driver.get_occ p in
            try
              let l = Hashtbl.find ht_orig_occ key in
              if List.mem occ l then Hashtbl.add ht_dup_occ ip occ
              else Hashtbl.replace ht_orig_occ key (occ :: l)
            with Not_found -> Hashtbl.add ht_orig_occ key [ occ ])
    (Geneweb_db.Driver.ipers base);
  Hashtbl.iter
    (fun key l -> Hashtbl.replace ht_orig_occ key (List.sort compare l))
    ht_orig_occ;
  Hashtbl.iter
    (fun ip _ ->
      let p = Driver.poi base ip in
      let sn = Driver.sou base (Driver.get_surname p) in
      let fn = Driver.sou base (Driver.get_first_name p) in
      let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
      try
        let list_occ = Hashtbl.find ht_orig_occ key in
        let rec loop list init new_list =
          match list with
          | x :: y :: l when y - x > 1 ->
              (succ x, List.rev_append (y :: succ x :: x :: new_list) l)
          | x :: l -> loop l (succ x) (x :: new_list)
          | [] -> (init, [ init ])
        in
        let new_occ, new_list_occ = loop list_occ 0 [] in
        Hashtbl.replace ht_dup_occ ip new_occ;
        Hashtbl.replace ht_orig_occ key new_list_occ
      with Not_found -> ())
    ht_dup_occ

let get_new_occ p =
  try Hashtbl.find ht_dup_occ (Driver.get_iper p)
  with Not_found -> Driver.get_occ p

type mfam = {
  m_ifam : Driver.ifam;
  m_fam : Driver.family;
  m_fath : Driver.person;
  m_moth : Driver.person;
  m_chil : Driver.person array;
}

let soy y = if y = 0 then "-0" else string_of_int y
let oc opts = match opts.Gwexport.oc with _, oc, _ -> oc

let print_date_dmy opts d =
  (match d.prec with
  | About -> Printf.ksprintf (oc opts) "~"
  | Maybe -> Printf.ksprintf (oc opts) "?"
  | Before -> Printf.ksprintf (oc opts) "<"
  | After -> Printf.ksprintf (oc opts) ">"
  | Sure | OrYear _ | YearInt _ -> ());
  if d.month = 0 then Printf.ksprintf (oc opts) "%s" (soy d.year)
  else if d.day = 0 then Printf.ksprintf (oc opts) "%d/%s" d.month (soy d.year)
  else Printf.ksprintf (oc opts) "%d/%d/%s" d.day d.month (soy d.year);
  match d.prec with
  | OrYear d2 ->
      if not !old_gw then
        if d2.month2 = 0 then Printf.ksprintf (oc opts) "|%s" (soy d2.year2)
        else if d2.day2 = 0 then
          Printf.ksprintf (oc opts) "|%d/%s" d2.month2 (soy d2.year2)
        else
          Printf.ksprintf (oc opts) "|%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
      else Printf.ksprintf (oc opts) "|%s" (soy d2.year2)
  | YearInt d2 ->
      if not !old_gw then
        if d2.month2 = 0 then Printf.ksprintf (oc opts) "..%s" (soy d2.year2)
        else if d2.day2 = 0 then
          Printf.ksprintf (oc opts) "..%d/%s" d2.month2 (soy d2.year2)
        else
          Printf.ksprintf (oc opts) "..%d/%d/%s" d2.day2 d2.month2
            (soy d2.year2)
      else Printf.ksprintf (oc opts) "..%s" (soy d2.year2)
  | _ -> ()

let is_printable = function '\000' .. '\031' -> false | _ -> true

let starting_char no_num s =
  match s.[0] with
  (*'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' *)
  | 'a' .. 'z' | 'A' .. 'Z' | '\xE0' .. '\xFD' | '\xC0' .. '\xDD' -> true
  | '0' .. '9' -> not no_num
  | '?' -> if s = "?" then true else false
  | _ -> false

let no_newlines s =
  let conv_char i = match s.[i] with '\n' | '\r' -> ' ' | _ -> s.[i] in
  String.init (String.length s) conv_char

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

let s_correct_string s =
  let s = gen_correct_string false false s in
  if s = "" then "_" else s

let s_correct_string_nonum s =
  let s = gen_correct_string true false s in
  if s = "" then "_" else s

let correct_string base is = s_correct_string (Driver.sou base is)

let correct_string_no_colon base is =
  gen_correct_string false true (Driver.sou base is)

let gen_print_date opts no_colon = function
  | Dgreg (d, Dgregorian) -> print_date_dmy opts d
  | Dgreg (d, Djulian) ->
      print_date_dmy opts (Calendar.julian_of_gregorian d);
      Printf.ksprintf (oc opts) "J"
  | Dgreg (d, Dfrench) ->
      print_date_dmy opts (Calendar.french_of_gregorian d);
      Printf.ksprintf (oc opts) "F"
  | Dgreg (d, Dhebrew) ->
      print_date_dmy opts (Calendar.hebrew_of_gregorian d);
      Printf.ksprintf (oc opts) "H"
  | Dtext t ->
      (* Dans le cas d'une date texte pour un titre, on échappe les ':' *)
      let t = gen_correct_string false no_colon t in
      Printf.ksprintf (oc opts) "0(%s)" t

let gen_print_date_option opts no_colon = function
  | Some d -> gen_print_date opts no_colon d
  | None -> ()

let print_date opts = gen_print_date opts false
let print_date_option opts = gen_print_date_option opts false
let print_title_date_option opts = gen_print_date_option opts true

let lines_list_of_string s =
  let rec loop lines len i =
    if i = String.length s then
      List.rev (if len = 0 then lines else Buff.get len :: lines)
    else if s.[i] = '\n' then
      let line = Buff.get len in
      loop (line :: lines) 0 (i + 1)
    else loop lines (Buff.store len s.[i]) (i + 1)
  in
  loop [] 0 0

let has_infos_not_dates opts base p =
  let has_picture_to_export =
    Driver.sou base (Driver.get_image p) <> "" && not opts.no_picture
  in
  Driver.get_first_names_aliases p <> []
  || Driver.get_surnames_aliases p <> []
  || Driver.sou base (Driver.get_public_name p) <> ""
  || has_picture_to_export
  || Driver.get_qualifiers p <> []
  || Driver.get_aliases p <> []
  || Driver.get_titles p <> []
  || Driver.get_access p <> IfTitles
  || Driver.sou base (Driver.get_occupation p) <> ""
  || (opts.source <> None || Driver.sou base @@ Driver.get_psources p <> "")
  || Driver.sou base (Driver.get_birth_place p) <> ""
  || (opts.source = None && Driver.sou base (Driver.get_birth_src p) <> "")
  || Driver.sou base (Driver.get_baptism_place p) <> ""
  || (opts.source = None && Driver.sou base (Driver.get_baptism_src p) <> "")
  || Driver.sou base (Driver.get_death_place p) <> ""
  || (opts.source = None && Driver.sou base (Driver.get_death_src p) <> "")
  || Driver.sou base (Driver.get_burial_place p) <> ""
  || (opts.source = None && Driver.sou base (Driver.get_burial_src p) <> "")

let has_infos opts base p =
  has_infos_not_dates opts base p
  || Driver.get_birth p <> Date.cdate_None
  || Driver.get_baptism p <> Date.cdate_None
  || Driver.get_death p <> NotDead

let print_if_not_equal_to opts x base lab is =
  if Driver.sou base is <> x then
    Printf.ksprintf (oc opts) " %s %s" lab (correct_string base is)

let print_src_if_not_equal_to opts x base lab is =
  match opts.source with
  | None ->
      if Driver.sou base is <> "" then print_if_not_equal_to opts x base lab is
  | Some "" -> ()
  | Some x -> Printf.ksprintf (oc opts) " %s %s" lab (s_correct_string x)

let print_if_no_empty opts = print_if_not_equal_to opts ""

let print_first_name_alias opts base is =
  Printf.ksprintf (oc opts) " {%s}" (correct_string base is)

let print_surname_alias opts base is =
  Printf.ksprintf (oc opts) " #salias %s" (correct_string base is)

let print_qualifier opts base is =
  Printf.ksprintf (oc opts) " #nick %s" (correct_string base is)

let print_alias opts base is =
  Printf.ksprintf (oc opts) " #alias %s" (correct_string base is)

let print_burial opts b =
  match b with
  | Buried cod -> (
      Printf.ksprintf (oc opts) " #buri";
      match Date.od_of_cdate cod with
      | Some d ->
          Printf.ksprintf (oc opts) " ";
          print_date opts d;
          ()
      | None -> ())
  | Cremated cod -> (
      Printf.ksprintf (oc opts) " #crem";
      match Date.od_of_cdate cod with
      | Some d ->
          Printf.ksprintf (oc opts) " ";
          print_date opts d;
          ()
      | None -> ())
  | UnknownBurial -> ()

let print_title opts base t =
  let t_date_start = Date.od_of_cdate t.t_date_start in
  let t_date_end = Date.od_of_cdate t.t_date_end in
  Printf.ksprintf (oc opts) " [";
  (match t.t_name with
  | Tmain -> Printf.ksprintf (oc opts) "*"
  | Tname s -> Printf.ksprintf (oc opts) "%s" (correct_string_no_colon base s)
  | Tnone -> ());
  Printf.ksprintf (oc opts) ":";
  Printf.ksprintf (oc opts) "%s" (correct_string_no_colon base t.t_ident);
  Printf.ksprintf (oc opts) ":";
  Printf.ksprintf (oc opts) "%s" (correct_string_no_colon base t.t_place);
  (if t.t_nth <> 0 then Printf.ksprintf (oc opts) ":"
   else
     match (t_date_start, t_date_end) with
     | Some _, _ | _, Some _ -> Printf.ksprintf (oc opts) ":"
     | _ -> ());
  print_title_date_option opts t_date_start;
  (if t.t_nth <> 0 then Printf.ksprintf (oc opts) ":"
   else
     match t_date_end with
     | Some _ -> Printf.ksprintf (oc opts) ":"
     | None -> ());
  print_title_date_option opts t_date_end;
  if t.t_nth <> 0 then Printf.ksprintf (oc opts) ":%d" t.t_nth;
  Printf.ksprintf (oc opts) "]"

let zero_birth_is_required opts base is_child p =
  if Driver.get_baptism p <> Date.cdate_None then false
  else
    match Driver.get_death p with
    | Death (_, _) | DeadYoung | DeadDontKnowWhen | OfCourseDead -> true
    | DontKnowIfDead
      when (not is_child)
           && (not (has_infos_not_dates opts base p))
           && Driver.p_first_name base p <> "?"
           && Driver.p_surname base p <> "?" ->
        true
    | _ -> false

let print_infos opts base is_child csrc cbp p =
  List.iter
    (print_first_name_alias opts base)
    (Driver.get_first_names_aliases p);
  List.iter (print_surname_alias opts base) (Driver.get_surnames_aliases p);
  (match Driver.get_public_name p with
  | s when Driver.sou base s <> "" ->
      Printf.ksprintf (oc opts) " (%s)" (correct_string base s)
  | _ -> ());
  if not opts.no_picture then
    print_if_no_empty opts base "#image" (Driver.get_image p);
  List.iter (print_qualifier opts base) (Driver.get_qualifiers p);
  List.iter (print_alias opts base) (Driver.get_aliases p);
  List.iter (print_title opts base) (Driver.get_titles p);
  (match Driver.get_access p with
  | IfTitles -> ()
  | Public -> Printf.ksprintf (oc opts) " #apubl"
  | Private -> Printf.ksprintf (oc opts) " #apriv"
  | SemiPublic -> Printf.ksprintf (oc opts) " #semipub");
  print_if_no_empty opts base "#occu" (Driver.get_occupation p);
  print_src_if_not_equal_to opts csrc base "#src" (Driver.get_psources p);
  (match Date.od_of_cdate (Driver.get_birth p) with
  | Some d ->
      Printf.ksprintf (oc opts) " ";
      print_date opts d
  | _ when zero_birth_is_required opts base is_child p ->
      Printf.ksprintf (oc opts) " 0"
  | None -> ());
  print_if_not_equal_to opts cbp base "#bp" (Driver.get_birth_place p);
  if opts.source = None then
    print_if_no_empty opts base "#bs" (Driver.get_birth_src p);
  (match Date.od_of_cdate (Driver.get_baptism p) with
  | Some d ->
      Printf.ksprintf (oc opts) " !";
      print_date opts d
  | None -> ());
  print_if_no_empty opts base "#pp" (Driver.get_baptism_place p);
  if opts.source = None then
    print_if_no_empty opts base "#ps" (Driver.get_baptism_src p);
  (match Driver.get_death p with
  | Death (dr, d) ->
      Printf.ksprintf (oc opts) " ";
      (match dr with
      | Killed -> Printf.ksprintf (oc opts) "k"
      | Murdered -> Printf.ksprintf (oc opts) "m"
      | Executed -> Printf.ksprintf (oc opts) "e"
      | Disappeared -> Printf.ksprintf (oc opts) "s"
      | _ -> ());
      print_date opts (Date.date_of_cdate d)
  | DeadYoung -> Printf.ksprintf (oc opts) " mj"
  | DeadDontKnowWhen -> Printf.ksprintf (oc opts) " 0"
  | DontKnowIfDead -> (
      match
        ( Date.od_of_cdate (Driver.get_birth p),
          Date.od_of_cdate (Driver.get_baptism p) )
      with
      | Some _, _ | _, Some _ -> Printf.ksprintf (oc opts) " ?"
      | _ -> ())
  | OfCourseDead -> Printf.ksprintf (oc opts) " od"
  | NotDead -> ());
  print_if_no_empty opts base "#dp" (Driver.get_death_place p);
  if opts.source = None then
    print_if_no_empty opts base "#ds" (Driver.get_death_src p);
  print_burial opts (Driver.get_burial p);
  print_if_no_empty opts base "#rp" (Driver.get_burial_place p);
  if opts.source = None then
    print_if_no_empty opts base "#rs" (Driver.get_burial_src p)

type gen = {
  mark : (Driver.iper, bool) Collection.Marker.t;
  mark_rel : (Driver.iper, bool) Collection.Marker.t;
  per_sel : Driver.iper -> bool;
  fam_sel : Driver.ifam -> bool;
  fam_done : (Driver.ifam, bool) Collection.Marker.t;
  mutable notes_pl_p : Driver.person list;
  mutable ext_files : (string * string list ref) list;
  mutable notes_alias : (string * string) list;
  mutable pevents_pl_p : Driver.person list;
}

let map_notes aliases f = try List.assoc f aliases with Not_found -> f

let add_linked_files gen from s some_linked_files =
  let slen = String.length s in
  let rec loop new_linked_files i =
    if i = slen then new_linked_files
    else if i < slen - 2 && s.[i] = '[' && s.[i + 1] = '[' && s.[i + 2] = '['
    then
      let j =
        let rec loop j =
          if j = slen then j
          else if
            j < slen - 2 && s.[j] = ']' && s.[j + 1] = ']' && s.[j + 2] = ']'
          then j + 3
          else loop (j + 1)
        in
        loop (i + 3)
      in
      if j > i + 6 then
        let b = String.sub s (i + 3) (j - i - 6) in
        let fname =
          try
            let k = String.index b '/' in
            String.sub b 0 k
          with Not_found -> b
        in
        let fname = map_notes gen.notes_alias fname in
        let f = from () in
        let new_linked_files =
          try
            let r = List.assoc fname gen.ext_files in
            if List.mem f !r then () else r := f :: !r;
            new_linked_files
          with Not_found ->
            let lf = (fname, ref [ f ]) in
            gen.ext_files <- lf :: gen.ext_files;
            lf :: new_linked_files
        in
        loop new_linked_files j
      else loop new_linked_files (i + 1)
    else loop new_linked_files (i + 1)
  in
  loop some_linked_files 0

let print_parent opts base gen p =
  let has_printed_parents =
    match Driver.get_parents p with
    | Some ifam -> gen.fam_sel ifam
    | None -> false
  in
  let first_parent_definition =
    if Collection.Marker.get gen.mark (Driver.get_iper p) then false
    else (
      Collection.Marker.set gen.mark (Driver.get_iper p) true;
      true)
  in
  let pr = (not has_printed_parents) && first_parent_definition in
  let has_infos = if pr then has_infos opts base p else false in
  let first_name = Driver.sou base (Driver.get_first_name p) in
  let surname = Driver.sou base (Driver.get_surname p) in
  Printf.ksprintf (oc opts) "%s %s%s" (s_correct_string surname)
    (s_correct_string first_name)
    (if first_name = "?" && surname = "?" then ""
     else if get_new_occ p = 0 then ""
     else "." ^ string_of_int (get_new_occ p));
  if pr then
    if has_infos then print_infos opts base false "" "" p
    else if first_name <> "?" && surname <> "?" then
      Printf.ksprintf (oc opts) " 0"

let print_child opts base fam_surname csrc cbp p =
  Printf.ksprintf (oc opts) "-";
  (match Driver.get_sex p with
  | Male -> Printf.ksprintf (oc opts) " h"
  | Female -> Printf.ksprintf (oc opts) " f"
  | _ -> ());
  Printf.ksprintf (oc opts) " %s"
    (s_correct_string (Driver.sou base (Driver.get_first_name p)));
  if Driver.p_first_name base p = "?" && Driver.p_surname base p = "?" then ()
  else if get_new_occ p = 0 then ()
  else Printf.ksprintf (oc opts) ".%d" (get_new_occ p);
  if not (Driver.Istr.equal (Driver.get_surname p) fam_surname) then
    Printf.ksprintf (oc opts) " %s"
      (s_correct_string_nonum (Driver.sou base (Driver.get_surname p)));
  print_infos opts base true csrc cbp p;
  Printf.ksprintf (oc opts) "\n"

let bogus_person base p =
  Driver.p_first_name base p = "?" && Driver.p_surname base p = "?"

let common_children proj base children =
  if Array.length children <= 1 then None
  else
    let list =
      List.map (fun p -> Driver.sou base (proj p)) (Array.to_list children)
    in
    if List.mem "" list then None
    else
      let list = List.sort compare list in
      let src_max, n_max, _, _ =
        List.fold_left
          (fun (src_max, n_max, prev_src, n) src ->
            if src = prev_src then
              let n = n + 1 in
              if n > n_max then (src, n, src, n) else (src_max, n_max, src, n)
            else (src_max, n_max, src, 1))
          ("", 0, "", 0) list
      in
      if n_max > 1 then Some src_max else None

let common_children_sources = common_children Driver.get_psources
let common_children_birth_place = common_children Driver.get_birth_place

let empty_family base m =
  bogus_person base m.m_fath && bogus_person base m.m_moth
  && Array.for_all (bogus_person base) m.m_chil

let string_of_witness_kind :
    Def.witness_kind -> ('a, unit, string, unit) format4 option = function
  | Witness_GodParent -> Some "#godp"
  | Witness -> None
  | Witness_CivilOfficer -> Some "#offi"
  | Witness_ReligiousOfficer -> Some "#reli"
  | Witness_Informant -> Some "#info"
  | Witness_Attending -> Some "#atte"
  | Witness_Mentioned -> Some "#ment"
  | Witness_Other -> Some "#othe"

let print_witness opts base gen p =
  Printf.ksprintf (oc opts) "%s %s%s"
    (correct_string base (Driver.get_surname p))
    (correct_string base (Driver.get_first_name p))
    (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
  if
    Array.length (Driver.get_family p) = 0
    && Driver.get_parents p = None
    && not (Collection.Marker.get gen.mark (Driver.get_iper p))
  then (
    Collection.Marker.set gen.mark (Driver.get_iper p) true;
    if has_infos opts base p then print_infos opts base false "" "" p
    else Printf.ksprintf (oc opts) " 0";
    (match Driver.sou base (Driver.get_notes p) with
    | "" ->
        if put_events_in_notes base p then gen.notes_pl_p <- p :: gen.notes_pl_p
    | _ -> gen.notes_pl_p <- p :: gen.notes_pl_p);
    if Driver.get_pevents p <> [] then gen.pevents_pl_p <- p :: gen.pevents_pl_p)

let print_pevent opts base gen e =
  (match e.epers_name with
  | Epers_Birth -> Printf.ksprintf (oc opts) "#birt"
  | Epers_Baptism -> Printf.ksprintf (oc opts) "#bapt"
  | Epers_Death -> Printf.ksprintf (oc opts) "#deat"
  | Epers_Burial -> Printf.ksprintf (oc opts) "#buri"
  | Epers_Cremation -> Printf.ksprintf (oc opts) "#crem"
  | Epers_Accomplishment -> Printf.ksprintf (oc opts) "#acco"
  | Epers_Acquisition -> Printf.ksprintf (oc opts) "#acqu"
  | Epers_Adhesion -> Printf.ksprintf (oc opts) "#adhe"
  | Epers_BaptismLDS -> Printf.ksprintf (oc opts) "#bapl"
  | Epers_BarMitzvah -> Printf.ksprintf (oc opts) "#barm"
  | Epers_BatMitzvah -> Printf.ksprintf (oc opts) "#basm"
  | Epers_Benediction -> Printf.ksprintf (oc opts) "#bles"
  | Epers_ChangeName -> Printf.ksprintf (oc opts) "#chgn"
  | Epers_Circumcision -> Printf.ksprintf (oc opts) "#circ"
  | Epers_Confirmation -> Printf.ksprintf (oc opts) "#conf"
  | Epers_ConfirmationLDS -> Printf.ksprintf (oc opts) "#conl"
  | Epers_Decoration -> Printf.ksprintf (oc opts) "#awar"
  | Epers_DemobilisationMilitaire -> Printf.ksprintf (oc opts) "#demm"
  | Epers_Diploma -> Printf.ksprintf (oc opts) "#degr"
  | Epers_Distinction -> Printf.ksprintf (oc opts) "#dist"
  | Epers_Dotation -> Printf.ksprintf (oc opts) "#endl"
  | Epers_DotationLDS -> Printf.ksprintf (oc opts) "#dotl"
  | Epers_Education -> Printf.ksprintf (oc opts) "#educ"
  | Epers_Election -> Printf.ksprintf (oc opts) "#elec"
  | Epers_Emigration -> Printf.ksprintf (oc opts) "#emig"
  | Epers_Excommunication -> Printf.ksprintf (oc opts) "#exco"
  | Epers_FamilyLinkLDS -> Printf.ksprintf (oc opts) "#flkl"
  | Epers_FirstCommunion -> Printf.ksprintf (oc opts) "#fcom"
  | Epers_Funeral -> Printf.ksprintf (oc opts) "#fune"
  | Epers_Graduate -> Printf.ksprintf (oc opts) "#grad"
  | Epers_Hospitalisation -> Printf.ksprintf (oc opts) "#hosp"
  | Epers_Illness -> Printf.ksprintf (oc opts) "#illn"
  | Epers_Immigration -> Printf.ksprintf (oc opts) "#immi"
  | Epers_ListePassenger -> Printf.ksprintf (oc opts) "#lpas"
  | Epers_MilitaryDistinction -> Printf.ksprintf (oc opts) "#mdis"
  | Epers_MilitaryPromotion -> Printf.ksprintf (oc opts) "#mpro"
  | Epers_MilitaryService -> Printf.ksprintf (oc opts) "#mser"
  | Epers_MobilisationMilitaire -> Printf.ksprintf (oc opts) "#mobm"
  | Epers_Naturalisation -> Printf.ksprintf (oc opts) "#natu"
  | Epers_Occupation -> Printf.ksprintf (oc opts) "#occu"
  | Epers_Ordination -> Printf.ksprintf (oc opts) "#ordn"
  | Epers_Property -> Printf.ksprintf (oc opts) "#prop"
  | Epers_Recensement -> Printf.ksprintf (oc opts) "#cens"
  | Epers_Residence -> Printf.ksprintf (oc opts) "#resi"
  | Epers_Retired -> Printf.ksprintf (oc opts) "#reti"
  | Epers_ScellentChildLDS -> Printf.ksprintf (oc opts) "#slgc"
  | Epers_ScellentParentLDS -> Printf.ksprintf (oc opts) "#slgp"
  | Epers_ScellentSpouseLDS -> Printf.ksprintf (oc opts) "#slgs"
  | Epers_VenteBien -> Printf.ksprintf (oc opts) "#vteb"
  | Epers_Will -> Printf.ksprintf (oc opts) "#will"
  | Epers_Name s -> Printf.ksprintf (oc opts) "#%s" (correct_string base s));
  Printf.ksprintf (oc opts) " ";
  let epers_date = Date.od_of_cdate e.epers_date in
  print_date_option opts epers_date;
  print_if_no_empty opts base "#p" e.epers_place;
  (* TODO *)
  (*print_if_no_empty opts base "#c" e.epers_cause;*)
  if opts.source = None then print_if_no_empty opts base "#s" e.epers_src;
  Printf.ksprintf (oc opts) "\n";
  Array.iter
    (fun (ip, wk) ->
      if gen.per_sel ip then (
        let p = Driver.poi base ip in
        Printf.ksprintf (oc opts) "wit";
        (match Driver.get_sex p with
        | Male -> Printf.ksprintf (oc opts) " m"
        | Female -> Printf.ksprintf (oc opts) " f"
        | _ -> ());
        Printf.ksprintf (oc opts) ": ";
        let sk = string_of_witness_kind wk in
        (match sk with
        | Some s -> Printf.ksprintf (oc opts) (s ^^ " ")
        | None -> ());
        print_witness opts base gen p;
        Printf.ksprintf (oc opts) "\n"))
    e.epers_witnesses;
  let note =
    if opts.no_notes <> `nnn then Driver.sou base e.epers_note else ""
  in
  if note <> "" then
    List.iter
      (fun line -> Printf.ksprintf (oc opts) "note %s\n" line)
      (lines_list_of_string note)

let get_persons_with_pevents m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (Driver.get_pevents fath, Driver.get_parents fath) with
    | [], _ | _, Some _ -> list
    | _ -> fath :: list
  in
  let list =
    match (Driver.get_pevents moth, Driver.get_parents moth) with
    | [], _ | _, Some _ -> list
    | _ -> moth :: list
  in
  Array.fold_right
    (fun p list -> if Driver.get_pevents p = [] then list else p :: list)
    m.m_chil list

let print_pevents_for_person opts base gen p =
  let pevents = Driver.get_pevents p in
  let surn = s_correct_string (Driver.p_surname base p) in
  let fnam = s_correct_string (Driver.p_first_name base p) in
  if pevents <> [] && surn <> "?" && fnam <> "?" then (
    Printf.ksprintf (oc opts) "\n";
    Printf.ksprintf (oc opts) "pevt %s %s%s\n" surn fnam
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    List.iter (print_pevent opts base gen) pevents;
    Printf.ksprintf (oc opts) "end pevt\n")

let rec list_memf f x = function
  | [] -> false
  | a :: l -> f x a || list_memf f x l

let eq_key p1 p2 = Driver.get_iper p1 = Driver.get_iper p2
let eq_key_fst (p1, _) (p2, _) = Driver.get_iper p1 = Driver.get_iper p2

let print_pevents opts base gen ml =
  let pl = List.fold_right get_persons_with_pevents ml gen.pevents_pl_p in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl)
      pl []
  in
  List.iter
    (fun p ->
      if gen.per_sel (Driver.get_iper p) then
        print_pevents_for_person opts base gen p)
    pl

let print_fevent opts base gen in_comment e =
  let print_sep () =
    if not in_comment then Printf.ksprintf (oc opts) "\n"
    else Printf.ksprintf (oc opts) " "
  in
  (match e.efam_name with
  | Efam_Marriage -> Printf.ksprintf (oc opts) "#marr"
  | Efam_NoMarriage -> Printf.ksprintf (oc opts) "#nmar"
  | Efam_NoMention -> Printf.ksprintf (oc opts) "#nmen"
  | Efam_Engage -> Printf.ksprintf (oc opts) "#enga"
  | Efam_Divorce -> Printf.ksprintf (oc opts) "#div"
  | Efam_Separated -> Printf.ksprintf (oc opts) "#sep"
  | Efam_Annulation -> Printf.ksprintf (oc opts) "#anul"
  | Efam_MarriageBann -> Printf.ksprintf (oc opts) "#marb"
  | Efam_MarriageContract -> Printf.ksprintf (oc opts) "#marc"
  | Efam_MarriageLicense -> Printf.ksprintf (oc opts) "#marl"
  | Efam_PACS -> Printf.ksprintf (oc opts) "#pacs"
  | Efam_Residence -> Printf.ksprintf (oc opts) "#resi"
  | Efam_Name n -> Printf.ksprintf (oc opts) "#%s" (correct_string base n));
  Printf.ksprintf (oc opts) " ";
  let efam_date = Date.od_of_cdate e.efam_date in
  print_date_option opts efam_date;
  print_if_no_empty opts base "#p" e.efam_place;
  (*print_if_no_empty opts base "#c" e.efam_cause;*)
  if opts.source = None then print_if_no_empty opts base "#s" e.efam_src;
  print_sep ();
  Array.iter
    (fun (ip, wk) ->
      if gen.per_sel ip then (
        let p = Driver.poi base ip in
        Printf.ksprintf (oc opts) "wit";
        (match Driver.get_sex p with
        | Male -> Printf.ksprintf (oc opts) " m"
        | Female -> Printf.ksprintf (oc opts) " f"
        | _ -> ());
        Printf.ksprintf (oc opts) ": ";
        let sk = string_of_witness_kind wk in
        (match sk with
        | Some s -> Printf.ksprintf (oc opts) (s ^^ " ")
        | None -> ());
        print_witness opts base gen p;
        print_sep ()))
    e.efam_witnesses;
  let note =
    if opts.no_notes <> `nnn then Driver.sou base e.efam_note else ""
  in
  if note <> "" then
    List.iter
      (fun line ->
        Printf.ksprintf (oc opts) "note %s" line;
        print_sep ())
      (lines_list_of_string note)

let print_comment_for_family opts base gen fam =
  let comm =
    if opts.no_notes <> `nnn then Driver.sou base (Driver.get_comment fam)
    else ""
  in
  (* Si on est en mode old_gw, on mets tous les évènements dans les notes. *)
  (* On supprime les 2 évènements principaux. *)
  let fevents =
    List.filter
      (fun evt ->
        match evt.efam_name with
        | Efam_Divorce | Efam_Engage | Efam_Marriage | Efam_NoMarriage
        | Efam_NoMention | Efam_Separated ->
            false
        | _ -> true)
      (Driver.get_fevents fam)
  in
  let has_evt =
    !old_gw
    && (fevents <> [] || Driver.sou base (Driver.get_marriage_note fam) <> "")
  in
  if comm <> "" || has_evt then (
    Printf.ksprintf (oc opts) "comm";
    if comm <> "" then Printf.ksprintf (oc opts) " %s" (no_newlines comm);
    if !old_gw then (
      if Driver.sou base (Driver.get_marriage_note fam) <> "" then
        Printf.ksprintf (oc opts) " marriage: %s"
          (no_newlines (Driver.sou base (Driver.get_marriage_note fam)));
      List.iter
        (fun e ->
          Printf.ksprintf (oc opts) " ";
          print_fevent opts base gen true e)
        fevents);
    Printf.ksprintf (oc opts) "\n")

let print_empty_family opts base p =
  let string_quest = Driver.insert_string base "?" in
  Printf.ksprintf (oc opts) "fam ? ?.0 + #noment ? ?.0\n";
  Printf.ksprintf (oc opts) "beg\n";
  print_child opts base string_quest "" "" p;
  Printf.ksprintf (oc opts) "end\n"

let print_family opts base gen m =
  let fam = m.m_fam in
  Printf.ksprintf (oc opts) "fam ";
  print_parent opts base gen m.m_fath;
  Printf.ksprintf (oc opts) " +";
  print_date_option opts (Date.od_of_cdate (Driver.get_marriage fam));
  let print_sexes s =
    let c x =
      match Driver.get_sex x with Male -> 'm' | Female -> 'f' | Neuter -> '?'
    in
    Printf.ksprintf (oc opts) " %s %c%c" s (c m.m_fath) (c m.m_moth)
  in
  (match Driver.get_relation fam with
  | Married -> ()
  | NotMarried -> Printf.ksprintf (oc opts) " #nm"
  | Engaged -> Printf.ksprintf (oc opts) " #eng"
  | NoSexesCheckNotMarried -> print_sexes "#nsck"
  | NoSexesCheckMarried -> print_sexes "#nsckm"
  | NoMention ->
      if not !old_gw then print_sexes "#noment"
      else Printf.ksprintf (oc opts) " #noment"
  (* TODO what should be done with those new options *)
  | MarriageBann -> if not !old_gw then print_sexes "#banns"
  | MarriageContract -> if not !old_gw then print_sexes "#contract"
  | MarriageLicense -> if not !old_gw then print_sexes "#license"
  | Pacs -> if not !old_gw then print_sexes "#pacs"
  | Residence -> if not !old_gw then print_sexes "#residence");
  print_if_no_empty opts base "#mp" (Driver.get_marriage_place fam);
  if opts.source = None then
    print_if_no_empty opts base "#ms" (Driver.get_marriage_src fam);
  (* divorce and separation are events, but we keep it if old_gw *)
  (if !old_gw then
     match Driver.get_divorce fam with
     | Divorced d ->
         let d = Date.od_of_cdate d in
         Printf.ksprintf (oc opts) " -";
         print_date_option opts d
     | _ -> ());
  (if !old_gw then
     match Driver.get_separation fam with
     | Separated _ -> Printf.ksprintf (oc opts) " #sep"
     | Separated_old -> Printf.ksprintf (oc opts) " #sep"
     | _ -> ());
  Printf.ksprintf (oc opts) " ";
  print_parent opts base gen m.m_moth;
  Printf.ksprintf (oc opts) "\n";
  Array.iter
    (fun ip ->
      if gen.per_sel ip then (
        let p = Driver.poi base ip in
        Printf.ksprintf (oc opts) "wit";
        (match Driver.get_sex p with
        | Male -> Printf.ksprintf (oc opts) " m"
        | Female -> Printf.ksprintf (oc opts) " f"
        | _ -> ());
        Printf.ksprintf (oc opts) ": ";
        print_witness opts base gen p;
        Printf.ksprintf (oc opts) "\n"))
    (Driver.get_witnesses fam);
  (match opts.source with
  | None ->
      if Driver.sou base (Driver.get_fsources fam) <> "" then
        Printf.ksprintf (oc opts) "src %s\n"
          (correct_string base (Driver.get_fsources fam))
  | Some "" -> ()
  | Some x -> Printf.ksprintf (oc opts) "src %s\n" (s_correct_string x));
  let csrc =
    match common_children_sources base m.m_chil with
    | Some s ->
        Printf.ksprintf (oc opts) "csrc %s\n" (s_correct_string s);
        s
    | _ -> ""
  in
  let cbp =
    match common_children_birth_place base m.m_chil with
    | Some s ->
        Printf.ksprintf (oc opts) "cbp %s\n" (s_correct_string s);
        s
    | _ -> ""
  in
  print_comment_for_family opts base gen fam;
  if (not !old_gw) && Driver.get_fevents fam <> [] then (
    Printf.ksprintf (oc opts) "fevt\n";
    List.iter (print_fevent opts base gen false) (Driver.get_fevents fam);
    Printf.ksprintf (oc opts) "end fevt\n");
  (match Array.length m.m_chil with
  | 0 -> ()
  | _ ->
      let fam_surname = Driver.get_surname m.m_fath in
      Printf.ksprintf (oc opts) "beg\n";
      Array.iter
        (fun p ->
          if gen.per_sel (Driver.get_iper p) then
            print_child opts base fam_surname csrc cbp p)
        m.m_chil;
      Printf.ksprintf (oc opts) "end\n");
  Collection.Marker.set gen.fam_done m.m_ifam true;
  let f _ =
    Printf.sprintf "family \"%s.%d %s\" & \"%s.%d %s\""
      (Driver.p_first_name base m.m_fath)
      (get_new_occ m.m_fath)
      (Driver.p_surname base m.m_fath)
      (Driver.p_first_name base m.m_moth)
      (get_new_occ m.m_moth)
      (Driver.p_surname base m.m_moth)
  in
  let s =
    let sl =
      let acc =
        [
          Driver.get_comment fam;
          Driver.get_marriage_note fam;
          Driver.get_marriage_src fam;
        ]
      in
      if opts.source = None then Driver.get_fsources fam :: acc else acc
    in
    let sl =
      if not !old_gw then
        let rec loop l accu =
          match l with
          | [] -> accu
          | evt :: l ->
              let acc =
                evt.efam_note
                :: (if opts.source = None then evt.efam_src :: accu else accu)
              in
              loop l acc
        in
        loop (Driver.get_fevents fam) sl
      else sl
    in
    String.concat " " (List.map (Driver.sou base) sl)
  in
  ignore (add_linked_files gen f s [] : _ list)

let get_persons_with_notes m list =
  let list =
    let fath = m.m_fath in
    match Driver.get_parents fath with Some _ -> list | None -> fath :: list
  in
  let list =
    let moth = m.m_moth in
    match Driver.get_parents moth with Some _ -> list | None -> moth :: list
  in
  Array.fold_right List.cons m.m_chil list

let notes_aliases bdir =
  let fname = Filename.concat bdir "notes.alias" in
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
  | Some ic ->
      let rec loop list =
        match try Some (input_line ic) with End_of_file -> None with
        | Some s ->
            let list =
              try
                let i = String.index s ' ' in
                ( String.sub s 0 i,
                  String.sub s (i + 1) (String.length s - i - 1) )
                :: list
              with Not_found -> list
            in
            loop list
        | None ->
            close_in ic;
            list
      in
      loop []
  | None -> []

let print_notes_for_person opts base gen p =
  let print_witness_in_notes witnesses =
    Array.iter
      (fun (ip, wk) ->
        let p = Driver.poi base ip in
        Printf.ksprintf (oc opts) "wit";
        (match Driver.get_sex p with
        | Male -> Printf.ksprintf (oc opts) " m"
        | Female -> Printf.ksprintf (oc opts) " f"
        | _ -> ());
        Printf.ksprintf (oc opts) ": ";
        let sk = string_of_witness_kind wk in
        (match sk with
        | Some s -> Printf.ksprintf (oc opts) (s ^^ " ")
        | None -> ());
        print_witness opts base gen p;
        Printf.ksprintf (oc opts) "\n")
      witnesses
  in
  let epers_name_to_string evt =
    match evt.epers_name with
    | Epers_Birth -> "birth"
    | Epers_Baptism -> "baptism"
    | Epers_Death -> "death"
    | Epers_Burial -> "burial"
    | Epers_Cremation -> "cremation"
    | _ -> ""
  in
  let notes =
    if opts.no_notes <> `nnn then Driver.sou base (Driver.get_notes p) else ""
  in
  let surn = s_correct_string (Driver.p_surname base p) in
  let fnam = s_correct_string (Driver.p_first_name base p) in
  (* Si on n'est en mode old_gw, on mets tous les évènements dans les notes. *)
  if (notes <> "" || put_events_in_notes base p) && surn <> "?" && fnam <> "?"
  then (
    Printf.ksprintf (oc opts) "\n";
    Printf.ksprintf (oc opts) "notes %s %s%s\n" surn fnam
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    Printf.ksprintf (oc opts) "beg\n";
    if notes <> "" then Printf.ksprintf (oc opts) "%s\n" notes;
    (if put_events_in_notes base p then
       let rec loop pevents =
         match pevents with
         | [] -> ()
         | evt :: events -> (
             match evt.epers_name with
             | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
             | Epers_Cremation ->
                 let name = epers_name_to_string evt in
                 let notes =
                   if opts.no_notes <> `nnn then Driver.sou base evt.epers_note
                   else ""
                 in
                 if notes <> "" then
                   Printf.ksprintf (oc opts) "%s: %s\n" name notes;
                 print_witness_in_notes evt.epers_witnesses;
                 loop events
             | _ ->
                 print_pevent opts base gen evt;
                 loop events)
       in
       loop (Driver.get_pevents p));
    Printf.ksprintf (oc opts) "end notes\n");
  let f _ =
    Printf.sprintf "person \"%s.%d %s\""
      (Driver.p_first_name base p)
      (get_new_occ p) (Driver.p_surname base p)
  in
  let s =
    let aux g = Driver.sou base (g p) in
    let sl =
      if opts.no_notes <> `nnn then
        [
          aux Driver.get_notes;
          aux Driver.get_birth_note;
          aux Driver.get_baptism_note;
          aux Driver.get_death_note;
          aux Driver.get_burial_note;
        ]
      else []
    in
    let sl =
      match opts.source with
      | Some "" -> sl
      | Some src -> src :: sl
      | None ->
          aux Driver.get_birth_src :: aux Driver.get_baptism_src
          :: aux Driver.get_death_src :: aux Driver.get_burial_src
          :: aux Driver.get_psources :: sl
    in
    if (not !old_gw) && opts.source = None then
      List.fold_left
        (fun acc e ->
          let acc =
            if opts.no_notes <> `nnn then Driver.sou base e.epers_note :: acc
            else acc
          in
          let acc =
            if opts.source = None then Driver.sou base e.epers_src :: acc
            else acc
          in
          acc)
        sl (Driver.get_pevents p)
    else sl
  in
  let s = String.concat " " s in
  ignore (add_linked_files gen f s [] : _ list)

let print_notes opts base gen ml =
  let pl = List.fold_right get_persons_with_notes ml gen.notes_pl_p in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl)
      pl []
  in
  List.iter
    (fun p ->
      if gen.per_sel (Driver.get_iper p) then
        print_notes_for_person opts base gen p)
    pl

let is_isolated p =
  match Driver.get_parents p with
  | Some _ -> false
  | None -> Array.length (Driver.get_family p) = 0

let is_definition_for_parent p =
  match Driver.get_parents p with Some _ -> false | None -> true

let get_isolated_related base m list =
  let concat_isolated p_relation ip list =
    let p = Driver.poi base ip in
    if List.mem_assq p list then list
    else if is_isolated p then
      match Driver.get_rparents p with
      | { r_fath = Some x; _ } :: _ when x = Driver.get_iper p_relation ->
          list @ [ (p, true) ]
      | { r_fath = None; r_moth = Some x; _ } :: _
        when x = Driver.get_iper p_relation ->
          list @ [ (p, true) ]
      | _ -> list
    else list
  in
  let list =
    if is_definition_for_parent m.m_fath then
      List.fold_right (concat_isolated m.m_fath)
        (Driver.get_related m.m_fath)
        list
    else list
  in
  let list =
    if is_definition_for_parent m.m_moth then
      List.fold_right (concat_isolated m.m_moth)
        (Driver.get_related m.m_moth)
        list
    else list
  in
  Array.fold_right
    (fun p list ->
      List.fold_right (concat_isolated p) (Driver.get_related p) list)
    m.m_chil list

let get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (Driver.get_rparents fath, Driver.get_parents fath) with
    | [], _ | _, Some _ -> list
    | _ -> (fath, false) :: list
  in
  let list =
    match (Driver.get_rparents moth, Driver.get_parents moth) with
    | [], _ | _, Some _ -> list
    | _ -> (moth, false) :: list
  in
  let list =
    Array.fold_right
      (fun ip list ->
        let p = Driver.poi base ip in
        match (Driver.get_rparents p, Driver.get_parents p) with
        | [], _ | _, Some _ -> list
        | { r_fath = Some x; _ } :: _, _ when x <> Driver.get_iper m.m_fath ->
            list
        | _ -> (p, false) :: list)
      (Driver.get_witnesses m.m_fam)
      list
  in
  Array.fold_right
    (fun p list ->
      match Driver.get_rparents p with [] -> list | _ -> (p, false) :: list)
    m.m_chil list

let print_relation_parent opts base mark defined_p p =
  Printf.ksprintf (oc opts) "%s %s%s"
    (correct_string base (Driver.get_surname p))
    (correct_string base (Driver.get_first_name p))
    (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
  if
    Array.length (Driver.get_family p) = 0
    && Driver.get_parents p = None
    && not (Collection.Marker.get mark (Driver.get_iper p))
  then (
    Collection.Marker.set mark (Driver.get_iper p) true;
    if has_infos opts base p then print_infos opts base false "" "" p
    else Printf.ksprintf (oc opts) " 0";
    defined_p := p :: !defined_p)

let print_relation_for_person opts base gen def_p r =
  let fath =
    match r.r_fath with
    | Some ip ->
        if gen.per_sel ip then
          let p = Driver.poi base ip in
          if
            Driver.sou base (Driver.get_first_name p) = "?"
            || Driver.sou base (Driver.get_surname p) = "?"
          then None
          else Some p
        else None
    | None -> None
  in
  let moth =
    match r.r_moth with
    | Some ip ->
        if gen.per_sel ip then
          let p = Driver.poi base ip in
          if
            Driver.sou base (Driver.get_first_name p) = "?"
            || Driver.sou base (Driver.get_surname p) = "?"
          then None
          else Some p
        else None
    | None -> None
  in
  let err_same_sex =
    match (fath, moth) with
    | Some fath, Some moth -> Driver.get_sex fath = Driver.get_sex moth
    | _ -> false
  in
  let print_err_one_relation p =
    Printf.ksprintf (oc opts) "- ";
    (match r.r_type with
    | Adoption -> Printf.ksprintf (oc opts) "adop"
    | Recognition -> Printf.ksprintf (oc opts) "reco"
    | CandidateParent -> Printf.ksprintf (oc opts) "cand"
    | GodParent -> Printf.ksprintf (oc opts) "godp"
    | FosterParent -> Printf.ksprintf (oc opts) "fost");
    if Driver.get_sex p = Male then Printf.ksprintf (oc opts) " fath"
    else Printf.ksprintf (oc opts) " moth";
    Printf.ksprintf (oc opts) ": ";
    print_relation_parent opts base gen.mark def_p p;
    Printf.ksprintf (oc opts) "\n"
  in
  match (fath, moth) with
  | None, None -> ()
  | _ ->
      if err_same_sex then
        match (fath, moth) with
        | Some fath, Some moth ->
            print_err_one_relation fath;
            print_err_one_relation moth
        | _ -> ()
      else (
        Printf.ksprintf (oc opts) "- ";
        (match r.r_type with
        | Adoption -> Printf.ksprintf (oc opts) "adop"
        | Recognition -> Printf.ksprintf (oc opts) "reco"
        | CandidateParent -> Printf.ksprintf (oc opts) "cand"
        | GodParent -> Printf.ksprintf (oc opts) "godp"
        | FosterParent -> Printf.ksprintf (oc opts) "fost");
        (match (fath, moth) with
        | Some fath, None ->
            if Driver.get_sex fath = Male then Printf.ksprintf (oc opts) " fath"
            else Printf.ksprintf (oc opts) " moth"
        | None, Some moth ->
            if Driver.get_sex moth = Female then
              Printf.ksprintf (oc opts) " moth"
            else Printf.ksprintf (oc opts) " fath"
        | _ -> ());
        Printf.ksprintf (oc opts) ": ";
        (match (fath, moth) with
        | Some fath, None -> print_relation_parent opts base gen.mark def_p fath
        | None, Some moth -> print_relation_parent opts base gen.mark def_p moth
        | Some fath, Some moth ->
            if Driver.get_sex fath = Male && Driver.get_sex moth = Female then (
              print_relation_parent opts base gen.mark def_p fath;
              Printf.ksprintf (oc opts) " + ";
              print_relation_parent opts base gen.mark def_p moth)
            else (
              print_relation_parent opts base gen.mark def_p moth;
              Printf.ksprintf (oc opts) " + ";
              print_relation_parent opts base gen.mark def_p fath)
        | _ -> ());
        Printf.ksprintf (oc opts) "\n")

let print_relations_for_person opts base gen def_p is_definition p =
  let surn = correct_string base (Driver.get_surname p) in
  let fnam = correct_string base (Driver.get_first_name p) in
  let exist_relation =
    List.exists
      (fun r ->
        match (r.r_fath, r.r_moth) with
        | Some ip1, Some ip2 -> gen.per_sel ip1 && gen.per_sel ip2
        | Some ip1, _ -> gen.per_sel ip1
        | _, Some ip2 -> gen.per_sel ip2
        | _ -> false)
      (Driver.get_rparents p)
  in
  if
    surn <> "?" && fnam <> "?" && exist_relation
    && not (Collection.Marker.get gen.mark_rel (Driver.get_iper p))
  then (
    Collection.Marker.set gen.mark_rel (Driver.get_iper p) true;
    Printf.ksprintf (oc opts) "\n";
    Printf.ksprintf (oc opts) "rel %s %s%s" surn fnam
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    if is_definition then (
      Collection.Marker.set gen.mark (Driver.get_iper p) true;
      def_p := p :: !def_p;
      if has_infos opts base p then print_infos opts base false "" "" p
      else Printf.ksprintf (oc opts) " 0";
      match Driver.get_sex p with
      | Male -> Printf.ksprintf (oc opts) " #h"
      | Female -> Printf.ksprintf (oc opts) " #f"
      | Neuter -> ());
    Printf.ksprintf (oc opts) "\n";
    Printf.ksprintf (oc opts) "beg\n";
    List.iter
      (print_relation_for_person opts base gen def_p)
      (Driver.get_rparents p);
    Printf.ksprintf (oc opts) "end\n")

let print_relations opts base gen ml =
  let pl = List.fold_right (get_persons_with_relations base) ml [] in
  let pl = List.fold_right (get_isolated_related base) ml pl in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else p :: pl)
      pl []
  in
  let rec loop = function
    | [] -> ()
    | (p, if_def) :: pl ->
        let def_p = ref [] in
        if Driver.get_rparents p <> [] && gen.per_sel (Driver.get_iper p) then (
          print_relations_for_person opts base gen def_p if_def p;
          List.iter (print_notes_for_person opts base gen) !def_p;
          if not !old_gw then
            List.iter (print_pevents_for_person opts base gen) !def_p);
        loop (pl @ List.map (fun p -> (p, false)) !def_p)
  in
  loop pl

let print_isolated_relations opts base gen p =
  let pl = [ (p, false) ] in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else p :: pl)
      pl []
  in
  let rec loop = function
    | [] -> ()
    | (p, if_def) :: pl ->
        let def_p = ref [] in
        if Driver.get_rparents p <> [] && gen.per_sel (Driver.get_iper p) then (
          print_relations_for_person opts base gen def_p if_def p;
          List.iter (print_notes_for_person opts base gen) !def_p);
        loop (pl @ List.map (fun p -> (p, false)) !def_p)
  in
  loop pl

let rec merge_families ifaml1f ifaml2f =
  match (ifaml1f, ifaml2f) with
  | ifam1 :: ifaml1, ifam2 :: ifaml2 ->
      let m1 = List.mem ifam1 ifaml2 in
      let m2 = List.mem ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then ifam2 :: merge_families ifaml1f ifaml2
      else if m2 then ifam1 :: merge_families ifaml1 ifaml2f
      else if ifam2 < ifam1 then ifam2 :: ifam1 :: merge_families ifaml1 ifaml2
      else if ifam1 < ifam2 then ifam1 :: ifam2 :: merge_families ifaml1 ifaml2
      else ifam1 :: merge_families ifaml1 ifaml2
  | ifaml1, [] -> ifaml1
  | [], ifaml2 -> ifaml2

(* let connected_families base fam_sel ifam cpl =
 *   let rec loop ifaml scanned =
 *     function
 *     | ip :: ipl ->
 *       let scanned = ip :: scanned in
 *       let ipl, ifaml =
 *         Array.fold_right begin fun i (acci, accf) ->
 *           if fam_sel i && not @@ List.mem i accf
 *           then
 *             let accf = i :: accf in
 *             let cpl = foi base ifam in
 *             let fa = get_father cpl in
 *             let mo = get_mother cpl in
 *             let acci =
 *               if not @@ List.mem fa acci
 *               && not @@ List.mem fa scanned
 *               then fa :: acci
 *               else acci
 *             in
 *             let acci =
 *               if not @@ List.mem mo acci
 *               && not @@ List.mem mo scanned
 *               then mo :: acci
 *               else acci
 *             in
 *             (acci, accf)
 *           else (acci, accf)
 *         end (get_family @@ Driver.poi base ip) (ipl, ifaml)
 *       in
 *       loop ifaml scanned ipl
 *     | [] -> ifaml
 *   in
 *   loop [ ifam ] [] [ get_father cpl ; get_mother cpl ]
 *   |> List.sort_uniq compare *)

let rec filter f = function
  | x :: l -> if f x then x :: filter f l else filter f l
  | [] -> []

let connected_families base fam_sel ifam cpl =
  let rec loop ifaml ipl_scanned = function
    | ip :: ipl ->
        if List.mem ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let u = Driver.poi base ip in
          let ifaml1 = Array.to_list (Driver.get_family u) in
          let ifaml1 = filter fam_sel ifaml1 in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                let cpl = Driver.foi base ifam in
                Driver.get_father cpl :: Driver.get_mother cpl :: ipl)
              ifaml1 ipl
          in
          loop ifaml (ip :: ipl_scanned) ipl
    | [] -> ifaml
  in
  loop [ ifam ] [] [ Driver.get_father cpl ]

let read_file_contents fname =
  match try Some (Secure.open_in fname) with Sys_error _ -> None with
  | Some ic -> (
      let len = ref 0 in
      try
        let rec loop () =
          len := Buff.store !len (input_char ic);
          loop ()
        in
        loop ()
      with End_of_file ->
        close_in ic;
        Buff.get !len)
  | None -> ""

type separate = ToSeparate | NotScanned | BeingScanned | Scanned

let rec find_ancestors base surn p list =
  match Driver.get_parents p with
  | Some ifam ->
      let cpl = Driver.foi base ifam in
      let fath = Driver.poi base (Driver.get_father cpl) in
      let moth = Driver.poi base (Driver.get_mother cpl) in
      if
        (not (Driver.Istr.equal (Driver.get_surname fath) surn))
        && not (Driver.Istr.equal (Driver.get_surname moth) surn)
      then p :: list
      else
        let list =
          if Driver.Istr.equal (Driver.get_surname fath) surn then
            find_ancestors base surn fath list
          else list
        in
        if Driver.Istr.equal (Driver.get_surname moth) surn then
          find_ancestors base surn moth list
        else list
  | None -> p :: list

let mark_branch base mark surn p =
  let rec loop top p =
    for i = 0 to Array.length (Driver.get_family p) - 1 do
      let ifam = (Driver.get_family p).(i) in
      if Collection.Marker.get mark ifam = NotScanned then
        let ifaml =
          connected_families base (fun _ -> true) ifam (Driver.foi base ifam)
        in
        let children =
          List.fold_left
            (fun list ifam ->
              let desc = Driver.foi base ifam in
              Array.fold_left
                (fun list ip -> Driver.poi base ip :: list)
                list (Driver.get_children desc))
            [] ifaml
        in
        if
          top
          || List.exists
               (fun p -> Driver.Istr.equal (Driver.get_surname p) surn)
               children
        then (
          List.iter
            (fun ifam -> Collection.Marker.set mark ifam ToSeparate)
            ifaml;
          List.iter (loop false) children)
    done
  in
  loop true p

let mark_someone base mark s =
  match Gutil.person_ht_find_all base s with
  | [ ip ] ->
      let p = Driver.poi base ip in
      let plist = find_ancestors base (Driver.get_surname p) p [] in
      List.iter (mark_branch base mark (Driver.get_surname p)) plist
  | [] ->
      Printf.eprintf "Error: \"%s\" is not found\n" s;
      flush stderr;
      exit 2
  | _ ->
      Printf.eprintf "Error: several answers for \"%s\"\n" s;
      flush stderr;
      exit 2

let scan_connex_component base test_action len ifam =
  let rec loop len ifam =
    let fam = Driver.foi base ifam in
    let fath = Driver.poi base (Driver.get_father fam) in
    let moth = Driver.poi base (Driver.get_mother fam) in
    let len =
      Array.fold_left
        (fun len ifam1 ->
          if ifam1 = ifam then len else test_action loop len ifam1)
        len (Driver.get_family fath)
    in
    let len =
      Array.fold_left
        (fun len ifam1 ->
          if ifam1 = ifam then len else test_action loop len ifam1)
        len (Driver.get_family moth)
    in
    let len =
      match Driver.get_parents fath with
      | Some ifam -> test_action loop len ifam
      | _ -> len
    in
    let len =
      match Driver.get_parents moth with
      | Some ifam -> test_action loop len ifam
      | _ -> len
    in
    let children = Driver.get_children fam in
    Array.fold_left
      (fun len ip ->
        Array.fold_left (test_action loop) len
          (Driver.get_family (Driver.poi base ip)))
      len children
  in
  loop len ifam

let mark_one_connex_component base mark ifam =
  let origin_file =
    Driver.sou base (Driver.get_origin_file (Driver.foi base ifam))
  in
  let test_action loop len ifam =
    if
      Collection.Marker.get mark ifam = NotScanned
      && Driver.sou base (Driver.get_origin_file (Driver.foi base ifam))
         = origin_file
    then (
      Collection.Marker.set mark ifam BeingScanned;
      loop (len + 1) ifam)
    else len
  in
  let _ = test_action (fun _ _ -> 1) 0 ifam in
  let len = 1 + scan_connex_component base test_action 0 ifam in
  let set_mark x =
    let test_action loop () ifam =
      if Collection.Marker.get mark ifam = BeingScanned then (
        Collection.Marker.set mark ifam x;
        loop () ifam)
    in
    test_action (fun _ _ -> ()) () ifam;
    scan_connex_component base test_action () ifam
  in
  if len <= !sep_limit && (!only_file = "" || !only_file = origin_file) then
    set_mark ToSeparate
  else (
    Printf.eprintf "%s: group of size %d not included\n" origin_file len;
    let cpl = Driver.foi base ifam in
    Printf.eprintf "    %s + %s\n"
      (Gutil.designation base (Driver.poi base (Driver.get_father cpl)))
      (Gutil.designation base (Driver.poi base (Driver.get_mother cpl)));
    flush stderr;
    set_mark Scanned)

let mark_connex_components base mark ifam =
  let test_action _loop _len ifam =
    if Collection.Marker.get mark ifam = NotScanned then
      mark_one_connex_component base mark ifam
  in
  scan_connex_component base test_action () ifam

let add_small_connex_components base mark =
  Collection.iter
    (fun i ->
      if Collection.Marker.get mark i = ToSeparate then
        mark_connex_components base mark i)
    (Geneweb_db.Driver.ifams base)

let separate base =
  match List.rev !separate_list with
  | [] -> fun _ -> false
  | list ->
      let ifams = Geneweb_db.Driver.ifams base in
      let mark =
        Geneweb_db.Driver.ifam_marker (Geneweb_db.Driver.ifams base) NotScanned
      in
      List.iter (mark_someone base mark) list;
      add_small_connex_components base mark;
      let len =
        Collection.fold
          (fun acc i ->
            if Collection.Marker.get mark i = ToSeparate then acc + 1 else acc)
          0 ifams
      in
      Printf.eprintf "*** extracted %d families\n" len;
      flush stderr;
      fun ifam -> Collection.Marker.get mark ifam = ToSeparate

let rs_printf opts s =
  let rec loop bol i =
    if i = String.length s then ()
    else if s.[i] = '\n' then (
      Printf.ksprintf (oc opts) "\n";
      loop true (i + 1))
    else (
      if bol then Printf.ksprintf (oc opts) "  ";
      Printf.ksprintf (oc opts) "%c" s.[i];
      loop false (i + 1))
  in
  loop true 0

let gwu opts isolated base in_dir out_dir src_oc_ht (per_sel, fam_sel) =
  if out_dir <> "" && not (Sys.file_exists out_dir) then
    Filesystem.create_dir ~parent:true out_dir;
  let to_separate = separate base in
  let out_oc_first = ref true in
  let _ofile, oc, close = opts.oc in
  let origin_file fname =
    if fname = "" || out_dir = "" then (oc, out_oc_first, close)
    else
      let fname = Filename.basename fname in
      try Hashtbl.find src_oc_ht fname
      with Not_found ->
        let oc = open_out (Filename.concat out_dir fname) in
        let ((out, _, _) as x) =
          (output_string oc, ref true, fun () -> close_out oc)
        in
        if not !raw_output then out "encoding: utf-8\n";
        if !old_gw then out "\n" else out "gwplus\n\n";
        Hashtbl.add src_oc_ht fname x;
        x
  in
  let gen =
    let mark =
      Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) false
    in
    let mark_rel =
      Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) false
    in
    let fam_done =
      Geneweb_db.Driver.ifam_marker (Geneweb_db.Driver.ifams base) false
    in
    {
      mark;
      mark_rel;
      per_sel;
      fam_sel;
      fam_done;
      notes_pl_p = [];
      ext_files = [];
      notes_alias = notes_aliases in_dir;
      pevents_pl_p = [];
    }
  in
  let nb_fam = Driver.nb_of_families base in
  if !Mutil.verbose then ProgrBar.start ();
  Collection.iteri
    (fun i ifam ->
      if !Mutil.verbose then ProgrBar.run i nb_fam;
      if not (Collection.Marker.get gen.fam_done ifam) then
        let fam = Driver.foi base ifam in
        let ifaml = connected_families base gen.fam_sel ifam fam in
        let oc, first, _close =
          if to_separate ifam then (oc, out_oc_first, close)
          else
            let fname = Driver.get_origin_file fam in
            origin_file
              (if Driver.Istr.is_empty fname then "" else Driver.sou base fname)
        in
        let f, _ooc, c = opts.oc in
        let opts = { opts with oc = (f, oc, c) } in
        let ml =
          List.fold_right
            (fun ifam ml ->
              let fam = Driver.foi base ifam in
              let m =
                {
                  m_ifam = ifam;
                  m_fam = fam;
                  m_fath = Driver.poi base (Driver.get_father fam);
                  m_moth = Driver.poi base (Driver.get_mother fam);
                  m_chil =
                    Array.map
                      (fun ip -> Driver.poi base ip)
                      (Driver.get_children fam);
                }
              in
              if empty_family base m then (
                Collection.Marker.set gen.fam_done m.m_ifam true;
                ml)
              else m :: ml)
            ifaml []
        in
        if ml <> [] then (
          gen.notes_pl_p <- [];
          gen.pevents_pl_p <- [];
          if not !first then Printf.ksprintf oc "\n";
          first := false;
          List.iter (print_family opts base gen) ml;
          print_notes opts base gen ml;
          print_relations opts base gen ml;
          if not !old_gw then print_pevents opts base gen ml))
    (Geneweb_db.Driver.ifams ~select:gen.fam_sel base);
  (* Ajout des personnes isolée à l'export. On leur ajoute des    *)
  (* parents pour pouvoir utiliser les autres fonctions normales. *)
  (* Export que si c'est toute la base.                           *)
  if isolated && opts.asc = None && opts.desc = None && opts.ascdesc = None then
    Collection.iter
      (fun i ->
        if
          (not @@ Collection.Marker.get gen.mark i)
          && (not @@ Collection.Marker.get gen.mark_rel i)
          && per_sel i
        then
          let p = Driver.poi base i in
          match Driver.get_parents p with
          | Some _ -> ()
          | None ->
              if
                bogus_person base p
                && not
                     (Driver.get_birth p <> Date.cdate_None
                     || Driver.get_baptism p <> Date.cdate_None
                     || Driver.get_first_names_aliases p <> []
                     || Driver.get_surnames_aliases p <> []
                     || Driver.sou base (Driver.get_public_name p) <> ""
                     || Driver.get_qualifiers p <> []
                     || Driver.get_aliases p <> []
                     || Driver.get_titles p <> []
                     || Driver.sou base (Driver.get_occupation p) <> ""
                     || Driver.sou base (Driver.get_birth_place p) <> ""
                     || Driver.sou base (Driver.get_birth_src p) <> ""
                     || Driver.sou base (Driver.get_baptism_place p) <> ""
                     || Driver.sou base (Driver.get_baptism_src p) <> ""
                     || Driver.sou base (Driver.get_death_place p) <> ""
                     || Driver.sou base (Driver.get_death_src p) <> ""
                     || Driver.sou base (Driver.get_burial_place p) <> ""
                     || Driver.sou base (Driver.get_burial_src p) <> ""
                     || Driver.sou base (Driver.get_notes p) <> ""
                     || Driver.sou base (Driver.get_psources p) <> ""
                     || Driver.get_rparents p <> []
                     || Driver.get_related p <> [])
              then ()
              else
                let oc, _first, _ =
                  origin_file (Driver.base_notes_origin_file base)
                in
                let f, _ooc, c = opts.oc in
                let opts = { opts with oc = (f, oc, c) } in
                Printf.ksprintf oc "\n";
                print_empty_family opts base p;
                print_notes_for_person opts base gen p;
                Collection.Marker.set gen.mark i true;
                print_isolated_relations opts base gen p)
      (Geneweb_db.Driver.ipers base);
  if !Mutil.verbose then ProgrBar.finish ();
  if opts.no_notes = `none then (
    let s = Driver.base_notes_read base "" in
    let oc, first, _ = origin_file (Driver.base_notes_origin_file base) in
    let f, _ooc, c = opts.oc in
    let opts = { opts with oc = (f, oc, c) } in
    if s <> "" then (
      if not !first then Printf.ksprintf oc "\n";
      first := false;
      Printf.ksprintf oc "notes-db\n";
      rs_printf opts s;
      Printf.ksprintf oc "\nend notes-db\n";
      ignore (add_linked_files gen (fun _ -> "database notes") s [] : _ list));
    (try
       let files =
         Sys.readdir (Filename.concat in_dir (Driver.base_wiznotes_dir base))
       in
       Array.sort compare files;
       for i = 0 to Array.length files - 1 do
         let file = files.(i) in
         if Filename.check_suffix file ".txt" then
           let wfile =
             List.fold_left Filename.concat in_dir
               [ Driver.base_wiznotes_dir base; file ]
           in
           let s = read_file_contents wfile in
           ignore
             (add_linked_files gen (fun _ -> "wizard \"" ^ file ^ "\"") s []
               : _ list)
       done
     with Sys_error _ -> ());
    let rec loop = function
      | [] -> ()
      | (f, _) :: files ->
          let fn =
            match NotesLinks.check_file_name f with
            | Some (dl, f) -> List.fold_right Filename.concat dl f
            | None -> "bad"
          in
          let s = Driver.base_notes_read base fn in
          let files =
            add_linked_files gen
              (fun _ -> Printf.sprintf "extended page \"%s\"" f)
              s files
          in
          loop files
    in
    loop gen.ext_files;

    List.iter
      (fun (f, r) ->
        let fn =
          match NotesLinks.check_file_name f with
          | Some (dl, f) -> List.fold_right Filename.concat dl f
          | None -> "bad"
        in
        let s = String.trim (Driver.base_notes_read base fn) in
        if s <> "" then (
          if not !first then Printf.ksprintf oc "\n";
          first := false;
          Printf.ksprintf oc "# extended page \"%s\" used by:\n" f;
          List.iter
            (fun f -> Printf.ksprintf oc "#  - %s\n" f)
            (List.sort compare !r);
          Printf.ksprintf oc "page-ext %s\n" f;
          rs_printf opts s;
          Printf.ksprintf oc "\nend page-ext\n"))
      (List.sort compare gen.ext_files);

    (* gen.ext_files does not list all files in notes_d. Format is note_link *)
    let ext_files_1 = List.map (fun (f, _) -> f) gen.ext_files in
    let notes_d = Filename.concat in_dir (Driver.base_notes_dir base) in
    let notes_d_length = String.length notes_d in
    let ext_files_2 =
      Filesystem.walk_folder ~recursive:true
        (fun fl files ->
          match fl with
          | File f when Filename.check_suffix f ".txt" ->
              Filename.chop_suffix f ".txt" :: files
          | File _ | Dir _ | Exn _ ->
              (* TODO: we may print a warning for errors. *)
              files)
        notes_d []
    in
    (* ext_files_2 list all files in notes_d. Format is system *)
    (* remove header part *)
    let ext_files_2 =
      List.map
        (fun f ->
          String.sub f (notes_d_length + 1)
            (String.length f - notes_d_length - 1))
        ext_files_2
    in
    (* remove from ext_files_2 those already in ext_files_1 *)
    let ext_files_2 =
      List.fold_left
        (fun acc f ->
          if List.mem (Util.sys_to_note_link f) ext_files_1 then acc
          else f :: acc)
        [] ext_files_2
    in
    (* ext_files_2 is the list of additional files that need to be saved *)
    (* dummy as these files are not referenced *)
    let ext_files_2 = List.map (fun f -> (f, dummy)) ext_files_2 in
    List.iter
      (fun (f, _) ->
        let fn = Filename.concat notes_d f ^ ".txt" in
        let s =
          if Sys.file_exists fn then read_file_contents fn
          else (
            Printf.eprintf "Missing file: %s\n" f;
            "")
        in
        let s_len = String.length s in
        (* false if there are no wiki links in the file ! *)
        let contains_wiki_link s =
          let rec loop s i =
            match NotesLinks.misc_notes_link s i with
            | NotesLinks.WLnone (j, _) when j < s_len -> loop s j
            | NotesLinks.WLnone _ -> false
            | _ -> true
          in
          loop s 0
        in
        if (contains_wiki_link s || !all_files) && s <> "" then (
          if not !first then Printf.ksprintf oc "\n";
          first := false;
          let f = Util.sys_to_note_link f in
          Printf.ksprintf oc "# extended page \"%s\" not referenced\n" f;
          Printf.ksprintf oc "page-ext %s\n" f;
          rs_printf opts s;
          Printf.ksprintf oc "\nend page-ext\n"))
      ext_files_2;
    let close () =
      flush_all ();
      close ();
      Hashtbl.iter (fun _ (_, _, close) -> close ()) src_oc_ht
    in
    try
      let files =
        Sys.readdir (Filename.concat in_dir (Driver.base_wiznotes_dir base))
      in
      Array.sort compare files;
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if Filename.check_suffix file ".txt" then (
          let wizid = Filename.chop_suffix file ".txt" in
          let wfile =
            List.fold_left Filename.concat in_dir
              [ Driver.base_wiznotes_dir base; file ]
          in
          let s = String.trim (read_file_contents wfile) in
          Printf.ksprintf oc "\nwizard-note %s\n" wizid;
          rs_printf opts s;
          Printf.ksprintf oc "\nend wizard-note\n")
      done;
      close ()
    with Sys_error _ -> close ())
