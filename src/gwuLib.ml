(* $Id: gwu.ml,v 5.45 2012-01-19 06:28:42 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

module type Select = sig
  value functions :
    Gwdb.base ->
    option Adef.iper ->
    option Def.iper ->
    list string ->
    option Adef.iper ->
    bool -> int -> bool -> int -> ((Adef.iper -> bool) * (Adef.ifam -> bool));
end;

module Make(Select : Select) = struct

open Def;
open Gutil;
open Gwdb;
open Printf;


(* Backward compatibility option before the additional fields. *)
value old_gw = ref False;

value put_events_in_notes base p =
  (* Si on est en mode old_gw, on mets tous les �v�nements *)
  (* dans les notes.                                       *)
  if old_gw.val then
    loop (get_pevents p) where rec loop pevents =
      match pevents with
      [ [] -> False
      | [evt :: events] ->
          match evt.epers_name with
          [ Epers_Birth | Epers_Baptism | Epers_Death |
            Epers_Burial | Epers_Cremation ->
              if sou base evt.epers_note <> "" ||
                 evt.epers_witnesses <> [| |]
              then True
              else loop events
          | _ -> True ] ]
  else False
;

value ht_dup_occ = Hashtbl.create 20001;
value ht_orig_occ = Hashtbl.create 20001;


value prepare_free_occ base =
  do {
    (* Parce qu'on est oblig� ... *)
    let sn = "?" in
    let fn = "?" in
    let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
    Hashtbl.add ht_orig_occ key [0];
    for i = 0 to nb_of_persons base -1 do {
      let ip = Adef.iper_of_int i in
      let p = poi base ip in
      let sn = sou base (get_surname p) in
      let fn = sou base (get_first_name p) in
      if sn = "?" && fn = "?" then ()
      else if Name.lower sn = "" || Name.lower fn = "" then do {
        let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
        let occ = get_occ p in
        try
          let l = Hashtbl.find ht_orig_occ key in
          if List.mem occ l then
            Hashtbl.add ht_dup_occ ip occ
          else
            Hashtbl.replace ht_orig_occ key [occ :: l]
        with
        [ Not_found -> Hashtbl.add ht_orig_occ key [occ] ]
      }
      else ()
    };
    Hashtbl.iter
      (fun key l ->
        Hashtbl.replace ht_orig_occ key (List.sort Pervasives.compare l))
      ht_orig_occ;
    let concat l1 l2 =
      let rec loop l1 l2 =
        match l1 with
        [ [] -> l2
        | [x :: l1] -> loop l1 [x :: l2] ]
      in
      loop (List.rev l1) l2
    in
    Hashtbl.iter
      (fun ip occ ->
        let p = poi base ip in
        let sn = sou base (get_surname p) in
        let fn = sou base (get_first_name p) in
        let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
  try
    do {
            let list_occ = Hashtbl.find ht_orig_occ key in
            let rec loop list init new_list =
              match list with
              [ [x; y :: l] ->
                  if y - x > 1 then
                    (succ x, concat (concat new_list [x; succ x; y]) l)
                  else
                    loop [y :: l] y (concat new_list [x])
              | [x :: l] -> loop l (succ x) (concat new_list [x])
              | [] -> (init, [init]) ]
            in
            let (new_occ, new_list_occ) = loop list_occ 0 [] in
            Hashtbl.replace ht_dup_occ ip new_occ;
            Hashtbl.replace ht_orig_occ key new_list_occ
    }
  with
        [ Not_found -> () ] )
      ht_dup_occ
  }
;

value get_new_occ p =
  try Hashtbl.find ht_dup_occ (get_key_index p) with
  [ Not_found -> get_occ p ]
;

type mfam =
  { m_ifam : ifam; m_fam : family; m_fath : person; m_moth : person;
    m_chil : array person }
;

value soy y = if y = 0 then "-0" else string_of_int y;

value print_date_dmy oc d =
  do {
    match d.prec with
    [ About -> fprintf oc "~"
    | Maybe -> fprintf oc "?"
    | Before -> fprintf oc "<"
    | After -> fprintf oc ">"
    | _ -> () ];
    if (*d.day = 0 &&*) d.month = 0 then fprintf oc "%s" (soy d.year)
    else if d.day = 0 then fprintf oc "%d/%s" d.month (soy d.year)
    else fprintf oc "%d/%d/%s" d.day d.month (soy d.year);
    match d.prec with
    [ OrYear d2 ->
        if not old_gw.val then
          if (*d2.day2 = 0 &&*) d2.month2 = 0 then fprintf oc "|%s" (soy d2.year2)
          else if d2.day2 = 0 then fprintf oc "|%d/%s" d2.month2 (soy d2.year2)
          else fprintf oc "|%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
        else
          fprintf oc "|%s" (soy d2.year2)
    | YearInt d2 ->
        if not old_gw.val then
          if (*d2.day2 = 0 &&*) d2.month2 = 0 then fprintf oc "..%s" (soy d2.year2)
          else if d2.day2 = 0 then fprintf oc "..%d/%s" d2.month2 (soy d2.year2)
          else fprintf oc "..%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
        else
          fprintf oc "..%s" (soy d2.year2)
    | _ -> () ]
  }
;

value is_printable =
  fun
  [ '\000'..'\031' -> False
  | _ -> True ]
;

value starting_char no_num s =
  match s.[0] with
  [ 'a'..'z' | 'A'..'Z' | '�'..'�' | '�'..'�' -> True
  | '0'..'9' -> not no_num
  | '?' -> if s = "?" then True else False
  | _ -> False ]
;

value no_newlines s =
  String.init (String.length s) conv_char
    where conv_char i =
      match s.[i] with
      [ '\n' | '\r' -> ' '
      | _ -> s.[i] ]
;

value raw_output = ref False;
value no_picture = ref False;
value isolated = ref False;

value gen_correct_string no_num no_colon s =
  let s = strip_spaces s in
  let s =
    if Mutil.utf_8_db.val || raw_output.val then s
    else Mutil.utf_8_of_iso_8859_1 s
  in
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else if len = 0 && not (starting_char no_num s) then
      loop i (Buff.store len '_')
    else
      match s.[i] with
      [ ' ' | '\n' | '\t' ->
          if i = String.length s - 1 then Buff.get len
          else loop (i + 1) (Buff.store len '_')
      | '_' | '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | ':' when no_colon ->
          let len = Buff.store len '\\' in
          loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | c ->
          let c = if is_printable c then c else '_' in
          loop (i + 1) (Buff.store len c) ]
;

value s_correct_string s =
  let s = gen_correct_string False False s in
  if s = "" then "_" else s
;
value s_correct_string_nonum s =
  let s = gen_correct_string True False s in
  if s = "" then "_" else s
;

value correct_string base is = s_correct_string (sou base is);
value correct_string_no_colon base is =
  gen_correct_string False True (sou base is)
;

value gen_print_date no_colon oc =
  fun
  [ Dgreg d Dgregorian -> print_date_dmy oc d
  | Dgreg d Djulian ->
      do {
        print_date_dmy oc (Calendar.julian_of_gregorian d);
        fprintf oc "J"
      }
  | Dgreg d Dfrench ->
      do {
        print_date_dmy oc (Calendar.french_of_gregorian d);
        fprintf oc "F"
      }
  | Dgreg d Dhebrew ->
      do {
        print_date_dmy oc (Calendar.hebrew_of_gregorian d);
        fprintf oc "H"
      }
  | Dtext t ->
      (* Dans le cas d'une date texte pour un titre, on �chappe les ':' *)
      let t = gen_correct_string False no_colon t in
      fprintf oc "0(%s)" t ]
;

value gen_print_date_option no_colon oc =
  fun
  [ Some d -> gen_print_date no_colon oc d
  | None -> () ]
;

value print_date oc = gen_print_date False oc;
value print_date_option oc = gen_print_date_option False oc;
value print_title_date_option oc = gen_print_date_option True oc;

value lines_list_of_string s =
  loop [] 0 0 where rec loop lines len i =
    if i = String.length s then
      List.rev (if len = 0 then lines else [Buff.get len :: lines])
    else if s.[i] = '\n' then
      let line = Buff.get len in
      loop [line :: lines] 0 (i + 1)
    else
      loop lines (Buff.store len s.[i]) (i + 1)
;

value has_infos_not_dates base p =
  get_first_names_aliases p <> [] || get_surnames_aliases p <> [] ||
  sou base (get_public_name p) <> "" || get_qualifiers p <> [] ||
  get_aliases p <> [] || get_titles p <> [] ||
  sou base (get_occupation p) <> "" || sou base (get_birth_place p) <> "" ||
  sou base (get_baptism_place p) <> "" ||
  sou base (get_death_place p) <> "" || sou base (get_psources p) <> ""
;

value has_infos base p =
  has_infos_not_dates base p || get_birth p <> Adef.codate_None ||
  get_baptism p <> Adef.codate_None || get_death p <> NotDead
;

value print_if_not_equal_to x oc base lab is =
  if sou base is = x then ()
  else fprintf oc " %s %s" lab (correct_string base is)
;

value print_if_no_empty = print_if_not_equal_to "";

value print_if_no_empty_endline oc base lab is =
  if sou base is = "" then ()
  else fprintf oc " %s %s\n" lab (correct_string base is)
;

value print_if_no_empty_no_newline oc base lab is =
  if sou base is = "" then ()
  else fprintf oc " %s %s" lab (no_newlines (correct_string base is))
;

value print_first_name_alias oc base is =
  fprintf oc " {%s}" (correct_string base is)
;

value print_surname_alias oc base is =
  fprintf oc " #salias %s" (correct_string base is)
;

value print_qualifier oc base is =
  fprintf oc " #nick %s" (correct_string base is)
;

value print_alias oc base is =
  fprintf oc " #alias %s" (correct_string base is)
;

value print_burial oc base b =
  match b with
  [ Buried cod ->
      do {
        fprintf oc " #buri";
        match Adef.od_of_codate cod with
        [ Some d -> do { fprintf oc " "; print_date oc d; () }
        | _ -> () ]
      }
  | Cremated cod ->
      do {
        fprintf oc " #crem";
        match Adef.od_of_codate cod with
        [ Some d -> do { fprintf oc " "; print_date oc d; () }
        | _ -> () ]
      }
  | UnknownBurial -> () ]
;

value print_title oc base t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  do {
    fprintf oc " [";
    match t.t_name with
    [ Tmain -> fprintf oc "*"
    | Tname s -> fprintf oc "%s" (correct_string_no_colon base s)
    | Tnone -> () ];
    fprintf oc ":";
    fprintf oc "%s" (correct_string_no_colon base t.t_ident);
    fprintf oc ":";
    fprintf oc "%s" (correct_string_no_colon base t.t_place);
    if t.t_nth <> 0 then fprintf oc ":"
    else
      match (t_date_start, t_date_end) with
      [ (Some _, _) | (_, Some _) -> fprintf oc ":"
      | _ -> () ];
    print_title_date_option oc t_date_start;
    if t.t_nth <> 0 then fprintf oc ":"
    else
      match t_date_end with
      [ Some _ -> fprintf oc ":"
      | _ -> () ];
    print_title_date_option oc t_date_end;
    if t.t_nth <> 0 then fprintf oc ":%d" t.t_nth else ();
    fprintf oc "]"
  }
;

value print_infos oc base is_child csrc cbp p =
  do {
    List.iter (print_first_name_alias oc base) (get_first_names_aliases p);
    List.iter (print_surname_alias oc base) (get_surnames_aliases p);
    match get_public_name p with
    [ s when sou base s <> "" ->
        fprintf oc " (%s)" (correct_string base s)
    | _ -> () ];
    if not no_picture.val then
      print_if_no_empty oc base "#image" (get_image p)
    else ();
    List.iter (print_qualifier oc base) (get_qualifiers p);
    List.iter (print_alias oc base) (get_aliases p);
    List.iter (print_title oc base) (get_titles p);
    match get_access p with
    [ IfTitles -> ()
    | Public -> fprintf oc " #apubl"
    | Private -> fprintf oc " #apriv" ];
    print_if_no_empty oc base "#occu" (get_occupation p);
    print_if_not_equal_to csrc oc base "#src" (get_psources p);
    match Adef.od_of_codate (get_birth p) with
    [ Some d -> do { fprintf oc " "; print_date oc d }
    | _ ->
        if get_baptism p <> Adef.codate_None then ()
        else
          match get_death p with
          [ Death _ _ | DeadYoung | DeadDontKnowWhen | OfCourseDead ->
              fprintf oc " 0"
          | DontKnowIfDead
            when
              not is_child && not (has_infos_not_dates base p) &&
              p_first_name base p <> "?" && p_surname base p <> "?" ->
              fprintf oc " 0"
          | _ -> () ] ];
    print_if_not_equal_to cbp oc base "#bp" (get_birth_place p);
    print_if_no_empty oc base "#bs" (get_birth_src p);
    match Adef.od_of_codate (get_baptism p) with
    [ Some d -> do { fprintf oc " !"; print_date oc d }
    | _ -> () ];
    print_if_no_empty oc base "#pp" (get_baptism_place p);
    print_if_no_empty oc base "#ps" (get_baptism_src p);
    match get_death p with
    [ Death dr d ->
        do {
          fprintf oc " ";
          match dr with
          [ Killed -> fprintf oc "k"
          | Murdered -> fprintf oc "m"
          | Executed -> fprintf oc "e"
          | Disappeared -> fprintf oc "s"
          | _ -> () ];
          print_date oc (Adef.date_of_cdate d)
        }
    | DeadYoung -> fprintf oc " mj"
    | DeadDontKnowWhen -> fprintf oc " 0"
    | DontKnowIfDead ->
        match
          (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p))
        with
        [ (Some _, _) | (_, Some _) -> fprintf oc " ?"
        | _ -> () ]
    | OfCourseDead -> fprintf oc " od"
    | NotDead -> () ];
    print_if_no_empty oc base "#dp" (get_death_place p);
    print_if_no_empty oc base "#ds" (get_death_src p);
    print_burial oc base (get_burial p);
    print_if_no_empty oc base "#rp" (get_burial_place p);
    print_if_no_empty oc base "#rs" (get_burial_src p)
  }
;

type gen =
  { mark : array bool;
    mark_rel : array bool;
    per_sel : iper -> bool;
    fam_sel : ifam -> bool;
    fam_done : array bool;
    notes_pl_p : mutable list person;
    ext_files : mutable list (string * ref (list string));
    notes_alias : mutable list (string * string);
    pevents_pl_p : mutable list person }
;

value map_notes aliases f =
  try List.assoc f aliases with [ Not_found -> f ]
;

value add_linked_files gen from s some_linked_files =
  let slen = String.length s in
  loop some_linked_files 0 where rec loop new_linked_files i =
    if i = slen then new_linked_files
    else if i < slen - 2 && s.[i] = '[' && s.[i+1] = '[' && s.[i+2] = '[' then
      let j =
        loop (i + 3) where rec loop j =
          if j = slen then j
          else if
            j < slen - 2 && s.[j] = ']' && s.[j+1] = ']' && s.[j+2] = ']'
          then j + 3
          else loop (j + 1)
      in
      if j > i + 6 then
        let b = String.sub s (i + 3) (j - i - 6) in
        let fname =
          try
            let k = String.index b '/' in
            String.sub b 0 k
          with
          [ Not_found -> b ]
        in
        let fname = map_notes gen.notes_alias fname in
        let f = from () in
        let new_linked_files =
          try
            let r = List.assoc fname gen.ext_files in
            do {
              if List.mem f r.val then () else r.val := [f :: r.val];
              new_linked_files
            }
          with
          [ Not_found ->
              let lf = (fname, ref [f]) in
              do {
                gen.ext_files := [lf :: gen.ext_files];
                [lf :: new_linked_files];
              } ]
        in
        loop new_linked_files j
      else loop new_linked_files (i + 1)
    else loop new_linked_files (i + 1)
;

value find_free_occ base f s i =
  let ipl = persons_of_name base (f ^ " " ^ s) in
  let first_name =  f in
  let surname =  s in
  let list_occ =
    loop [] ipl where rec loop list =
      fun
      [ [ip :: ipl] ->
          let p = poi base ip in
          if not (List.mem (get_occ p) list) &&
             first_name =  (p_first_name base p) &&
             surname =  (p_surname base p) then
            loop [get_occ p :: list] ipl
          else loop list ipl
      | [] -> list ]
  in
  let list_occ = List.sort compare list_occ in
  loop 0 list_occ where rec loop cnt1 =
    fun
    [ [cnt2 :: list] ->
        if cnt1 = cnt2 then loop (cnt1 + 1) list else cnt1
    | [] -> cnt1 ]
;

value print_parent oc base gen fam p =
  let has_printed_parents =
    match get_parents p with
    [ Some ifam -> gen.fam_sel ifam
    | None -> False ]
  in
  let first_parent_definition =
    if gen.mark.(Adef.int_of_iper (get_key_index p)) then False
    else do { gen.mark.(Adef.int_of_iper (get_key_index p)) := True; True }
  in
  let pr = not has_printed_parents && first_parent_definition in
  let has_infos = if pr then has_infos base p else False in
  let first_name = sou base (get_first_name p) in
  let surname = sou base (get_surname p) in
  do {
    fprintf oc "%s %s%s" (s_correct_string surname)
      (s_correct_string first_name)
      (if first_name = "?" && surname = "?" then ""
       else if get_new_occ p = 0 then ""
       else "." ^ string_of_int (get_new_occ p));
    if pr then
      if has_infos then print_infos oc base False "" "" p
      else if first_name <> "?" && surname <> "?" then fprintf oc " 0"
      else ()
    else ()
  }
;

value print_child oc base fam_surname csrc cbp p =
  do {
    fprintf oc "-";
    match get_sex p with
    [ Male -> fprintf oc " h"
    | Female -> fprintf oc " f"
    | _ -> () ];
    fprintf oc " %s" (s_correct_string (sou base (get_first_name p)));
    if p_first_name base p = "?" && p_surname base p = "?" then ()
    else if get_new_occ p = 0  then ()
    else fprintf oc ".%d" (get_new_occ p);
    if not (eq_istr (get_surname p) fam_surname) then
      fprintf oc " %s" (s_correct_string_nonum (sou base (get_surname p)))
    else ();
    print_infos oc base True csrc cbp p;
    fprintf oc "\n"
  }
;

value bogus_person base p =
  p_first_name base p = "?" && p_surname base p = "?"
;

value common_children proj base children =
  if Array.length children <= 1 then None
  else
    let list =
      List.map (fun p -> sou base (proj p)) (Array.to_list children)
    in
    if List.mem "" list then None
    else
      let list = List.sort compare list in
      let (src_max, n_max, _, _) =
        List.fold_left
          (fun (src_max, n_max, prev_src, n) src ->
             if src = prev_src then
               let n = n + 1 in
               if n > n_max then (src, n, src, n)
               else (src_max, n_max, src, n)
             else (src_max, n_max, src, 1))
          ("", 0, "", 0) list
      in
      if n_max > 1 then Some src_max else None
;

value common_children_sources = common_children get_psources;
value common_children_birth_place = common_children get_birth_place;

value array_forall f a =
  loop 0 where rec loop i =
    if i = Array.length a then True
    else if f a.(i) then loop (i + 1)
    else False
;

value empty_family base m =
  bogus_person base m.m_fath && bogus_person base m.m_moth &&
  array_forall (bogus_person base) m.m_chil
;

value print_witness oc base gen p =
  do {
    fprintf oc "%s %s%s" (correct_string base (get_surname p))
      (correct_string base (get_first_name p))
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    if Array.length (get_family p) = 0 && get_parents p = None &&
       not gen.mark.(Adef.int_of_iper (get_key_index p))
    then do {
      gen.mark.(Adef.int_of_iper (get_key_index p)) := True;
      if has_infos base p then print_infos oc base False "" "" p
      else fprintf oc " 0";
      match sou base (get_notes p) with
      [ "" ->
          if put_events_in_notes base p then
            gen.notes_pl_p := [p :: gen.notes_pl_p]
          else ()
      | _ -> gen.notes_pl_p := [p :: gen.notes_pl_p] ];
      if get_pevents p <> [] then gen.pevents_pl_p := [p :: gen.pevents_pl_p]
      else ();
    }
    else ()
  }
;

value print_pevent oc base gen e = do {
  match e.epers_name with
  [ Epers_Birth -> fprintf oc "#birt"
  | Epers_Baptism -> fprintf oc "#bapt"
  | Epers_Death -> fprintf oc "#deat"
  | Epers_Burial -> fprintf oc "#buri"
  | Epers_Cremation -> fprintf oc "#crem"
  | Epers_Accomplishment -> fprintf oc "#acco"
  | Epers_Acquisition -> fprintf oc "#acqu"
  | Epers_Adhesion -> fprintf oc "#adhe"
  | Epers_BaptismLDS -> fprintf oc "#bapl"
  | Epers_BarMitzvah -> fprintf oc "#barm"
  | Epers_BatMitzvah -> fprintf oc "#basm"
  | Epers_Benediction -> fprintf oc "#bles"
  | Epers_ChangeName -> fprintf oc "#chgn"
  | Epers_Circumcision -> fprintf oc "#circ"
  | Epers_Confirmation -> fprintf oc "#conf"
  | Epers_ConfirmationLDS -> fprintf oc "#conl"
  | Epers_Decoration -> fprintf oc "#awar"
  | Epers_DemobilisationMilitaire -> fprintf oc "#demm"
  | Epers_Diploma -> fprintf oc "#degr"
  | Epers_Distinction -> fprintf oc "#dist"
  | Epers_Dotation -> fprintf oc "#endl"
  | Epers_DotationLDS -> fprintf oc "#dotl"
  | Epers_Education -> fprintf oc "#educ"
  | Epers_Election -> fprintf oc "#elec"
  | Epers_Emigration -> fprintf oc "#emig"
  | Epers_Excommunication -> fprintf oc "#exco"
  | Epers_FamilyLinkLDS -> fprintf oc "#flkl"
  | Epers_FirstCommunion -> fprintf oc "#fcom"
  | Epers_Funeral -> fprintf oc "#fune"
  | Epers_Graduate -> fprintf oc "#grad"
  | Epers_Hospitalisation -> fprintf oc "#hosp"
  | Epers_Illness -> fprintf oc "#illn"
  | Epers_Immigration -> fprintf oc "#immi"
  | Epers_ListePassenger -> fprintf oc "#lpas"
  | Epers_MilitaryDistinction -> fprintf oc "#mdis"
  | Epers_MilitaryPromotion -> fprintf oc "#mpro"
  | Epers_MilitaryService -> fprintf oc "#mser"
  | Epers_MobilisationMilitaire -> fprintf oc "#mobm"
  | Epers_Naturalisation -> fprintf oc "#natu"
  | Epers_Occupation -> fprintf oc "#occu"
  | Epers_Ordination -> fprintf oc "#ordn"
  | Epers_Property -> fprintf oc "#prop"
  | Epers_Recensement -> fprintf oc "#cens"
  | Epers_Residence-> fprintf oc "#resi"
  | Epers_Retired -> fprintf oc "#reti"
  | Epers_ScellentChildLDS -> fprintf oc "#slgc"
  | Epers_ScellentParentLDS -> fprintf oc "#slgp"
  | Epers_ScellentSpouseLDS -> fprintf oc "#slgs"
  | Epers_VenteBien -> fprintf oc "#vteb"
  | Epers_Will -> fprintf oc "#will"
  | Epers_Name s -> fprintf oc "#%s" (correct_string base s) ];
  fprintf oc " ";
  let epers_date = Adef.od_of_codate e.epers_date in
  print_date_option oc epers_date;
  print_if_no_empty oc base "#p" e.epers_place;
  (* TODO *)
  (*print_if_no_empty oc base "#c" e.epers_cause;*)
  print_if_no_empty oc base "#s" e.epers_src;
  fprintf oc "\n";
  Array.iter
    (fun (ip, wk) ->
       if gen.per_sel ip then do {
         let p = poi base ip in
         fprintf oc "wit";
         match get_sex p with
         [ Male -> fprintf oc " m"
         | Female -> fprintf oc " f"
         | _ -> () ];
         fprintf oc ": ";
         match wk with
         [ Witness_GodParent -> fprintf oc "#godp "
         | Witness_Officer   -> fprintf oc "#offi "
         | _ -> () ];
         print_witness oc base gen p;
         fprintf oc "\n"
       }
       else ())
    e.epers_witnesses;
  let note = sou base e.epers_note in
  if note <> "" then
    List.iter
      (fun line -> fprintf oc "note %s\n" line)
      (lines_list_of_string note)
  else ();
};

value get_persons_with_pevents base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (get_pevents fath, get_parents fath) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [fath :: list] ]
  in
  let list =
    match (get_pevents moth, get_parents moth) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [moth :: list] ]
  in
  List.fold_right
    (fun p list ->
       match get_pevents p with
       [ [] -> list
       | _ -> [p :: list] ])
    (Array.to_list m.m_chil) list
;

value print_pevents_for_person oc base gen p =
  let pevents = get_pevents p in
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  if pevents <> [] && surn <> "?" && fnam <> "?" then do {
    fprintf oc "\n";
    fprintf oc "pevt %s %s%s\n" surn fnam
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    List.iter (print_pevent oc base gen) pevents;
    fprintf oc "end pevt\n";
  }
  else ()
;

value rec list_memf f x =
  fun
  [ [] -> False
  | [a :: l] -> f x a || list_memf f x l ]
;

value eq_key p1 p2 = get_key_index p1 = get_key_index p2;
value eq_key_fst (p1, _) (p2, _) = get_key_index p1 = get_key_index p2;

value print_pevents oc base gen ml =
  let pl =
    List.fold_right (get_persons_with_pevents base) ml gen.pevents_pl_p
  in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else [p :: pl]) pl []
  in
  List.iter
    (fun p ->
       if gen.per_sel (get_key_index p) then
         print_pevents_for_person oc base gen p
       else ())
    pl
;

value print_fevent oc base gen in_comment e = do {
  let print_sep () =
    if not in_comment then fprintf oc "\n" else fprintf oc " "
  in
  match e.efam_name with
  [ Efam_Marriage -> fprintf oc "#marr"
  | Efam_NoMarriage -> fprintf oc "#nmar"
  | Efam_NoMention -> fprintf oc "#nmen"
  | Efam_Engage -> fprintf oc "#enga"
  | Efam_Divorce -> fprintf oc "#div"
  | Efam_Separated -> fprintf oc "#sep"
  | Efam_Annulation -> fprintf oc "#anul"
  | Efam_MarriageBann -> fprintf oc "#marb"
  | Efam_MarriageContract -> fprintf oc "#marc"
  | Efam_MarriageLicense -> fprintf oc "#marl"
  | Efam_PACS -> fprintf oc "#pacs"
  | Efam_Residence -> fprintf oc "#resi"
  | Efam_Name n -> fprintf oc "#%s" (correct_string base n) ];
  fprintf oc " ";
  let efam_date = Adef.od_of_codate e.efam_date in
  print_date_option oc efam_date;
  print_if_no_empty oc base "#p" e.efam_place;
  (*print_if_no_empty oc base "#c" e.efam_cause;*)
  print_if_no_empty oc base "#s" e.efam_src;
  print_sep ();
  Array.iter
    (fun (ip, wk) ->
       if gen.per_sel ip then do {
         let p = poi base ip in
         fprintf oc "wit";
         match get_sex p with
         [ Male -> fprintf oc " m"
         | Female -> fprintf oc " f"
         | _ -> () ];
         fprintf oc ": ";
         match wk with
         [ Witness_GodParent -> fprintf oc "#godp "
         | Witness_Officer   -> fprintf oc "#offi "
         | _ -> () ];
         print_witness oc base gen p;
         print_sep ();
       }
       else ())
    e.efam_witnesses;
  let note = sou base e.efam_note in
  if note <> "" then
    List.iter
      (fun line -> do {fprintf oc "note %s" line; print_sep ();})
      (lines_list_of_string note)
  else ();
};

value print_comment_for_family oc base gen fam =
  let comm = sou base (get_comment fam) in
  (* Si on est en mode old_gw, on mets tous les �v�nements dans les notes. *)
  (* On supprime les 2 �v�nements principaux. *)
  let fevents =
    List.filter
      (fun evt ->
         match evt.efam_name with
         [ Efam_Divorce | Efam_Engage | Efam_Marriage | Efam_NoMarriage
         | Efam_NoMention | Efam_Separated -> False
         | _ -> True ])
      (get_fevents fam)
  in
  let has_evt =
    old_gw.val && (fevents <> [] || sou base (get_marriage_note fam) <> "")
  in
  if (comm <> "" || has_evt) then do {
    fprintf oc "comm";
    if comm <> "" then fprintf oc " %s" (no_newlines comm)
    else ();
    if old_gw.val then do {
      if sou base (get_marriage_note fam) <> "" then
        fprintf oc " marriage: %s"
          (no_newlines (sou base (get_marriage_note fam)))
      else ();
      List.iter
        (fun e -> do {fprintf oc " "; print_fevent oc base gen True e})
        fevents;
    }
    else ();
    fprintf oc "\n";
  }
  else ()
;

value print_empty_family oc base p = do {
  let string_quest = Gwdb.insert_string base "?" in
  fprintf oc "fam ? ?.0 + #noment ? ?.0\n";
  fprintf oc "beg\n";
  print_child oc base string_quest "" "" p;
  fprintf oc "end\n";
};

value has_infos_isolated base p =
  has_infos_not_dates base p || get_birth p <> Adef.codate_None ||
  get_baptism p <> Adef.codate_None
;


value print_family oc base gen m =
  let fam = m.m_fam in
  do {
    fprintf oc "fam ";
    print_parent oc base gen fam m.m_fath;
    fprintf oc " +";
    print_date_option oc (Adef.od_of_codate (get_marriage fam));
    match get_relation fam with
    [ NotMarried -> fprintf oc " #nm"
    | Married -> ()
    | Engaged -> fprintf oc " #eng"
    | NoSexesCheckNotMarried ->
        let c x =
          match get_sex x with
          [ Male -> 'm'
          | Female -> 'f'
          | Neuter -> '?' ]
        in
        fprintf oc " #nsck %c%c" (c m.m_fath) (c m.m_moth)
    | NoSexesCheckMarried ->
        let c x =
          match get_sex x with
          [ Male -> 'm'
          | Female -> 'f'
          | Neuter -> '?' ]
        in
        fprintf oc " #nsckm %c%c" (c m.m_fath) (c m.m_moth)
    | NoMention -> fprintf oc " #noment" ];
    print_if_no_empty oc base "#mp" (get_marriage_place fam);
    print_if_no_empty oc base "#ms" (get_marriage_src fam);
    match get_divorce fam with
    [ NotDivorced -> ()
    | Separated -> fprintf oc " #sep"
    | Divorced d ->
        let d = Adef.od_of_codate d in
        do { fprintf oc " -"; print_date_option oc d } ];
    fprintf oc " ";
    print_parent oc base gen fam m.m_moth;
    fprintf oc "\n";
    Array.iter
      (fun ip ->
         if gen.per_sel ip then do {
           let p = poi base ip in
           fprintf oc "wit";
           match get_sex p with
           [ Male -> fprintf oc " m"
           | Female -> fprintf oc " f"
           | _ -> () ];
           fprintf oc ": ";
           print_witness oc base gen p;
           fprintf oc "\n"
         }
         else ())
      (get_witnesses fam);
    let fsources = sou base (get_fsources fam) in
    match fsources with
    [ "" -> ()
    | s -> fprintf oc "src %s\n" (correct_string base (get_fsources fam)) ];
    let csrc =
      match common_children_sources base m.m_chil with
      [ Some s -> do { fprintf oc "csrc %s\n" (s_correct_string s); s }
      | _ -> "" ]
    in
    let cbp =
      match common_children_birth_place base m.m_chil with
      [ Some s -> do { fprintf oc "cbp %s\n" (s_correct_string s); s }
      | _ -> "" ]
    in
    print_comment_for_family oc base gen fam;
    if not old_gw.val && (get_fevents fam) <> [] then do {
      fprintf oc "fevt\n";
      List.iter (print_fevent oc base gen False) (get_fevents fam);
      fprintf oc "end fevt\n";
    }
    else ();
    match Array.length m.m_chil with
    [ 0 -> ()
    | _ ->
        let fam_surname = get_surname m.m_fath in
        do {
          fprintf oc "beg\n";
          Array.iter
            (fun p ->
               if gen.per_sel (get_key_index p) then
                 print_child oc base fam_surname csrc cbp p
               else ())
            m.m_chil;
          fprintf oc "end\n"
        } ];
    gen.fam_done.(Adef.int_of_ifam m.m_ifam) := True;
    let f _ =
      sprintf "family \"%s.%d %s\" & \"%s.%d %s\"" (p_first_name base m.m_fath)
        (get_new_occ m.m_fath) (p_surname base m.m_fath)
        (p_first_name base m.m_moth) (get_new_occ m.m_moth)
        (p_surname base m.m_moth)
    in
    ignore (add_linked_files gen f fsources [] : list _);
    let s =
      let sl =
        [get_comment fam; get_fsources fam;
         get_marriage_note fam; get_marriage_src fam]
      in
      let sl =
        if not old_gw.val then
          loop (get_fevents fam) sl where rec loop l accu =
            match l with
            [ [] -> accu
            | [evt :: l] -> loop l [evt.efam_note; evt.efam_src :: accu]]
        else sl
      in
      String.concat " " (List.map (sou base) sl)
    in
    ignore (add_linked_files gen f s [] : list _);
  }
;

value get_persons_with_notes base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (sou base (get_notes fath), get_parents fath) with
    [ ("", _) ->
        match get_parents fath with
        [ Some _ -> list
        | None ->
            if put_events_in_notes base fath then [fath :: list] else list ]
    | (_, Some _) -> list
    | _ -> [fath :: list] ]
  in
  let list =
    match (sou base (get_notes moth), get_parents moth) with
    [ ("", _) ->
        match get_parents moth with
        [ Some _ -> list
        | None ->
            if put_events_in_notes base moth then [moth :: list] else list ]
    | (_, Some _) -> list
    | _ -> [moth :: list] ]
  in
  List.fold_right
    (fun p list ->
       match sou base (get_notes p) with
       [ "" -> if put_events_in_notes base p then [p :: list] else list
       | _ -> [p :: list] ])
    (Array.to_list m.m_chil) list
;

value notes_aliases bdir =
  let fname = Filename.concat bdir "notes.alias" in
  match try Some (Secure.open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop [] where rec loop list =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some s ->
            let list =
              try
                let i = String.index s ' ' in
                [(String.sub s 0 i,
                  String.sub s (i + 1) (String.length s - i - 1)) :: list]
              with
              [ Not_found -> list ]
            in
            loop list
        | None -> do { close_in ic; list } ]
  | None -> [] ]
;

value print_notes_for_person oc base gen p = do {
  let print_witness_in_notes witnesses =
    Array.iter
      (fun (ip, wk) -> do {
         let p = poi base ip in
         fprintf oc "wit";
         match get_sex p with
         [ Male -> fprintf oc " m"
         | Female -> fprintf oc " f"
         | _ -> () ];
         fprintf oc ": ";
         match wk with
         [ Witness_GodParent -> fprintf oc "#godp "
         | Witness_Officer   -> fprintf oc "#offi "
         | _ -> () ];
         print_witness oc base gen p;
         fprintf oc "\n"
      })
      witnesses
  in
  let notes = sou base (get_notes p) in
  let surn = s_correct_string (p_surname base p) in
  let fnam = s_correct_string (p_first_name base p) in
  (* Si on n'est en mode old_gw, on mets tous les �v�nements dans les notes. *)
  if (notes <> "" || put_events_in_notes base p) && surn <> "?" && fnam <> "?"
  then do {
    fprintf oc "\n";
    fprintf oc "notes %s %s%s\n" surn fnam
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    fprintf oc "beg\n";
    if notes <> "" then fprintf oc "%s\n" notes
    else ();
    if put_events_in_notes base p then do {
      loop (get_pevents p) where rec loop pevents =
        match pevents with
        [ [] -> ()
        | [evt :: events] ->
            match evt.epers_name with
            [ Epers_Birth | Epers_Baptism | Epers_Death |
              Epers_Burial | Epers_Cremation ->
                do {
                  let name =
                    match evt.epers_name with
                    [ Epers_Birth -> "birth"
                    | Epers_Baptism -> "baptism"
                    | Epers_Death -> "death"
                    | Epers_Burial -> "burial"
                    | Epers_Cremation -> "creamation"
                    | _ -> "" ]
                  in
                  let notes = sou base evt.epers_note in
                  if notes <> "" then fprintf oc "%s: %s\n" name notes
                  else ();
                  print_witness_in_notes evt.epers_witnesses;
                  loop events
                }
            | name -> do {print_pevent oc base gen evt; loop events } ] ]
    }
    else ();
    fprintf oc "end notes\n";
  }
  else ();
  let f _ =
    sprintf "person \"%s.%d %s\"" (p_first_name base p) (get_new_occ p)
      (p_surname base p)
  in
  ignore (add_linked_files gen f notes [] : list _);
  let s =
    let sl =
      [get_notes p; get_occupation p; get_birth_note p; get_birth_src p;
       get_baptism_note p; get_baptism_src p; get_death_note p;
       get_death_src p; get_burial_note p; get_burial_src p; get_psources p]
    in
    let sl =
      if not old_gw.val then
        loop (get_pevents p) sl where rec loop l accu =
          match l with
          [ [] -> accu
          | [evt :: l] -> loop l [evt.epers_note; evt.epers_src :: accu]]
      else sl
    in
    String.concat " " (List.map (sou base) sl)
  in
  ignore (add_linked_files gen f s [] : list _);
};

value print_notes oc base gen ml =
  let pl = List.fold_right (get_persons_with_notes base) ml gen.notes_pl_p in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else [p :: pl]) pl []
  in
  List.iter
    (fun p ->
       if gen.per_sel (get_key_index p) then
         print_notes_for_person oc base gen p
       else ())
    pl
;

value is_isolated base p =
  match get_parents p with
  [ Some _ -> False
  | None -> Array.length (get_family p) = 0 ]
;

value is_definition_for_parent base p =
  match get_parents p with
  [ Some _ -> False
  | None -> True ]
;

value get_isolated_related base m list =
  let concat_isolated p_relation ip list =
    let p = poi base ip in
    if List.mem_assq p list then list
    else if is_isolated base p then
      match get_rparents p with
      [ [{r_fath = Some x} :: _] when x = get_key_index p_relation ->
          list @ [(p, True)]
      | [{r_fath = None; r_moth = Some x} :: _]
        when x = get_key_index p_relation ->
          list @ [(p, True)]
      | _ -> list ]
    else list
  in
  let list =
    if is_definition_for_parent base m.m_fath then
      List.fold_right (concat_isolated m.m_fath) (get_related m.m_fath) list
    else list
  in
  let list =
    if is_definition_for_parent base m.m_moth then
      List.fold_right (concat_isolated m.m_moth) (get_related m.m_moth) list
    else list
  in
  let list =
    List.fold_right
      (fun p list -> List.fold_right (concat_isolated p) (get_related p) list)
      (Array.to_list m.m_chil) list
  in
  list
;

value get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (get_rparents fath, get_parents fath) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [(fath, False) :: list] ]
  in
  let list =
    match (get_rparents moth, get_parents moth) with
    [ ([], _) | (_, Some _) -> list
    | _ -> [(moth, False) :: list] ]
  in
  let list =
    List.fold_right
      (fun ip list ->
         let p = poi base ip in
         match (get_rparents p, get_parents p) with
         [ ([], _) | (_, Some _) -> list
         | ([{r_fath = Some x} :: _], _) when x <> get_key_index m.m_fath ->
             list
         | _ -> [(p, False) :: list] ])
      (Array.to_list (get_witnesses m.m_fam)) list
  in
  List.fold_right
    (fun p list ->
       match get_rparents p with
       [ [] -> list
       | _ -> [(p, False) :: list] ])
    (Array.to_list m.m_chil) list
;

value print_relation_parent oc base mark defined_p p =
  do {
    fprintf oc "%s %s%s" (correct_string base (get_surname p))
      (correct_string base (get_first_name p))
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    if Array.length (get_family p) = 0 && get_parents p = None &&
       not mark.(Adef.int_of_iper (get_key_index p))
    then do {
      mark.(Adef.int_of_iper (get_key_index p)) := True;
      if has_infos base p then print_infos oc base False "" "" p
      else fprintf oc " 0";
      defined_p.val := [p :: defined_p.val]
    }
    else ()
  }
;

value print_relation_for_person oc base gen def_p p r =
  let fath =
    match r.r_fath with
    [ Some ip ->
        if gen.per_sel ip then
          let p = poi base ip in
          if sou base (get_first_name p) = "?" ||
             sou base (get_surname p) = "?"
          then None
          else Some p
        else None
    | None -> None ]
  in
  let moth =
    match r.r_moth with
    [ Some ip ->
        if gen.per_sel ip then
          let p = poi base ip in
          if sou base (get_first_name p) = "?" ||
             sou base (get_surname p) = "?"
          then None
          else Some p
        else None
    | None -> None ]
  in
  let err_same_sex =
    match (fath, moth) with
    [ (Some fath, Some moth) -> get_sex fath = get_sex moth
    | _ -> False ]
  in
  let print_err_one_relation p =
    do {
      fprintf oc "- ";
      match r.r_type with
      [ Adoption -> fprintf oc "adop"
      | Recognition -> fprintf oc "reco"
      | CandidateParent -> fprintf oc "cand"
      | GodParent -> fprintf oc "godp"
      | FosterParent -> fprintf oc "fost" ];
      if get_sex p = Male then fprintf oc " fath"
      else fprintf oc " moth";
      fprintf oc ": ";
      print_relation_parent oc base gen.mark def_p p;
      fprintf oc "\n"
    }
  in
  match (fath, moth) with
  [ (None, None) -> ()
  | _ ->
      if err_same_sex then
        match (fath, moth) with
        [ (Some fath, Some moth) ->
            do {
              print_err_one_relation fath;
              print_err_one_relation moth;
            }
        | _ -> ()]
      else
      do {
        fprintf oc "- ";
        match r.r_type with
        [ Adoption -> fprintf oc "adop"
        | Recognition -> fprintf oc "reco"
        | CandidateParent -> fprintf oc "cand"
        | GodParent -> fprintf oc "godp"
        | FosterParent -> fprintf oc "fost" ];
        match (fath, moth) with
        [ (Some fath, None) ->
            if get_sex fath = Male then fprintf oc " fath"
            else fprintf oc " moth"
        | (None, Some moth) ->
            if get_sex moth = Female then fprintf oc " moth"
            else fprintf oc " fath"
        | _ -> () ];
        fprintf oc ": ";
        match (fath, moth) with
        [ (Some fath, None) -> print_relation_parent oc base gen.mark def_p fath
        | (None, Some moth) -> print_relation_parent oc base gen.mark def_p moth
        | (Some fath, Some moth) ->
            if get_sex fath = Male && get_sex moth = Female then do {
              print_relation_parent oc base gen.mark def_p fath;
              fprintf oc " + ";
              print_relation_parent oc base gen.mark def_p moth
            }
          else do {
              print_relation_parent oc base gen.mark def_p moth;
              fprintf oc " + ";
              print_relation_parent oc base gen.mark def_p fath
            }
        | _ -> () ];
        fprintf oc "\n"
      } ]
;

value print_relations_for_person oc base gen def_p is_definition p =
  let surn = correct_string base (get_surname p) in
  let fnam = correct_string base (get_first_name p) in
  let exist_relation =
    List.exists
      (fun r ->
         match (r.r_fath, r.r_moth) with
         [ (Some ip1, Some ip2) -> gen.per_sel ip1 && gen.per_sel ip2
         | (Some ip1, _) -> gen.per_sel ip1
         | (_, Some ip2) -> gen.per_sel ip2
         | _ -> False ])
      (get_rparents p)
  in
  if surn <> "?" && fnam <> "?" && exist_relation &&
     not gen.mark_rel.(Adef.int_of_iper (get_key_index p))
  then do {
    gen.mark_rel.(Adef.int_of_iper (get_key_index p)) := True;
    fprintf oc "\n";
    fprintf oc "rel %s %s%s" surn fnam
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    if is_definition then do {
      gen.mark.(Adef.int_of_iper (get_key_index p)) := True;
      def_p.val := [p :: def_p.val];
      if has_infos base p then print_infos oc base False "" "" p
      else fprintf oc " 0";
      match get_sex p with
      [ Male -> fprintf oc " #h"
      | Female -> fprintf oc " #f"
      | Neuter -> () ]
    }
    else ();
    fprintf oc "\n";
    fprintf oc "beg\n";
    List.iter (print_relation_for_person oc base gen def_p p)
      (get_rparents p);
    fprintf oc "end\n"
  }
  else ()
;

value print_relations oc base gen ml =
  let pl = List.fold_right (get_persons_with_relations base) ml [] in
  let pl = List.fold_right (get_isolated_related base) ml pl in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else [p :: pl]) pl []
  in
  let rec loop =
    fun
    [ [] -> ()
    | [(p, if_def) :: pl] ->
        let def_p = ref [] in
        do {
          if get_rparents p <> [] && gen.per_sel (get_key_index p) then do {
            print_relations_for_person oc base gen def_p if_def p;
            List.iter (print_notes_for_person oc base gen) def_p.val;
            if not old_gw.val then
              List.iter (print_pevents_for_person oc base gen) def_p.val
            else ()
          }
          else ();
          loop (pl @ List.map (fun p -> (p, False)) def_p.val)
        } ]
  in
  loop pl
;

value print_isolated_relations oc base gen p =
  let pl = [(p, False)] in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key_fst p pl then pl else [p :: pl]) pl []
  in
  let rec loop =
    fun
    [ [] -> ()
    | [(p, if_def) :: pl] ->
        let def_p = ref [] in
        do {
          if get_rparents p <> [] && gen.per_sel (get_key_index p) then do {
            print_relations_for_person oc base gen def_p if_def p;
            List.iter (print_notes_for_person oc base gen) def_p.val
          }
          else ();
          loop (pl @ List.map (fun p -> (p, False)) def_p.val)
        } ]
  in
  loop pl
;

value rec merge_families ifaml1f ifaml2f =
  match (ifaml1f, ifaml2f) with
  [ ([ifam1 :: ifaml1], [ifam2 :: ifaml2]) ->
      let m1 = List.mem ifam1 ifaml2 in
      let m2 = List.mem ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then [ifam2 :: merge_families ifaml1f ifaml2]
      else if m2 then [ifam1 :: merge_families ifaml1 ifaml2f]
      else if ifam2 < ifam1 then
        [ifam2; ifam1 :: merge_families ifaml1 ifaml2]
      else if ifam1 < ifam2 then
        [ifam1; ifam2 :: merge_families ifaml1 ifaml2]
      else [ifam1 :: merge_families ifaml1 ifaml2]
  | (ifaml1, []) -> ifaml1
  | ([], ifaml2) -> ifaml2 ]
;

value rec filter f =
  fun
  [ [x :: l] -> if f x then [x :: filter f l] else filter f l
  | [] -> [] ]
;

value connected_families base fam_sel ifam cpl =
  loop [ifam] [] [get_father cpl]
  where rec loop ifaml ipl_scanned =
    fun
    [ [ip :: ipl] ->
        if List.mem ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let u = poi base ip in
          let ifaml1 = Array.to_list (get_family u) in
          let ifaml1 = filter fam_sel ifaml1 in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                 let cpl = foi base ifam in
                 [get_father cpl; get_mother cpl :: ipl])
              ifaml1 ipl
          in
          loop ifaml [ip :: ipl_scanned] ipl
    | [] -> ifaml ]
;

value find_person base p1 po p2 =
  match person_of_key base p1 p2 po with
  [ Some ip -> ip
  | None ->
      do {
        printf "Not found: %s%s %s\n" p1
          (if po = 0 then "" else " " ^ string_of_int po) p2;
        flush stdout;
        exit 2
      } ]
;

value read_file_contents fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let len = ref 0 in
      try
        loop () where rec loop () =
          do { len.val := Buff.store len.val (input_char ic); loop () }
      with
      [ End_of_file -> Buff.get len.val ]
  | None -> "" ]
;

(* Separate option *)

type separate =
  [ ToSeparate
  | NotScanned
  | BeingScanned
  | Scanned ]
;

value rec find_ancestors base surn p list =
  match get_parents p with
  [ Some ifam ->
      let cpl = foi base ifam in
      let fath = poi base (get_father cpl) in
      let moth = poi base (get_mother cpl) in
      if not (eq_istr (get_surname fath) surn) &&
         not (eq_istr (get_surname moth) surn)
      then [p :: list]
      else
        let list =
          if eq_istr (get_surname fath) surn then
            find_ancestors base surn fath list
          else list
        in
        let list =
          if eq_istr (get_surname moth) surn then
            find_ancestors base surn moth list
          else list
        in
        list
  | None -> [p :: list] ]
;

value mark_branch base mark surn p =
  loop True p where rec loop top p =
    for i = 0 to Array.length (get_family p) - 1 do {
      let ifam = (get_family p).(i) in
      match mark.(Adef.int_of_ifam ifam) with
      [ NotScanned ->
          let ifaml =
            connected_families base (fun _ -> True) ifam (foi base ifam)
          in
          let children =
            List.fold_left
              (fun list ifam ->
                 let desc = foi base ifam in
                 Array.fold_left (fun list ip -> [poi base ip :: list]) list
                   (get_children desc))
              [] ifaml
          in
          if top ||
             List.exists (fun p -> eq_istr (get_surname p) surn) children
          then do {
            List.iter (fun ifam -> mark.(Adef.int_of_ifam ifam) := ToSeparate)
              ifaml;
            List.iter (loop False) children
          }
          else ()
      | _ -> () ]
    }
;

value mark_someone base mark s =
  match Gutil.person_ht_find_all base s with
  [ [ip] ->
      let p = poi base ip in
      let plist = find_ancestors base (get_surname p) p [] in
      List.iter (mark_branch base mark (get_surname p)) plist
  | [] ->
      do {
        eprintf "Error: \"%s\" is not found\n" s; flush stderr; exit 2
      }
  | _ ->
      do {
        eprintf "Error: several answers for \"%s\"\n" s;
        flush stderr;
        exit 2
      } ]
;

value sep_limit = ref 21;
value only_file = ref "";
value separate_list = ref [];

value scan_connex_component base test_action len ifam =
  loop len ifam where rec loop len ifam =
    let fam = foi base ifam in
    let fath = poi base (get_father fam) in
    let moth = poi base (get_mother fam) in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (get_family fath)
    in
    let len =
      Array.fold_left
        (fun len ifam1 ->
           if ifam1 = ifam then len else test_action loop len ifam1)
        len (get_family moth)
    in
    let len =
      match get_parents fath with
      [ Some ifam -> test_action loop len ifam
      | _ -> len ]
    in
    let len =
      match get_parents moth with
      [ Some ifam -> test_action loop len ifam
      | _ -> len ]
    in
    let children = get_children fam in
    let len =
      Array.fold_left
        (fun len ip ->
           Array.fold_left (test_action loop) len (get_family (poi base ip)))
        len children
    in
    len
;

value mark_one_connex_component base mark ifam =
  let origin_file = sou base (get_origin_file (foi base ifam)) in
  let test_action loop len ifam =
    if mark.(Adef.int_of_ifam ifam) = NotScanned &&
       sou base (get_origin_file (foi base ifam)) = origin_file
    then do {
      mark.(Adef.int_of_ifam ifam) := BeingScanned; loop (len + 1) ifam
    }
    else len
  in
  let _ = test_action (fun _ _ -> 1) 0 ifam in
  let len = 1 + scan_connex_component base test_action 0 ifam in
  let set_mark x =
    let test_action loop () ifam =
      if mark.(Adef.int_of_ifam ifam) = BeingScanned then do {
        mark.(Adef.int_of_ifam ifam) := x; loop () ifam
      }
      else ()
    in
    do {
      test_action (fun _ _ -> ()) () ifam;
      scan_connex_component base test_action () ifam
    }
  in
  if len <= sep_limit.val
  && (only_file.val = "" || only_file.val = origin_file) then
    set_mark ToSeparate
  else do {
    eprintf "%s: group of size %d not included\n" origin_file len;
    let cpl = foi base ifam in
    eprintf "    %s + %s\n" (designation base (poi base (get_father cpl)))
      (designation base (poi base (get_mother cpl)));
    flush stderr;
    set_mark Scanned
  }
;

value mark_connex_components base mark ifam =
  let test_action loop len ifam =
    if mark.(Adef.int_of_ifam ifam) = NotScanned then
      mark_one_connex_component base mark ifam
    else ()
  in
  scan_connex_component base test_action () ifam
;

value add_small_connex_components base mark =
  for i = 0 to nb_of_families base - 1 do {
    if mark.(i) = ToSeparate then
      mark_connex_components base mark (Adef.ifam_of_int i)
    else ()
  }
;

value separate base =
  match List.rev separate_list.val with
  [ [] -> fun _ -> False
  | list ->
      let mark = Array.make (nb_of_families base) NotScanned in
      do {
        List.iter (mark_someone base mark) list;
        add_small_connex_components base mark;
        let len =
          loop 0 0 where rec loop len i =
            if i = nb_of_families base then len
            else if mark.(i) = ToSeparate then loop (len + 1) (i + 1)
            else loop len (i + 1)
        in
        eprintf "*** extracted %d families\n" len;
        flush stderr;
        fun ifam -> mark.(Adef.int_of_ifam ifam) = ToSeparate
      } ]
;

value rs_printf oc s =
  loop True 0 where rec loop bol i =
    if i = String.length s then ()
    else if s.[i] = '\n' then do { fprintf oc "\n"; loop True (i + 1) }
    else do {
      if bol then fprintf oc "  " else ();
      fprintf oc "%c" s.[i];
      loop False (i + 1)
    }
;

(* Main *)

value surnames = ref [];
value no_spouses_parents = ref False;
value no_notes = ref False;
value censor = ref 0;
value with_siblings = ref False;
value maxlev = ref (-1);

value gwu base in_dir out_dir out_oc src_oc_ht anc desc ancdesc =
  let to_separate = separate base in
  let anc =
    match anc with
    [ Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None ]
  in
  let desc =
    match desc with
    [ Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None ]
  in
  let ancdesc =
    match ancdesc with
    [ Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None ]
  in
  let out_oc_first = ref True in
  let origin_file fname =
    if out_dir = "" then (out_oc, out_oc_first)
    else if fname = "" then (out_oc, out_oc_first)
    else
      try Hashtbl.find src_oc_ht fname with
      [ Not_found ->
          let oc = open_out (Filename.concat out_dir fname) in
          let x = (oc, ref True) in
          do {
            if raw_output.val then () else fprintf oc "encoding: utf-8\n";
            if old_gw.val then fprintf oc "\n" else fprintf oc "gwplus\n\n";
            Hashtbl.add src_oc_ht fname x;
            x
          } ]
  in
  let gen =
    let mark = Array.make (nb_of_persons base) False in
    let mark_rel = Array.make (nb_of_persons base) False in
    let (per_sel, fam_sel) =
      Select.functions base anc desc surnames.val ancdesc no_spouses_parents.val
        censor.val with_siblings.val maxlev.val
    in
    let fam_done = Array.make (nb_of_families base) False in
    {mark = mark; mark_rel = mark_rel; per_sel = per_sel; fam_sel = fam_sel;
     fam_done = fam_done; notes_pl_p = []; ext_files = [];
     notes_alias = notes_aliases in_dir; pevents_pl_p = []}
  in
  let nb_fam = nb_of_families base in
  do {
    if Mutil.verbose.val then ProgrBar.start () else ();
    for i = 0 to nb_fam - 1 do {
      if Mutil.verbose.val then ProgrBar.run i nb_fam else ();
      let ifam = Adef.ifam_of_int i in
      let fam = foi base ifam in
      if is_deleted_family fam then ()
      else if gen.fam_done.(i) then ()
      else if gen.fam_sel ifam then
        let ifaml = connected_families base gen.fam_sel ifam fam in
        let (oc, first) =
          if to_separate ifam then (out_oc, out_oc_first)
          else origin_file (sou base (get_origin_file fam))
        in
        let ml =
          List.fold_right
            (fun ifam ml ->
               let fam = foi base ifam in
               let m =
                 {m_ifam = ifam; m_fam = fam;
                  m_fath = poi base (get_father fam);
                  m_moth = poi base (get_mother fam);
                  m_chil =
                    Array.map (fun ip -> poi base ip) (get_children fam)}
               in
               if empty_family base m then do {
                 gen.fam_done.(Adef.int_of_ifam m.m_ifam) := True;
                 ml
               }
               else [m :: ml])
            ifaml []
        in
        if ml <> [] then do {
          gen.notes_pl_p := [];
          gen.pevents_pl_p := [];
          if not first.val then fprintf oc "\n" else ();
          first.val := False;
          List.iter (print_family oc base gen) ml;
          print_notes oc base gen ml;
          print_relations oc base gen ml;
          if not old_gw.val then print_pevents oc base gen ml
          else ();
        }
        else ()
      else ()
    };
    (* Ajout des personnes isol�e � l'export. On leur ajoute des    *)
    (* parents pour pouvoir utiliser les autres fonctions normales. *)
    (* Export que si c'est toute la base.                           *)
    if isolated.val && anc = None && desc = None && ancdesc = None then
      for i = 0 to nb_of_persons base - 1 do {
        if gen.mark.(i) || gen.mark_rel.(i) then ()
        else
          let p = poi base (Adef.iper_of_int i) in
          match get_parents p with
          [ Some _ -> ()
          | None ->
              if bogus_person base p &&
                 not (get_birth p <> Adef.codate_None || get_baptism p <> Adef.codate_None ||
                      get_first_names_aliases p <> [] || get_surnames_aliases p <> [] ||
                      sou base (get_public_name p) <> "" || get_qualifiers p <> [] ||
                      get_aliases p <> [] || get_titles p <> [] ||
                      sou base (get_occupation p) <> "" || sou base (get_birth_place p) <> "" ||
                      sou base (get_birth_src p) <> "" ||
                      sou base (get_baptism_place p) <> "" || sou base (get_baptism_src p) <> "" ||
                      sou base (get_death_place p) <> "" || sou base (get_death_src p) <> "" ||
                      sou base (get_burial_place p) <> "" || sou base (get_burial_src p) <> "" ||
                      sou base (get_notes p) <> "" || sou base (get_psources p) <> "" ||
                      get_rparents p <> [] || get_related p <> [])
              then ()
              else
                let (oc, _first) = origin_file (base_notes_origin_file base) in
                do {
                  fprintf oc "\n";
                  print_empty_family oc base p;
                  print_notes_for_person oc base gen p;
                  gen.mark.(i) := True;
                  print_isolated_relations oc base gen p;
                }]
      }
    else ();
    if Mutil.verbose.val then ProgrBar.finish () else ();
    if not no_notes.val then do {
      let s = base_notes_read base "" in
      let (oc, first) = origin_file (base_notes_origin_file base) in
      if s <> "" then do {
        if not first.val then fprintf oc "\n" else ();
        first.val := False;
        fprintf oc "notes-db\n";
        rs_printf oc s;
        fprintf oc "\nend notes-db\n";
        ignore (add_linked_files gen (fun _ -> "database notes") s [] : list _);
      }
      else ();
      try
        let files =
          Sys.readdir (Filename.concat in_dir (base_wiznotes_dir base))
        in
        do {
          Array.sort compare files;
          for i = 0 to Array.length files - 1 do {
            let file = files.(i) in
            if Filename.check_suffix file ".txt" then do {
              let wfile =
                List.fold_left Filename.concat in_dir
                  [base_wiznotes_dir base; file]
              in
              let s = read_file_contents wfile in
              ignore (add_linked_files gen
                (fun _ -> "wizard \"" ^ file ^ "\"") s [] : list _);
            }
            else ()
          };
        }
      with
      [ Sys_error _ -> () ];
      let rec loop =
        fun
        [ [] -> ()
        | [(f, _) :: files] ->
            let fn =
              match NotesLinks.check_file_name f with
              [ Some (dl, f) -> List.fold_right Filename.concat dl f
              | None -> "bad" ]
            in
            let s = base_notes_read base fn in
            let files =
              add_linked_files gen (fun _ -> sprintf "extended page \"%s\"" f)
                s files
            in
            loop files ]
      in
      loop gen.ext_files;
      List.iter
        (fun (f, r) ->
           let fn =
             match NotesLinks.check_file_name f with
             [ Some (dl, f) -> List.fold_right Filename.concat dl f
             | None -> "bad" ]
           in
           let s = strip_spaces (base_notes_read base fn) in
           if s <> "" then do {
             if not first.val then fprintf oc "\n" else ();
             first.val := False;
             fprintf oc "# extended page \"%s\" used by:\n" f;
             List.iter (fun f -> fprintf oc "#  - %s\n" f)
               (List.sort compare r.val);
             fprintf oc "page-ext %s\n" f;
             rs_printf oc s;
             fprintf oc "\nend page-ext\n";
           }
           else ())
        (List.sort compare gen.ext_files);
      try
        let files =
          Sys.readdir (Filename.concat in_dir (base_wiznotes_dir base))
        in
        do {
          Array.sort compare files;
          for i = 0 to Array.length files - 1 do {
            let file = files.(i) in
            if Filename.check_suffix file ".txt" then do {
              let wizid = Filename.chop_suffix file ".txt" in
              let wfile =
                List.fold_left Filename.concat in_dir
                  [base_wiznotes_dir base; file]
              in
              let s = strip_spaces (read_file_contents wfile) in
              fprintf oc "\nwizard-note %s\n" wizid;
              rs_printf oc s;
              fprintf oc "\nend wizard-note\n";
            }
            else ()
          };
        }
      with
      [ Sys_error _ -> () ];
    }
    else ();
  }
;

value in_file = ref "";
value out_file = ref "";
value out_dir = ref "";
value anc_1st = ref "";
value anc_occ = ref 0;
value anc_2nd = ref "";
value desc_1st = ref "";
value desc_occ = ref 0;
value desc_2nd = ref "";
value ancdesc_1st = ref "";
value ancdesc_occ = ref 0;
value ancdesc_2nd = ref "";

type arg_state =
  [ ASnone
  | ASwaitAncOcc
  | ASwaitAncSurn
  | ASwaitDescOcc
  | ASwaitDescSurn
  | ASwaitAncdescOcc
  | ASwaitAncdescSurn ]
;
value arg_state = ref ASnone;
value mem = ref False;

value speclist =
  [("-o", Arg.String (fun s -> out_file.val := s),
    "<file>    output file name (else stdout)");
   ("-odir", Arg.String (fun s -> out_dir.val := s),
    "<dir>  create files from original name in directory (else on -o file)");
   ("-mem", Arg.Set mem, "        save memory space, but slower");
   ("-a",
    Arg.String
      (fun s -> do { anc_1st.val := s; arg_state.val := ASwaitAncOcc }),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors of...");
   ("-d",
    Arg.String
      (fun s -> do { desc_1st.val := s; arg_state.val := ASwaitDescOcc }),
    "\"<1st_name>\" [num] \"<surname>\" : select descendants of...");
   ("-ad",
    Arg.String
      (fun s ->
         do { ancdesc_1st.val := s; arg_state.val := ASwaitAncdescOcc }),
    "\
\"<1st_name>\" [num] \"<surname>\" : select ancestors of...
    and all their descendants (has no effect if -a and/or -d used,
    option -nsp is forced).");
   ("-aws",
    Arg.String
      (fun s ->
         do {
           anc_1st.val := s;
           arg_state.val := ASwaitAncOcc;
           with_siblings.val := True;
           ()
         }),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors with siblings");
   ("-s", Arg.String (fun x -> surnames.val := [x :: surnames.val]),
    "\"<surname>\" : select this surname (option usable several times)");
   ("-maxlev", Arg.Int (fun i -> maxlev.val := i),
    "\"<level>\" : maximum level of generations of descendants");
   ("-nsp", Arg.Set no_spouses_parents,
    ": no spouses' parents (for options -s and -d)");
   ("-nn", Arg.Set no_notes, ": no (database) notes");
   ("-nopicture", Arg.Set no_picture, ": Don't extract individual picture.");
   ("-isolated", Arg.Set isolated, ": Export isolated persons (work only if export all database).");
   ("-c", Arg.Int (fun i -> censor.val := i), "\
<num> :
     When a person is born less than <num> years ago, it is not exported unless
     it is Public. All the spouses and descendants are also censored.");
   ("-old_gw", Arg.Set old_gw, ": Do not export additional fields \
(for backward compatibility: < 7.00)");
   ("-raw", Arg.Set raw_output,
    "raw output (without possible utf-8 conversion)");
   ("-v", Arg.Set Mutil.verbose, "verbose");
   ("-sep",
    Arg.String (fun s -> separate_list.val := [s :: separate_list.val]), "\
\"1st_name.num surname\" :
     To use together with the option \"-odir\": separate this person and
     all his ancestors and descendants sharing the same surname. All the
     concerned families are displayed on standard output instead of their
     associated files. This option can be used several times.");
   ("-sep_only_file", Arg.String (fun s -> only_file.val := s), "\
<file> :
     With option \"-sep\", tells to separate only groups of that file.");
   ("-sep_limit", Arg.Int (fun i -> sep_limit.val := i), "\
<num> :
     When using the option \"-sep\", groups of families can become isolated
     in the files. Gwu reconnects them to the separated families (i.e.
     displays them to standard output) if the size of these groups is less
     than " ^ string_of_int sep_limit.val ^ "\
. The present option changes this limit.")]
;

value anonfun s =
  match arg_state.val with
  [ ASnone ->
      if in_file.val = "" then in_file.val := s
      else raise (Arg.Bad "Cannot treat several databases")
  | ASwaitAncOcc ->
      try
        do { anc_occ.val := int_of_string s; arg_state.val := ASwaitAncSurn }
      with
      [ Failure _ ->
          do { anc_occ.val := 0; anc_2nd.val := s; arg_state.val := ASnone } ]
  | ASwaitAncSurn -> do { anc_2nd.val := s; arg_state.val := ASnone }
  | ASwaitDescOcc ->
      try
        do {
          desc_occ.val := int_of_string s; arg_state.val := ASwaitDescSurn
        }
      with
      [ Failure _ ->
          do {
            desc_occ.val := 0; desc_2nd.val := s; arg_state.val := ASnone
          } ]
  | ASwaitDescSurn -> do { desc_2nd.val := s; arg_state.val := ASnone }
  | ASwaitAncdescOcc ->
      try
        do {
          ancdesc_occ.val := int_of_string s;
          arg_state.val := ASwaitAncdescSurn
        }
      with
      [ Failure _ ->
          do {
            ancdesc_occ.val := 0;
            ancdesc_2nd.val := s;
            arg_state.val := ASnone
          } ]
  | ASwaitAncdescSurn ->
      do { ancdesc_2nd.val := s; arg_state.val := ASnone } ]
;

value errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
[options] <base_file>
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
Options are:"
;

value main () =
  do {
    Mutil.verbose.val := False;
    Argl.parse speclist anonfun errmsg;
    if in_file.val = "" then do {
      printf "Missing base\n";
      printf "Use option -help for usage\n";
      flush stdout;
      exit 2
    }
    else ();
    Secure.set_base_dir (Filename.dirname in_file.val);
    let anc =
      if anc_1st.val <> "" then
        if anc_2nd.val = "" then do {
          printf "Misused option -a\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        }
        else Some (anc_1st.val, anc_occ.val, anc_2nd.val)
      else None
    in
    let desc =
      if desc_1st.val <> "" then
        if desc_2nd.val = "" then do {
          printf "Misused option -d\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        }
        else Some (desc_1st.val, desc_occ.val, desc_2nd.val)
      else None
    in
    let ancdesc =
      if ancdesc_1st.val <> "" then
        if anc_1st.val <> "" || desc_1st.val <> "" then do {
          printf "Option -ad skipped since -a and/or -d used\n"; None
        }
        else if ancdesc_2nd.val = "" then do {
          printf "Misused option -ad\n";
          printf "Use option -help for usage\n";
          flush stdout;
          exit 2
        }
        else do {
          no_spouses_parents.val := True;
          Some (ancdesc_1st.val, ancdesc_occ.val, ancdesc_2nd.val)
        }
      else None
    in
    let base = Gwdb.open_base in_file.val in
    let in_dir =
      if Filename.check_suffix in_file.val ".gwb" then in_file.val
      else in_file.val ^ ".gwb"
    in
    let src_oc_ht = Hashtbl.create 1009 in
    let () = load_ascends_array base in
    let () = load_strings_array base in
    if not mem.val then
      let () = load_couples_array base in
      let () = load_unions_array base in
      let () = load_descends_array base in
      ()
    else ();
    let out_oc =
      if out_file.val = "" then stdout else open_out out_file.val
    in
    if raw_output.val then () else fprintf out_oc "encoding: utf-8\n";
    if old_gw.val then fprintf out_oc "\n" else fprintf out_oc "gwplus\n\n";
    prepare_free_occ base;
    gwu base in_dir out_dir.val out_oc src_oc_ht anc desc ancdesc;
    Hashtbl.iter (fun _ (oc, _) -> do { flush oc; close_out oc })
      src_oc_ht;
    flush out_oc;
    if out_file.val = "" then () else close_out out_oc
  }
;

value run_main = Printexc.print main;

end;
