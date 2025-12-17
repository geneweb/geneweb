(* Copyright (c) 1998-2007 INRIA *)

let old_gw = ref false
let only_file = ref ""
let out_dir = ref ""
let raw_output = ref false
let sep_limit = ref 21
let separate_list = ref []

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
          match Gwdb.get_pevent_name evt with
          | Def.Epers_Birth | Def.Epers_Baptism | Def.Epers_Death
          | Def.Epers_Burial | Def.Epers_Cremation ->
              if
                Gwdb.sou base (Gwdb.get_pevent_note evt) <> ""
                || Gwdb.get_pevent_witnesses evt <> [||]
              then true
              else loop events
          | Def.Epers_Accomplishment | Def.Epers_Acquisition
          | Def.Epers_Adhesion | Def.Epers_BaptismLDS | Def.Epers_BarMitzvah
          | Def.Epers_BatMitzvah | Def.Epers_Benediction | Def.Epers_ChangeName
          | Def.Epers_Circumcision | Def.Epers_Confirmation
          | Def.Epers_ConfirmationLDS | Def.Epers_Decoration
          | Def.Epers_DemobilisationMilitaire | Def.Epers_Diploma
          | Def.Epers_Distinction | Def.Epers_Dotation | Def.Epers_DotationLDS
          | Def.Epers_Education | Def.Epers_Election | Def.Epers_Emigration
          | Def.Epers_Excommunication | Def.Epers_FamilyLinkLDS
          | Def.Epers_FirstCommunion | Def.Epers_Funeral | Def.Epers_Graduate
          | Def.Epers_Hospitalisation | Def.Epers_Illness
          | Def.Epers_Immigration | Def.Epers_ListePassenger
          | Def.Epers_MilitaryDistinction | Def.Epers_MilitaryPromotion
          | Def.Epers_MilitaryService | Def.Epers_MobilisationMilitaire
          | Def.Epers_Naturalisation | Def.Epers_Occupation
          | Def.Epers_Ordination | Def.Epers_Property | Def.Epers_Recensement
          | Def.Epers_Residence | Def.Epers_Retired | Def.Epers_ScellentChildLDS
          | Def.Epers_ScellentParentLDS | Def.Epers_ScellentSpouseLDS
          | Def.Epers_VenteBien | Def.Epers_Will | Def.Epers_Adoption
          | Def.Epers_Name _ ->
              true)
    in
    loop (Gwdb.get_pevents p)
  else false

let (ht_dup_occ : (Gwdb.iper, int) Hashtbl.t) = Hashtbl.create 20001
let (ht_orig_occ : (string, int list) Hashtbl.t) = Hashtbl.create 20001

let prepare_free_occ ?(select = fun _ -> true) base =
  (* Parce qu'on est obligé ... *)
  let sn = "?" in
  let fn = "?" in
  let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
  Hashtbl.add ht_orig_occ key [ 0 ];
  Gwdb.Collection.iter
    (fun ip ->
      if select ip then
        let p = Gwdb.poi base ip in
        let sn = Gwdb.sou base (Gwdb.get_surname p) in
        let fn = Gwdb.sou base (Gwdb.get_first_name p) in
        if sn = "?" && fn = "?" then ()
        else
          let fn = Name.lower fn in
          let sn = Name.lower sn in
          if fn = "" || sn = "" then
            let key = fn ^ " #@# " ^ sn in
            let occ = Gwdb.get_occ p in
            match Hashtbl.find_opt ht_orig_occ key with
            | Some l ->
                if List.mem occ l then Hashtbl.add ht_dup_occ ip occ
                else Hashtbl.replace ht_orig_occ key (occ :: l)
            | None -> Hashtbl.add ht_orig_occ key [ occ ])
    (Gwdb.ipers base);
  Hashtbl.iter
    (fun key l -> Hashtbl.replace ht_orig_occ key (List.sort compare l))
    ht_orig_occ;
  Hashtbl.iter
    (fun ip _ ->
      let p = Gwdb.poi base ip in
      let sn = Gwdb.sou base (Gwdb.get_surname p) in
      let fn = Gwdb.sou base (Gwdb.get_first_name p) in
      let key = Name.lower fn ^ " #@# " ^ Name.lower sn in
      Option.iter
        (fun list_occ ->
          let rec loop list init new_list =
            match list with
            | x :: y :: l when y - x > 1 ->
                (succ x, List.rev_append (y :: succ x :: x :: new_list) l)
            | x :: l -> loop l (succ x) (x :: new_list)
            | [] -> (init, [ init ])
          in
          let new_occ, new_list_occ = loop list_occ 0 [] in
          Hashtbl.replace ht_dup_occ ip new_occ;
          Hashtbl.replace ht_orig_occ key new_list_occ)
        (Hashtbl.find_opt ht_orig_occ key))
    ht_dup_occ

let get_new_occ p =
  match Hashtbl.find_opt ht_dup_occ (Gwdb.get_iper p) with
  | Some o -> o
  | None -> Gwdb.get_occ p

type mfam = {
  m_ifam : Gwdb.ifam;
  m_fam : Gwdb.family;
  m_fath : Gwdb.person;
  m_moth : Gwdb.person;
  m_chil : Gwdb.person array;
}

let soy y = if y = 0 then "-0" else string_of_int y
let oc opts = match opts.Gwexport.oc with _, oc, _ -> oc

let print_date_dmy opts d =
  (match d.Date.prec with
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
  | Sure | About | Maybe | Before | After -> ()

let is_printable = function '\000' .. '\031' -> false | _ -> true

let starting_char no_num s =
  match s.[0] with
  (*'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' *)
  | 'a' .. 'z' | 'A' .. 'Z' | '\xE0' .. '\xFD' | '\xC0' .. '\xDD' -> true
  | '0' .. '9' -> not no_num
  | '?' -> s = "?"
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

let correct_string base is = s_correct_string (Gwdb.sou base is)

let correct_string_no_colon base is =
  gen_correct_string false true (Gwdb.sou base is)

let gen_print_date opts no_colon = function
  | Date.Dgreg (d, Dgregorian) -> print_date_dmy opts d
  | Dgreg (d, Djulian) ->
      print_date_dmy opts (Date.convert ~from:Dgregorian ~to_:Djulian d);
      Printf.ksprintf (oc opts) "J"
  | Dgreg (d, Dfrench) ->
      print_date_dmy opts (Date.convert ~from:Dgregorian ~to_:Dfrench d);
      Printf.ksprintf (oc opts) "F"
  | Dgreg (d, Dhebrew) ->
      print_date_dmy opts (Date.convert ~from:Dgregorian ~to_:Dhebrew d);
      Printf.ksprintf (oc opts) "H"
  | Dgreg (d, Dislamic) ->
      print_date_dmy opts (Date.convert ~from:Dgregorian ~to_:Dislamic d);
      Printf.ksprintf (oc opts) "I"
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

let has_infos_not_dates opts base p =
  let has_picture_to_export =
    Gwdb.sou base (Gwdb.get_image p) <> "" && not opts.Gwexport.no_picture
  in
  Gwdb.get_first_names_aliases p <> []
  || Gwdb.get_surnames_aliases p <> []
  || Gwdb.sou base (Gwdb.get_public_name p) <> ""
  || has_picture_to_export
  || Gwdb.get_qualifiers p <> []
  || Gwdb.get_aliases p <> []
  || Gwdb.get_titles p <> []
  || Gwdb.get_access p <> Def.IfTitles
  || Gwdb.sou base (Gwdb.get_occupation p) <> ""
  || (opts.Gwexport.source <> None || Gwdb.sou base @@ Gwdb.get_psources p <> "")
  || Gwdb.sou base (Gwdb.get_birth_place p) <> ""
  || (opts.Gwexport.source = None && Gwdb.sou base (Gwdb.get_birth_src p) <> "")
  || Gwdb.sou base (Gwdb.get_baptism_place p) <> ""
  || opts.Gwexport.source = None
     && Gwdb.sou base (Gwdb.get_baptism_src p) <> ""
  || Gwdb.sou base (Gwdb.get_death_place p) <> ""
  || (opts.Gwexport.source = None && Gwdb.sou base (Gwdb.get_death_src p) <> "")
  || Gwdb.sou base (Gwdb.get_burial_place p) <> ""
  || (opts.Gwexport.source = None && Gwdb.sou base (Gwdb.get_burial_src p) <> "")

let has_infos opts base p =
  has_infos_not_dates opts base p
  || Gwdb.get_birth p <> Date.cdate_None
  || Gwdb.get_baptism p <> Date.cdate_None
  || Gwdb.get_death p <> Def.NotDead

let print_if_not_equal_to opts x base lab is =
  if Gwdb.sou base is <> x then
    Printf.ksprintf (oc opts) " %s %s" lab (correct_string base is)

let print_src_if_not_equal_to opts x base lab is =
  match opts.Gwexport.source with
  | None ->
      if Gwdb.sou base is <> "" then print_if_not_equal_to opts x base lab is
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
  | Def.Buried cod -> (
      Printf.ksprintf (oc opts) " #buri";
      match Date.od_of_cdate cod with
      | Some d ->
          Printf.ksprintf (oc opts) " ";
          print_date opts d;
          ()
      | None -> ())
  | Def.Cremated cod -> (
      Printf.ksprintf (oc opts) " #crem";
      match Date.od_of_cdate cod with
      | Some d ->
          Printf.ksprintf (oc opts) " ";
          print_date opts d;
          ()
      | None -> ())
  | Def.UnknownBurial -> ()

let print_title opts base t =
  let t_date_start = Date.od_of_cdate t.Def.t_date_start in
  let t_date_end = Date.od_of_cdate t.Def.t_date_end in
  Printf.ksprintf (oc opts) " [";
  (match t.Def.t_name with
  | Def.Tmain -> Printf.ksprintf (oc opts) "*"
  | Def.Tname s ->
      Printf.ksprintf (oc opts) "%s" (correct_string_no_colon base s)
  | Def.Tnone -> ());
  Printf.ksprintf (oc opts) ":";
  Printf.ksprintf (oc opts) "%s" (correct_string_no_colon base t.Def.t_ident);
  Printf.ksprintf (oc opts) ":";
  Printf.ksprintf (oc opts) "%s" (correct_string_no_colon base t.Def.t_place);
  (if t.Def.t_nth <> 0 then Printf.ksprintf (oc opts) ":"
  else
    match (t_date_start, t_date_end) with
    | Some _, _ | _, Some _ -> Printf.ksprintf (oc opts) ":"
    | _ -> ());
  print_title_date_option opts t_date_start;
  (if t.Def.t_nth <> 0 then Printf.ksprintf (oc opts) ":"
  else
    match t_date_end with Some _ -> Printf.ksprintf (oc opts) ":" | None -> ());
  print_title_date_option opts t_date_end;
  if t.Def.t_nth <> 0 then Printf.ksprintf (oc opts) ":%d" t.Def.t_nth;
  Printf.ksprintf (oc opts) "]"

let zero_birth_is_required opts base is_child p =
  if Gwdb.get_baptism p <> Date.cdate_None then false
  else
    match Gwdb.get_death p with
    | Def.Death (_, _) | Def.DeadYoung | Def.DeadDontKnowWhen | Def.OfCourseDead
      ->
        true
    | Def.DontKnowIfDead ->
        (not is_child)
        && (not (has_infos_not_dates opts base p))
        && Gwdb.p_first_name base p <> "?"
        && Gwdb.p_surname base p <> "?"
    | Def.NotDead -> false

type gen = {
  pevents_done : (Gwdb.iper, bool) Gwdb.Marker.t;
  infos_done : (Gwdb.iper, bool) Gwdb.Marker.t;
  notes_done : (Gwdb.iper, bool) Gwdb.Marker.t;
  mark : (Gwdb.iper, bool) Gwdb.Marker.t;
  mark_rel : (Gwdb.iper, bool) Gwdb.Marker.t;
  per_sel : Gwdb.iper -> bool;
  fam_sel : Gwdb.ifam -> bool;
  fam_done : (Gwdb.ifam, bool) Gwdb.Marker.t;
  mutable notes_pl_p : Gwdb.person list;
  mutable ext_files : (string * string list ref) list;
  mutable notes_alias : (string * string) list;
  mutable pevents_pl_p : Gwdb.person list;
}

let print_infos gen opts base is_child csrc cbp p =
  let is_marked = Gwdb.Marker.get gen.infos_done (Gwdb.get_iper p) in
  if not is_marked then (
    Gwdb.Marker.set gen.infos_done (Gwdb.get_iper p) true;
    List.iter
      (print_first_name_alias opts base)
      (Gwdb.get_first_names_aliases p);
    List.iter (print_surname_alias opts base) (Gwdb.get_surnames_aliases p);
    (match Gwdb.get_public_name p with
    | s when Gwdb.sou base s <> "" ->
        Printf.ksprintf (oc opts) " (%s)" (correct_string base s)
    | _ -> ());
    if not opts.no_picture then
      print_if_no_empty opts base "#image" (Gwdb.get_image p);
    List.iter (print_qualifier opts base) (Gwdb.get_qualifiers p);
    List.iter (print_alias opts base) (Gwdb.get_aliases p);
    List.iter (print_title opts base) (Gwdb.get_titles p);
    (match Gwdb.get_access p with
    | IfTitles -> ()
    | Public -> Printf.ksprintf (oc opts) " #apubl"
    | Private -> Printf.ksprintf (oc opts) " #apriv");
    print_if_no_empty opts base "#occu" (Gwdb.get_occupation p);
    print_src_if_not_equal_to opts csrc base "#src" (Gwdb.get_psources p);
    (match Date.od_of_cdate (Gwdb.get_birth p) with
    | Some d ->
        Printf.ksprintf (oc opts) " ";
        print_date opts d
    | _ when zero_birth_is_required opts base is_child p ->
        Printf.ksprintf (oc opts) " 0"
    | None -> ());
    print_if_not_equal_to opts cbp base "#bp" (Gwdb.get_birth_place p);
    if opts.source = None then
      print_if_no_empty opts base "#bs" (Gwdb.get_birth_src p);
    (match Date.od_of_cdate (Gwdb.get_baptism p) with
    | Some d ->
        Printf.ksprintf (oc opts) " !";
        print_date opts d
    | None -> ());
    print_if_no_empty opts base "#pp" (Gwdb.get_baptism_place p);
    if opts.source = None then
      print_if_no_empty opts base "#ps" (Gwdb.get_baptism_src p);
    (match Gwdb.get_death p with
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
          ( Date.od_of_cdate (Gwdb.get_birth p),
            Date.od_of_cdate (Gwdb.get_baptism p) )
        with
        | Some _, _ | _, Some _ -> Printf.ksprintf (oc opts) " ?"
        | _ -> ())
    | OfCourseDead -> Printf.ksprintf (oc opts) " od"
    | NotDead -> ());
    print_if_no_empty opts base "#dp" (Gwdb.get_death_place p);
    if opts.source = None then
      print_if_no_empty opts base "#ds" (Gwdb.get_death_src p);
    print_burial opts (Gwdb.get_burial p);
    print_if_no_empty opts base "#rp" (Gwdb.get_burial_place p);
    if opts.source = None then
      print_if_no_empty opts base "#rs" (Gwdb.get_burial_src p))

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
          Option.fold (String.index_opt b '/') ~some:(String.sub b 0) ~none:b
        in
        let fname =
          Option.value ~default:fname (List.assoc_opt fname gen.notes_alias)
        in
        let new_linked_files =
          match List.assoc_opt fname gen.ext_files with
          | Some r ->
              if List.mem from !r then () else r := from :: !r;
              new_linked_files
          | None ->
              let lf = (fname, ref [ from ]) in
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
    match Gwdb.get_parents p with
    | Some ifam -> gen.fam_sel ifam
    | None -> false
  in
  let first_parent_definition =
    if Gwdb.Marker.get gen.mark (Gwdb.get_iper p) then false
    else (
      Gwdb.Marker.set gen.mark (Gwdb.get_iper p) true;
      true)
  in
  let pr = (not has_printed_parents) && first_parent_definition in
  let has_infos = if pr then has_infos opts base p else false in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  Printf.ksprintf (oc opts) "%s %s%s" (s_correct_string surname)
    (s_correct_string first_name)
    (if first_name = "?" && surname = "?" then ""
    else if get_new_occ p = 0 then ""
    else "." ^ string_of_int (get_new_occ p));
  if pr then
    if has_infos then print_infos gen opts base false "" "" p
    else if first_name <> "?" && surname <> "?" then
      Printf.ksprintf (oc opts) " 0"

let print_child gen opts base fam_surname csrc cbp p =
  Printf.ksprintf (oc opts) "-";
  (match Gwdb.get_sex p with
  | Def.Male -> Printf.ksprintf (oc opts) " h"
  | Def.Female -> Printf.ksprintf (oc opts) " f"
  | Def.Neuter -> ());
  Printf.ksprintf (oc opts) " %s"
    (s_correct_string (Gwdb.sou base (Gwdb.get_first_name p)));
  if Gwdb.p_first_name base p = "?" && Gwdb.p_surname base p = "?" then ()
  else if get_new_occ p = 0 then ()
  else Printf.ksprintf (oc opts) ".%d" (get_new_occ p);
  if not (Gwdb.eq_istr (Gwdb.get_surname p) fam_surname) then
    Printf.ksprintf (oc opts) " %s"
      (s_correct_string_nonum (Gwdb.sou base (Gwdb.get_surname p)));
  print_infos gen opts base true csrc cbp p;
  Printf.ksprintf (oc opts) "\n"

let bogus_person base p =
  Gwdb.p_first_name base p = "?" && Gwdb.p_surname base p = "?"

let common_children proj base children =
  if Array.length children <= 1 then None
  else
    let list =
      List.map (fun p -> Gwdb.sou base (proj p)) (Array.to_list children)
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

let common_children_sources = common_children Gwdb.get_psources
let common_children_birth_place = common_children Gwdb.get_birth_place

let empty_family base m =
  bogus_person base m.m_fath && bogus_person base m.m_moth
  && Array.for_all (bogus_person base) m.m_chil

let string_of_witness_kind :
    Def.witness_kind -> ('a, unit, string, unit) format4 option = function
  | Def.Witness_GodParent -> Some "#godp"
  | Def.Witness -> None
  | Def.Witness_CivilOfficer -> Some "#offi"
  | Def.Witness_ReligiousOfficer -> Some "#reli"
  | Def.Witness_Informant -> Some "#info"
  | Def.Witness_Attending -> Some "#atte"
  | Def.Witness_Mentioned -> Some "#ment"
  | Def.Witness_Other -> Some "#othe"

let print_multiline opts tag s =
  let lines = String.split_on_char '\n' s in
  List.iter
    (fun s -> if s <> "" then Printf.ksprintf (oc opts) "%s %s\n" tag s)
    lines

let print_witnesses opts base gen ~use_per_sel witnesses =
  let print_witness p =
    Printf.ksprintf (oc opts) "%s %s%s"
      (correct_string base (Gwdb.get_surname p))
      (correct_string base (Gwdb.get_first_name p))
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    if
      Array.length (Gwdb.get_family p) = 0
      && Gwdb.get_parents p = None
      && not (Gwdb.Marker.get gen.mark (Gwdb.get_iper p))
    then (
      Gwdb.Marker.set gen.mark (Gwdb.get_iper p) true;
      if has_infos opts base p then print_infos gen opts base false "" "" p
      else Printf.ksprintf (oc opts) " 0";
      (match Gwdb.sou base (Gwdb.get_notes p) with
      | "" ->
          if put_events_in_notes base p then
            gen.notes_pl_p <- p :: gen.notes_pl_p
      | _ -> gen.notes_pl_p <- p :: gen.notes_pl_p);
      if Gwdb.get_pevents p <> [] then gen.pevents_pl_p <- p :: gen.pevents_pl_p)
  in
  Array.iter
    (fun (ip, wk, wnote) ->
      if (not use_per_sel) || gen.per_sel ip then (
        let p = Gwdb.poi base ip in
        Printf.ksprintf (oc opts) "wit";
        (match Gwdb.get_sex p with
        | Def.Male -> Printf.ksprintf (oc opts) " m"
        | Def.Female -> Printf.ksprintf (oc opts) " f"
        | Def.Neuter -> ());
        Printf.ksprintf (oc opts) ": ";
        let sk = string_of_witness_kind wk in
        (match sk with
        | Some s -> Printf.ksprintf (oc opts) (s ^^ " ")
        | None -> ());
        print_witness p;
        Printf.ksprintf (oc opts) "\n";
        (* print witness note *)
        if opts.notes && not (Gwdb.is_empty_string wnote) then
          let wnote = Gwdb.sou base wnote in
          print_multiline opts "wnote" wnote))
    witnesses

let print_pevent opts base gen e =
  (match Gwdb.get_pevent_name e with
  | Def.Epers_Birth -> Printf.ksprintf (oc opts) "#birt"
  | Def.Epers_Baptism -> Printf.ksprintf (oc opts) "#bapt"
  | Def.Epers_Death -> Printf.ksprintf (oc opts) "#deat"
  | Def.Epers_Burial -> Printf.ksprintf (oc opts) "#buri"
  | Def.Epers_Cremation -> Printf.ksprintf (oc opts) "#crem"
  | Def.Epers_Accomplishment -> Printf.ksprintf (oc opts) "#acco"
  | Def.Epers_Acquisition -> Printf.ksprintf (oc opts) "#acqu"
  | Def.Epers_Adhesion -> Printf.ksprintf (oc opts) "#adhe"
  | Def.Epers_BaptismLDS -> Printf.ksprintf (oc opts) "#bapl"
  | Def.Epers_BarMitzvah -> Printf.ksprintf (oc opts) "#barm"
  | Def.Epers_BatMitzvah -> Printf.ksprintf (oc opts) "#basm"
  | Def.Epers_Benediction -> Printf.ksprintf (oc opts) "#bles"
  | Def.Epers_ChangeName -> Printf.ksprintf (oc opts) "#chgn"
  | Def.Epers_Circumcision -> Printf.ksprintf (oc opts) "#circ"
  | Def.Epers_Confirmation -> Printf.ksprintf (oc opts) "#conf"
  | Def.Epers_ConfirmationLDS -> Printf.ksprintf (oc opts) "#conl"
  | Def.Epers_Decoration -> Printf.ksprintf (oc opts) "#awar"
  | Def.Epers_DemobilisationMilitaire -> Printf.ksprintf (oc opts) "#demm"
  | Def.Epers_Diploma -> Printf.ksprintf (oc opts) "#degr"
  | Def.Epers_Distinction -> Printf.ksprintf (oc opts) "#dist"
  | Def.Epers_Dotation -> Printf.ksprintf (oc opts) "#endl"
  | Def.Epers_DotationLDS -> Printf.ksprintf (oc opts) "#dotl"
  | Def.Epers_Education -> Printf.ksprintf (oc opts) "#educ"
  | Def.Epers_Election -> Printf.ksprintf (oc opts) "#elec"
  | Def.Epers_Emigration -> Printf.ksprintf (oc opts) "#emig"
  | Def.Epers_Excommunication -> Printf.ksprintf (oc opts) "#exco"
  | Def.Epers_FamilyLinkLDS -> Printf.ksprintf (oc opts) "#flkl"
  | Def.Epers_FirstCommunion -> Printf.ksprintf (oc opts) "#fcom"
  | Def.Epers_Funeral -> Printf.ksprintf (oc opts) "#fune"
  | Def.Epers_Graduate -> Printf.ksprintf (oc opts) "#grad"
  | Def.Epers_Hospitalisation -> Printf.ksprintf (oc opts) "#hosp"
  | Def.Epers_Illness -> Printf.ksprintf (oc opts) "#illn"
  | Def.Epers_Immigration -> Printf.ksprintf (oc opts) "#immi"
  | Def.Epers_ListePassenger -> Printf.ksprintf (oc opts) "#lpas"
  | Def.Epers_MilitaryDistinction -> Printf.ksprintf (oc opts) "#mdis"
  | Def.Epers_MilitaryPromotion -> Printf.ksprintf (oc opts) "#mpro"
  | Def.Epers_MilitaryService -> Printf.ksprintf (oc opts) "#mser"
  | Def.Epers_MobilisationMilitaire -> Printf.ksprintf (oc opts) "#mobm"
  | Def.Epers_Naturalisation -> Printf.ksprintf (oc opts) "#natu"
  | Def.Epers_Occupation -> Printf.ksprintf (oc opts) "#occu"
  | Def.Epers_Ordination -> Printf.ksprintf (oc opts) "#ordn"
  | Def.Epers_Property -> Printf.ksprintf (oc opts) "#prop"
  | Def.Epers_Recensement -> Printf.ksprintf (oc opts) "#cens"
  | Def.Epers_Residence -> Printf.ksprintf (oc opts) "#resi"
  | Def.Epers_Retired -> Printf.ksprintf (oc opts) "#reti"
  | Def.Epers_ScellentChildLDS -> Printf.ksprintf (oc opts) "#slgc"
  | Def.Epers_ScellentParentLDS -> Printf.ksprintf (oc opts) "#slgp"
  | Def.Epers_ScellentSpouseLDS -> Printf.ksprintf (oc opts) "#slgs"
  | Def.Epers_VenteBien -> Printf.ksprintf (oc opts) "#vteb"
  | Def.Epers_Will -> Printf.ksprintf (oc opts) "#will"
  | Def.Epers_Adoption -> Printf.ksprintf (oc opts) "#adop"
  | Def.Epers_Name s -> Printf.ksprintf (oc opts) "#%s" (correct_string base s));
  let epers_date = Date.od_of_cdate (Gwdb.get_pevent_date e) in
  (match epers_date with
  | None -> ()
  | Some d ->
      Printf.ksprintf (oc opts) " ";
      print_date opts d);
  print_if_no_empty opts base "#p" (Gwdb.get_pevent_place e);
  if opts.source = None then
    print_if_no_empty opts base "#s" (Gwdb.get_pevent_src e);
  Printf.ksprintf (oc opts) "\n";
  print_witnesses opts base gen ~use_per_sel:true
    (Gwdb.get_pevent_witnesses_and_notes e);
  let note =
    if opts.notes then Gwdb.sou base (Gwdb.get_pevent_note e) else ""
  in
  print_multiline opts "note" note

let get_persons_with_pevents m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (Gwdb.get_pevents fath, Gwdb.get_parents fath) with
    | [], _ | _, Some _ -> list
    | _ -> fath :: list
  in
  let list =
    match (Gwdb.get_pevents moth, Gwdb.get_parents moth) with
    | [], _ | _, Some _ -> list
    | _ -> moth :: list
  in
  Array.fold_right
    (fun p list -> if Gwdb.get_pevents p = [] then list else p :: list)
    m.m_chil list

let print_pevents_for_person opts base gen p =
  if not (Gwdb.Marker.get gen.pevents_done (Gwdb.get_iper p)) then (
    Gwdb.Marker.set gen.pevents_done (Gwdb.get_iper p) true;
    let pevents = Gwdb.get_pevents p in
    let surn = s_correct_string (Gwdb.p_surname base p) in
    let fnam = s_correct_string (Gwdb.p_first_name base p) in
    if pevents <> [] && surn <> "?" && fnam <> "?" then (
      Printf.ksprintf (oc opts) "\n";
      Printf.ksprintf (oc opts) "pevt %s %s%s\n" surn fnam
        (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
      List.iter (print_pevent opts base gen) pevents;
      Printf.ksprintf (oc opts) "end pevt\n"))

let rec list_memf f x = function
  | [] -> false
  | a :: l -> f x a || list_memf f x l

let eq_key p1 p2 = Gwdb.get_iper p1 = Gwdb.get_iper p2
let eq_key_fst (p1, _) (p2, _) = Gwdb.get_iper p1 = Gwdb.get_iper p2

let print_pevents opts base gen ml =
  let pl = List.fold_right get_persons_with_pevents ml gen.pevents_pl_p in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl)
      pl []
  in
  List.iter
    (fun p ->
      if gen.per_sel (Gwdb.get_iper p) then
        print_pevents_for_person opts base gen p)
    pl

let print_fevent opts base gen in_comment e =
  let print_sep () =
    if not in_comment then Printf.ksprintf (oc opts) "\n"
    else Printf.ksprintf (oc opts) " "
  in
  (match Gwdb.get_fevent_name e with
  | Def.Efam_Marriage -> Printf.ksprintf (oc opts) "#marr"
  | Def.Efam_NoMarriage -> Printf.ksprintf (oc opts) "#nmar"
  | Def.Efam_NoMention -> Printf.ksprintf (oc opts) "#nmen"
  | Def.Efam_Engage -> Printf.ksprintf (oc opts) "#enga"
  | Def.Efam_Divorce -> Printf.ksprintf (oc opts) "#div"
  | Def.Efam_Separated -> Printf.ksprintf (oc opts) "#sep"
  | Def.Efam_Annulation -> Printf.ksprintf (oc opts) "#anul"
  | Def.Efam_MarriageBann -> Printf.ksprintf (oc opts) "#marb"
  | Def.Efam_MarriageContract -> Printf.ksprintf (oc opts) "#marc"
  | Def.Efam_MarriageLicense -> Printf.ksprintf (oc opts) "#marl"
  | Def.Efam_PACS -> Printf.ksprintf (oc opts) "#pacs"
  | Def.Efam_Residence -> Printf.ksprintf (oc opts) "#resi"
  | Def.Efam_Name n -> Printf.ksprintf (oc opts) "#%s" (correct_string base n));
  let efam_date = Date.od_of_cdate (Gwdb.get_fevent_date e) in
  (match efam_date with
  | None -> ()
  | Some d ->
      Printf.ksprintf (oc opts) " ";
      print_date opts d);
  print_if_no_empty opts base "#p" (Gwdb.get_fevent_place e);
  if opts.source = None then
    print_if_no_empty opts base "#s" (Gwdb.get_fevent_src e);
  print_sep ();
  print_witnesses opts base gen ~use_per_sel:true
    (Gwdb.get_fevent_witnesses_and_notes e);
  let note =
    if opts.notes then Gwdb.sou base (Gwdb.get_fevent_note e) else ""
  in
  let note_lines = String.split_on_char '\n' note in
  if note <> "" then
    List.iter
      (fun line ->
        Printf.ksprintf (oc opts) "note %s" line;
        print_sep ())
      note_lines

let print_comment_for_family opts base gen fam =
  let comm =
    if opts.Gwexport.notes then Gwdb.sou base (Gwdb.get_comment fam) else ""
  in
  (* Si on est en mode old_gw, on mets tous les évènements dans les notes. *)
  (* On supprime les 2 évènements principaux. *)
  let fevents =
    List.filter
      (fun evt ->
        match Gwdb.get_fevent_name evt with
        | Def.Efam_Divorce | Def.Efam_Engage | Def.Efam_Marriage
        | Def.Efam_NoMarriage | Def.Efam_NoMention | Def.Efam_Separated ->
            false
        | Def.Efam_Annulation | Def.Efam_MarriageBann
        | Def.Efam_MarriageContract | Def.Efam_MarriageLicense | Def.Efam_PACS
        | Def.Efam_Residence | Def.Efam_Name _ ->
            true)
      (Gwdb.get_fevents fam)
  in
  let has_evt =
    !old_gw
    && (fevents <> [] || Gwdb.sou base (Gwdb.get_marriage_note fam) <> "")
  in
  if comm <> "" || has_evt then print_multiline opts "comm" comm;
  if !old_gw then (
    if Gwdb.sou base (Gwdb.get_marriage_note fam) <> "" then
      Printf.ksprintf (oc opts) " marriage: %s"
        (no_newlines (Gwdb.sou base (Gwdb.get_marriage_note fam)));
    List.iter
      (fun e ->
        Printf.ksprintf (oc opts) " ";
        print_fevent opts base gen true e)
      fevents;
    Printf.ksprintf (oc opts) "\n")

let print_empty_family gen opts base p =
  let string_quest = Gwdb.insert_string base "?" in
  Printf.ksprintf (oc opts) "fam ? ?.0 + #noment ? ?.0\n";
  Printf.ksprintf (oc opts) "beg\n";
  print_child gen opts base string_quest "" "" p;
  Printf.ksprintf (oc opts) "end\n"

let print_family opts base gen m =
  let fam = m.m_fam in
  let fath, moth = (m.m_fath, m.m_moth) in
  if
    Gwdb.eq_iper (Gwdb.get_iper fath) Gwdb.dummy_iper
    || Gwdb.eq_iper (Gwdb.get_iper moth) Gwdb.dummy_iper
  then ()
  else (
    Printf.ksprintf (oc opts) "fam ";
    print_parent opts base gen m.m_fath;
    Printf.ksprintf (oc opts) " +";
    print_date_option opts (Date.od_of_cdate (Gwdb.get_marriage fam));
    let print_sexes s =
      let c x =
        match Gwdb.get_sex x with
        | Def.Male -> 'm'
        | Def.Female -> 'f'
        | Def.Neuter -> '?'
      in
      Printf.ksprintf (oc opts) " %s %c%c" s (c m.m_fath) (c m.m_moth)
    in
    (match Gwdb.get_relation fam with
    | Def.Married -> ()
    | Def.NotMarried -> Printf.ksprintf (oc opts) " #nm"
    | Def.Engaged -> Printf.ksprintf (oc opts) " #eng"
    | Def.NoSexesCheckNotMarried -> print_sexes "#nsck"
    | Def.NoSexesCheckMarried -> print_sexes "#nsckm"
    | Def.NoMention -> print_sexes "#noment"
    | Def.MarriageBann -> print_sexes "#banns"
    | Def.MarriageContract -> print_sexes "#contract"
    | Def.MarriageLicense -> print_sexes "#license"
    | Def.Pacs -> print_sexes "#pacs"
    | Def.Residence -> print_sexes "#residence");
    print_if_no_empty opts base "#mp" (Gwdb.get_marriage_place fam);
    if opts.source = None then
      print_if_no_empty opts base "#ms" (Gwdb.get_marriage_src fam);
    (match Gwdb.get_divorce fam with
    | Def.NotDivorced -> ()
    | Def.Separated -> Printf.ksprintf (oc opts) " #sep"
    | Def.Divorced d ->
        let d = Date.od_of_cdate d in
        Printf.ksprintf (oc opts) " -";
        print_date_option opts d);
    Printf.ksprintf (oc opts) " ";
    print_parent opts base gen m.m_moth;
    Printf.ksprintf (oc opts) "\n";
    let witnesses =
      Array.map
        (fun ip -> (ip, Def.Witness, Gwdb.empty_string))
        (Gwdb.get_witnesses fam)
    in
    print_witnesses opts base gen ~use_per_sel:true witnesses;
    (match opts.source with
    | None ->
        if Gwdb.sou base (Gwdb.get_fsources fam) <> "" then
          Printf.ksprintf (oc opts) "src %s\n"
            (correct_string base (Gwdb.get_fsources fam))
    | Some "" -> ()
    | Some x -> Printf.ksprintf (oc opts) "src %s\n" (s_correct_string x));
    let csrc =
      match common_children_sources base m.m_chil with
      | Some s ->
          Printf.ksprintf (oc opts) "csrc %s\n" (s_correct_string s);
          s
      | None -> ""
    in
    let cbp =
      match common_children_birth_place base m.m_chil with
      | Some s ->
          Printf.ksprintf (oc opts) "cbp %s\n" (s_correct_string s);
          s
      | None -> ""
    in
    print_comment_for_family opts base gen fam;
    if (not !old_gw) && Gwdb.get_fevents fam <> [] then (
      Printf.ksprintf (oc opts) "fevt\n";
      List.iter (print_fevent opts base gen false) (Gwdb.get_fevents fam);
      Printf.ksprintf (oc opts) "end fevt\n");
    (match Array.length m.m_chil with
    | 0 -> ()
    | _ ->
        let fam_surname = Gwdb.get_surname m.m_fath in
        Printf.ksprintf (oc opts) "beg\n";
        Array.iter
          (fun p ->
            if gen.per_sel (Gwdb.get_iper p) then
              print_child gen opts base fam_surname csrc cbp p)
          m.m_chil;
        Printf.ksprintf (oc opts) "end\n");
    Gwdb.Marker.set gen.fam_done m.m_ifam true;
    let from =
      Printf.sprintf "family \"%s.%d %s\" & \"%s.%d %s\""
        (Gwdb.p_first_name base m.m_fath)
        (get_new_occ m.m_fath)
        (Gwdb.p_surname base m.m_fath)
        (Gwdb.p_first_name base m.m_moth)
        (get_new_occ m.m_moth)
        (Gwdb.p_surname base m.m_moth)
    in
    let s =
      let sl =
        let acc =
          [
            Gwdb.get_comment fam;
            Gwdb.get_marriage_note fam;
            Gwdb.get_marriage_src fam;
          ]
        in
        if opts.source = None then Gwdb.get_fsources fam :: acc else acc
      in
      let sl =
        if not !old_gw then
          let rec loop l accu =
            match l with
            | [] -> accu
            | evt :: l ->
                let acc =
                  Gwdb.get_fevent_note evt
                  ::
                  (if opts.source = None then Gwdb.get_fevent_src evt :: accu
                  else accu)
                in
                loop l acc
          in
          loop (Gwdb.get_fevents fam) sl
        else sl
      in
      String.concat " " (List.map (Gwdb.sou base) sl)
    in
    ignore (add_linked_files gen from s [] : _ list))

let get_persons_with_notes m list =
  let list =
    let fath = m.m_fath in
    match Gwdb.get_parents fath with Some _ -> list | None -> fath :: list
  in
  let list =
    let moth = m.m_moth in
    match Gwdb.get_parents moth with Some _ -> list | None -> moth :: list
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
              Option.fold (String.index_opt s ' ')
                ~some:(fun i ->
                  ( String.sub s 0 i,
                    String.sub s (i + 1) (String.length s - i - 1) )
                  :: list)
                ~none:list
            in
            loop list
        | None ->
            close_in ic;
            list
      in
      loop []
  | None -> []

let print_notes_for_person opts base gen p =
  if not (Gwdb.Marker.get gen.notes_done (Gwdb.get_iper p)) then (
    Gwdb.Marker.set gen.notes_done (Gwdb.get_iper p) true;
    let notes =
      if opts.Gwexport.notes then Gwdb.sou base (Gwdb.get_notes p) else ""
    in
    let surn = s_correct_string (Gwdb.p_surname base p) in
    let fnam = s_correct_string (Gwdb.p_first_name base p) in
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
             match Gwdb.get_pevent_name evt with
             | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
             | Epers_Cremation ->
                 let name =
                   (* TODO use Check.string_of_epers_name instead? *)
                   match Gwdb.get_pevent_name evt with
                   | Epers_Birth -> "birth"
                   | Epers_Baptism -> "baptism"
                   | Epers_Death -> "death"
                   | Epers_Burial -> "burial"
                   | Epers_Cremation -> "cremation"
                   | Epers_Accomplishment | Epers_Acquisition | Epers_Adhesion
                   | Epers_BaptismLDS | Epers_BarMitzvah | Epers_BatMitzvah
                   | Epers_Benediction | Epers_ChangeName | Epers_Circumcision
                   | Epers_Confirmation | Epers_ConfirmationLDS
                   | Epers_Decoration | Epers_DemobilisationMilitaire
                   | Epers_Diploma | Epers_Distinction | Epers_Dotation
                   | Epers_DotationLDS | Epers_Education | Epers_Election
                   | Epers_Emigration | Epers_Excommunication
                   | Epers_FamilyLinkLDS | Epers_FirstCommunion | Epers_Funeral
                   | Epers_Graduate | Epers_Hospitalisation | Epers_Illness
                   | Epers_Immigration | Epers_ListePassenger
                   | Epers_MilitaryDistinction | Epers_MilitaryPromotion
                   | Epers_MilitaryService | Epers_MobilisationMilitaire
                   | Epers_Naturalisation | Epers_Occupation | Epers_Ordination
                   | Epers_Property | Epers_Recensement | Epers_Residence
                   | Epers_Retired | Epers_ScellentChildLDS
                   | Epers_ScellentParentLDS | Epers_ScellentSpouseLDS
                   | Epers_VenteBien | Epers_Will | Epers_Adoption
                   | Epers_Name _ ->
                       ""
                 in
                 let notes =
                   if opts.Gwexport.notes then
                     Gwdb.sou base (Gwdb.get_pevent_note evt)
                   else ""
                 in
                 if notes <> "" then
                   Printf.ksprintf (oc opts) "%s: %s\n" name notes;
                 print_witnesses opts base gen ~use_per_sel:false
                   (Gwdb.get_pevent_witnesses_and_notes evt);
                 loop events
             | Epers_Accomplishment | Epers_Acquisition | Epers_Adhesion
             | Epers_BaptismLDS | Epers_BarMitzvah | Epers_BatMitzvah
             | Epers_Benediction | Epers_ChangeName | Epers_Circumcision
             | Epers_Confirmation | Epers_ConfirmationLDS | Epers_Decoration
             | Epers_DemobilisationMilitaire | Epers_Diploma | Epers_Distinction
             | Epers_Dotation | Epers_DotationLDS | Epers_Education
             | Epers_Election | Epers_Emigration | Epers_Excommunication
             | Epers_FamilyLinkLDS | Epers_FirstCommunion | Epers_Funeral
             | Epers_Graduate | Epers_Hospitalisation | Epers_Illness
             | Epers_Immigration | Epers_ListePassenger
             | Epers_MilitaryDistinction | Epers_MilitaryPromotion
             | Epers_MilitaryService | Epers_MobilisationMilitaire
             | Epers_Naturalisation | Epers_Occupation | Epers_Ordination
             | Epers_Property | Epers_Recensement | Epers_Residence
             | Epers_Retired | Epers_ScellentChildLDS | Epers_ScellentParentLDS
             | Epers_ScellentSpouseLDS | Epers_VenteBien | Epers_Will
             | Epers_Adoption | Epers_Name _ ->
                 print_pevent opts base gen evt;
                 loop events)
       in
       loop (Gwdb.get_pevents p));
      Printf.ksprintf (oc opts) "end notes\n");
    let from =
      Printf.sprintf "person \"%s.%d %s\"" (Gwdb.p_first_name base p)
        (get_new_occ p) (Gwdb.p_surname base p)
    in
    let s =
      let aux g = Gwdb.sou base (g p) in
      let sl =
        if opts.Gwexport.notes then
          [
            aux Gwdb.get_notes;
            aux Gwdb.get_birth_note;
            aux Gwdb.get_baptism_note;
            aux Gwdb.get_death_note;
            aux Gwdb.get_burial_note;
          ]
        else []
      in
      let sl =
        match opts.source with
        | Some "" -> sl
        | Some src -> src :: sl
        | None ->
            aux Gwdb.get_birth_src :: aux Gwdb.get_baptism_src
            :: aux Gwdb.get_death_src :: aux Gwdb.get_burial_src
            :: aux Gwdb.get_psources :: sl
      in
      if (not !old_gw) && opts.source = None then
        List.fold_left
          (fun acc e ->
            let acc =
              if opts.Gwexport.notes then
                Gwdb.sou base (Gwdb.get_pevent_note e) :: acc
              else acc
            in
            let acc =
              if opts.source = None then
                Gwdb.sou base (Gwdb.get_pevent_src e) :: acc
              else acc
            in
            acc)
          sl (Gwdb.get_pevents p)
      else sl
    in
    let s = String.concat " " s in
    ignore (add_linked_files gen from s [] : _ list))

let print_notes opts base gen ml =
  let pl = List.fold_right get_persons_with_notes ml gen.notes_pl_p in
  let pl =
    List.fold_right
      (fun p pl -> if list_memf eq_key p pl then pl else p :: pl)
      pl []
  in
  List.iter
    (fun p ->
      if gen.per_sel (Gwdb.get_iper p) then
        print_notes_for_person opts base gen p)
    pl

let is_isolated p =
  match Gwdb.get_parents p with
  | Some _ -> false
  | None -> Array.length (Gwdb.get_family p) = 0

let is_definition_for_parent p =
  match Gwdb.get_parents p with Some _ -> false | None -> true

let get_isolated_related base m list =
  let concat_isolated p_relation ip list =
    let p = Gwdb.poi base ip in
    if List.mem_assq p list then list
    else if is_isolated p then
      match Gwdb.get_rparents p with
      | { Def.r_fath = Some x } :: _ when x = Gwdb.get_iper p_relation ->
          list @ [ (p, true) ]
      | { Def.r_fath = None; Def.r_moth = Some x } :: _
        when x = Gwdb.get_iper p_relation ->
          list @ [ (p, true) ]
      | _ -> list
    else list
  in
  let list =
    if is_definition_for_parent m.m_fath then
      List.fold_right (concat_isolated m.m_fath)
        (Gwdb.get_related m.m_fath)
        list
    else list
  in
  let list =
    if is_definition_for_parent m.m_moth then
      List.fold_right (concat_isolated m.m_moth)
        (Gwdb.get_related m.m_moth)
        list
    else list
  in
  Array.fold_right
    (fun p list ->
      List.fold_right (concat_isolated p) (Gwdb.get_related p) list)
    m.m_chil list

let get_persons_with_relations base m list =
  let fath = m.m_fath in
  let moth = m.m_moth in
  let list =
    match (Gwdb.get_rparents fath, Gwdb.get_parents fath) with
    | [], _ | _, Some _ -> list
    | _ -> (fath, false) :: list
  in
  let list =
    match (Gwdb.get_rparents moth, Gwdb.get_parents moth) with
    | [], _ | _, Some _ -> list
    | _ -> (moth, false) :: list
  in
  let list =
    Array.fold_right
      (fun ip list ->
        let p = Gwdb.poi base ip in
        match (Gwdb.get_rparents p, Gwdb.get_parents p) with
        | [], _ | _, Some _ -> list
        | { Def.r_fath = Some x } :: _, _ when x <> Gwdb.get_iper m.m_fath ->
            list
        | _ -> (p, false) :: list)
      (Gwdb.get_witnesses m.m_fam)
      list
  in
  Array.fold_right
    (fun p list ->
      match Gwdb.get_rparents p with [] -> list | _ :: _ -> (p, false) :: list)
    m.m_chil list

let print_relation_parent gen opts base defined_p p =
  Printf.ksprintf (oc opts) "%s %s%s"
    (correct_string base (Gwdb.get_surname p))
    (correct_string base (Gwdb.get_first_name p))
    (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
  if
    Array.length (Gwdb.get_family p) = 0
    && Gwdb.get_parents p = None
    && not (Gwdb.Marker.get gen.mark (Gwdb.get_iper p))
  then (
    Gwdb.Marker.set gen.mark (Gwdb.get_iper p) true;
    if has_infos opts base p then print_infos gen opts base false "" "" p
    else Printf.ksprintf (oc opts) " 0";
    defined_p := p :: !defined_p)

let print_relation_for_person opts base gen def_p r =
  let fath =
    match r.Def.r_fath with
    | Some ip ->
        if gen.per_sel ip then
          let p = Gwdb.poi base ip in
          if
            Gwdb.sou base (Gwdb.get_first_name p) = "?"
            || Gwdb.sou base (Gwdb.get_surname p) = "?"
          then None
          else Some p
        else None
    | None -> None
  in
  let moth =
    match r.Def.r_moth with
    | Some ip ->
        if gen.per_sel ip then
          let p = Gwdb.poi base ip in
          if
            Gwdb.sou base (Gwdb.get_first_name p) = "?"
            || Gwdb.sou base (Gwdb.get_surname p) = "?"
          then None
          else Some p
        else None
    | None -> None
  in
  let err_same_sex =
    match (fath, moth) with
    | Some fath, Some moth -> Gwdb.get_sex fath = Gwdb.get_sex moth
    | _ -> false
  in
  let print_err_one_relation p =
    Printf.ksprintf (oc opts) "- ";
    (match r.Def.r_type with
    | Def.Adoption -> Printf.ksprintf (oc opts) "adop"
    | Def.Recognition -> Printf.ksprintf (oc opts) "reco"
    | Def.CandidateParent -> Printf.ksprintf (oc opts) "cand"
    | Def.GodParent -> Printf.ksprintf (oc opts) "godp"
    | Def.FosterParent -> Printf.ksprintf (oc opts) "fost");
    if Gwdb.get_sex p = Def.Male then Printf.ksprintf (oc opts) " fath"
    else Printf.ksprintf (oc opts) " moth";
    Printf.ksprintf (oc opts) ": ";
    print_relation_parent gen opts base def_p p;
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
        (match r.Def.r_type with
        | Def.Adoption -> Printf.ksprintf (oc opts) "adop"
        | Def.Recognition -> Printf.ksprintf (oc opts) "reco"
        | Def.CandidateParent -> Printf.ksprintf (oc opts) "cand"
        | Def.GodParent -> Printf.ksprintf (oc opts) "godp"
        | Def.FosterParent -> Printf.ksprintf (oc opts) "fost");
        (match (fath, moth) with
        | Some fath, None ->
            if Gwdb.get_sex fath = Def.Male then
              Printf.ksprintf (oc opts) " fath"
            else Printf.ksprintf (oc opts) " moth"
        | None, Some moth ->
            if Gwdb.get_sex moth = Def.Female then
              Printf.ksprintf (oc opts) " moth"
            else Printf.ksprintf (oc opts) " fath"
        | _ -> ());
        Printf.ksprintf (oc opts) ": ";
        (match (fath, moth) with
        | Some fath, None -> print_relation_parent gen opts base def_p fath
        | None, Some moth -> print_relation_parent gen opts base def_p moth
        | Some fath, Some moth ->
            if Gwdb.get_sex fath = Male && Gwdb.get_sex moth = Female then (
              print_relation_parent gen opts base def_p fath;
              Printf.ksprintf (oc opts) " + ";
              print_relation_parent gen opts base def_p moth)
            else (
              print_relation_parent gen opts base def_p moth;
              Printf.ksprintf (oc opts) " + ";
              print_relation_parent gen opts base def_p fath)
        | _ -> ());
        Printf.ksprintf (oc opts) "\n")

let print_relations_for_person mark opts base gen def_p is_definition p =
  let surn = correct_string base (Gwdb.get_surname p) in
  let fnam = correct_string base (Gwdb.get_first_name p) in
  let exist_relation =
    List.exists
      (fun r ->
        match (r.Def.r_fath, r.Def.r_moth) with
        | Some ip1, Some ip2 -> gen.per_sel ip1 && gen.per_sel ip2
        | Some ip1, _ -> gen.per_sel ip1
        | _, Some ip2 -> gen.per_sel ip2
        | _ -> false)
      (Gwdb.get_rparents p)
  in
  if
    surn <> "?" && fnam <> "?" && exist_relation
    && not (Gwdb.Marker.get gen.mark_rel (Gwdb.get_iper p))
  then (
    Gwdb.Marker.set gen.mark_rel (Gwdb.get_iper p) true;
    Printf.ksprintf (oc opts) "\n";
    Printf.ksprintf (oc opts) "rel %s %s%s" surn fnam
      (if get_new_occ p = 0 then "" else "." ^ string_of_int (get_new_occ p));
    if is_definition then (
      let already_marked = Gwdb.Marker.get gen.mark (Gwdb.get_iper p) in
      Gwdb.Marker.set gen.mark (Gwdb.get_iper p) true;
      def_p := p :: !def_p;
      if has_infos opts base p then print_infos mark opts base false "" "" p
      else Printf.ksprintf (oc opts) " 0";
      if not already_marked then
        match Gwdb.get_sex p with
        | Male -> Printf.ksprintf (oc opts) " #h"
        | Female -> Printf.ksprintf (oc opts) " #f"
        | Neuter -> ());
    Printf.ksprintf (oc opts) "\n";
    Printf.ksprintf (oc opts) "beg\n";
    List.iter
      (print_relation_for_person opts base gen def_p)
      (Gwdb.get_rparents p);
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
        if Gwdb.get_rparents p <> [] && gen.per_sel (Gwdb.get_iper p) then (
          print_relations_for_person gen opts base gen def_p if_def p;
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
        if Gwdb.get_rparents p <> [] && gen.per_sel (Gwdb.get_iper p) then (
          print_relations_for_person gen opts base gen def_p if_def p;
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

let connected_families base fam_sel ifam cpl =
  let rec loop ifaml ipl_scanned = function
    | ip :: ipl ->
        if List.mem ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let u = Gwdb.poi base ip in
          let ifaml1 = Array.to_list (Gwdb.get_family u) in
          let ifaml1 = List.filter fam_sel ifaml1 in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                let cpl = Gwdb.foi base ifam in
                Gwdb.get_father cpl :: Gwdb.get_mother cpl :: ipl)
              ifaml1 ipl
          in
          loop ifaml (ip :: ipl_scanned) ipl
    | [] -> ifaml
  in
  loop [ ifam ] [] [ Gwdb.get_father cpl ]

type separate = ToSeparate | NotScanned | BeingScanned | Scanned

let rec find_ancestors base surn p list =
  match Gwdb.get_parents p with
  | Some ifam ->
      let cpl = Gwdb.foi base ifam in
      let fath = Gwdb.poi base (Gwdb.get_father cpl) in
      let moth = Gwdb.poi base (Gwdb.get_mother cpl) in
      if
        (not (Gwdb.eq_istr (Gwdb.get_surname fath) surn))
        && not (Gwdb.eq_istr (Gwdb.get_surname moth) surn)
      then p :: list
      else
        let list =
          if Gwdb.eq_istr (Gwdb.get_surname fath) surn then
            find_ancestors base surn fath list
          else list
        in
        if Gwdb.eq_istr (Gwdb.get_surname moth) surn then
          find_ancestors base surn moth list
        else list
  | None -> p :: list

let mark_branch base mark surn p =
  let rec loop top p =
    for i = 0 to Array.length (Gwdb.get_family p) - 1 do
      let ifam = (Gwdb.get_family p).(i) in
      if Gwdb.Marker.get mark ifam = NotScanned then
        let ifaml =
          connected_families base (fun _ -> true) ifam (Gwdb.foi base ifam)
        in
        let children =
          List.fold_left
            (fun list ifam ->
              let desc = Gwdb.foi base ifam in
              Array.fold_left
                (fun list ip -> Gwdb.poi base ip :: list)
                list (Gwdb.get_children desc))
            [] ifaml
        in
        if
          top
          || List.exists
               (fun p -> Gwdb.eq_istr (Gwdb.get_surname p) surn)
               children
        then (
          List.iter (fun ifam -> Gwdb.Marker.set mark ifam ToSeparate) ifaml;
          List.iter (loop false) children)
    done
  in
  loop true p

let mark_someone base mark s =
  match Gutil.person_ht_find_all base s with
  | [ ip ] ->
      let p = Gwdb.poi base ip in
      let plist = find_ancestors base (Gwdb.get_surname p) p [] in
      List.iter (mark_branch base mark (Gwdb.get_surname p)) plist
  | [] ->
      Printf.eprintf "Error: \"%s\" is not found\n" s;
      flush stderr;
      exit 2
  | _ :: _ :: _ ->
      Printf.eprintf "Error: several answers for \"%s\"\n" s;
      flush stderr;
      exit 2

let scan_connex_component base test_action len ifam =
  let rec loop len ifam =
    let fam = Gwdb.foi base ifam in
    let fath = Gwdb.poi base (Gwdb.get_father fam) in
    let moth = Gwdb.poi base (Gwdb.get_mother fam) in
    let len =
      Array.fold_left
        (fun len ifam1 ->
          if ifam1 = ifam then len else test_action loop len ifam1)
        len (Gwdb.get_family fath)
    in
    let len =
      Array.fold_left
        (fun len ifam1 ->
          if ifam1 = ifam then len else test_action loop len ifam1)
        len (Gwdb.get_family moth)
    in
    let len =
      match Gwdb.get_parents fath with
      | Some ifam -> test_action loop len ifam
      | None -> len
    in
    let len =
      match Gwdb.get_parents moth with
      | Some ifam -> test_action loop len ifam
      | None -> len
    in
    let children = Gwdb.get_children fam in
    Array.fold_left
      (fun len ip ->
        Array.fold_left (test_action loop) len
          (Gwdb.get_family (Gwdb.poi base ip)))
      len children
  in
  loop len ifam

let mark_one_connex_component base mark ifam =
  let origin_file = Gwdb.sou base (Gwdb.get_origin_file (Gwdb.foi base ifam)) in
  let test_action loop len ifam =
    if
      Gwdb.Marker.get mark ifam = NotScanned
      && Gwdb.sou base (Gwdb.get_origin_file (Gwdb.foi base ifam)) = origin_file
    then (
      Gwdb.Marker.set mark ifam BeingScanned;
      loop (len + 1) ifam)
    else len
  in
  let _ = test_action (fun _ _ -> 1) 0 ifam in
  let len = 1 + scan_connex_component base test_action 0 ifam in
  let set_mark x =
    let test_action loop () ifam =
      if Gwdb.Marker.get mark ifam = BeingScanned then (
        Gwdb.Marker.set mark ifam x;
        loop () ifam)
    in
    test_action (fun _ _ -> ()) () ifam;
    scan_connex_component base test_action () ifam
  in
  if len <= !sep_limit && (!only_file = "" || !only_file = origin_file) then
    set_mark ToSeparate
  else (
    Printf.eprintf "%s: group of size %d not included\n" origin_file len;
    let cpl = Gwdb.foi base ifam in
    Printf.eprintf "    %s + %s\n"
      (Gutil.designation base (Gwdb.poi base (Gwdb.get_father cpl)))
      (Gutil.designation base (Gwdb.poi base (Gwdb.get_mother cpl)));
    flush stderr;
    set_mark Scanned)

let mark_connex_components base mark ifam =
  let test_action _loop _len ifam =
    if Gwdb.Marker.get mark ifam = NotScanned then
      mark_one_connex_component base mark ifam
  in
  scan_connex_component base test_action () ifam

let add_small_connex_components base mark =
  Gwdb.Collection.iter
    (fun i ->
      if Gwdb.Marker.get mark i = ToSeparate then
        mark_connex_components base mark i)
    (Gwdb.ifams base)

let separate base =
  match List.rev !separate_list with
  | [] -> fun _ -> false
  | list ->
      let ifams = Gwdb.ifams base in
      let mark = Gwdb.ifam_marker (Gwdb.ifams base) NotScanned in
      List.iter (mark_someone base mark) list;
      add_small_connex_components base mark;
      let len =
        Gwdb.Collection.fold
          (fun acc i ->
            if Gwdb.Marker.get mark i = ToSeparate then acc + 1 else acc)
          0 ifams
      in
      Printf.eprintf "*** extracted %d families\n" len;
      flush stderr;
      fun ifam -> Gwdb.Marker.get mark ifam = ToSeparate

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
  let to_separate = separate base in
  let out_oc_first = ref true in
  let _ofile, oc, close = opts.Gwexport.oc in
  let origin_file fname =
    if out_dir = "" then (oc, out_oc_first, close)
    else if fname = "" then (oc, out_oc_first, close)
    else
      match Hashtbl.find_opt src_oc_ht fname with
      | Some x -> x
      | None ->
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
    let pevents_done = Gwdb.iper_marker (Gwdb.ipers base) false in
    let infos_done = Gwdb.iper_marker (Gwdb.ipers base) false in
    let notes_done = Gwdb.iper_marker (Gwdb.ipers base) false in
    let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
    let mark_rel = Gwdb.iper_marker (Gwdb.ipers base) false in
    let fam_done = Gwdb.ifam_marker (Gwdb.ifams base) false in
    {
      notes_done;
      infos_done;
      pevents_done;
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
  let nb_fam = Gwdb.nb_of_families base in
  if !Mutil.verbose then ProgrBar.start ();
  Gwdb.Collection.iteri
    (fun i ifam ->
      if !Mutil.verbose then ProgrBar.run i nb_fam;
      if not (Gwdb.Marker.get gen.fam_done ifam) then
        let fam = Gwdb.foi base ifam in
        let ifaml = connected_families base gen.fam_sel ifam fam in
        let oc, first, _close =
          if to_separate ifam then (oc, out_oc_first, close)
          else origin_file (Gwdb.sou base (Gwdb.get_origin_file fam))
        in
        let ml =
          List.fold_right
            (fun ifam ml ->
              let fam = Gwdb.foi base ifam in
              let m =
                {
                  m_ifam = ifam;
                  m_fam = fam;
                  m_fath = Gwdb.poi base (Gwdb.get_father fam);
                  m_moth = Gwdb.poi base (Gwdb.get_mother fam);
                  m_chil =
                    Array.map
                      (fun ip -> Gwdb.poi base ip)
                      (Gwdb.get_children fam);
                }
              in
              if empty_family base m then (
                Gwdb.Marker.set gen.fam_done m.m_ifam true;
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
    (Gwdb.ifams ~select:gen.fam_sel base);
  (* Ajout des personnes isolée à l'export. On leur ajoute des    *)
  (* parents pour pouvoir utiliser les autres fonctions normales. *)
  (* Export que si c'est toute la base.                           *)
  if isolated && opts.asc = None && opts.desc = None && opts.ascdesc = None then
    Gwdb.Collection.iter
      (fun i ->
        if
          (not @@ Gwdb.Marker.get gen.mark i)
          && (not @@ Gwdb.Marker.get gen.mark_rel i)
          && per_sel i
        then
          let p = Gwdb.poi base i in
          match Gwdb.get_parents p with
          | Some _ -> ()
          | None ->
              if
                bogus_person base p
                && not
                     (Gwdb.get_birth p <> Date.cdate_None
                     || Gwdb.get_baptism p <> Date.cdate_None
                     || Gwdb.get_first_names_aliases p <> []
                     || Gwdb.get_surnames_aliases p <> []
                     || Gwdb.sou base (Gwdb.get_public_name p) <> ""
                     || Gwdb.get_qualifiers p <> []
                     || Gwdb.get_aliases p <> []
                     || Gwdb.get_titles p <> []
                     || Gwdb.sou base (Gwdb.get_occupation p) <> ""
                     || Gwdb.sou base (Gwdb.get_birth_place p) <> ""
                     || Gwdb.sou base (Gwdb.get_birth_src p) <> ""
                     || Gwdb.sou base (Gwdb.get_baptism_place p) <> ""
                     || Gwdb.sou base (Gwdb.get_baptism_src p) <> ""
                     || Gwdb.sou base (Gwdb.get_death_place p) <> ""
                     || Gwdb.sou base (Gwdb.get_death_src p) <> ""
                     || Gwdb.sou base (Gwdb.get_burial_place p) <> ""
                     || Gwdb.sou base (Gwdb.get_burial_src p) <> ""
                     || Gwdb.sou base (Gwdb.get_notes p) <> ""
                     || Gwdb.sou base (Gwdb.get_psources p) <> ""
                     || Gwdb.get_rparents p <> []
                     || Gwdb.get_related p <> [])
              then ()
              else
                let oc, _first, _ =
                  origin_file (Gwdb.base_notes_origin_file base)
                in
                Printf.ksprintf oc "\n";
                print_empty_family gen opts base p;
                print_notes_for_person opts base gen p;
                Gwdb.Marker.set gen.mark i true;
                print_isolated_relations opts base gen p)
      (Gwdb.ipers base);
  if !Mutil.verbose then ProgrBar.finish ();
  if opts.base_notes then (
    let s = Gwdb.base_notes_read base "" in
    let oc, first, _ = origin_file (Gwdb.base_notes_origin_file base) in
    if s <> "" then (
      if not !first then Printf.ksprintf oc "\n";
      first := false;
      Printf.ksprintf oc "notes-db\n";
      rs_printf opts s;
      Printf.ksprintf oc "\nend notes-db\n";
      ignore (add_linked_files gen "database notes" s [] : _ list));
    (try
       let files =
         Sys.readdir (Filename.concat in_dir (Gwdb.base_wiznotes_dir base))
       in
       Array.sort compare files;
       for i = 0 to Array.length files - 1 do
         let file = files.(i) in
         if Filename.check_suffix file ".txt" then
           let wfile =
             List.fold_left Filename.concat in_dir
               [ Gwdb.base_wiznotes_dir base; file ]
           in
           let s = Mutil.read_file_content wfile in
           ignore
             (add_linked_files gen ("wizard \"" ^ file ^ "\"") s [] : _ list)
       done
     with Sys_error _ -> ());
    let rec loop = function
      | [] -> ()
      | (f, _) :: files ->
          let fn =
            match Geneweb.NotesLinks.check_file_name f with
            | Some (dl, f) -> List.fold_right Filename.concat dl f
            | None -> (* TODO error here? *) "bad"
          in
          let s = Gwdb.base_notes_read base fn in
          let files =
            add_linked_files gen
              (Printf.sprintf "extended page \"%s\"" f)
              s files
          in
          loop files
    in
    loop gen.ext_files;
    List.iter
      (fun (f, r) ->
        let fn =
          match Geneweb.NotesLinks.check_file_name f with
          | Some (dl, f) -> List.fold_right Filename.concat dl f
          | None -> (* TODO error here? *) "bad"
        in
        let s = String.trim (Gwdb.base_notes_read base fn) in
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
    let close () =
      flush_all ();
      close ();
      Hashtbl.iter (fun _ (_, _, close) -> close ()) src_oc_ht
    in
    try
      let files =
        Sys.readdir (Filename.concat in_dir (Gwdb.base_wiznotes_dir base))
      in
      Array.sort compare files;
      for i = 0 to Array.length files - 1 do
        let file = files.(i) in
        if Filename.check_suffix file ".txt" then (
          let wizid = Filename.chop_suffix file ".txt" in
          let wfile =
            List.fold_left Filename.concat in_dir
              [ Gwdb.base_wiznotes_dir base; file ]
          in
          let content = Mutil.read_file_content wfile in
          let s = String.trim content in
          Printf.ksprintf oc "\nwizard-note %s\n" wizid;
          rs_printf opts s;
          Printf.ksprintf oc "\nend wizard-note\n")
      done;
      close ()
    with Sys_error _ -> close ())

let gwu_simple ~export_isolated opts =
  match opts.Gwexport.base with
  | None -> assert false
  | Some (ifile, base) ->
      let select = Gwexport.select opts in
      let in_dir =
        if Filename.check_suffix ifile ".gwb" then ifile else ifile ^ ".gwb"
      in
      let src_oc_ht = Hashtbl.create 1009 in
      let () = Gwdb.load_ascends_array base in
      let () = Gwdb.load_strings_array base in
      (if not opts.Gwexport.mem then
       let () = Gwdb.load_couples_array base in
       let () = Gwdb.load_unions_array base in
       let () = Gwdb.load_descends_array base in
       ());
      let _ofile, oc, close = opts.Gwexport.oc in
      if not !raw_output then oc "encoding: utf-8\n";
      if !old_gw then oc "\n" else oc "gwplus\n\n";
      prepare_free_occ base;
      gwu opts export_isolated base in_dir !out_dir src_oc_ht select;
      Hashtbl.iter (fun _ (_, _, close) -> close ()) src_oc_ht;
      close ()
