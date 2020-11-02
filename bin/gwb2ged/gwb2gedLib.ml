(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Def
open Gwdb

let int_of_iper =
  let ht = Hashtbl.create 0 in
  fun i ->
    try Hashtbl.find ht i
    with Not_found ->
      let x = Hashtbl.length ht in
      Hashtbl.add ht i x ;
      x

let int_of_ifam =
  let ht = Hashtbl.create 0 in
  fun i ->
    try Hashtbl.find ht i
    with Not_found ->
      let x = Hashtbl.length ht in
      Hashtbl.add ht i x ;
      x

let month_txt =
  [| "JAN"; "FEB"; "MAR"; "APR"; "MAY"; "JUN"; "JUL"; "AUG"; "SEP"; "OCT";
     "NOV"; "DEC" |]

let french_txt =
  [| "VEND"; "BRUM"; "FRIM"; "NIVO"; "PLUV"; "VENT"; "GERM"; "FLOR"; "PRAI";
     "MESS"; "THER"; "FRUC"; "COMP" |]

let hebrew_txt =
  [| "TSH"; "CSH"; "KSL"; "TVT"; "SHV"; "ADR"; "ADS"; "NSN"; "IYR"; "SVN";
     "TMZ"; "AAV"; "ELL" |]

let ged_month cal m =
  match cal with
    Dgregorian | Djulian ->
      if m >= 1 && m <= Array.length month_txt then month_txt.(m-1)
      else failwith "ged_month"
  | Dfrench ->
      if m >= 1 && m <= Array.length french_txt then french_txt.(m-1)
      else failwith "ged_month"
  | Dhebrew ->
      if m >= 1 && m <= Array.length hebrew_txt then hebrew_txt.(m-1)
      else failwith "ged_month"

let encode opts s =
  match opts.Gwexport.charset with
  | Ansel -> Ansel.of_iso_8859_1 @@ Mutil.iso_8859_1_of_utf_8 s
  | Ascii | Ansi -> Mutil.iso_8859_1_of_utf_8 s
  | Utf8 -> s

let max_len = 78

let br = "<br>"
let find_br s ini_i =
  let ini = "<br" in
  let rec loop i j =
    if i = String.length ini then
      let rec loop2 j =
        if j = String.length s then br
        else if s.[j] = '>' then String.sub s ini_i (j - ini_i + 1)
        else loop2 (j + 1)
      in
      loop2 j
    else if j = String.length s then br
    else if String.unsafe_get ini i = String.unsafe_get s j then
      loop (i + 1) (j + 1)
    else br
  in
  loop 0 ini_i

(** [display_note_aux oc tagn s len i] outputs text [s] with CONT/CONC
    tag. GEDCOM lines are limited to 255 characters. However, the
    CONCatenation or CONTinuation tags can be used to expand a field
    beyond this limit. Lines are cut and align with [max_len]
    characters for easy display/printing.
    @see <https://www.familysearch.org/developers/docs/gedcom/> GEDCOM
    STANDARD 5.5, Appendix A CONC and CONT tag
    @param oc specifies output channel
    @param tagn specifies the current gedcom tag level (0, 1, ...)
    @param s specifies text to print to the output channel (already
    encode with gedcom charset)
    @param len specifies the number of characters (char or wide char)
    already printed
    @param i specifies the last char index (index to s -- one byte
    char) *)
let rec display_note_aux opts oc tagn s len i =
  (* FIXME: Rewrite this so we can get rid of this custom [nbc] *)
  let nbc c =
    if Char.code c < 0b10000000 then 1
    else if Char.code c < 0b11000000 then -1
    else if Char.code c < 0b11100000 then 2
    else if Char.code c < 0b11110000 then 3
    else if Char.code c < 0b11111000 then 4
    else if Char.code c < 0b11111100 then 5
    else if Char.code c < 0b11111110 then 6
    else -1
  in
  let j = ref i in
  (* read wide char (case charset UTF-8) or char (other charset) in s string*)
  let rec output_onechar () =
    if !j = String.length s then decr j
    (* non wide char / UTF-8 char *)
    else if opts.Gwexport.charset <> Utf8 then output_char oc s.[i]
    (* 1 to 4 bytes UTF-8 wide char *)
    else if i = !j || nbc s.[!j] = -1 then begin
      output_char oc s.[!j];
      incr j;
      output_onechar ()
    end
    else decr j
  in
  if !j = String.length s then Printf.fprintf oc "\n"
  else
    (* \n, <br>, <br \> : cut text for CONTinuate with new gedcom line *)
    let br = find_br s i in
    if i <= String.length s - String.length br &&
       String.lowercase_ascii (String.sub s i (String.length br)) = br
    then
      begin
        Printf.fprintf oc "\n%d CONT " (succ tagn);
        let i = i + String.length br in
        let i = if i < String.length s && s.[i] = '\n' then i + 1 else i in
        display_note_aux opts oc tagn s
          (String.length (string_of_int (succ tagn) ^ " CONT ")) i
      end
    else if s.[i] = '\n' then
      begin
        Printf.fprintf oc "\n%d CONT " (succ tagn);
        let i = if i < String.length s then i + 1 else i in
        display_note_aux opts oc tagn s
          (String.length (string_of_int (succ tagn) ^ " CONT ")) i
      end
    (* cut text at max length for CONCat with next gedcom line *)
    else if len = max_len then
      begin Printf.fprintf oc "\n%d CONC " (succ tagn);
        display_note_aux opts
          oc tagn s (String.length ((string_of_int (succ tagn)) ^ " CONC ")) i
      end
    (* continue same gedcom line *)
    else
      begin
        output_onechar ();
        display_note_aux opts oc tagn s (len + 1) (!j + 1)
      end

let display_note opts oc tagn s =
  let tag = Printf.sprintf "%d NOTE " tagn in
  Printf.fprintf oc "%s" tag;
  display_note_aux opts oc tagn (encode opts s) (String.length tag) 0

let ged_header opts base oc ifile ofile =
  Printf.fprintf oc "0 HEAD\n";
  Printf.fprintf oc "1 SOUR GeneWeb\n";
  Printf.fprintf oc "2 VERS %s\n" Version.txt;
  Printf.fprintf oc "2 NAME %s\n" (Filename.basename Sys.argv.(0));
  Printf.fprintf oc "2 CORP INRIA\n";
  Printf.fprintf oc "3 ADDR http://www.geneweb.org\n";
  Printf.fprintf oc "2 DATA %s\n"
    (let fname = Filename.basename ifile in
     if Filename.check_suffix fname ".gwb" then fname else fname ^ ".gwb");
  begin try
    let tm = Unix.localtime (Unix.time ()) in
    let mon = ged_month Dgregorian (tm.Unix.tm_mon + 1) in
    Printf.fprintf oc "1 DATE %02d %s %d\n" tm.Unix.tm_mday mon
      (1900 + tm.Unix.tm_year);
    Printf.fprintf oc "2 TIME %02d:%02d:%02d\n" tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec
  with _ -> ()
  end;
  if ofile <> "" then Printf.fprintf oc "1 FILE %s\n" (Filename.basename ofile);
  Printf.fprintf oc "1 GEDC\n";
  begin match opts.Gwexport.charset with
    Ansel | Ansi | Ascii -> Printf.fprintf oc "2 VERS 5.5\n"
  | Utf8 -> Printf.fprintf oc "2 VERS 5.5.1\n"
  end;
  Printf.fprintf oc "2 FORM LINEAGE-LINKED\n";
  begin match opts.Gwexport.charset with
    Ansel -> Printf.fprintf oc "1 CHAR ANSEL\n"
  | Ansi -> Printf.fprintf oc "1 CHAR ANSI\n"
  | Ascii -> Printf.fprintf oc "1 CHAR ASCII\n"
  | Utf8 -> Printf.fprintf oc "1 CHAR UTF-8\n"
  end;
  if not opts.Gwexport.no_notes then
    match base_notes_read base "" with
    | "" -> () | s -> display_note opts oc 1 s

let sub_string_index s t =
  let rec loop i j =
    if j = String.length t then Some (i - j)
    else if i = String.length s then None
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

let ged_1st_name base p =
  let fn = sou base (get_first_name p) in
  match get_first_names_aliases p with
    n :: _ ->
      let fna = sou base n in
      begin match sub_string_index fna fn with
        Some i ->
          let j = i + String.length fn in
          String.sub fna 0 i ^ "\"" ^ fn ^ "\"" ^
          String.sub fna j (String.length fna - j)
      | None -> fn
      end
  | [] -> fn

let string_of_list =
  let rec loop r =
    function
      s :: l -> if r = "" then loop s l else loop (r ^ "," ^ s) l
    | [] -> r
  in
  loop ""

let ged_index oc per =
  Printf.fprintf oc "1 _GWID %s\n" (Gwdb.string_of_iper (get_iper per))

let ged_name opts base oc per =
  Printf.fprintf oc "1 NAME %s /%s/\n"
    (encode opts (Mutil.nominative (ged_1st_name base per)))
    (encode opts (Mutil.nominative (sou base (get_surname per))));
  let n = sou base (get_public_name per) in
  if n <> "" then Printf.fprintf oc "2 GIVN %s\n" (encode opts n);
  begin match get_qualifiers per with
    nn :: _ -> Printf.fprintf oc "2 NICK %s\n" (encode opts (sou base nn))
  | [] -> ()
  end;
  begin match get_surnames_aliases per with
    [] -> ()
  | list ->
      let list = List.map (fun n -> encode opts (sou base n)) list in
      Printf.fprintf oc "2 SURN %s\n" (string_of_list list)
  end;
  List.iter (fun s -> Printf.fprintf oc "1 NAME %s\n" (encode opts (sou base s)))
    (get_aliases per)

let ged_sex oc per =
  match get_sex per with
    Male -> Printf.fprintf oc "1 SEX M\n"
  | Female -> Printf.fprintf oc "1 SEX F\n"
  | Neuter -> ()

let ged_calendar oc =
  function
    Dgregorian -> ()
  | Djulian -> Printf.fprintf oc "@#DJULIAN@ "
  | Dfrench -> Printf.fprintf oc "@#DFRENCH R@ "
  | Dhebrew -> Printf.fprintf oc "@#DHEBREW@ "

let ged_date_dmy oc dt cal =
  begin match dt.prec with
    Sure -> ()
  | About -> Printf.fprintf oc "ABT "
  | Maybe -> Printf.fprintf oc "EST "
  | Before -> Printf.fprintf oc "BEF "
  | After -> Printf.fprintf oc "AFT "
  | OrYear _ -> Printf.fprintf oc "BET "
  | YearInt _ -> Printf.fprintf oc "BET "
  end;
  ged_calendar oc cal;
  if dt.day <> 0 then Printf.fprintf oc "%02d " dt.day;
  if dt.month <> 0 then Printf.fprintf oc "%s " (ged_month cal dt.month);
  Printf.fprintf oc "%d" dt.year;
  match dt.prec with
    OrYear dmy2 ->
      Printf.fprintf oc " AND ";
      ged_calendar oc cal;
      if dmy2.day2 <> 0 then Printf.fprintf oc "%02d " dmy2.day2;
      if dmy2.month2 <> 0 then Printf.fprintf oc "%s " (ged_month cal dmy2.month2);
      Printf.fprintf oc "%d" dmy2.year2
  | YearInt dmy2 ->
      Printf.fprintf oc " AND ";
      ged_calendar oc cal;
      if dmy2.day2 <> 0 then Printf.fprintf oc "%02d " dmy2.day2;
      if dmy2.month2 <> 0 then Printf.fprintf oc "%s " (ged_month cal dmy2.month2);
      Printf.fprintf oc "%d" dmy2.year2
  | _ -> ()

let ged_date oc =
  function
    Dgreg (d, Dgregorian) -> ged_date_dmy oc d Dgregorian
  | Dgreg (d, Djulian) ->
      ged_date_dmy oc (Calendar.julian_of_gregorian d) Djulian
  | Dgreg (d, Dfrench) ->
      ged_date_dmy oc (Calendar.french_of_gregorian d) Dfrench
  | Dgreg (d, Dhebrew) ->
      ged_date_dmy oc (Calendar.hebrew_of_gregorian d) Dhebrew
  | Dtext t -> Printf.fprintf oc "(%s)" t

let ged_ev_detail opts oc n typ d pl note src =
  begin match typ, d, pl, note, src with
    | "", None, "", "", "" -> Printf.fprintf oc " Y"
    | _ -> ()
  end;
  Printf.fprintf oc "\n";
  if typ = "" then () else Printf.fprintf oc "%d TYPE %s\n" n typ;
  begin match d with
      Some d -> Printf.fprintf oc "%d DATE " n; ged_date oc d; Printf.fprintf oc "\n"
    | None -> ()
  end;
  if pl <> "" then Printf.fprintf oc "%d PLAC %s\n" n (encode opts pl);
  if note <> "" then display_note opts oc n note;
  if opts.Gwexport.source = None && src <> ""
  then Printf.fprintf oc "%d SOUR %s\n" n (encode opts src)

let ged_tag_pevent base evt =
  match evt.epers_name with
    Epers_Birth -> "BIRT"
  | Epers_Baptism -> "BAPM"
  | Epers_Death -> "DEAT"
  | Epers_Burial -> "BURI"
  | Epers_Cremation -> "CREM"
  | Epers_Accomplishment -> "Accomplishment"
  | Epers_Acquisition -> "Acquisition"
  | Epers_Adhesion -> "Membership"
  | Epers_BaptismLDS -> "BAPL"
  | Epers_BarMitzvah -> "BARM"
  | Epers_BatMitzvah -> "BASM"
  | Epers_Benediction -> "BLES"
  | Epers_ChangeName -> "Change name"
  | Epers_Circumcision -> "Circumcision"
  | Epers_Confirmation -> "CONF"
  | Epers_ConfirmationLDS -> "CONL"
  | Epers_Decoration -> "Award"
  | Epers_DemobilisationMilitaire -> "Military discharge"
  | Epers_Diploma -> "Degree"
  | Epers_Distinction -> "Distinction"
  | Epers_Dotation -> "ENDL"
  | Epers_DotationLDS -> "DotationLDS"
  | Epers_Education -> "EDUC"
  | Epers_Election -> "Election"
  | Epers_Emigration -> "EMIG"
  | Epers_Excommunication -> "Excommunication"
  | Epers_FamilyLinkLDS -> "Family link LDS"
  | Epers_FirstCommunion -> "FCOM"
  | Epers_Funeral -> "Funeral"
  | Epers_Graduate -> "GRAD"
  | Epers_Hospitalisation -> "Hospitalization"
  | Epers_Illness -> "Illness"
  | Epers_Immigration -> "IMMI"
  | Epers_ListePassenger -> "Passenger list"
  | Epers_MilitaryDistinction -> "Military distinction"
  | Epers_MilitaryPromotion -> "Military promotion"
  | Epers_MilitaryService -> "Military service"
  | Epers_MobilisationMilitaire -> "Military mobilization"
  | Epers_Naturalisation -> "NATU"
  | Epers_Occupation -> "OCCU"
  | Epers_Ordination -> "ORDN"
  | Epers_Property -> "PROP"
  | Epers_Recensement -> "CENS"
  | Epers_Residence -> "RESI"
  | Epers_Retired -> "RETI"
  | Epers_ScellentChildLDS -> "SLGC"
  | Epers_ScellentParentLDS -> "Scellent parent LDS"
  | Epers_ScellentSpouseLDS -> "SLGS"
  | Epers_VenteBien -> "Property sale"
  | Epers_Will -> "WILL"
  | Epers_Name n -> sou base n

let is_primary_pevents =
  function
    Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial |
    Epers_Cremation | Epers_BaptismLDS | Epers_BarMitzvah | Epers_BatMitzvah |
    Epers_Benediction | Epers_Confirmation | Epers_ConfirmationLDS |
    Epers_Dotation | Epers_Education | Epers_Emigration |
    Epers_FirstCommunion | Epers_Graduate | Epers_Immigration |
    Epers_Naturalisation | Epers_Occupation | Epers_Ordination |
    Epers_Property | Epers_Recensement | Epers_Residence | Epers_Retired |
    Epers_ScellentChildLDS | Epers_ScellentSpouseLDS | Epers_Will ->
      true
  | _ -> false

let ged_pevent opts base oc per_sel evt =
  let typ =
    if is_primary_pevents evt.epers_name then
      let tag = ged_tag_pevent base evt in Printf.fprintf oc "1 %s" tag; ""
    else begin Printf.fprintf oc "1 EVEN"; ged_tag_pevent base evt end
  in
  let date = Adef.od_of_cdate evt.epers_date in
  let place = sou base evt.epers_place in
  let note = sou base evt.epers_note in
  let src = sou base evt.epers_src in
  ged_ev_detail opts oc 2 typ date place note src;
  Array.iter
    (fun (ip, wk) ->
       if per_sel ip then
         begin
           Printf.fprintf oc "2 ASSO @I%d@\n" (int_of_iper ip + 1);
           Printf.fprintf oc "3 TYPE INDI\n";
           match wk with
           | Witness -> Printf.fprintf oc "3 RELA witness\n"
           | Witness_GodParent -> Printf.fprintf oc "3 RELA GODP\n"
           | Witness_Officer -> Printf.fprintf oc "3 RELA officer\n"
         end)
    evt.epers_witnesses

let adop_fam_list = ref []
let adop_fam_cnt = ref 0

let ged_adoption base per_sel oc per r =
  let sel =
    match r.r_fath, r.r_moth with
      Some ip1, Some ip2 -> per_sel ip1 && per_sel ip2
    | Some ip1, _ -> per_sel ip1
    | _, Some ip2 -> per_sel ip2
    | _ -> true
  in
  if sel then
    begin
      Printf.fprintf oc "1 ADOP Y\n";
      adop_fam_list :=
        (r.r_fath, r.r_moth, get_iper per) :: !adop_fam_list;
      incr adop_fam_cnt;
      Printf.fprintf oc "2 FAMC @F%d@\n" (nb_of_families base + !adop_fam_cnt);
      Printf.fprintf oc "3 ADOP ";
      begin match r.r_fath, r.r_moth with
        Some _, None -> Printf.fprintf oc "HUSB"
      | None, Some _ -> Printf.fprintf oc "WIFE"
      | Some _, Some _ -> Printf.fprintf oc "BOTH"
      | _ -> ()
      end;
      Printf.fprintf oc "\n"
    end

let ged_fam_adop oc i (fath, moth, _) =
  Printf.fprintf oc "0 @F%d@ FAM\n" i;
  begin match fath with
    Some i -> Printf.fprintf oc "1 HUSB @I%d@\n" (int_of_iper i + 1)
  | _ -> ()
  end;
  match moth with
    Some i -> Printf.fprintf oc "1 WIFE @I%d@\n" (int_of_iper i + 1)
  | _ -> ()

let ged_ind_ev_str opts base oc per per_sel =
  List.iter (ged_pevent opts base oc per_sel) (get_pevents per)

let ged_title opts base oc per tit =
  Printf.fprintf oc "1 TITL ";
  Printf.fprintf oc "%s" (encode opts (sou base tit.t_ident));
  begin match sou base tit.t_place with
    "" -> ()
  | pl -> Printf.fprintf oc ", %s" (encode opts pl)
  end;
  if tit.t_nth <> 0 then Printf.fprintf oc ", %d" tit.t_nth;
  Printf.fprintf oc "\n";
  begin match
    Adef.od_of_cdate tit.t_date_start, Adef.od_of_cdate tit.t_date_end
  with
    None, None -> ()
  | Some sd, None ->
      Printf.fprintf oc "2 DATE FROM "; ged_date oc sd; Printf.fprintf oc "\n"
  | None, Some sd -> Printf.fprintf oc "2 DATE TO "; ged_date oc sd; Printf.fprintf oc "\n"
  | Some sd1, Some sd2 ->
      Printf.fprintf oc "2 DATE FROM ";
      ged_date oc sd1;
      Printf.fprintf oc " TO ";
      ged_date oc sd2;
      Printf.fprintf oc "\n"
  end;
  match tit.t_name with
    Tmain ->
      Printf.fprintf oc "2 NOTE %s\n" (encode opts (sou base (get_public_name per)))
  | Tname n -> Printf.fprintf oc "2 NOTE %s\n" (encode opts (sou base n))
  | Tnone -> ()

let ged_ind_attr_str opts base oc per =
  begin match sou base (get_occupation per) with
    "" -> ()
  | occu -> Printf.fprintf oc "1 OCCU %s\n" (encode opts occu)
  end;
  List.iter (ged_title opts base oc per) (get_titles per)

let ged_famc fam_sel oc asc =
  match get_parents asc with
    Some ifam ->
      if fam_sel ifam then
        Printf.fprintf oc "1 FAMC @F%d@\n" (int_of_ifam ifam + 1)
  | None -> ()

let ged_fams fam_sel oc ifam =
  if fam_sel ifam then Printf.fprintf oc "1 FAMS @F%d@\n" (int_of_ifam ifam + 1)

let ged_godparent per_sel oc godp =
  function
    Some ip ->
      if per_sel ip then
        begin
          Printf.fprintf oc "1 ASSO @I%d@\n" (int_of_iper ip + 1);
          Printf.fprintf oc "2 TYPE INDI\n";
          Printf.fprintf oc "2 RELA %s\n" godp
        end
  | None -> ()

let ged_witness fam_sel oc ifam =
  if fam_sel ifam then
    begin
      Printf.fprintf oc "1 ASSO @F%d@\n" (int_of_ifam ifam + 1);
      Printf.fprintf oc "2 TYPE FAM\n";
      Printf.fprintf oc "2 RELA witness\n"
    end

let ged_asso base (per_sel, fam_sel) oc per =
  List.iter
    (fun r ->
       if r.r_type = GodParent then
         begin
           ged_godparent per_sel oc "GODF" r.r_fath;
           ged_godparent per_sel oc "GODM" r.r_moth
         end)
    (get_rparents per);
  List.iter
    (fun ic ->
       let c = poi base ic in
       if get_sex c = Male then
         List.iter
           (fun ifam ->
              let fam = foi base ifam in
              if Array.mem (get_iper per) (get_witnesses fam) then
                ged_witness fam_sel oc ifam)
           (Array.to_list (get_family c)))
    (get_related per)

let ged_psource opts base oc per =
  match opts.Gwexport.source with
  | Some s -> Printf.fprintf oc "1 SOUR %s\n" (encode opts s)
  | None ->
    match sou base (get_psources per) with
    | "" -> ()
    | s -> Printf.fprintf oc "1 SOUR %s\n" (encode opts s)

let has_image_file opts base p =
  let s = Util.default_image_name base p in
  let f = Filename.concat opts.Gwexport.img_base_path s in
  if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None

let ged_multimedia_link opts base oc per =
  match sou base (get_image per) with
  |"" ->
    if not opts.Gwexport.no_picture && opts.Gwexport.picture_path then
      begin match has_image_file opts base per with
        | Some s -> Printf.fprintf oc "1 OBJE\n"; Printf.fprintf oc "2 FILE %s\n" s
        | None -> ()
      end
  | s ->
    if not opts.Gwexport.no_picture then
      begin Printf.fprintf oc "1 OBJE\n"; Printf.fprintf oc "2 FILE %s\n" s end

let ged_note opts base oc per =
  match sou base (get_notes per) with
    "" -> ()
  | s -> display_note opts oc 1 s

let ged_marriage opts base oc fam =
  match
    Adef.od_of_cdate (get_marriage fam), sou base (get_marriage_place fam),
    get_relation fam
  with d, pl, _ ->
    Printf.fprintf oc "1 %s" (if get_relation fam = Engaged then "ENGA" else "MARR");
    let typ =
      if get_relation fam = NoSexesCheckNotMarried ||
         get_relation fam = NoSexesCheckMarried
      then
        "gay"
      else ""
    in
    let note = sou base (get_marriage_note fam) in
    let src = sou base (get_marriage_src fam) in
    ged_ev_detail opts oc 2 typ d pl note src;
    if get_relation fam = NotMarried then Printf.fprintf oc "2 PLAC unmarried\n"

let ged_divorce opts oc fam =
  match get_divorce fam with
  | NotDivorced -> ()
  | Separated -> ()
  | Divorced cd ->
    let d = Adef.od_of_cdate cd in
    Printf.fprintf oc "1 DIV" ;
    ged_ev_detail opts oc 2 "" d "" "" ""

let ged_tag_fevent base evt =
  match evt.efam_name with
  | Efam_Marriage -> "MARR"
  | Efam_NoMarriage -> "unmarried"
  | Efam_NoMention -> "nomen"
  | Efam_Engage -> "ENGA"
  | Efam_Divorce -> "DIV"
  | Efam_Separated -> "SEP"
  | Efam_Annulation -> "ANUL"
  | Efam_MarriageBann -> "MARB"
  | Efam_MarriageContract -> "MARC"
  | Efam_MarriageLicense -> "MARL"
  | Efam_PACS -> "pacs"
  | Efam_Residence -> "residence"
  | Efam_Name n -> sou base n

let is_primary_fevents =
  function
  | Efam_Marriage | Efam_Engage | Efam_Divorce | Efam_Separated
  | Efam_Annulation | Efam_MarriageBann | Efam_MarriageContract
  | Efam_MarriageLicense ->
    true
  | _ -> false

let ged_fevent opts base oc per_sel evt =
  let typ =
    if is_primary_fevents evt.efam_name
    then
      let tag = ged_tag_fevent base evt in
      Printf.fprintf oc "1 %s" tag ;
      ""
    else begin
      Printf.fprintf oc "1 EVEN" ;
      ged_tag_fevent base evt
    end
  in
  let date = Adef.od_of_cdate evt.efam_date in
  let place = sou base evt.efam_place in
  let note = sou base evt.efam_note in
  let src = sou base evt.efam_src in
  ged_ev_detail opts oc 2 typ date place note src;
  Array.iter begin fun (ip, wk) ->
    if per_sel ip then
      begin
        Printf.fprintf oc "2 ASSO @I%d@\n" (int_of_iper ip + 1);
        Printf.fprintf oc "3 TYPE INDI\n";
        match wk with
        | Witness -> Printf.fprintf oc "3 RELA witness\n"
        | Witness_GodParent -> Printf.fprintf oc "3 RELA GODP\n"
        | Witness_Officer -> Printf.fprintf oc "3 RELA officer\n"
      end
  end evt.efam_witnesses

let ged_child per_sel oc chil =
  if per_sel chil then Printf.fprintf oc "1 CHIL @I%d@\n" (int_of_iper chil + 1)

let ged_fsource opts base oc fam =
  match opts.Gwexport.source with
  | Some s -> Printf.fprintf oc "1 SOUR %s\n" (encode opts s)
  | None ->
    match sou base (get_fsources fam) with
    | "" -> ()
    | s -> Printf.fprintf oc "1 SOUR %s\n" (encode opts s)

let ged_comment opts base oc fam =
  match sou base (get_comment fam) with
  | "" -> ()
  | s -> display_note opts oc 1 s

let has_personal_infos base per =
  get_parents per <> None
  || sou base (get_first_name per) <> "?"
  || sou base (get_surname per) <> "?"
  || get_birth per <> Adef.cdate_None
  || sou base (get_birth_place per) <> ""
  || get_death per <> NotDead && get_death per <> DontKnowIfDead
  || sou base (get_occupation per) <> ""
  || get_titles per <> []

let ged_ind_record with_indexes opts base (per_sel, fam_sel as sel) oc i =
  let per = poi base i in
  if has_personal_infos base per then begin
    Printf.fprintf oc "0 @I%d@ INDI\n" (int_of_iper i + 1);
    ged_name opts base oc per;
    if with_indexes then ged_index oc per;
    ged_sex oc per;
    ged_ind_ev_str opts base oc per per_sel;
    ged_ind_attr_str opts base oc per;
    ged_famc fam_sel oc per;
    Array.iter (ged_fams fam_sel oc) (get_family per);
    ged_asso base sel oc per;
    ged_psource opts base oc per;
    ged_multimedia_link opts base oc per;
    ged_note opts base oc per
  end

let ged_fam_record opts base (per_sel, fam_sel) oc ifam =
  let fam = foi base ifam in
  Printf.fprintf oc "0 @F%d@ FAM\n" (int_of_ifam ifam + 1);
  List.iter (ged_fevent opts base oc per_sel) (get_fevents fam);
  if per_sel (get_father fam)
  && has_personal_infos base (poi base (get_father fam))
  then Printf.fprintf oc "1 HUSB @I%d@\n" (int_of_iper (get_father fam) + 1);
  if per_sel (get_mother fam)
  && has_personal_infos base (poi base (get_mother fam))
  then Printf.fprintf oc "1 WIFE @I%d@\n" (int_of_iper (get_mother fam) + 1);
  Array.iter (ged_child per_sel oc) (get_children fam);
  ged_fsource opts base oc fam ;
  ged_comment opts base oc fam

let gwb2ged with_indexes opts (per_sel, fam_sel as sel) =
  match opts.Gwexport.base with
  | Some (ifile, base) ->
    let ofile, oc = opts.Gwexport.oc in
    if not opts.Gwexport.mem then
      begin
        load_ascends_array base;
        load_unions_array base;
        load_couples_array base;
        load_descends_array base
      end;
    ged_header opts base oc ifile ofile;
    flush oc;
    Gwdb.Collection.iter begin fun i ->
      if per_sel i then ged_ind_record with_indexes opts base sel oc i
    end (Gwdb.ipers base) ;
    Gwdb.Collection.iter begin fun i ->
      if fam_sel i then ged_fam_record opts base sel oc i
    end (Gwdb.ifams base) ;
    let _ =
      List.fold_right (fun adop i -> ged_fam_adop oc i adop; i + 1)
        !adop_fam_list (nb_of_families base + 1)
    in
    Printf.fprintf oc "0 TRLR\n";
    flush oc ;
    if ofile <> "" then close_out oc
  | None -> assert false
