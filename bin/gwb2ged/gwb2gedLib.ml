(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

let int_of_iper =
  let ht = Hashtbl.create 0 in
  fun i ->
    try Hashtbl.find ht i
    with Not_found ->
      let x = Hashtbl.length ht in
      Hashtbl.add ht i x;
      x

let int_of_ifam =
  let ht = Hashtbl.create 0 in
  fun i ->
    try Hashtbl.find ht i
    with Not_found ->
      let x = Hashtbl.length ht in
      Hashtbl.add ht i x;
      x

let month_txt =
  [|
    "JAN";
    "FEB";
    "MAR";
    "APR";
    "MAY";
    "JUN";
    "JUL";
    "AUG";
    "SEP";
    "OCT";
    "NOV";
    "DEC";
  |]

let french_txt =
  [|
    "VEND";
    "BRUM";
    "FRIM";
    "NIVO";
    "PLUV";
    "VENT";
    "GERM";
    "FLOR";
    "PRAI";
    "MESS";
    "THER";
    "FRUC";
    "COMP";
  |]

let hebrew_txt =
  [|
    "TSH";
    "CSH";
    "KSL";
    "TVT";
    "SHV";
    "ADR";
    "ADS";
    "NSN";
    "IYR";
    "SVN";
    "TMZ";
    "AAV";
    "ELL";
  |]

let ged_month cal m =
  match cal with
  | Dgregorian | Djulian ->
      if m >= 1 && m <= Array.length month_txt then month_txt.(m - 1)
      else failwith "ged_month"
  | Dfrench ->
      if m >= 1 && m <= Array.length french_txt then french_txt.(m - 1)
      else failwith "ged_month"
  | Dhebrew ->
      if m >= 1 && m <= Array.length hebrew_txt then hebrew_txt.(m - 1)
      else failwith "ged_month"

let encode opts s =
  match opts.Gwexport.charset with
  | Gwexport.Ansel -> Ansel.of_iso_8859_1 @@ Mutil.iso_8859_1_of_utf_8 s
  | Gwexport.Ascii | Gwexport.Ansi -> Mutil.iso_8859_1_of_utf_8 s
  | Gwexport.Utf8 -> s

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

let oc opts = match opts.Gwexport.oc with _, oc, _ -> oc

(** [display_note_aux opts tagn s len i] outputs text [s] with CONT/CONC tag.
    GEDCOM lines are limited to 255 characters. However, the CONCatenation or
    CONTinuation tags can be used to expand a field beyond this limit. Lines are
    cut and align with [max_len] characters for easy display/printing.
    @see <https://www.familysearch.org/developers/docs/gedcom/>
      GEDCOM STANDARD 5.5, Appendix A CONC and CONT tag
    @param opts carries output channel
    @param tagn specifies the current gedcom tag level (0, 1, ...)
    @param s
      specifies text to print to the output channel (already encode with gedcom
      charset)
    @param len
      specifies the number of characters (char or wide char) already printed
    @param i specifies the last char index (index to s -- one byte char) *)

let rec display_note_aux opts tagn s len i =
  let j = ref i in
  (* read wide char (case charset UTF-8) or char (other charset) in s string*)
  if !j = String.length s then Printf.ksprintf (oc opts) "\n"
  else
    (* \n, <br>, <br \> : cut text for CONTinuate with new gedcom line *)
    let br = find_br s i in
    if
      i <= String.length s - String.length br
      && String.lowercase_ascii (String.sub s i (String.length br)) = br
    then (
      Printf.ksprintf (oc opts) "\n%d CONT " (succ tagn);
      let i = i + String.length br in
      let i = if i < String.length s && s.[i] = '\n' then i + 1 else i in
      display_note_aux opts tagn s
        (String.length (string_of_int (succ tagn) ^ " CONT "))
        i)
    else if s.[i] = '\n' then (
      Printf.ksprintf (oc opts) "\n%d CONT " (succ tagn);
      let i = if i < String.length s then i + 1 else i in
      display_note_aux opts tagn s
        (String.length (string_of_int (succ tagn) ^ " CONT "))
        i)
    else if
      (* cut text at max length for CONCat with next gedcom line *)
      len = max_len
    then (
      Printf.ksprintf (oc opts) "\n%d CONC " (succ tagn);
      display_note_aux opts tagn s
        (String.length (string_of_int (succ tagn) ^ " CONC "))
        i)
    else (* continue same gedcom line *)
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
      (* FIXME: avoid this buffer *)
      let b = Buffer.create 4 in
      let rec output_onechar () =
        if !j = String.length s then decr j (* non wide char / UTF-8 char *)
        else if opts.Gwexport.charset <> Gwexport.Utf8 then
          Buffer.add_char b s.[i] (* 1 to 4 bytes UTF-8 wide char *)
        else if i = !j || nbc s.[!j] = -1 then (
          Buffer.add_char b s.[!j];
          incr j;
          output_onechar ())
        else decr j
      in
      output_onechar ();
      (oc opts) (Buffer.contents b);
      display_note_aux opts tagn s (len + 1) (!j + 1)

let display_note opts tagn s =
  let tag = Printf.sprintf "%d NOTE " tagn in
  Printf.ksprintf (oc opts) "%s" tag;
  display_note_aux opts tagn (encode opts s) (String.length tag) 0

let ged_header opts base ifile ofile =
  Printf.ksprintf (oc opts) "0 HEAD\n";
  Printf.ksprintf (oc opts) "1 SOUR GeneWeb\n";
  Printf.ksprintf (oc opts) "2 VERS %s\n" Version.ver;
  Printf.ksprintf (oc opts) "2 NAME %s\n" (Filename.basename Sys.argv.(0));
  Printf.ksprintf (oc opts) "2 CORP INRIA\n";
  Printf.ksprintf (oc opts) "3 ADDR http://www.geneweb.org\n";
  Printf.ksprintf (oc opts) "2 DATA %s\n"
    (let fname = Filename.basename ifile in
     if Filename.check_suffix fname ".gwb" then fname else fname ^ ".gwb");
  (try
     let tm = Unix.localtime (Unix.time ()) in
     let mon = ged_month Dgregorian (tm.Unix.tm_mon + 1) in
     Printf.ksprintf (oc opts) "1 DATE %02d %s %d\n" tm.Unix.tm_mday mon
       (1900 + tm.Unix.tm_year);
     Printf.ksprintf (oc opts) "2 TIME %02d:%02d:%02d\n" tm.Unix.tm_hour
       tm.Unix.tm_min tm.Unix.tm_sec
   with _ -> ());
  if ofile <> "" then
    Printf.ksprintf (oc opts) "1 FILE %s\n" (Filename.basename ofile);
  Printf.ksprintf (oc opts) "1 GEDC\n";
  (match opts.Gwexport.charset with
  | Gwexport.Ansel | Gwexport.Ansi | Gwexport.Ascii ->
      Printf.ksprintf (oc opts) "2 VERS 5.5\n"
  | Gwexport.Utf8 -> Printf.ksprintf (oc opts) "2 VERS 5.5.1\n");
  Printf.ksprintf (oc opts) "2 FORM LINEAGE-LINKED\n";
  (match opts.Gwexport.charset with
  | Gwexport.Ansel -> Printf.ksprintf (oc opts) "1 CHAR ANSEL\n"
  | Gwexport.Ansi -> Printf.ksprintf (oc opts) "1 CHAR ANSI\n"
  | Gwexport.Ascii -> Printf.ksprintf (oc opts) "1 CHAR ASCII\n"
  | Gwexport.Utf8 -> Printf.ksprintf (oc opts) "1 CHAR UTF-8\n");
  if opts.Gwexport.no_notes = `none then
    match Driver.base_notes_read base "" with
    | "" -> ()
    | s -> display_note opts 1 s

let sub_string_index s t =
  let rec loop i j =
    if j = String.length t then Some (i - j)
    else if i = String.length s then None
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

let ged_1st_name base p =
  let fn = Driver.sou base (Driver.get_first_name p) in
  match Driver.get_first_names_aliases p with
  | n :: _ -> (
      let fna = Driver.sou base n in
      match sub_string_index fna fn with
      | Some i ->
          let j = i + String.length fn in
          String.sub fna 0 i ^ "\"" ^ fn ^ "\""
          ^ String.sub fna j (String.length fna - j)
      | None -> fn)
  | [] -> fn

let string_of_list =
  let rec loop r = function
    | s :: l -> if r = "" then loop s l else loop (r ^ "," ^ s) l
    | [] -> r
  in
  loop ""

let ged_index opts per =
  Printf.ksprintf (oc opts) "1 _GWID %s\n"
    (Driver.Iper.to_string (Driver.get_iper per))

let ged_name opts base per =
  Printf.ksprintf (oc opts) "1 NAME %s /%s/\n"
    (encode opts (Mutil.nominative (ged_1st_name base per)))
    (encode opts (Mutil.nominative (Driver.sou base (Driver.get_surname per))));
  let n = Driver.sou base (Driver.get_public_name per) in
  if n <> "" then Printf.ksprintf (oc opts) "2 GIVN %s\n" (encode opts n);
  (match Driver.get_qualifiers per with
  | nn :: _ ->
      Printf.ksprintf (oc opts) "2 NICK %s\n" (encode opts (Driver.sou base nn))
  | [] -> ());
  (match Driver.get_surnames_aliases per with
  | [] -> ()
  | list ->
      let list = List.map (fun n -> encode opts (Driver.sou base n)) list in
      Printf.ksprintf (oc opts) "2 SURN %s\n" (string_of_list list));
  List.iter
    (fun s ->
      Printf.ksprintf (oc opts) "1 NAME %s\n" (encode opts (Driver.sou base s)))
    (Driver.get_aliases per)

let ged_sex opts per =
  match Driver.get_sex per with
  | Male -> Printf.ksprintf (oc opts) "1 SEX M\n"
  | Female -> Printf.ksprintf (oc opts) "1 SEX F\n"
  | Neuter -> ()

let ged_calendar opts = function
  | Dgregorian -> ()
  | Djulian -> Printf.ksprintf (oc opts) "@#DJULIAN@ "
  | Dfrench -> Printf.ksprintf (oc opts) "@#DFRENCH R@ "
  | Dhebrew -> Printf.ksprintf (oc opts) "@#DHEBREW@ "

let ged_date_dmy opts dt cal =
  (match dt.prec with
  | Sure -> ()
  | About -> Printf.ksprintf (oc opts) "ABT "
  | Maybe -> Printf.ksprintf (oc opts) "EST "
  | Before -> Printf.ksprintf (oc opts) "BEF "
  | After -> Printf.ksprintf (oc opts) "AFT "
  | OrYear _ -> Printf.ksprintf (oc opts) "BET "
  | YearInt _ -> Printf.ksprintf (oc opts) "BET ");
  ged_calendar opts cal;
  if dt.day <> 0 then Printf.ksprintf (oc opts) "%02d " dt.day;
  if dt.month <> 0 then Printf.ksprintf (oc opts) "%s " (ged_month cal dt.month);
  if dt.year >= 0 then Printf.ksprintf (oc opts) "%d" dt.year
  else Printf.ksprintf (oc opts) "%d BCE" (-dt.year);
  match dt.prec with
  | OrYear dmy2 ->
      Printf.ksprintf (oc opts) " AND ";
      ged_calendar opts cal;
      if dmy2.day2 <> 0 then Printf.ksprintf (oc opts) "%02d " dmy2.day2;
      if dmy2.month2 <> 0 then
        Printf.ksprintf (oc opts) "%s " (ged_month cal dmy2.month2);
      Printf.ksprintf (oc opts) "%d" dmy2.year2
  | YearInt dmy2 ->
      Printf.ksprintf (oc opts) " AND ";
      ged_calendar opts cal;
      if dmy2.day2 <> 0 then Printf.ksprintf (oc opts) "%02d " dmy2.day2;
      if dmy2.month2 <> 0 then
        Printf.ksprintf (oc opts) "%s " (ged_month cal dmy2.month2);
      Printf.ksprintf (oc opts) "%d" dmy2.year2
  | _ -> ()

let ged_date opts = function
  | Dgreg (d, Dgregorian) -> ged_date_dmy opts d Dgregorian
  | Dgreg (d, Djulian) ->
      ged_date_dmy opts (Calendar.julian_of_gregorian d) Djulian
  | Dgreg (d, Dfrench) ->
      ged_date_dmy opts (Calendar.french_of_gregorian d) Dfrench
  | Dgreg (d, Dhebrew) ->
      ged_date_dmy opts (Calendar.hebrew_of_gregorian d) Dhebrew
  | Dtext t -> Printf.ksprintf (oc opts) "(%s)" t

let print_sour opts n s = Printf.ksprintf (oc opts) "%d SOUR %s\n" n s

let ged_ev_detail opts n typ d pl note src =
  (match (typ, d, pl, note, src) with
  | "", None, "", "", "" -> Printf.ksprintf (oc opts) " Y"
  | _ -> ());
  Printf.ksprintf (oc opts) "\n";
  if typ = "" then () else Printf.ksprintf (oc opts) "%d TYPE %s\n" n typ;
  (match d with
  | Some d ->
      Printf.ksprintf (oc opts) "%d DATE " n;
      ged_date opts d;
      Printf.ksprintf (oc opts) "\n"
  | None -> ());
  if pl <> "" then Printf.ksprintf (oc opts) "%d PLAC %s\n" n (encode opts pl);
  if opts.Gwexport.no_notes <> `nnn && note <> "" then display_note opts n note;
  if opts.Gwexport.source = None && src <> "" then
    print_sour opts n (encode opts src)

let ged_tag_pevent base evt =
  match evt.epers_name with
  | Epers_Birth -> "BIRT"
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
  | Epers_Name n -> Driver.sou base n

let is_primary_pevents = function
  | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial | Epers_Cremation
  | Epers_BaptismLDS | Epers_BarMitzvah | Epers_BatMitzvah | Epers_Benediction
  | Epers_Confirmation | Epers_ConfirmationLDS | Epers_Dotation
  | Epers_Education | Epers_Emigration | Epers_FirstCommunion | Epers_Graduate
  | Epers_Immigration | Epers_Naturalisation | Epers_Occupation
  | Epers_Ordination | Epers_Property | Epers_Recensement | Epers_Residence
  | Epers_Retired | Epers_ScellentChildLDS | Epers_ScellentSpouseLDS
  | Epers_Will ->
      true
  | _ -> false

let relation_format_of_witness_kind :
    witness_kind -> ('a, unit, string, unit) format4 = function
  | Witness -> "3 RELA Witness"
  | Witness_GodParent -> "3 RELA GODP"
  | Witness_CivilOfficer -> "3 RELA Civil officer"
  | Witness_ReligiousOfficer -> "3 RELA Religious officer"
  | Witness_Informant -> "3 RELA Informant"
  | Witness_Attending -> "3 RELA Attending"
  | Witness_Mentioned -> "3 RELA Mentioned"
  | Witness_Other -> "3 RELA Other"

let oc' opts s = Printf.ksprintf (oc opts) (s ^^ "\n")
let oc_witness_kind opts wk = oc' opts (relation_format_of_witness_kind wk)

let ged_pevent opts base per_sel evt =
  let typ =
    if is_primary_pevents evt.epers_name then (
      let tag = ged_tag_pevent base evt in
      Printf.ksprintf (oc opts) "1 %s" tag;
      "")
    else (
      Printf.ksprintf (oc opts) "1 EVEN";
      ged_tag_pevent base evt)
  in
  let date = Date.od_of_cdate evt.epers_date in
  let place = Driver.sou base evt.epers_place in
  let note = Driver.sou base evt.epers_note in
  let src = Driver.sou base evt.epers_src in
  ged_ev_detail opts 2 typ date place note src;
  Array.iter
    (fun (ip, wk) ->
      if per_sel ip then (
        Printf.ksprintf (oc opts) "2 ASSO @I%d@\n" (int_of_iper ip + 1);
        Printf.ksprintf (oc opts) "3 TYPE INDI\n";
        oc_witness_kind opts wk))
    evt.epers_witnesses

let adop_fam_list = ref []

let ged_fam_adop opts i (fath, moth, _) =
  Printf.ksprintf (oc opts) "0 @F%d@ FAM\n" i;
  (match fath with
  | Some i -> Printf.ksprintf (oc opts) "1 HUSB @I%d@\n" (int_of_iper i + 1)
  | _ -> ());
  match moth with
  | Some i -> Printf.ksprintf (oc opts) "1 WIFE @I%d@\n" (int_of_iper i + 1)
  | _ -> ()

let ged_ind_ev_str opts base per per_sel =
  List.iter (ged_pevent opts base per_sel) (Driver.get_pevents per)

let ged_title opts base per tit =
  Printf.ksprintf (oc opts) "1 TITL ";
  Printf.ksprintf (oc opts) "%s" (encode opts (Driver.sou base tit.t_ident));
  (match Driver.sou base tit.t_place with
  | "" -> ()
  | pl -> Printf.ksprintf (oc opts) ", %s" (encode opts pl));
  if tit.t_nth <> 0 then Printf.ksprintf (oc opts) ", %d" tit.t_nth;
  Printf.ksprintf (oc opts) "\n";
  (match
     (Date.od_of_cdate tit.t_date_start, Date.od_of_cdate tit.t_date_end)
   with
  | None, None -> ()
  | Some sd, None ->
      Printf.ksprintf (oc opts) "2 DATE FROM ";
      ged_date opts sd;
      Printf.ksprintf (oc opts) "\n"
  | None, Some sd ->
      Printf.ksprintf (oc opts) "2 DATE TO ";
      ged_date opts sd;
      Printf.ksprintf (oc opts) "\n"
  | Some sd1, Some sd2 ->
      Printf.ksprintf (oc opts) "2 DATE FROM ";
      ged_date opts sd1;
      Printf.ksprintf (oc opts) " TO ";
      ged_date opts sd2;
      Printf.ksprintf (oc opts) "\n");
  match tit.t_name with
  | Tmain ->
      Printf.ksprintf (oc opts) "2 NOTE %s\n"
        (encode opts (Driver.sou base (Driver.get_public_name per)))
  | Tname n ->
      Printf.ksprintf (oc opts) "2 NOTE %s\n" (encode opts (Driver.sou base n))
  | Tnone -> ()

let ged_ind_attr_str opts base per =
  (match Driver.sou base (Driver.get_occupation per) with
  | "" -> ()
  | occu -> Printf.ksprintf (oc opts) "1 OCCU %s\n" (encode opts occu));
  List.iter (ged_title opts base per) (Driver.get_titles per)

let ged_famc opts fam_sel asc =
  match Driver.get_parents asc with
  | Some ifam ->
      if fam_sel ifam then
        Printf.ksprintf (oc opts) "1 FAMC @F%d@\n" (int_of_ifam ifam + 1)
  | None -> ()

let ged_fams opts fam_sel ifam =
  if fam_sel ifam then
    Printf.ksprintf (oc opts) "1 FAMS @F%d@\n" (int_of_ifam ifam + 1)

let ged_godparent opts per_sel godp = function
  | Some ip ->
      if per_sel ip then (
        Printf.ksprintf (oc opts) "1 ASSO @I%d@\n" (int_of_iper ip + 1);
        Printf.ksprintf (oc opts) "2 TYPE INDI\n";
        Printf.ksprintf (oc opts) "2 RELA %s\n" godp)
  | None -> ()

let ged_witness opts fam_sel ifam =
  if fam_sel ifam then (
    Printf.ksprintf (oc opts) "1 ASSO @F%d@\n" (int_of_ifam ifam + 1);
    Printf.ksprintf (oc opts) "2 TYPE FAM\n";
    Printf.ksprintf (oc opts) "2 RELA witness\n")

let ged_asso opts base (per_sel, fam_sel) per =
  List.iter
    (fun r ->
      if r.r_type = GodParent then (
        ged_godparent opts per_sel "GODF" r.r_fath;
        ged_godparent opts per_sel "GODM" r.r_moth))
    (Driver.get_rparents per);
  List.iter
    (fun ic ->
      let c = Driver.poi base ic in
      if Driver.get_sex c = Male then
        List.iter
          (fun ifam ->
            let fam = Driver.foi base ifam in
            if Array.mem (Driver.get_iper per) (Driver.get_witnesses fam) then
              ged_witness opts fam_sel ifam)
          (Array.to_list (Driver.get_family c)))
    (Driver.get_related per)

let ged_psource opts base per =
  match opts.Gwexport.source with
  | Some "" -> ()
  | Some s -> print_sour opts 1 (encode opts s)
  | None -> (
      match Driver.sou base (Driver.get_psources per) with
      | "" -> ()
      | s -> print_sour opts 1 (encode opts s))

let has_image_file opts base p =
  let s = Image.default_image_filename "portraits" base p in
  let f = Filename.concat opts.Gwexport.img_base_path s in
  if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None

let ged_multimedia_link opts base per =
  match Driver.sou base (Driver.get_image per) with
  | "" -> (
      if (not opts.Gwexport.no_picture) && opts.Gwexport.picture_path then
        match has_image_file opts base per with
        | Some s ->
            Printf.ksprintf (oc opts) "1 OBJE\n";
            Printf.ksprintf (oc opts) "2 FILE %s\n" s
        | None -> ())
  | s ->
      if not opts.Gwexport.no_picture then (
        Printf.ksprintf (oc opts) "1 OBJE\n";
        Printf.ksprintf (oc opts) "2 FILE %s\n" s)

let ged_note opts base per =
  if opts.Gwexport.no_notes <> `nnn then
    match Driver.sou base (Driver.get_notes per) with
    | "" -> ()
    | s -> display_note opts 1 s

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
  | Efam_Name n -> Driver.sou base n

let is_primary_fevents = function
  | Efam_Marriage | Efam_Engage | Efam_Divorce | Efam_Separated
  | Efam_Annulation | Efam_MarriageBann | Efam_MarriageContract
  | Efam_MarriageLicense ->
      true
  | _ -> false

let ged_fevent opts base per_sel evt =
  let typ =
    if is_primary_fevents evt.efam_name then (
      let tag = ged_tag_fevent base evt in
      Printf.ksprintf (oc opts) "1 %s" tag;
      "")
    else (
      Printf.ksprintf (oc opts) "1 EVEN";
      ged_tag_fevent base evt)
  in
  let date = Date.od_of_cdate evt.efam_date in
  let place = Driver.sou base evt.efam_place in
  let note = Driver.sou base evt.efam_note in
  let src = Driver.sou base evt.efam_src in
  ged_ev_detail opts 2 typ date place note src;
  Array.iter
    (fun (ip, wk) ->
      if per_sel ip then (
        Printf.ksprintf (oc opts) "2 ASSO @I%d@\n" (int_of_iper ip + 1);
        Printf.ksprintf (oc opts) "3 TYPE INDI\n";
        oc_witness_kind opts wk))
    evt.efam_witnesses

let ged_child opts per_sel chil =
  if per_sel chil then
    Printf.ksprintf (oc opts) "1 CHIL @I%d@\n" (int_of_iper chil + 1)

let ged_fsource opts base fam =
  match opts.Gwexport.source with
  | Some "" -> ()
  | Some s -> print_sour opts 1 (encode opts s)
  | None -> (
      match Driver.sou base (Driver.get_fsources fam) with
      | "" -> ()
      | s -> print_sour opts 1 (encode opts s))

let ged_comment opts base fam =
  if opts.Gwexport.no_notes <> `nnn then
    match Driver.sou base (Driver.get_comment fam) with
    | "" -> ()
    | s -> display_note opts 1 s

let has_personal_infos base per =
  Driver.get_parents per <> None
  || Driver.sou base (Driver.get_first_name per) <> "?"
  || Driver.sou base (Driver.get_surname per) <> "?"
  || Driver.get_birth per <> Date.cdate_None
  || Driver.sou base (Driver.get_birth_place per) <> ""
  || (Driver.get_death per <> NotDead && Driver.get_death per <> DontKnowIfDead)
  || Driver.sou base (Driver.get_occupation per) <> ""
  || Driver.get_titles per <> []

let ged_ind_record with_indexes opts base ((per_sel, fam_sel) as sel) i =
  let per = Driver.poi base i in
  if has_personal_infos base per then (
    Printf.ksprintf (oc opts) "0 @I%d@ INDI\n" (int_of_iper i + 1);
    ged_name opts base per;
    if with_indexes then ged_index opts per;
    ged_sex opts per;
    ged_ind_ev_str opts base per per_sel;
    ged_ind_attr_str opts base per;
    ged_famc opts fam_sel per;
    Array.iter (ged_fams opts fam_sel) (Driver.get_family per);
    ged_asso opts base sel per;
    ged_psource opts base per;
    ged_multimedia_link opts base per;
    ged_note opts base per)

let ged_fam_record opts base (per_sel, _fam_sel) ifam =
  let fam = Driver.foi base ifam in
  Printf.ksprintf (oc opts) "0 @F%d@ FAM\n" (int_of_ifam ifam + 1);
  List.iter (ged_fevent opts base per_sel) (Driver.get_fevents fam);
  if
    per_sel (Driver.get_father fam)
    && has_personal_infos base (Driver.poi base (Driver.get_father fam))
  then
    Printf.ksprintf (oc opts) "1 HUSB @I%d@\n"
      (int_of_iper (Driver.get_father fam) + 1);
  if
    per_sel (Driver.get_mother fam)
    && has_personal_infos base (Driver.poi base (Driver.get_mother fam))
  then
    Printf.ksprintf (oc opts) "1 WIFE @I%d@\n"
      (int_of_iper (Driver.get_mother fam) + 1);
  Array.iter (ged_child opts per_sel) (Driver.get_children fam);
  ged_fsource opts base fam;
  ged_comment opts base fam

let gwb2ged base with_indexes opts ((per_sel, fam_sel) as sel) =
  let bname = Driver.bname base in
  let ofile, oc, close = opts.Gwexport.oc in
  if not opts.Gwexport.mem then (
    Driver.load_ascends_array base;
    Driver.load_unions_array base;
    Driver.load_couples_array base;
    Driver.load_descends_array base);
  ged_header opts base bname ofile;
  Collection.iter
    (fun i -> if per_sel i then ged_ind_record with_indexes opts base sel i)
    (Driver.ipers base);
  Collection.iter
    (fun i -> if fam_sel i then ged_fam_record opts base sel i)
    (Driver.ifams base);
  let _ =
    List.fold_right
      (fun adop i ->
        ged_fam_adop opts i adop;
        i + 1)
      !adop_fam_list
      (Driver.nb_of_families base + 1)
  in
  Printf.ksprintf oc "0 TRLR\n";
  close ()
