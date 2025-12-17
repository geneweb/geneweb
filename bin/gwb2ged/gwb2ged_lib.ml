(* Copyright (c) 1998-2007 INRIA *)

let int_of_iper =
  let ht = Hashtbl.create 0 in
  fun i ->
    match Hashtbl.find_opt ht i with
    | Some x -> x
    | None ->
        let x = Hashtbl.length ht in
        Hashtbl.add ht i x;
        x

let int_of_ifam =
  let ht = Hashtbl.create 0 in
  fun i ->
    match Hashtbl.find_opt ht i with
    | Some x -> x
    | None ->
        let x = Hashtbl.length ht in
        Hashtbl.add ht i x;
        x

(* We use negative ifams to avoid conflicts with existing ids *)
let fresh_ifam =
  let i = ref (-1) in
  fun () ->
    let n = !i in
    decr i;
    Gwdb.ifam_of_int n

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
  | Date.Dgregorian | Djulian ->
      Ext_option.return_if
        (m >= 1 && m <= Array.length month_txt)
        (fun () -> month_txt.(m - 1))
  | Dfrench ->
      Ext_option.return_if
        (m >= 1 && m <= Array.length french_txt)
        (fun () -> french_txt.(m - 1))
  | Dhebrew ->
      Ext_option.return_if
        (m >= 1 && m <= Array.length hebrew_txt)
        (fun () -> hebrew_txt.(m - 1))
  | Dislamic -> assert false

(* Reference:
   https://gedcom.io/specifications/FamilySearchGEDCOMv7.html#date *)
module Ged_date : sig
  module Day_month : sig
    type t

    val make :
      calendar:Date.calendar ->
      day:int option ->
      month:int ->
      (t, [> `Invalid_day | `Invalid_month ]) result
  end

  type t

  val make :
    calendar:Date.calendar -> day_month:Day_month.t option -> year:int -> t

  val to_string : t -> string
end = struct
  module Day_month : sig
    type t

    val make :
      calendar:Date.calendar ->
      day:int option ->
      month:int ->
      (t, [> `Invalid_day | `Invalid_month ]) result

    val day : t -> int option
    val month : t -> int
  end = struct
    type t = { day : int option; month : int }

    let day day_month = day_month.day
    let month day_month = day_month.month
    let check_day day = if day > 0 then Ok () else Error `Invalid_day

    let check_month ~calendar month =
      match ged_month calendar month with
      | Some _ -> Ok ()
      | None -> Error `Invalid_month

    let make ~calendar ~day ~month =
      let ( >>= ) = Result.bind in
      check_month ~calendar month >>= fun () ->
      Option.fold ~none:(Ok ()) ~some:check_day day >>= fun () ->
      Ok { day; month }
  end

  type t = {
    calendar : Date.calendar;
    day_month : Day_month.t option;
    year : int;
  }

  let calendar date = date.calendar
  let day date = Option.bind date.day_month Day_month.day
  let month date = Option.map Day_month.month date.day_month
  let year date = date.year
  let make ~calendar ~day_month ~year = { calendar; day_month; year }

  let to_string date =
    let month_code date =
      Option.bind (month date) (ged_month @@ calendar date)
    in
    String.concat " "
      (List.filter_map Fun.id
         [
           Option.map (Printf.sprintf "%02d") (day date);
           month_code date;
           Some (Int.to_string @@ year date);
         ])
end

let encode opts s =
  match opts.Gwexport.charset with
  | Gwexport.Ansel -> Geneweb.Ansel.of_iso_8859_1 @@ Utf8.iso_8859_1_of_utf_8 s
  | Gwexport.Ascii | Gwexport.Ansi -> Utf8.iso_8859_1_of_utf_8 s
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

let oc opts =
  let _, oc, _ = opts.Gwexport.oc in
  oc

(** [display_note_aux opts tagn s len i] outputs text [s] with CONT/CONC
    tag. GEDCOM lines are limited to 255 characters. However, the
    CONCatenation or CONTinuation tags can be used to expand a field
    beyond this limit. Lines are cut and align with [max_len]
    characters for easy display/printing.
    @see <https://www.familysearch.org/developers/docs/gedcom/> GEDCOM
    STANDARD 5.5, Appendix A CONC and CONT tag
    @param opts carries output channel
    @param tagn specifies the current gedcom tag level (0, 1, ...)
    @param s specifies text to print to the output channel (already
    encode with gedcom charset)
    @param len specifies the number of characters (char or wide char)
    already printed
    @param i specifies the last char index (index to s -- one byte
    char) *)

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
    else
      (* continue same gedcom line *)

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

let display_note opts ?source_page tagn s =
  if opts.Gwexport.notes && s <> "" then (
    let tag = Printf.sprintf "%d NOTE " tagn in
    Printf.ksprintf (oc opts) "%s" tag;
    display_note_aux opts tagn (encode opts s) (String.length tag) 0);
  Option.iter
    (fun source_page ->
      (* source_page is used to add a source with page information;
         so we can re-import wiki notes and correctly re-link them together *)
      Printf.ksprintf (oc opts) "%d SOUR\n" (tagn + 1);
      Printf.ksprintf (oc opts) "%d PAGE %s\n" (tagn + 2) source_page)
    source_page

let write_base_notes opts base =
  (* TODO WIKI what about wizard notes *)
  (* TODO WIKI we lose the "title"/page name *)
  (* list of (filename, file_content) *)
  let wiki_notes =
    (* read base notes (wiki) folder *)
    (* TODO use a Path module *)
    let path =
      Filename.concat (Gwdb.bname base ^ ".gwb") (Gwdb.base_notes_dir base)
    in
    let wiki_filenames =
      if Sys.file_exists path then Sys.readdir path else [||]
    in
    let wiki_pages =
      Array.fold_left
        (fun acc filename ->
          if Filename.check_suffix filename ".txt" then
            let file = Filename.concat path filename in
            let content = Mutil.read_file_content file in
            (filename, content) :: acc
          else acc)
        [] wiki_filenames
    in
    (* TODO WIKI base_notes should be a file in base notes folder `base.gwb/notes_d`;
       currently by default it is the file `base.gwb/notes`;
       rename it "index.txt" or "index.wiki" *)
    let main_notes = ("notes", Gwdb.base_notes_read base "") in
    (* main notes should be first in gedcom *)
    main_notes :: wiki_pages
  in
  List.iter
    (fun (filename, content) ->
      let source_page = Printf.sprintf "geneweb wiki notes: %s" filename in
      display_note opts 1 ~source_page content)
    wiki_notes

let ged_header opts base ifile ofile =
  Printf.ksprintf (oc opts) "0 HEAD\n";
  Printf.ksprintf (oc opts) "1 SOUR Geneanet\n";
  Printf.ksprintf (oc opts) "2 NAME GeneWeb\n";
  Printf.ksprintf (oc opts) "2 VERS %s\n" Geneweb.Version.txt;
  Printf.ksprintf (oc opts) "2 CORP Geneanet\n";
  Printf.ksprintf (oc opts) "3 ADDR https://www.geneanet.org/\n";
  Printf.ksprintf (oc opts) "2 DATA %s\n"
    (let fname = Filename.basename ifile in
     if Filename.check_suffix fname ".gwb" then fname else fname ^ ".gwb");
  (try
     let tm = Unix.localtime (Unix.time ()) in
     let today =
       let calendar = Date.Dgregorian in
       let day_month =
         Result.fold ~ok:Option.some
           ~error:(fun _ -> assert false)
           (Ged_date.Day_month.make ~calendar ~day:(Some tm.Unix.tm_mday)
              ~month:(tm.Unix.tm_mon + 1))
       in
       Ged_date.make ~calendar ~day_month ~year:(1900 + tm.Unix.tm_year)
     in
     Printf.ksprintf (oc opts) "1 DATE %s\n" (Ged_date.to_string today);
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
  if opts.Gwexport.base_notes then write_base_notes opts base

let sub_string_index s t =
  let rec loop i j =
    if j = String.length t then Some (i - j)
    else if i = String.length s then None
    else if s.[i] = t.[j] then loop (i + 1) (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

let ged_1st_name base p =
  let fn = Gwdb.sou base (Gwdb.get_first_name p) in
  match Gwdb.get_first_names_aliases p with
  | n :: _ -> (
      let fna = Gwdb.sou base n in
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
    (Gwdb.string_of_iper (Gwdb.get_iper per))

let ged_name opts base per =
  Printf.ksprintf (oc opts) "1 NAME %s /%s/\n"
    (encode opts (Mutil.nominative (ged_1st_name base per)))
    (encode opts (Mutil.nominative (Gwdb.sou base (Gwdb.get_surname per))));
  let n = Gwdb.sou base (Gwdb.get_public_name per) in
  if n <> "" then Printf.ksprintf (oc opts) "2 GIVN %s\n" (encode opts n);
  (match Gwdb.get_qualifiers per with
  | nn :: _ ->
      Printf.ksprintf (oc opts) "2 NICK %s\n" (encode opts (Gwdb.sou base nn))
  | [] -> ());
  (match Gwdb.get_surnames_aliases per with
  | [] -> ()
  | list ->
      let list = List.map (fun n -> encode opts (Gwdb.sou base n)) list in
      Printf.ksprintf (oc opts) "2 SURN %s\n" (string_of_list list));
  List.iter
    (fun s ->
      Printf.ksprintf (oc opts) "1 NAME %s\n" (encode opts (Gwdb.sou base s)))
    (Gwdb.get_aliases per)

let ged_sex opts per =
  match Gwdb.get_sex per with
  | Def.Male -> Printf.ksprintf (oc opts) "1 SEX M\n"
  | Def.Female -> Printf.ksprintf (oc opts) "1 SEX F\n"
  | Def.Neuter -> ()

let ged_calendar opts = function
  | Date.Dgregorian -> ()
  | Djulian -> Printf.ksprintf (oc opts) "@#DJULIAN@ "
  | Dfrench -> Printf.ksprintf (oc opts) "@#DFRENCH R@ "
  | Dhebrew -> Printf.ksprintf (oc opts) "@#DHEBREW@ "
  | Dislamic -> assert false

let ged_date_dmy opts dt cal =
  let ged_date ~calendar ~day ~month ~year =
    let optional component = if component = 0 then None else Some component in
    let rec day_month ~calendar ~day ~month =
      let log_error message =
        prerr_endline @@ Printf.sprintf "Date error: %s" message
      in
      match Ged_date.Day_month.make ~calendar ~day ~month with
      | Ok day_month -> Some day_month
      | Error `Invalid_month ->
          let error_message =
            Printf.sprintf "invalid month: no month '%d' in calendar '%s'" month
              (Def_show.show_calendar calendar)
          in
          log_error error_message;
          None
      | Error `Invalid_day ->
          let error_message =
            Printf.sprintf
              "invalid day: no day '%s' in month '%d' of calendar '%s'"
              (Option.fold ~none:"none" ~some:Int.to_string day)
              month
              (Def_show.show_calendar calendar)
          in
          log_error error_message;
          day_month ~calendar ~day:None ~month
    in
    Ged_date.make ~calendar
      ~day_month:
        (Option.bind (optional month) (fun month ->
             day_month ~calendar ~day:(optional day) ~month))
      ~year
  in
  let dt =
    let prec =
      match dt.Date.prec with
      | (Sure | About | Maybe | Before | After | YearInt _) as prec -> prec
      | OrYear date -> YearInt date
    in
    Date.normalize_interval ~calendar:cal { dt with prec }
  in
  (match dt.Date.prec with
  | Sure -> ()
  | About -> Printf.ksprintf (oc opts) "ABT "
  | Maybe -> Printf.ksprintf (oc opts) "EST "
  | Before -> Printf.ksprintf (oc opts) "BEF "
  | After -> Printf.ksprintf (oc opts) "AFT "
  | OrYear _ | YearInt _ -> Printf.ksprintf (oc opts) "BET ");
  ged_calendar opts cal;
  oc opts
    (Ged_date.to_string
    @@ ged_date ~calendar:cal ~day:dt.day ~month:dt.month ~year:dt.year);
  match dt.prec with
  | OrYear dmy2 | YearInt dmy2 ->
      Printf.ksprintf (oc opts) " AND ";
      ged_calendar opts cal;
      oc opts
        (Ged_date.to_string
        @@ ged_date ~calendar:cal ~day:dmy2.day2 ~month:dmy2.month2
             ~year:dmy2.year2)
  | Sure | About | Maybe | Before | After -> ()

let ged_date opts = function
  | Date.Dgreg (d, (Dgregorian | Dislamic)) -> ged_date_dmy opts d Dgregorian
  | Dgreg (d, Djulian) ->
      ged_date_dmy opts (Date.convert ~from:Dgregorian ~to_:Djulian d) Djulian
  | Dgreg (d, Dfrench) ->
      ged_date_dmy opts (Date.convert ~from:Dgregorian ~to_:Dfrench d) Dfrench
  | Dgreg (d, Dhebrew) ->
      ged_date_dmy opts (Date.convert ~from:Dgregorian ~to_:Dhebrew d) Dhebrew
  | Dtext t -> Printf.ksprintf (oc opts) "(%s)" t

let print_sour opts n s = Printf.ksprintf (oc opts) "%d SOUR %s\n" n s

let ged_ev_detail opts n typ d pl note src =
  (match (typ, d, pl, note, src) with
  | "", None, "", "", "" -> Printf.ksprintf (oc opts) " Y"
  | _ -> ());
  Printf.ksprintf (oc opts) "\n";
  if typ = "" then () else Printf.ksprintf (oc opts) "%d TYPE %s\n" n typ;
  Option.iter
    (fun d ->
      Printf.ksprintf (oc opts) "%d DATE " n;
      ged_date opts d;
      Printf.ksprintf (oc opts) "\n")
    d;
  if pl <> "" then Printf.ksprintf (oc opts) "%d PLAC %s\n" n (encode opts pl);
  display_note opts n note;
  if opts.Gwexport.source = None && src <> "" then
    print_sour opts n (encode opts src)

let ged_tag_pevent base evt_name =
  match evt_name with
  | Def.Epers_Birth -> "BIRT"
  | Def.Epers_Baptism -> "BAPM"
  | Def.Epers_Death -> "DEAT"
  | Def.Epers_Burial -> "BURI"
  | Def.Epers_Cremation -> "CREM"
  | Def.Epers_Accomplishment -> "Accomplishment"
  | Def.Epers_Acquisition -> "Acquisition"
  | Def.Epers_Adhesion -> "Membership"
  | Def.Epers_BaptismLDS -> "BAPL"
  | Def.Epers_BarMitzvah -> "BARM"
  | Def.Epers_BatMitzvah -> "BASM"
  | Def.Epers_Benediction -> "BLES"
  | Def.Epers_ChangeName -> "Change name"
  | Def.Epers_Circumcision -> "Circumcision"
  | Def.Epers_Confirmation -> "CONF"
  | Def.Epers_ConfirmationLDS -> "CONL"
  | Def.Epers_Decoration -> "Award"
  | Def.Epers_DemobilisationMilitaire -> "Military discharge"
  | Def.Epers_Diploma -> "Degree"
  | Def.Epers_Distinction -> "Distinction"
  | Def.Epers_Dotation -> "ENDL"
  | Def.Epers_DotationLDS -> "DotationLDS"
  | Def.Epers_Education -> "EDUC"
  | Def.Epers_Election -> "Election"
  | Def.Epers_Emigration -> "EMIG"
  | Def.Epers_Excommunication -> "Excommunication"
  | Def.Epers_FamilyLinkLDS -> "Family link LDS"
  | Def.Epers_FirstCommunion -> "FCOM"
  | Def.Epers_Funeral -> "Funeral"
  | Def.Epers_Graduate -> "GRAD"
  | Def.Epers_Hospitalisation -> "Hospitalization"
  | Def.Epers_Illness -> "Illness"
  | Def.Epers_Immigration -> "IMMI"
  | Def.Epers_ListePassenger -> "Passenger list"
  | Def.Epers_MilitaryDistinction -> "Military distinction"
  | Def.Epers_MilitaryPromotion -> "Military promotion"
  | Def.Epers_MilitaryService -> "Military service"
  | Def.Epers_MobilisationMilitaire -> "Military mobilization"
  | Def.Epers_Naturalisation -> "NATU"
  | Def.Epers_Occupation -> "OCCU"
  | Def.Epers_Ordination -> "ORDN"
  | Def.Epers_Property -> "PROP"
  | Def.Epers_Recensement -> "CENS"
  | Def.Epers_Residence -> "RESI"
  | Def.Epers_Retired -> "RETI"
  | Def.Epers_ScellentChildLDS -> "SLGC"
  | Def.Epers_ScellentParentLDS -> "Scellent parent LDS"
  | Def.Epers_ScellentSpouseLDS -> "SLGS"
  | Def.Epers_VenteBien -> "Property sale"
  | Def.Epers_Will -> "WILL"
  | Def.Epers_Name n -> Gwdb.sou base n
  | Def.Epers_Adoption -> "ADOP"

let is_primary_pevents = function
  | Def.Epers_Birth | Def.Epers_Baptism | Def.Epers_Death | Def.Epers_Burial
  | Def.Epers_Cremation | Def.Epers_BaptismLDS | Def.Epers_BarMitzvah
  | Def.Epers_BatMitzvah | Def.Epers_Benediction | Def.Epers_Confirmation
  | Def.Epers_ConfirmationLDS | Def.Epers_Dotation | Def.Epers_Education
  | Def.Epers_Emigration | Def.Epers_FirstCommunion | Def.Epers_Graduate
  | Def.Epers_Immigration | Def.Epers_Naturalisation | Def.Epers_Occupation
  | Def.Epers_Ordination | Def.Epers_Property | Def.Epers_Recensement
  | Def.Epers_Residence | Def.Epers_Retired | Def.Epers_ScellentChildLDS
  | Def.Epers_ScellentSpouseLDS | Def.Epers_Will ->
      true
  | Def.Epers_Accomplishment | Def.Epers_Acquisition | Def.Epers_Adhesion
  | Def.Epers_ChangeName | Def.Epers_Circumcision | Def.Epers_Decoration
  | Def.Epers_DemobilisationMilitaire | Def.Epers_Diploma
  | Def.Epers_Distinction | Def.Epers_DotationLDS | Def.Epers_Election
  | Def.Epers_Excommunication | Def.Epers_FamilyLinkLDS | Def.Epers_Funeral
  | Def.Epers_Hospitalisation | Def.Epers_Illness | Def.Epers_ListePassenger
  | Def.Epers_MilitaryDistinction | Def.Epers_MilitaryPromotion
  | Def.Epers_MilitaryService | Def.Epers_MobilisationMilitaire
  | Def.Epers_ScellentParentLDS | Def.Epers_VenteBien | Def.Epers_Name _
  | Def.Epers_Adoption ->
      false

let relation_format_of_witness_kind :
    Def.witness_kind -> ('a, unit, string, unit) format4 = function
  | Def.Witness -> "3 RELA Witness"
  | Def.Witness_GodParent -> "3 RELA GODP"
  | Def.Witness_CivilOfficer -> "3 RELA Civil officer"
  | Def.Witness_ReligiousOfficer -> "3 RELA Religious officer"
  | Def.Witness_Informant -> "3 RELA Informant"
  | Def.Witness_Attending -> "3 RELA Attending"
  | Def.Witness_Mentioned -> "3 RELA Mentioned"
  | Def.Witness_Other -> "3 RELA Other"

let oc' opts s = Printf.ksprintf (oc opts) (s ^^ "\n")
let oc_witness_kind opts wk = oc' opts (relation_format_of_witness_kind wk)

let witness_format opts base per_sel (ip, wk, wnote) =
  if per_sel ip then (
    Printf.ksprintf (oc opts) "2 ASSO @I%d@\n" (int_of_iper ip + 1);
    Printf.ksprintf (oc opts) "3 TYPE INDI\n";
    oc_witness_kind opts wk;
    display_note opts 3 (Gwdb.sou base wnote))

let ged_pevent opts base per_sel evt =
  let name = Gwdb.get_pevent_name evt in
  let typ =
    if is_primary_pevents name then (
      let tag = ged_tag_pevent base name in
      Printf.ksprintf (oc opts) "1 %s" tag;
      "")
    else (
      Printf.ksprintf (oc opts) "1 EVEN";
      ged_tag_pevent base name)
  in
  let date = Date.od_of_cdate (Gwdb.get_pevent_date evt) in
  let place = Gwdb.sou base (Gwdb.get_pevent_place evt) in
  let note = Gwdb.sou base (Gwdb.get_pevent_note evt) in
  let src = Gwdb.sou base (Gwdb.get_pevent_src evt) in
  ged_ev_detail opts 2 typ date place note src;
  let witnesses = Gwdb.get_pevent_witnesses_and_notes evt in
  Array.iter (witness_format opts base per_sel) witnesses

let adop_fam_list = ref []

let ged_fam_adop opts (fath, moth, i) =
  Printf.ksprintf (oc opts) "0 @F%d@ FAM\n" (i + 1);
  Option.iter
    (fun i -> Printf.ksprintf (oc opts) "1 HUSB @I%d@\n" (int_of_iper i + 1))
    fath;
  Option.iter
    (fun i -> Printf.ksprintf (oc opts) "1 WIFE @I%d@\n" (int_of_iper i + 1))
    moth

let ged_ind_ev_str opts base per per_sel =
  List.iter (ged_pevent opts base per_sel) (Gwdb.get_pevents per)

let ged_title opts base per tit =
  Printf.ksprintf (oc opts) "1 TITL ";
  Printf.ksprintf (oc opts) "%s" (encode opts (Gwdb.sou base tit.Def.t_ident));
  (match Gwdb.sou base tit.Def.t_place with
  | "" -> ()
  | pl -> Printf.ksprintf (oc opts) ", %s" (encode opts pl));
  if tit.Def.t_nth <> 0 then Printf.ksprintf (oc opts) ", %d" tit.Def.t_nth;
  Printf.ksprintf (oc opts) "\n";
  (match
     (Date.od_of_cdate tit.Def.t_date_start, Date.od_of_cdate tit.Def.t_date_end)
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
  match tit.Def.t_name with
  | Def.Tmain ->
      Printf.ksprintf (oc opts) "2 NOTE %s\n"
        (encode opts (Gwdb.sou base (Gwdb.get_public_name per)))
  | Def.Tname n ->
      Printf.ksprintf (oc opts) "2 NOTE %s\n" (encode opts (Gwdb.sou base n))
  | Def.Tnone -> ()

let ged_ind_attr_str opts base per =
  (match Gwdb.sou base (Gwdb.get_occupation per) with
  | "" -> ()
  | occu -> Printf.ksprintf (oc opts) "1 OCCU %s\n" (encode opts occu));
  List.iter (ged_title opts base per) (Gwdb.get_titles per)

let ged_famc opts fam_sel asc =
  Option.iter
    (fun ifam ->
      if fam_sel ifam then
        Printf.ksprintf (oc opts) "1 FAMC @F%d@\n" (int_of_ifam ifam + 1))
    (Gwdb.get_parents asc)

let ged_fams opts fam_sel ifam =
  if fam_sel ifam then
    Printf.ksprintf (oc opts) "1 FAMS @F%d@\n" (int_of_ifam ifam + 1)

let ged_godparent opts per_sel godp =
  Option.iter (fun ip ->
      if per_sel ip then (
        Printf.ksprintf (oc opts) "1 ASSO @I%d@\n" (int_of_iper ip + 1);
        Printf.ksprintf (oc opts) "2 TYPE INDI\n";
        Printf.ksprintf (oc opts) "2 RELA %s\n" godp))

let ged_witness opts fam_sel ifam =
  if fam_sel ifam then (
    Printf.ksprintf (oc opts) "1 ASSO @F%d@\n" (int_of_ifam ifam + 1);
    Printf.ksprintf (oc opts) "2 TYPE FAM\n";
    Printf.ksprintf (oc opts) "2 RELA witness\n")

let ged_adoption ~opts ~per_sel ~ip ~fath ~moth =
  if per_sel ip then (
    let n_ifam = int_of_ifam (fresh_ifam ()) in
    adop_fam_list := (fath, moth, n_ifam) :: !adop_fam_list;
    let adop =
      match (fath, moth) with
      | Some _, Some _ -> "BOTH"
      | Some _, None -> "HUSB"
      | None, Some _ -> "WIFE"
      | None, None -> ""
    in
    Printf.ksprintf (oc opts) "1 ADOP\n";
    Printf.ksprintf (oc opts) "2 FAMC @F%d@\n" (n_ifam + 1);
    Printf.ksprintf (oc opts) "3 ADOP %s\n" adop)

let ged_asso opts base (per_sel, fam_sel) per =
  List.iter
    (fun r ->
      match r.Def.r_type with
      | Def.GodParent ->
          ged_godparent opts per_sel "GODF" r.Def.r_fath;
          ged_godparent opts per_sel "GODM" r.Def.r_moth
      | Adoption ->
          ged_adoption ~opts ~per_sel ~ip:(Gwdb.get_iper per) ~fath:r.Def.r_fath
            ~moth:r.Def.r_moth
      | Recognition | CandidateParent | FosterParent -> ())
    (Gwdb.get_rparents per);
  List.iter
    (fun ic ->
      let c = Gwdb.poi base ic in
      if Gwdb.get_sex c = Def.Male then
        List.iter
          (fun ifam ->
            let fam = Gwdb.foi base ifam in
            if Array.mem (Gwdb.get_iper per) (Gwdb.get_witnesses fam) then
              ged_witness opts fam_sel ifam)
          (Array.to_list (Gwdb.get_family c)))
    (Gwdb.get_related per)

let ged_psource opts base per =
  match opts.Gwexport.source with
  | Some "" -> ()
  | Some s -> print_sour opts 1 (encode opts s)
  | None -> (
      match Gwdb.sou base (Gwdb.get_psources per) with
      | "" -> ()
      | s -> print_sour opts 1 (encode opts s))

let has_image_file opts base p =
  let s = Geneweb.Image.default_portrait_filename base p in
  let f = Filename.concat opts.Gwexport.img_base_path s in
  if Sys.file_exists (f ^ ".gif") then Some (f ^ ".gif")
  else if Sys.file_exists (f ^ ".jpg") then Some (f ^ ".jpg")
  else if Sys.file_exists (f ^ ".png") then Some (f ^ ".png")
  else None

let ged_multimedia_link opts base per =
  match Gwdb.sou base (Gwdb.get_image per) with
  | "" ->
      if (not opts.Gwexport.no_picture) && opts.Gwexport.picture_path then
        Option.iter
          (fun s ->
            Printf.ksprintf (oc opts) "1 OBJE\n";
            Printf.ksprintf (oc opts) "2 FILE %s\n" s)
          (has_image_file opts base per)
  | s ->
      if not opts.Gwexport.no_picture then (
        Printf.ksprintf (oc opts) "1 OBJE\n";
        Printf.ksprintf (oc opts) "2 FILE %s\n" s)

let ged_note opts base per =
  display_note opts 1 (Gwdb.sou base (Gwdb.get_notes per))

let ged_tag_fevent base evt_name =
  match evt_name with
  | Def.Efam_Marriage -> "MARR"
  | Def.Efam_NoMarriage -> "unmarried"
  | Def.Efam_NoMention -> "nomen"
  | Def.Efam_Engage -> "ENGA"
  | Def.Efam_Divorce -> "DIV"
  | Def.Efam_Separated -> "SEP"
  | Def.Efam_Annulation -> "ANUL"
  | Def.Efam_MarriageBann -> "MARB"
  | Def.Efam_MarriageContract -> "MARC"
  | Def.Efam_MarriageLicense -> "MARL"
  | Def.Efam_PACS -> "pacs"
  | Def.Efam_Residence -> "residence"
  | Def.Efam_Name n -> Gwdb.sou base n

let is_primary_fevents = function
  | Def.Efam_Marriage | Def.Efam_Engage | Def.Efam_Divorce | Def.Efam_Separated
  | Def.Efam_Annulation | Def.Efam_MarriageBann | Def.Efam_MarriageContract
  | Def.Efam_MarriageLicense ->
      true
  | Def.Efam_NoMarriage | Def.Efam_NoMention | Def.Efam_PACS
  | Def.Efam_Residence | Def.Efam_Name _ ->
      false

let ged_fevent opts base per_sel evt =
  let name = Gwdb.get_fevent_name evt in
  let typ =
    if is_primary_fevents name then (
      let tag = ged_tag_fevent base name in
      Printf.ksprintf (oc opts) "1 %s" tag;
      "")
    else (
      Printf.ksprintf (oc opts) "1 EVEN";
      ged_tag_fevent base name)
  in
  let date = Date.od_of_cdate (Gwdb.get_fevent_date evt) in
  let place = Gwdb.sou base (Gwdb.get_fevent_place evt) in
  let note = Gwdb.sou base (Gwdb.get_fevent_note evt) in
  let src = Gwdb.sou base (Gwdb.get_fevent_src evt) in
  ged_ev_detail opts 2 typ date place note src;
  let witnesses = Gwdb.get_fevent_witnesses_and_notes evt in
  Array.iter (witness_format opts base per_sel) witnesses

let ged_child opts per_sel chil =
  if per_sel chil then
    Printf.ksprintf (oc opts) "1 CHIL @I%d@\n" (int_of_iper chil + 1)

let ged_fsource opts base fam =
  match opts.Gwexport.source with
  | Some "" -> ()
  | Some s -> print_sour opts 1 (encode opts s)
  | None -> (
      match Gwdb.sou base (Gwdb.get_fsources fam) with
      | "" -> ()
      | s -> print_sour opts 1 (encode opts s))

let ged_comment opts base fam =
  display_note opts 1 (Gwdb.sou base (Gwdb.get_comment fam))

let has_personal_infos base per =
  Gwdb.get_parents per <> None
  || Gwdb.sou base (Gwdb.get_first_name per) <> "?"
  || Gwdb.sou base (Gwdb.get_surname per) <> "?"
  || Gwdb.get_birth per <> Date.cdate_None
  || Gwdb.sou base (Gwdb.get_birth_place per) <> ""
  || Gwdb.get_death per <> Def.NotDead
     && Gwdb.get_death per <> Def.DontKnowIfDead
  || Gwdb.sou base (Gwdb.get_occupation per) <> ""
  || Gwdb.get_titles per <> []

let ged_ind_record with_indexes opts base ((per_sel, fam_sel) as sel) i =
  let per = Gwdb.poi base i in
  if has_personal_infos base per then (
    Printf.ksprintf (oc opts) "0 @I%d@ INDI\n" (int_of_iper i + 1);
    ged_name opts base per;
    if with_indexes then ged_index opts per;
    ged_sex opts per;
    ged_ind_ev_str opts base per per_sel;
    ged_ind_attr_str opts base per;
    ged_famc opts fam_sel per;
    Array.iter (ged_fams opts fam_sel) (Gwdb.get_family per);
    ged_asso opts base sel per;
    ged_psource opts base per;
    ged_multimedia_link opts base per;
    ged_note opts base per)

let ged_fam_record opts base (per_sel, _fam_sel) ifam =
  let fam = Gwdb.foi base ifam in
  Printf.ksprintf (oc opts) "0 @F%d@ FAM\n" (int_of_ifam ifam + 1);
  List.iter (ged_fevent opts base per_sel) (Gwdb.get_fevents fam);
  if
    per_sel (Gwdb.get_father fam)
    && has_personal_infos base (Gwdb.poi base (Gwdb.get_father fam))
  then
    Printf.ksprintf (oc opts) "1 HUSB @I%d@\n"
      (int_of_iper (Gwdb.get_father fam) + 1);
  if
    per_sel (Gwdb.get_mother fam)
    && has_personal_infos base (Gwdb.poi base (Gwdb.get_mother fam))
  then
    Printf.ksprintf (oc opts) "1 WIFE @I%d@\n"
      (int_of_iper (Gwdb.get_mother fam) + 1);
  Array.iter (ged_child opts per_sel) (Gwdb.get_children fam);
  ged_fsource opts base fam;
  ged_comment opts base fam

let gwb2ged with_indexes opts ((per_sel, fam_sel) as sel) =
  match opts.Gwexport.base with
  | Some (ifile, base) ->
      let ofile, oc, close = opts.Gwexport.oc in
      if not opts.Gwexport.mem then (
        Gwdb.load_ascends_array base;
        Gwdb.load_unions_array base;
        Gwdb.load_couples_array base;
        Gwdb.load_descends_array base);
      ged_header opts base ifile ofile;
      Gwdb.Collection.iter
        (fun i -> if per_sel i then ged_ind_record with_indexes opts base sel i)
        (Gwdb.ipers base);
      Gwdb.Collection.iter
        (fun i -> if fam_sel i then ged_fam_record opts base sel i)
        (Gwdb.ifams base);
      let () = List.iter (fun adop -> ged_fam_adop opts adop) !adop_fam_list in
      Printf.ksprintf oc "0 TRLR\n";
      close ()
  | None -> assert false
