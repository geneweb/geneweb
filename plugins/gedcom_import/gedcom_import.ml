open Geneweb
open Def

type b = { persons: (string, string, string) gen_person Res.Array.t
         ; families: (string, string, string) gen_family Res.Array.t
         }

let next_line s i =
  if i >= String.length s then raise End_of_file
  else match String.index_from_opt s i '\010' with
  | Some j -> j + 1
  | None -> String.length s

let input s =
  let i =
    (* strip UTF-8 BOM if exists *)
    if String.length s >= 3
    && String.unsafe_get s 0 = '\239'
    && String.unsafe_get s 1 = '\187'
    && String.unsafe_get s 2 = '\191'
    then ref 3
    else ref 0
  in
  fun () -> let cur = !i in let j = next_line s cur in i := j ; (s, cur, j - cur)

let mk_calendar = function
  | Gedcom.JULIAN -> Djulian
  | Gedcom.GREGORIAN -> Dgregorian
  | Gedcom.FRENCH -> Dfrench
  | Gedcom.HEBREW -> Dhebrew
  | Gedcom.UNKNOWN -> assert false
  | Gedcom.ROMAN -> assert false

let mk_dgreg prec (day, month, year, calendar, _delta) =
  Dgreg ({ day ; month ; year ; delta = 0 ; prec }, mk_calendar calendar)

let mk_gedcom_date node = let s = Gedcom.value node in Gedcom.parse_date s 0 (String.length s)

let gedcom_date_to_gw = function
  | Gedcom.Date_SURE d -> mk_dgreg Sure d
  | Gedcom.Date_ABT d -> mk_dgreg About d
  | Gedcom.Date_CAL d -> mk_dgreg Maybe d
  | Gedcom.Date_EST d -> mk_dgreg Maybe d
  | Gedcom.Date_INT (d, s) -> assert false
  | Gedcom.Date_TEXT s -> Dtext s
  | Gedcom.Range_BEF d -> mk_dgreg Before d
  | Gedcom.Range_AFT d -> mk_dgreg After d
  | Gedcom.Range_BET_AND (d1, d2) -> assert false
  | Gedcom.Period_FROM d -> mk_dgreg After d
  | Gedcom.Period_TO d -> mk_dgreg Before d
  | Gedcom.Period_FROM_TO (d1, d2) -> assert false

let mk_date node =
  mk_gedcom_date node |> gedcom_date_to_gw

(* FIXME: handle xref *)
let value node =
  Gedcom.value node

let empty_pevent epers_name =
  { epers_name
  ; epers_date = Adef.cdate_None
  ; epers_place = ""
  ; epers_reason = ""
  ; epers_note = ""
  ; epers_src = ""
  ; epers_witnesses = [||]
  }

let empty_person key_index =
  { first_name = ""
  ; surname = ""
  ; occ = 0
  ; public_name = ""
  ; image = ""
  ; qualifiers = []
  ; aliases = []
  ; first_names_aliases = []
  ; surnames_aliases = []
  ; titles = []
  ; rparents = []
  ; related = []
  ; occupation = ""
  ; sex = Neuter
  ; access = IfTitles
  ; birth = Adef.cdate_None
  ; birth_place = ""
  ; birth_note = ""
  ; birth_src = ""
  ; baptism = Adef.cdate_None
  ; baptism_place = ""
  ; baptism_note = ""
  ; baptism_src = ""
  ; death = DontKnowIfDead
  ; death_place = ""
  ; death_note = ""
  ; death_src = ""
  ; burial = UnknownBurial
  ; burial_place = ""
  ; burial_note = ""
  ; burial_src = ""
  ; pevents = []
  ; notes = ""
  ; psources = ""
  ; key_index
  }

let empty_fevent efam_name =
  { efam_name
  ; efam_date = Adef.cdate_None
  ; efam_place = ""
  ; efam_reason = ""
  ; efam_note = ""
  ; efam_src = ""
  ; efam_witnesses = [||]
  }

let empty_family fam_index =
  { marriage = Adef.cdate_None
  ; marriage_place = ""
  ; marriage_note = ""
  ; marriage_src = ""
  ; witnesses = [||]
  ; relation = Def.NotMarried
  ; divorce = NotDivorced
  ; fevents = []
  ; comment = ""
  ; origin_file = ""
  ; fsources = ""
  ; fam_index
  }

let mk_event place date src note e nodes =
  List.fold_left begin fun e n ->
    let node = Gedcom.node n in
    match Gedcom.tag node with
    | "PLAC" -> place e (value node)
    | "DATE" -> date e (Adef.cdate_of_date (mk_date node))
    | "SOUR" -> src e (value node)
    | "NOTE" -> note e (value node)
    | s -> prerr_endline (__LOC__ ^ ": " ^ s) ; e
  end e nodes

let mk_pevent epers_name nodes =
  mk_event
    (fun e epers_place -> { e with epers_place })
    (fun e epers_date ->  { e with epers_date })
    (fun e epers_src -> { e with epers_src })
    (fun e epers_note -> { e with epers_note })
    (empty_pevent epers_name)
    nodes

let w_pevent p epers_name n =
  { p with pevents = mk_pevent epers_name (Gedcom.children n) :: p.pevents }

let mk_fevent efam_name nodes =
  mk_event
    (fun e efam_place -> { e with efam_place })
    (fun e efam_date ->  { e with efam_date })
    (fun e efam_src -> { e with efam_src })
    (fun e efam_note -> { e with efam_note })
    (empty_fevent efam_name)
    nodes

let w_fevent f efam_name n =
  { f with fevents = mk_fevent efam_name (Gedcom.children n) :: f.fevents }

let empty_title =
  { t_name = Tnone
  ; t_ident = ""
  ; t_place = ""
  ; t_date_start = Adef.cdate_None
  ; t_date_end = Adef.cdate_None
  ; t_nth = 0
  }

let mk_title n =
  List.fold_left begin fun t n ->
    match Gedcom.node n |> Gedcom.tag |> String.uppercase_ascii with
    | "DATE" ->
      begin match Gedcom.node n |> mk_gedcom_date with
        | Gedcom.Date_SURE _
        | Gedcom.Date_ABT _
        | Gedcom.Date_CAL _
        | Gedcom.Date_EST _
        | Gedcom.Range_BEF _
        | Gedcom.Range_AFT _
        | Gedcom.Period_FROM _
        | Gedcom.Period_TO _
          as d -> { t with t_date_start = Adef.cdate_of_date (gedcom_date_to_gw d) }
        | Gedcom.Date_INT (d, s) -> assert false
        | Gedcom.Date_TEXT s -> assert false
        | Gedcom.Range_BET_AND (d1, d2) -> assert false
        | Gedcom.Period_FROM_TO (d1, d2) -> assert false
      end
    | _ -> t
  end empty_title (Gedcom.children n) 

let mk_person n =
  let key = Gedcom.node n |> Gedcom.xref in
  List.fold_left begin fun p n ->
    match Gedcom.node n |> Gedcom.tag |> String.uppercase_ascii with
    | "BIRT" -> w_pevent p Epers_Birth n
    | "BAPM" -> w_pevent p Epers_Baptism n
    | "BURI" -> w_pevent p Epers_Burial n
    | "CREM" -> w_pevent p Epers_Cremation n
    | "DEAT" -> w_pevent p Epers_Death n

    | "CHR" -> w_pevent p Epers_Baptism n
    | "ACCOMPLISHMENT" -> w_pevent p Epers_Accomplishment n
    | "ACQUISITION" -> w_pevent p Epers_Acquisition n
    | "AWARD" | "DISTINCTION" -> w_pevent p Epers_Distinction n
    | "BAPL" | "LDS BAPTISM" -> w_pevent p Epers_BaptismLDS n
    | "BARM" -> w_pevent p Epers_BarMitzvah n
    | "BASM" -> w_pevent p Epers_BatMitzvah n
    | "BLES" -> w_pevent p Epers_Benediction n
    | "CENS" -> w_pevent p Epers_Recensement n
    | "CIRCUMCISION" -> w_pevent p Epers_Circumcision n
    | "CONF" -> w_pevent p Epers_Confirmation n
    | "CONL" | "LDS CONFIRMATION" -> w_pevent p Epers_ConfirmationLDS n
    | "DEGREE" -> w_pevent p Epers_Diploma n
    | "DECO" -> w_pevent p Epers_Decoration n
    | "LDS DOTATION" | "LDS ENDOWMENT" -> w_pevent p Epers_DotationLDS n
    | "EDUC" -> w_pevent p Epers_Education n
    | "ELECTION" -> w_pevent p Epers_Election n
    | "EMIG" -> w_pevent p Epers_Emigration n
    | "ENDL" -> w_pevent p Epers_Dotation n
    | "EXCOMMUNICATION" -> w_pevent p Epers_Excommunication n
    | "FAMILY LINK LDS" -> w_pevent p Epers_FamilyLinkLDS n
    | "FCOM" -> w_pevent p Epers_FirstCommunion n
    | "FUNERAL" -> w_pevent p Epers_Funeral n
    | "GRAD" -> w_pevent p Epers_Graduate n
    | "HOSPITALIZATION" -> w_pevent p Epers_Hospitalisation n
    | "ILLNESS" -> w_pevent p Epers_Illness n
    | "IMMI" -> w_pevent p Epers_Immigration n
    | "MEMBERSHIP" -> w_pevent p Epers_Adhesion n
    | "MILITARY DISCHARGE" -> w_pevent p Epers_DemobilisationMilitaire n
    | "MILITARY DISTINCTION" -> w_pevent p Epers_MilitaryDistinction n
    | "MILITARY PROMOTION" -> w_pevent p Epers_MilitaryPromotion n
    | "MILITARY SERVICE" -> w_pevent p Epers_MilitaryService n
    | "MILITARY MOBILIZATION" -> w_pevent p Epers_MobilisationMilitaire n
    | "CHANGE NAME" -> w_pevent p Epers_ChangeName n
    | "NATU" -> w_pevent p Epers_Naturalisation n
    | "OCCU" | "OCCUPATION" -> w_pevent p Epers_Occupation n
    | "ORDN" -> w_pevent p Epers_Ordination n
    | "PASSENGER LIST" -> w_pevent p Epers_ListePassenger n
    | "PROP" -> w_pevent p Epers_Property n
    | "RESI" | "residence" -> w_pevent p Epers_Residence n
    | "RETI" -> w_pevent p Epers_Retired n
    | "SCELLENT PARENT LDS" -> w_pevent p Epers_ScellentParentLDS n
    | "SLGC" | "LDS SEALING CHILD" -> w_pevent p Epers_ScellentChildLDS n
    | "SLGS" | "LDS SEALING SPOUSE" -> w_pevent p Epers_ScellentSpouseLDS n
    | "PROPERTY SALE" -> w_pevent p Epers_VenteBien n
    | "WILL" -> w_pevent p Epers_Will n

    | "TITL" -> { p with titles = mk_title n :: p.titles }
    
    | _ -> p
  end (empty_person key) (Gedcom.children n)

let mk_family n =
  let key = Gedcom.node n |> Gedcom.xref in
  List.fold_left begin fun f n ->
    match Gedcom.node n |> Gedcom.tag |> String.uppercase_ascii with
    | "MARR" -> w_fevent f Efam_Marriage n
    | "UNMARRIED" -> w_fevent f Efam_NoMarriage n
    | "NOMEN" -> w_fevent f Efam_NoMention n
    | "ENGA" -> w_fevent f Efam_Engage n
    | "DIV" -> w_fevent f Efam_Divorce n
    | "SEP" | "SEPARATION" -> w_fevent f Efam_Separated n
    | "ANUL" -> w_fevent f Efam_Annulation n
    | "MARB" -> w_fevent f Efam_MarriageBann n
    | "MARC" -> w_fevent f Efam_MarriageContract n
    | "MARL" -> w_fevent f Efam_MarriageLicense n
    | "PACS" -> w_fevent f Efam_PACS n
    | "RESI" | "RESIDENCE" -> w_fevent f Efam_Residence n
    | _ -> f
  end (empty_family key) (Gedcom.children n)

let report = (fun s -> prerr_endline @@ "ERR: " ^ s)

let length s =
  Gedcom.fold0 report (input s) (0, 0) @@ fun (p, f) n ->
  match Gedcom.node n |> Gedcom.tag with
  | "INDI" -> (p + 1, f)
  |  "FAM" -> (p, f + 1)
  |      _ -> (p, f)

let nodes s =
  List.rev @@ Gedcom.fold0 report (input s) [] @@ fun acc n -> n :: acc

let base s =
  let init = { persons = Res.Array.empty ()
             ; families = Res.Array.empty ()
             }
  in
  Gedcom.fold0 report (input s) init @@ fun acc n ->
  match Gedcom.node n |> Gedcom.tag with
  | "INDI" -> Res.Array.add_one acc.persons (mk_person n) ; acc
  |  "FAM" -> Res.Array.add_one acc.families (mk_family n) ; acc
  |      _ -> acc
  
