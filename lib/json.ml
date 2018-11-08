open Def
open Gwdb
(* open Yojson *)

let json_of_istr base istr =
  let str = sou base istr in
  if str = "" then `Null else `String str

let make_key lastname firstname occ =
  let n = Name.lower lastname in
  let p = Name.lower firstname in
  let occ = if occ = 0 then "" else string_of_int occ in
  Printf.sprintf "%s|%s|%s" n p occ

let rec json_list_of_istr_list base strlist json =
  match strlist with
    [] -> json
  | str::remains -> let json = json @ [json_of_istr base str] in json_list_of_istr_list base remains json

let string_of_pevent_name base name =
  match name with
    Epers_Birth -> `String "birth"
  | Epers_Baptism -> `String "baptism"
  | Epers_Death -> `String "death"
  | Epers_Burial -> `String "burial"
  | Epers_Cremation -> `String "cremation"
  | Epers_Accomplishment -> `String "accomplishment"
  | Epers_Acquisition -> `String "aquisition"
  | Epers_Adhesion -> `String "adhesion"
  | Epers_BaptismLDS -> `String "baptismlds"
  | Epers_BarMitzvah -> `String "barmitzvah"
  | Epers_BatMitzvah -> `String "batmitzvah"
  | Epers_Benediction -> `String "benediction"
  | Epers_ChangeName -> `String "changename"
  | Epers_Circumcision -> `String "circumcision"
  | Epers_Confirmation -> `String "confirmation"
  | Epers_ConfirmationLDS -> `String "confirmationlds"
  | Epers_Decoration -> `String "decoration"
  | Epers_DemobilisationMilitaire -> `String "demobilisationmilitaire"
  | Epers_Diploma -> `String "diploma"
  | Epers_Distinction -> `String "distinction"
  | Epers_Dotation -> `String "dotation"
  | Epers_DotationLDS -> `String "dotationlds"
  | Epers_Education -> `String "education"
  | Epers_Election -> `String "election"
  | Epers_Emigration -> `String "emigration"
  | Epers_Excommunication -> `String "excommunication"
  | Epers_FamilyLinkLDS -> `String "familylinklds"
  | Epers_FirstCommunion -> `String "firstcommunion"
  | Epers_Funeral -> `String "funeral"
  | Epers_Graduate -> `String "graduate"
  | Epers_Hospitalisation -> `String "hospitalisation"
  | Epers_Illness -> `String "illness"
  | Epers_Immigration -> `String "immigration"
  | Epers_ListePassenger -> `String "listepassenger"
  | Epers_MilitaryDistinction -> `String "militarydistinction"
  | Epers_MilitaryPromotion -> `String "militarypromotion"
  | Epers_MilitaryService -> `String "militaryservice"
  | Epers_MobilisationMilitaire -> `String "mobilisationmilitaire"
  | Epers_Naturalisation -> `String "naturalisation"
  | Epers_Occupation -> `String "occupation"
  | Epers_Ordination -> `String "ordination"
  | Epers_Property -> `String "property"
  | Epers_Recensement -> `String "recensement"
  | Epers_Residence -> `String "residence"
  | Epers_Retired -> `String "retired"
  | Epers_ScellentChildLDS -> `String "scellentchildlds"
  | Epers_ScellentParentLDS -> `String "scellentparentlds"
  | Epers_ScellentSpouseLDS -> `String "scellentspouselds"
  | Epers_VenteBien -> `String "ventebien"
  | Epers_Will -> `String "will"
  | Epers_Name name -> json_of_istr base name


let json_of_dmy dmy = `Assoc [
  ("day", `Int dmy.day);
  ("month", `Int dmy.month);
  ("year", `Int dmy.year);
]

let json_of_dmy2 dmy = `Assoc [
  ("day", `Int dmy.day2);
  ("month", `Int dmy.month2);
  ("year", `Int dmy.year2);
]

let json_of_date_cal dt cal =
  let date1 = json_of_dmy dt in
  let prec = match dt.prec with
    Sure -> "sure"
  | About -> "about"
  | Maybe -> "maybe"
  | Before -> "before"
  | After -> "after"
  | OrYear _ -> "or"
  | YearInt _ -> "between"
  in
  let date2 = match dt.prec with
    OrYear dmy2 -> json_of_dmy2 dmy2
  | YearInt dmy2 -> json_of_dmy2 dmy2
  | _ -> `Null
  in
  `Assoc [
    ("prec", `String prec);
    ("dmy1", date1);
    ("dmy2", date2);
    ("calendar", `String cal);
  ]

let json_of_date oc =
  match oc with
    Dgreg (d, Dgregorian) -> json_of_date_cal d "gregorian"
  | Dgreg (d, Djulian) -> json_of_date_cal d "julian"
  | Dgreg (d, Dfrench) -> json_of_date_cal d "french"
  | Dgreg (d, Dhebrew) -> json_of_date_cal d "hebrew"
  | Dtext t -> `String t

let json_of_cdate cd = match Adef.od_of_cdate cd with
    None -> `Null
  | Some date -> json_of_date date

let json_of_pevent base pevent = `Assoc [
  ("place", json_of_istr base (pevent.epers_place));
  ("reason", json_of_istr base (pevent.epers_reason));
  ("note", json_of_istr base (pevent.epers_note));
  ("src", json_of_istr base (pevent.epers_src));
  ("name", string_of_pevent_name base pevent.epers_name);
  ("date", json_of_cdate pevent.epers_date);
]

let rec json_of_pevents base pevents json =
  match pevents with
    [] -> json
  | pevent::remains -> let json = [json_of_pevent base pevent] @ json in json_of_pevents base remains json

let json_of_title_name base name = match name with
    Tmain -> `String ""
  | Tname s -> json_of_istr base s
  | Tnone -> `Null

let json_of_title base gen_title = `Assoc [
  ("name", json_of_title_name base gen_title.t_name);
  ("date_start", json_of_cdate gen_title.t_date_start);
  ("date_end", json_of_cdate gen_title.t_date_end);
  ("nth", `Int gen_title.t_nth);
  ("ident", json_of_istr base gen_title.t_ident);
  ("place", json_of_istr base gen_title.t_place)
]

let rec json_of_titles base gen_titles json =
  match gen_titles with
    [] -> json
  | gen_title::remains -> let json = [json_of_title base gen_title] @ json in json_of_titles base remains json

let json_of_relation_type r_type = match r_type with
    Adoption -> `String "adoption"
  | Recognition -> `String "recognition"
  | CandidateParent -> `String "candidate_parent"
  | GodParent -> `String "god_parent"
  | FosterParent -> `String "foster_parent"

let json_of_iper_option iper_opt = match iper_opt with
    Some p -> `Int (Adef.int_of_iper p)
  | None -> `Null

let json_of_rparent base gen_relation = `Assoc [
  ("type", json_of_relation_type gen_relation.r_type );
  ("source", json_of_istr base gen_relation.r_sources);
  ("father", json_of_iper_option gen_relation.r_fath);
  ("mother", json_of_iper_option gen_relation.r_moth);
]

let rec json_of_rparents base gen_relations json =
  match gen_relations with
    [] -> json
  | gen_relation::remains -> let json = [json_of_rparent base gen_relation] @ json in json_of_rparents base remains json

let rec json_of_ipers_list related json =
  match related with
    [] -> json
  | rel::remains -> let json = [`Int (Adef.int_of_iper rel)] @ json in json_of_ipers_list remains json

let json_of_person base person =
  let firstname = json_of_istr base (get_first_name person) in
  let lastname =  json_of_istr base (get_surname person) in
  let occ = get_occ person in
  let key = make_key (sou base (get_surname person)) (sou base (get_first_name person)) occ in
  let index = Adef.int_of_iper (get_key_index person) in
  let access = match get_access person with
      Private -> 2
    | Public  -> 1
    | _ -> 0
  in
  let sex = match get_sex person with
      Male -> 1
    | Female -> 2
    | _ -> 0
  in
  let image = json_of_istr base (get_image person) in
  let public_name = json_of_istr base (get_public_name person) in
  let qualifiers = json_list_of_istr_list base (get_qualifiers person) [] in
  let aliases = json_list_of_istr_list base (get_aliases person) [] in
  let first_names_aliases = json_list_of_istr_list base (get_first_names_aliases person) [] in
  let surnames_aliases = json_list_of_istr_list base (get_surnames_aliases person) [] in
  let titles = json_of_titles base (get_titles person) [] in
  let pevents = json_of_pevents base (get_pevents person) [] in
  let notes =  json_of_istr base (get_notes person) in
  let psources = json_of_istr base (get_psources person) in
  let rparents = json_of_rparents base (get_rparents person) [] in
  let related = json_of_ipers_list (get_related person) [] in
    `Assoc [
      ("index", `Int index);
      ("key", `String key);
      ("firstname", firstname);
      ("lastname", lastname);
      ("sex", `Int sex);
      ("occ", `Int occ);
      ("access", `Int access);
      ("image", image);
      ("public_name", public_name);
      ("qualifiers" , `List qualifiers);
      ("aliases" , `List aliases);
      ("titles", `List titles);
      ("first_names_aliases" , `List first_names_aliases);
      ("surnames_aliases" , `List surnames_aliases);
      ("pevents" , `List pevents);
      ("note", notes);
      ("psources", psources);
      ("related", `List related);
      ("rparents", `List rparents)
    ]

let json_of_relation_kind relation_kind = match relation_kind with
    Married -> `String "married"
  | NotMarried ->  `String "not_married"
  | Engaged ->  `String  "engaged"
  | NoSexesCheckNotMarried ->  `String  "no_sexes_check_not_married"
  | NoMention ->  `String  "no_mention"
  | NoSexesCheckMarried -> `String "no_sexes_check_married"

let json_of_divorce divorce = match divorce with
    NotDivorced -> `Null
  | Divorced date -> json_of_cdate date
  | Separated -> `Bool true

let json_of_fevent_name base name = match name with
    Efam_Marriage -> `String "marriage"
  | Efam_NoMarriage -> `String "no_marriage"
  | Efam_NoMention -> `String "no_mention"
  | Efam_Engage -> `String "engaged"
  | Efam_Divorce -> `String "divorce"
  | Efam_Separated -> `String "separated"
  | Efam_Annulation -> `String "annulation"
  | Efam_MarriageBann -> `String "marriage_bann"
  | Efam_MarriageContract -> `String "marriage_contract"
  | Efam_MarriageLicense -> `String "marriage_license"
  | Efam_PACS -> `String "pacs"
  | Efam_Residence -> `String "residence"
  | Efam_Name s -> json_of_istr base s

let json_of_fevent_witness_kind witness_kind = match witness_kind with
    Witness -> `String "witness"
  | Witness_GodParent -> `String "godparent"
  | Witness_Officer -> `String "officer"

let json_of_fevent_witness witness = match witness with
  (person , witness_kind) -> `Assoc [
    ("person", `Int (Adef.int_of_iper person));
    ("type", json_of_fevent_witness_kind witness_kind)
  ]

let rec json_of_fevent_witnesses witnesses json =
  match witnesses with
    [] -> json
  | witness::remains -> let json = [json_of_fevent_witness witness] @ json in json_of_fevent_witnesses remains json

let json_of_fevent base fevent = `Assoc [
  ("place", json_of_istr base (fevent.efam_place));
  ("reason", json_of_istr base (fevent.efam_reason));
  ("note", json_of_istr base (fevent.efam_note));
  ("src", json_of_istr base (fevent.efam_src));
  ("name", (json_of_fevent_name base fevent.efam_name));
  ("date", json_of_cdate fevent.efam_date);
  ("witnesses", `List (json_of_fevent_witnesses (Array.to_list fevent.efam_witnesses) []));
]

let rec json_of_fevents base pevents json =
  match pevents with
    [] -> json
  | pevent::remains -> let json = [json_of_fevent base pevent] @ json in json_of_fevents base remains json

let json_of_family base family =
  let marriage_place = json_of_istr base (get_marriage_place family) in
  let marriage = json_of_cdate (get_marriage family) in
  let marriage_note = json_of_istr base (get_marriage_note family) in
  let marriage_src = json_of_istr base (get_marriage_src family) in
  let divorce = json_of_divorce (get_divorce family) in
  let relation_kind = json_of_relation_kind (get_relation family) in
  let fevents = json_of_fevents base (get_fevents family) [] in
  let comment = json_of_istr base (get_comment family) in
  let origin_file = json_of_istr base (get_origin_file family) in
  let fsources = json_of_istr base (get_fsources family) in
  (* let fam_index = Adef.int_of_ifam (get_key_index family) in *)
  let witnesses_list = Array.to_list (get_witnesses family) in
  let witnesses = json_of_ipers_list witnesses_list [] in
  let children = json_of_ipers_list (Array.to_list (get_children family)) [] in
  let parents = json_of_ipers_list (Array.to_list (get_parent_array family)) [] in
    `Assoc [
      ("marriage_place", marriage_place);
      ("marriage", marriage);
      ("marriage_note", marriage_note);
      ("marriage_src", marriage_src);
      ("divorce", divorce);
      ("relation_kind", relation_kind);
      ("fevents", `List fevents);
      ("comment", comment);
      ("origin_file", origin_file);
      ("fsources", fsources);
      ("witnesses", `List witnesses);
      ("children", `List children);
      ("parents", `List parents);
    ]
