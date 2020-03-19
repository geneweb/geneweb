open Def

module J = Yojson.Basic.Util

let json_of_istr s = `String s

let key_of_iper bname i =
  `String (bname ^ ":" ^ string_of_int i)

let key_of_ifam bname i =
  `String (bname ^ ":" ^ string_of_int i)

let handler_of_iper bname i =
  `String ("geneweb_persons/" ^ bname ^ ":" ^ string_of_int i)

let handler_of_ifam bname i =
  `String ("geneweb_families/" ^ bname ^ ":" ^ string_of_int i)

let rec filter_out_null ?(except = []) (`Assoc a) =
  `Assoc begin List.filter_map begin function
      | (f, (`String ""|`Null|`List[]|`Assoc[]))
        when not @@ List.mem f except -> None
      | (f, (`Assoc _ as a)) -> begin match filter_out_null ~except a with
          | `Assoc [] -> None
          | a -> Some (f, a)
        end
      | x -> Some x
    end a end

let to_string = function
  | `String s -> s
  | `Null -> ""
  | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (Yojson.Basic.to_string x)

let get_string ~__LOC__ js name =
  (* print_endline __LOC__ ; *)
  to_string @@ J.member name js

let get_int ~__LOC__:_ js name =
  (* print_endline __LOC__ ; *)
  J.to_int (J.member name js)

let get_list name fn js =
  match J.member name js with
  | `List l -> List.map fn l
  | `Null -> []
  | _ -> failwith __LOC__

(** gwdb to json  *)

let json_of_dmy dmy =
  filter_out_null @@
  `Assoc [ "day", `Int dmy.day
         ; "delta", (if dmy.delta = 0 then `Null else `Int dmy.delta)
         ; "month", `Int dmy.month
         ; "year", `Int dmy.year
         ]

let json_of_dmy2 dmy =
  `Assoc [ "day", `Int dmy.day2
         ; "month", `Int dmy.month2
         ; "year", `Int dmy.year2
         ]

let json_of_date_cal dt cal =
  let date1 = json_of_dmy dt in
  let prec = match dt.prec with
    | Sure -> "sure"
    | About -> "about"
    | Maybe -> "maybe"
    | Before -> "before"
    | After -> "after"
    | OrYear _ -> "or"
    | YearInt _ -> "between"
  in
  let date2 = match dt.prec with
    | OrYear dmy2 -> json_of_dmy2 dmy2
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
  | Dgreg (d, Dgregorian) -> json_of_date_cal d "gregorian"
  | Dgreg (d, Djulian) -> json_of_date_cal d "julian"
  | Dgreg (d, Dfrench) -> json_of_date_cal d "french"
  | Dgreg (d, Dhebrew) -> json_of_date_cal d "hebrew"
  | Dtext t -> `String t

let json_of_cdate cd = match Adef.od_of_cdate cd with
  | None -> `Null
  | Some date -> json_of_date date

let json_of_pevent_name = function
  | Epers_Birth -> `String "birth"
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
  | Epers_Name name -> `String name

let json_of_event_witness_kind witness_kind = match witness_kind with
  | Witness -> `String "witness"
  | Witness_GodParent -> `String "godparent"

let json_of_event_witness bname (person, witness_kind) =
  filter_out_null @@
  `Assoc [ ("person", handler_of_iper bname person)
         ; ("type", json_of_event_witness_kind witness_kind)
         ]

let json_of_pevent bname pevent =
  `Assoc [ ("place", `String pevent.epers_place)
         ; ("reason", `String pevent.epers_reason)
         ; ("note", `String pevent.epers_note)
         ; ("src", `String pevent.epers_src)
         ; ("name", json_of_pevent_name pevent.epers_name)
         ; ("date", json_of_cdate pevent.epers_date)
         ; ("witnesses", `List (Array.to_list @@
                                Array.map (json_of_event_witness bname) pevent.epers_witnesses) )
         ]

let json_of_title_name = function
  | Tmain -> `String ""
  | Tname s -> `String s
  | Tnone -> `Null

let json_of_title gen_title =
  `Assoc [ ("name", json_of_title_name gen_title.t_name)
         ; ("date_start", json_of_cdate gen_title.t_date_start)
         ; ("date_end", json_of_cdate gen_title.t_date_end)
         ; ("nth", `Int gen_title.t_nth)
         ; ("ident", `String gen_title.t_ident)
         ; ("place", `String gen_title.t_place)
         ]

let json_of_relation_kind = function
  | Married -> `String "married"
  | NotMarried -> `String "not_married"
  | Engaged -> `String  "engaged"
  | NoSexesCheckNotMarried -> `String "no_sexes_check_not_married"
  | NoMention -> `String "no_mention"
  | NoSexesCheckMarried -> `String "no_sexes_check_married"
  | MarriageBann -> `String "marriage_bann"
  | MarriageContract -> `String "marriage_contract"
  | MarriageLicense -> `String "marriage_license"
  | Pacs -> `String "pacs"
  | Residence -> `String "residence"

let json_of_fevent_name = function
  | Efam_Marriage -> `String "marriage"
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
  | Efam_Name s -> `String s

let json_of_fevent bname fevent =
  `Assoc [ "date", json_of_cdate fevent.efam_date
         ; "name", json_of_fevent_name fevent.efam_name
         ; "note", `String fevent.efam_note
         ; "place", `String fevent.efam_place
         ; "reason", `String fevent.efam_reason
         ; "src", `String fevent.efam_src
         ; "witnesses"
         , `List (Array.to_list @@
                  Array.map (json_of_event_witness bname) fevent.efam_witnesses)
         ]

let json_of_divorce = function
  | NotDivorced -> `Bool false
  | Divorced date -> json_of_cdate date
  | Separated -> `Bool true

let json_of_relation_type = function
  | Adoption -> `String "adoption"
  | Recognition -> `String "recognition"
  | CandidateParent -> `String "candidate_parent"
  | GodParent -> `String "god_parent"
  | FosterParent -> `String "foster_parent"

let json_of_rparent bname gen_relation =
  `Assoc [ "father", (match gen_relation.r_fath with
             | Some i -> handler_of_iper bname i
             | None -> `Null)
         ; "mother", (match gen_relation.r_moth with
               | Some i -> handler_of_iper bname i
               | _ -> `Null)
         ; "source", `String gen_relation.r_sources
         ; "type", json_of_relation_type gen_relation.r_type
         ]

let json_of_death = function
  | Def.NotDead -> `String "notdead"
  | Death (Killed, _) -> `String "killed"
  | Death (Murdered, _) -> `String "murdered"
  | Death (Executed, _) -> `String "executed"
  | Death (Disappeared, _) -> `String "disappeared"
  | Death (Unspecified, _) -> `String "unspecified"
  | DeadYoung -> `String "deadyoung"
  | DeadDontKnowWhen -> `String "deaddontknowwhen"
  | DontKnowIfDead -> `String "dontknowifdead"
  | OfCourseDead -> `String "ofcoursedead"

let json_of_person bname p a u =
  filter_out_null @@
  `Assoc
    [ "access", `Int (match p.access with Private -> 2 | Public -> 1 | _ -> 0)
    ; "aliases", `List (List.map json_of_istr p.aliases)
    ; "death", json_of_death p.death
    ; "first_names_aliases", `List (List.map json_of_istr p.first_names_aliases)
    ; "firstname", json_of_istr p.first_name
    ; "image", json_of_istr p.image
    ; "lastname", json_of_istr p.surname
    ; "note", json_of_istr p.notes
    ; "occ", `Int p.occ
    ; "occupation", json_of_istr p.occupation
    ; "parents", (match a.parents with Some x -> handler_of_ifam bname x
                                     | None -> `Null)
    ; "consang"
    , if a.consang = Adef.no_consang then `Null
      else `Int (Adef.fix_repr a.consang)
    ; "pevents", `List (List.map (json_of_pevent bname) p.pevents)
    ; "psources", json_of_istr p.psources
    ; "public_name", json_of_istr p.public_name
    ; "qualifiers", `List (List.map json_of_istr p.qualifiers)
    ; "related", `List (List.map (handler_of_iper bname) p.related)
    ; "rparents", `List (List.map (json_of_rparent bname) p.rparents)
    ; "sex", `Int (match p.sex with Male -> 1 | Female -> 2 | _ -> 0)
    ; "surnames_aliases", `List (List.map json_of_istr p.surnames_aliases)
    ; "titles", `List (List.map json_of_title p.titles)
    ; "unions", `List (List.map (handler_of_ifam bname) (Array.to_list u.family) )
    ]

(* FIXME: do not include useless marriage and divorce fields (contained in fevents) *)
let json_of_family bname f c d =
  filter_out_null @@
  `Assoc
    [ ("marriage_place", json_of_istr f.marriage_place)
    ; ("marriage", json_of_cdate f.marriage)
    ; ("marriage_note", json_of_istr f.marriage_note)
    ; ("marriage_src", json_of_istr f.marriage_src)
    ; ("divorce", json_of_divorce f.divorce)
    ; ("relation_kind", json_of_relation_kind f.relation)
    ; ("fevents", `List (List.map (json_of_fevent bname) f.fevents))
    ; ("comment", json_of_istr f.comment)
    ; ("origin_file", json_of_istr f.origin_file)
    ; ("fsources", json_of_istr f.fsources)
    ; ("witnesses", `List (List.map (handler_of_iper bname) @@ Array.to_list f.witnesses))
    ; ("children", `List (List.map (handler_of_iper bname) @@ Array.to_list d.children))
    ; ("parents", `List (List.map (handler_of_iper bname) @@ Array.to_list @@ Adef.parent_array c))
    ]
