module Driver = Geneweb_db.Driver

module type S = sig
  type ctx
  type 'a feeder = 'a -> ctx -> ctx
  type 'a hasher = ?salt:string -> 'a -> string

  val feeder_to_hasher : 'a feeder -> 'a hasher
  val ( <+> ) : (ctx -> ctx) -> (ctx -> ctx) -> ctx -> ctx
  val string : string feeder
  val int : int feeder
  val bool : bool feeder
  val list : 'a feeder -> 'a list feeder
  val array : 'a feeder -> 'a array feeder
  val pair : 'a feeder -> 'b feeder -> ('a * 'b) feeder
  val option : 'a feeder -> 'a option feeder
  val iper : Geneweb_db.Driver.iper feeder
  val ifam : Geneweb_db.Driver.ifam feeder
  val istr : Geneweb_db.Driver.istr feeder
  val calendar : Adef.calendar feeder
  val dmy2 : Adef.dmy2 feeder
  val precision : Adef.precision feeder
  val dmy : Adef.dmy feeder
  val date : Adef.date feeder
  val cdate : Adef.cdate feeder
  val death_reason : Def.death_reason feeder
  val death : Def.death feeder
  val gen_title_name : 'string feeder -> 'string Def.gen_title_name feeder
  val gen_title : 'string feeder -> 'string Def.gen_title feeder
  val sex : Def.sex feeder
  val access : Def.access feeder
  val relation_type : Def.relation_type feeder
  val gen_couple : 'person feeder -> 'person Adef.gen_couple feeder
  val gen_union : 'family feeder -> 'family Def.gen_union feeder
  val gen_descend : 'person feeder -> 'person Def.gen_descend feeder

  val gen_relation :
    'person feeder ->
    'string feeder ->
    ('person, 'string) Def.gen_relation feeder

  val burial : Def.burial feeder

  val gen_pers_event_name :
    'string feeder -> 'string Def.gen_pers_event_name feeder

  val witness_kind : Def.witness_kind feeder

  val gen_pers_event :
    'person feeder ->
    'string feeder ->
    ('person, 'string) Def.gen_pers_event feeder

  val gen_person :
    'iper feeder ->
    'person feeder ->
    'string feeder ->
    ('iper, 'person, 'string) Def.gen_person feeder

  val divorce : Def.divorce feeder

  val gen_fam_event_name :
    'string feeder -> 'string Def.gen_fam_event_name feeder

  val gen_fam_event :
    'person feeder ->
    'string feeder ->
    ('person, 'string) Def.gen_fam_event feeder

  val relation_kind : Def.relation_kind feeder

  val gen_family :
    'person feeder ->
    'ifam feeder ->
    'string feeder ->
    ('person, 'ifam, 'string) Def.gen_family feeder
end

module Make (H : Digestif.S) = struct
  type ctx = H.ctx
  type 'a feeder = 'a -> ctx -> ctx
  type 'a hasher = ?salt:string -> 'a -> string

  let ( <+> ) f g ctx = f (g ctx)

  let feeder_to_hasher f ?salt u =
    let ctx = H.init () in
    let h =
      match salt with None -> f u ctx | Some s -> H.feed_string (f u ctx) s
    in
    H.to_hex @@ H.get h

  let string s ctx = H.feed_string ctx s
  let int i = string @@ string_of_int i
  let bool b = string (if b then "true" else "false")
  let list f l ctx = List.fold_left (fun ctx a -> f a ctx) ctx l
  let array f l ctx = Array.fold_left (fun ctx a -> f a ctx) ctx l
  let pair f g (x, y) = f x <+> g y

  let option f x =
    match x with Some u -> string "Some" <+> f u | None -> string "None"

  let dmy2 Adef.{ day2; month2; year2; delta2 } =
    int day2 <+> int month2 <+> int year2 <+> int delta2

  let precision (p : Adef.precision) =
    match p with
    | Sure -> string "Sure"
    | About -> string "about"
    | Maybe -> string "Maybe"
    | Before -> string "Before"
    | After -> string "After"
    | OrYear d -> string "OrYear" <+> dmy2 d
    | YearInt d -> string "YearInt" <+> dmy2 d

  let dmy Adef.{ day; month; year; prec; delta } =
    int day <+> int month <+> int year <+> precision prec <+> int delta

  let iper (i : Geneweb_db.Driver.iper) = string @@ Driver.Iper.to_string i
  let ifam (i : Geneweb_db.Driver.ifam) = string @@ Driver.Ifam.to_string i
  let istr (i : Geneweb_db.Driver.istr) = string @@ Driver.Istr.to_string i

  let calendar (c : Adef.calendar) =
    match c with
    | Dgregorian -> string "Dgregorian"
    | Djulian -> string "Djulian"
    | Dfrench -> string "Dfrench"
    | Dhebrew -> string "Dhebrew"

  let date (d : Adef.date) =
    match d with
    | Dgreg (d, c) -> dmy d <+> calendar c
    | Dtext s -> string "Dtext" <+> string s

  let cdate (c : Adef.cdate) =
    match c with
    | Cgregorian i -> string "Cgregorian" <+> int i
    | Cjulian i -> string "Cjulian" <+> int i
    | Cfrench i -> string "Cfrench" <+> int i
    | Chebrew i -> string "Chebrew" <+> int i
    | Ctext s -> string "Ctext" <+> string s
    | Cdate d -> string "Cdate" <+> date d
    | Cnone -> string "Cnone"

  let death_reason (r : Def.death_reason) =
    match r with
    | Killed -> string "Killed"
    | Murdered -> string "Murdered"
    | Executed -> string "Executed"
    | Disappeared -> string "Disappeared"
    | Unspecified -> string "Unspecified"

  let death (d : Def.death) =
    match d with
    | NotDead -> string "NotDead"
    | Death (r, c) -> death_reason r <+> cdate c
    | DeadYoung -> string "DeadYoung"
    | DeadDontKnowWhen -> string "DeadDontKnowWhen"
    | DontKnowIfDead -> string "DontKnowIfDead"
    | OfCourseDead -> string "OfCourseDead"

  let gen_title_name feed_string (t : _ Def.gen_title_name) =
    match t with
    | Tmain -> string "Tmain"
    | Tname s -> string "Tname" <+> feed_string s
    | Tnone -> string "Tnone"

  let gen_title feed_string
      Def.{ t_name; t_ident; t_place; t_date_start; t_date_end; t_nth } =
    ((gen_title_name feed_string) t_name
    <+> feed_string t_ident
    <+> feed_string t_place
    <+> cdate t_date_start
    <+> cdate t_date_end
    <+> int t_nth)
    [@ocamlformat "disable"]

  let sex (s : Def.sex) =
    match s with
    | Male -> string "Male"
    | Female -> string "Female"
    | Neuter -> string "Neuter"

  let access (a : Def.access) =
    match a with
    | IfTitles -> string "IfTitles"
    | Public -> string "Public"
    | SemiPublic -> string "SemiPublic"
    | Private -> string "Private"

  let relation_type (r : Def.relation_type) =
    match r with
    | Adoption -> string "Adoption"
    | Recognition -> string "Recognition"
    | CandidateParent -> string "CandidateParent"
    | GodParent -> string "GodParent"
    | FosterParent -> string "FosterParent"

  let gen_relation feed_pers feed_string
      Def.{ r_type; r_fath; r_moth; r_sources } =
    relation_type r_type
    <+> (option feed_pers) r_fath
    <+> (option feed_pers) r_moth
    <+> feed_string r_sources

  let burial (b : Def.burial) =
    match b with
    | UnknownBurial -> string "UnknownBurial"
    | Buried c -> string "Buried" <+> cdate c
    | Cremated c -> string "Cremated" <+> cdate c

  let gen_pers_event_name feed_string (e : _ Def.gen_pers_event_name) =
    match e with
    | Epers_Birth -> string "Epers_Birth"
    | Epers_Baptism -> string "Epers_Baptism"
    | Epers_Death -> string "Epers_Death"
    | Epers_Burial -> string "Epers_Burial"
    | Epers_Cremation -> string "Epers_Cremation"
    | Epers_Accomplishment -> string "Epers_Accomplishment"
    | Epers_Acquisition -> string "Epers_Acquisition"
    | Epers_Adhesion -> string "Epers_Adhesion"
    | Epers_BaptismLDS -> string "Epers_BaptismLDS"
    | Epers_BarMitzvah -> string "Epers_BarMitzvah"
    | Epers_BatMitzvah -> string "Epers_BatMitzvah"
    | Epers_Benediction -> string "Epers_Benediction"
    | Epers_ChangeName -> string "Epers_ChangeName"
    | Epers_Circumcision -> string "Epers_Circumcision"
    | Epers_Confirmation -> string "Epers_Confirmation"
    | Epers_ConfirmationLDS -> string "Epers_ConfirmationLDS"
    | Epers_Decoration -> string "Epers_Decoration"
    | Epers_DemobilisationMilitaire -> string "Epers_DemobilisationMilitaire"
    | Epers_Diploma -> string "Epers_Diploma"
    | Epers_Distinction -> string "Epers_Distinction"
    | Epers_Dotation -> string "Epers_Dotation"
    | Epers_DotationLDS -> string "Epers_DotationLDS"
    | Epers_Education -> string "Epers_Education"
    | Epers_Election -> string "Epers_Election"
    | Epers_Emigration -> string "Epers_Emigration"
    | Epers_Excommunication -> string "Epers_Excommunication"
    | Epers_FamilyLinkLDS -> string "Epers_FamilyLinkLDS"
    | Epers_FirstCommunion -> string "Epers_FirstCommunion"
    | Epers_Funeral -> string "Epers_Funeral"
    | Epers_Graduate -> string "Epers_Graduate"
    | Epers_Hospitalisation -> string "Epers_Hospitalisation"
    | Epers_Illness -> string "Epers_Illness"
    | Epers_Immigration -> string "Epers_Immigration"
    | Epers_ListePassenger -> string "Epers_ListePassenger"
    | Epers_MilitaryDistinction -> string "Epers_MilitaryDistinction"
    | Epers_MilitaryPromotion -> string "Epers_MilitaryPromotion"
    | Epers_MilitaryService -> string "Epers_MilitaryService"
    | Epers_MobilisationMilitaire -> string "Epers_MobilisationMilitaire"
    | Epers_Naturalisation -> string "Epers_Naturalisation"
    | Epers_Occupation -> string "Epers_Occupation"
    | Epers_Ordination -> string "Epers_Ordination"
    | Epers_Property -> string "Epers_Property"
    | Epers_Recensement -> string "Epers_Recensement"
    | Epers_Residence -> string "Epers_Residence"
    | Epers_Retired -> string "Epers_Retired"
    | Epers_ScellentChildLDS -> string "Epers_ScellentChildLDS"
    | Epers_ScellentParentLDS -> string "Epers_ScellentParentLDS"
    | Epers_ScellentSpouseLDS -> string "Epers_ScellentSpouseLDS"
    | Epers_VenteBien -> string "Epers_VenteBien"
    | Epers_Will -> string "Epers_Will"
    | Epers_Name s -> string "Epers_Name" <+> feed_string s

  let witness_kind (w : Def.witness_kind) =
    match w with
    | Witness -> string "Witness"
    | Witness_GodParent -> string "Witness_GodParent"
    | Witness_CivilOfficer -> string "Witness_CivilOfficer"
    | Witness_ReligiousOfficer -> string "Witness_ReligiousOfficer"
    | Witness_Informant -> string "Witness_Informant"
    | Witness_Attending -> string "Witness_Attending"
    | Witness_Mentioned -> string "Witness_Mentioned"
    | Witness_Other -> string "Witness_Other"

  let gen_pers_event feed_pers feed_string
      Def.
        {
          epers_name;
          epers_date;
          epers_place;
          epers_reason;
          epers_note;
          epers_src;
          epers_witnesses;
        } =
    ((gen_pers_event_name feed_string) epers_name
    <+> cdate epers_date
    <+> feed_string epers_place
    <+> feed_string epers_reason
    <+> feed_string epers_note
    <+> feed_string epers_note
    <+> feed_string epers_src
    <+> (array @@ pair feed_pers witness_kind) epers_witnesses)
    [@ocamlformat "disable"]

  let gen_person feed_iper feed_pers feed_string
      Def.
        {
          first_name;
          surname;
          occ;
          image;
          public_name;
          qualifiers;
          aliases;
          first_names_aliases;
          surnames_aliases;
          titles;
          rparents;
          related;
          occupation;
          sex = s;
          access = a;
          birth;
          birth_place;
          birth_note;
          birth_src;
          baptism;
          baptism_place;
          baptism_note;
          baptism_src;
          death = d;
          death_place;
          death_note;
          death_src;
          burial = b;
          burial_place;
          burial_note;
          burial_src;
          pevents;
          notes;
          psources;
          key_index;
        } =
    (feed_string first_name
    <+> feed_string surname
    <+> int occ
    <+> feed_string image
    <+> feed_string public_name
    <+> (list feed_string) qualifiers
    <+> (list feed_string) aliases
    <+> (list feed_string) first_names_aliases
    <+> (list feed_string) surnames_aliases
    <+> (list @@ gen_title feed_string) titles
    <+> feed_string occupation
    <+> (list @@ gen_relation feed_pers feed_string) rparents
    <+> (list feed_pers) related
    <+> sex s
    <+> access a
    <+> cdate birth
    <+> feed_string birth_place
    <+> feed_string birth_note
    <+> feed_string birth_src
    <+> cdate baptism
    <+> feed_string baptism_place
    <+> feed_string baptism_note
    <+> feed_string baptism_src
    <+> death d
    <+> feed_string death_place
    <+> feed_string death_note
    <+> feed_string death_src
    <+> burial b
    <+> feed_string burial_place
    <+> feed_string burial_note
    <+> feed_string burial_src
    <+> (list @@ gen_pers_event feed_pers feed_string) pevents
    <+> feed_string notes
    <+> feed_string psources
    <+> feed_iper key_index)
    [@ocamlformat "disable"]

  let divorce (d : Def.divorce) =
    match d with
    | NotDivorced -> string "NotDivorced"
    | Divorced c -> string "Divorced" <+> cdate c
    | Separated_old -> string "Separated_old"
    | NotSeparated -> string "NotSeparated"
    | Separated c -> string "Separated" <+> cdate c

  let gen_fam_event_name feed_string (e : _ Def.gen_fam_event_name) =
    match e with
    | Efam_Marriage -> string "Efam_Marriage"
    | Efam_NoMarriage -> string "Efam_NoMarriage"
    | Efam_NoMention -> string "Efam_NoMention"
    | Efam_Engage -> string "Efam_Engage"
    | Efam_Divorce -> string "Efam_Divorce"
    | Efam_Separated -> string "Efam_Separated"
    | Efam_Annulation -> string "Efam_Annulation"
    | Efam_MarriageBann -> string "Efam_MarriageBann"
    | Efam_MarriageContract -> string "Efam_MarriageContract"
    | Efam_MarriageLicense -> string "Efam_MarriageLicense"
    | Efam_PACS -> string "Efam_PACS"
    | Efam_Residence -> string "Efam_Residence"
    | Efam_Name s -> string "Efam_Name" <+> feed_string s

  let gen_fam_event feed_pers feed_string
      Def.
        {
          efam_name;
          efam_date;
          efam_place;
          efam_reason;
          efam_note;
          efam_src;
          efam_witnesses;
        } =
    ((gen_fam_event_name feed_string) efam_name
    <+> cdate efam_date
    <+> feed_string efam_place
    <+> feed_string efam_reason
    <+> feed_string efam_note
    <+> feed_string efam_src
    <+> (array @@ pair feed_pers witness_kind) efam_witnesses)
    [@ocamlformat "disable"]

  let relation_kind (k : Def.relation_kind) =
    match k with
    | Married -> string "Married"
    | NotMarried -> string "NotMarried"
    | Engaged -> string "Engaged"
    | NoSexesCheckNotMarried -> string "NoSexesCheckNotMarried"
    | NoMention -> string "NoMention"
    | NoSexesCheckMarried -> string "NoSexesCheckMarried"
    | MarriageBann -> string "MarriageBann"
    | MarriageContract -> string "MarriageContract"
    | MarriageLicense -> string "MarriageLicense"
    | Pacs -> string "Pacs"
    | Residence -> string "Residence"

  let gen_couple feed_pers Adef.{ father; mother } =
    feed_pers father <+> feed_pers mother

  let gen_union feed_fam Def.{ family } = array feed_fam family
  let gen_descend feed_pers Def.{ children } = array feed_pers children

  let gen_family feed_pers feed_ifam feed_string
      Def.
        {
          marriage;
          marriage_place;
          marriage_note;
          marriage_src;
          witnesses;
          relation;
          divorce = d;
          fevents;
          comment;
          origin_file;
          fsources;
          fam_index;
        } =
    (cdate marriage
    <+> feed_string marriage_place
    <+> feed_string marriage_note
    <+> feed_string marriage_src
    <+> (array feed_pers) witnesses
    <+> relation_kind relation
    <+> divorce d
    <+> (list @@ gen_fam_event feed_pers feed_string) fevents
    <+> feed_string comment
    <+> feed_string origin_file
    <+> feed_string fsources
    <+> feed_ifam fam_index)
    [@ocamlformat "disable"]
end

module SHA256 = Make (Digestif.SHA256)
