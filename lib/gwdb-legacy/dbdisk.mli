type fix = Adef.fix (* FIXME: expose its type *)
type cdate = Def.cdate (* FIXME: expose its type *)

type date = Def.date =
  | Dgreg of dmy * calendar
  | Dtext of string
and calendar = Def.calendar = Dgregorian | Djulian | Dfrench | Dhebrew
and dmy = Def.dmy =
  { day : int; month : int; year : int; prec : precision; delta : int }
and dmy2 = Def.dmy2 = { day2 : int; month2 : int; year2 : int; delta2 : int }
and precision = Def.precision =
  | Sure
  | About
  | Maybe
  | Before
  | After
  | OrYear of dmy2
  | YearInt of dmy2

type relation_kind = Def.relation_kind =
  | Married
  | NotMarried
  | Engaged
  | NoSexesCheckNotMarried
  | NoMention
  | NoSexesCheckMarried
  | MarriageBann
  | MarriageContract
  | MarriageLicense
  | Pacs
  | Residence

type divorce = Def.divorce =
  | NotDivorced
  | Divorced of cdate
  | Separated

type death_reason = Def.death_reason = Killed | Murdered | Executed | Disappeared | Unspecified

type death = Def.death =
  | NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead

type burial = Def.burial =
  | UnknownBurial
  | Buried of cdate
  | Cremated of cdate

type access = Def.access = IfTitles | Public | Private

type 'string gen_title_name = 'string Def.gen_title_name =
  | Tmain
  | Tname of 'string
  | Tnone

type 'string gen_title = 'string Def.gen_title =
  { t_name : 'string gen_title_name;
    t_ident : 'string;
    t_place : 'string;
    t_date_start : cdate;
    t_date_end : cdate;
    t_nth : int }

type witness_kind = Def.witness_kind = Witness | Witness_GodParent | Witness_Officer

type 'string gen_pers_event_name = 'string Def.gen_pers_event_name =
  | Epers_Birth
  | Epers_Baptism
  | Epers_Death
  | Epers_Burial
  | Epers_Cremation
  | Epers_Accomplishment
  | Epers_Acquisition
  | Epers_Adhesion
  | Epers_BaptismLDS
  | Epers_BarMitzvah
  | Epers_BatMitzvah
  | Epers_Benediction
  | Epers_ChangeName
  | Epers_Circumcision
  | Epers_Confirmation
  | Epers_ConfirmationLDS
  | Epers_Decoration
  | Epers_DemobilisationMilitaire
  | Epers_Diploma
  | Epers_Distinction
  | Epers_Dotation
  | Epers_DotationLDS
  | Epers_Education
  | Epers_Election
  | Epers_Emigration
  | Epers_Excommunication
  | Epers_FamilyLinkLDS
  | Epers_FirstCommunion
  | Epers_Funeral
  | Epers_Graduate
  | Epers_Hospitalisation
  | Epers_Illness
  | Epers_Immigration
  | Epers_ListePassenger
  | Epers_MilitaryDistinction
  | Epers_MilitaryPromotion
  | Epers_MilitaryService
  | Epers_MobilisationMilitaire
  | Epers_Naturalisation
  | Epers_Occupation
  | Epers_Ordination
  | Epers_Property
  | Epers_Recensement
  | Epers_Residence
  | Epers_Retired
  | Epers_ScellentChildLDS
  | Epers_ScellentParentLDS
  | Epers_ScellentSpouseLDS
  | Epers_VenteBien
  | Epers_Will
  | Epers_Name of 'string

type ('person, 'string) gen_pers_event = ('person, 'string) Def.gen_pers_event =
  { epers_name : 'string gen_pers_event_name;
    epers_date : cdate;
    epers_place : 'string;
    epers_reason : 'string;
    epers_note : 'string;
    epers_src : 'string;
    epers_witnesses : ('person * witness_kind) array }

type 'string gen_fam_event_name = 'string Def.gen_fam_event_name =
  | Efam_Marriage
  | Efam_NoMarriage
  | Efam_NoMention
  | Efam_Engage
  | Efam_Divorce
  | Efam_Separated
  | Efam_Annulation
  | Efam_MarriageBann
  | Efam_MarriageContract
  | Efam_MarriageLicense
  | Efam_PACS
  | Efam_Residence
  | Efam_Name of 'string

type ('person, 'string) gen_fam_event = ('person, 'string) Def.gen_fam_event =
  { efam_name : 'string gen_fam_event_name;
    efam_date : cdate;
    efam_place : 'string;
    efam_reason : 'string;
    efam_note : 'string;
    efam_src : 'string;
    efam_witnesses : ('person * witness_kind) array }

type relation_type = Def.relation_type =
  | Adoption
  | Recognition
  | CandidateParent
  | GodParent
  | FosterParent

type ('person, 'string) gen_relation = ('person, 'string) Def.gen_relation =
  { r_type : relation_type;
    r_fath : 'person option;
    r_moth : 'person option;
    r_sources : 'string }

type sex = Def.sex = Male | Female | Neuter

type place = Def.place =
  { other : string;
    town : string;
    township : string;
    canton : string;
    district : string;
    county : string;
    region : string;
    country : string }

(* person *)

type ('iper, 'person, 'string) gen_person = ('iper, 'person, 'string) Def.gen_person =
  { first_name : 'string;
    surname : 'string;
    occ : int;
    image : 'string;
    public_name : 'string;
    qualifiers : 'string list;
    aliases : 'string list;
    first_names_aliases : 'string list;
    surnames_aliases : 'string list;
    titles : 'string gen_title list;
    rparents : ('person, 'string) gen_relation list;
    related : 'person list;
    occupation : 'string;
    sex : sex;
    access : access;
    birth : cdate;
    birth_place : 'string;
    birth_note : 'string;
    birth_src : 'string;
    baptism : cdate;
    baptism_place : 'string;
    baptism_note : 'string;
    baptism_src : 'string;
    death : death;
    death_place : 'string;
    death_note : 'string;
    death_src : 'string;
    burial : burial;
    burial_place : 'string;
    burial_note : 'string;
    burial_src : 'string;
    pevents : ('person, 'string) gen_pers_event list;
    notes : 'string;
    psources : 'string;
    key_index : 'iper }

type 'family gen_ascend = 'family Def.gen_ascend = { parents : 'family option; consang : fix }

type 'family gen_union = 'family Def.gen_union = { family : 'family array }

(* family *)

type ('person, 'ifam, 'string) gen_family = ('person, 'ifam, 'string) Def.gen_family =
  { marriage : cdate;
    marriage_place : 'string;
    marriage_note : 'string;
    marriage_src : 'string;
    witnesses : 'person array;
    relation : relation_kind;
    divorce : divorce;
    fevents : ('person, 'string) gen_fam_event list;
    comment : 'string;
    origin_file : 'string;
    fsources : 'string;
    fam_index : 'ifam }

type 'person gen_couple = 'person Def.gen_couple (* FIXME: expose its type *)

type 'person gen_descend = 'person Def.gen_descend = { children : 'person array }

type dsk_person = (int, int, int) gen_person
type dsk_ascend = int gen_ascend
type dsk_union = int gen_union
type dsk_family = (int, int, int) gen_family
type dsk_couple = int gen_couple
type dsk_descend = int gen_descend

type dsk_title = int gen_title

type 'a record_access =
  { load_array : unit -> unit
  ; get : int -> 'a
  ; get_nopending : int -> 'a
  ; set : int -> 'a -> unit
  ; mutable len : int
  ; output_array : out_channel -> unit
  ; clear_array : unit -> unit
  }

type string_person_index =
  { find : int -> int list
  ; cursor : string -> int
  ; next : int -> int
  }

type visible_record_access =
  { v_write : unit -> unit; v_get : (dsk_person -> bool) -> int -> bool }

type perm = RDONLY | RDRW

type base_data =
  { persons : dsk_person record_access
  ; ascends : dsk_ascend record_access
  ; unions : dsk_union record_access
  ; visible : visible_record_access
  ; families : dsk_family record_access
  ; couples : dsk_couple record_access
  ; descends : dsk_descend record_access
  ; strings : string record_access
  ; particles_txt : string list
  ; particles : Re.re Lazy.t
  ; bnotes : Def.base_notes
  ; bdir : string
  ; perm : perm
  }

type base_func =
  { person_of_key : string -> string -> int -> int option
  ; persons_of_name : string -> int list
  ; strings_of_sname : string -> int list
  ; strings_of_fname : string -> int list
  ; persons_of_surname : string_person_index
  ; persons_of_first_name : string_person_index
  ; patch_person : int -> dsk_person -> unit
  ; patch_ascend : int -> dsk_ascend -> unit
  ; patch_union : int -> dsk_union -> unit
  ; patch_family : int -> dsk_family -> unit
  ; patch_couple : int -> dsk_couple -> unit
  ; patch_descend : int -> dsk_descend -> unit
  ; patch_name : string -> int -> unit
  ; insert_string : string -> int
  ; commit_patches : unit -> unit
  ; commit_notes : string -> string -> unit
  ; cleanup : unit -> unit
  ; nb_of_real_persons : unit -> int
  ; iper_exists : int -> bool
  ; ifam_exists : int -> bool
  }

type base_version = GnWb0020 | GnWb0021 | GnWb0022 | GnWb0023 | GnWb0024

type dsk_base = { data : base_data
                ; func : base_func
                ; version : base_version }
