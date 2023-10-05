(** {1 Aliases to [Def] and [Adef]} *)

type fix = Adef.fix (* FIXME: expose its type *)
type cdate = Def.cdate (* FIXME: expose its type *)

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

type divorce = Def.divorce = NotDivorced | Divorced of cdate | Separated

type death_reason = Def.death_reason =
  | Killed
  | Murdered
  | Executed
  | Disappeared
  | Unspecified

type death = Def.death =
  | NotDead
  | Death of death_reason * cdate
  | DeadYoung
  | DeadDontKnowWhen
  | DontKnowIfDead
  | OfCourseDead

type burial = Def.burial = UnknownBurial | Buried of cdate | Cremated of cdate
type access = Def.access = IfTitles | Public | Private

type 'string gen_title_name = 'string Def.gen_title_name =
  | Tmain
  | Tname of 'string
  | Tnone

type 'string gen_title = 'string Def.gen_title = {
  t_name : 'string gen_title_name;
  t_ident : 'string;
  t_place : 'string;
  t_date_start : cdate;
  t_date_end : cdate;
  t_nth : int;
}

type witness_kind = Def.witness_kind =
  | Witness
  | Witness_GodParent
  | Witness_CivilOfficer
  | Witness_ReligiousOfficer
  | Witness_Informant
  | Witness_Attending
  | Witness_Mentioned
  | Witness_Other

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

type ('person, 'string) gen_pers_event = {
  (*('person, 'string) Def.gen_pers_event =*)
  epers_name : 'string gen_pers_event_name;
  epers_date : cdate;
  epers_place : 'string;
  epers_reason : 'string;
  epers_note : 'string;
  epers_src : 'string;
  epers_witnesses : ('person * witness_kind) array;
}

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

type ('person, 'string) gen_fam_event = {
  (*('person, 'string) Def.gen_fam_event =*)
  efam_name : 'string gen_fam_event_name;
  efam_date : cdate;
  efam_place : 'string;
  efam_reason : 'string;
  efam_note : 'string;
  efam_src : 'string;
  efam_witnesses : ('person * witness_kind) array;
}

type relation_type = Def.relation_type =
  | Adoption
  | Recognition
  | CandidateParent
  | GodParent
  | FosterParent

type ('person, 'string) gen_relation = ('person, 'string) Def.gen_relation = {
  r_type : relation_type;
  r_fath : 'person option;
  r_moth : 'person option;
  r_sources : 'string;
}

type sex = Def.sex = Male | Female | Neuter

type place = Def.place = {
  other : string;
  town : string;
  township : string;
  canton : string;
  district : string;
  county : string;
  region : string;
  country : string;
}

(* person *)

type ('iper, 'person, 'string) gen_person = {
  first_name : 'string;
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
  key_index : 'iper;
}
(*= ('iper, 'person, 'string) Def.gen_person*)

type 'family gen_ascend = 'family Def.gen_ascend = {
  parents : 'family option;
  consang : fix;
}

type 'family gen_union = 'family Def.gen_union = { family : 'family array }

(* family *)

type ('person, 'ifam, 'string) gen_family = {
  (*('person, 'ifam, 'string) Def.gen_family =*)
  marriage : cdate;
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
  fam_index : 'ifam;
}

type 'person gen_couple = 'person Def.gen_couple (* FIXME: expose its type *)

type 'person gen_descend = 'person Def.gen_descend = {
  children : 'person array;
}

type dsk_person = (int, int, int) gen_person
(** Extended person's entry in the base *)

type dsk_ascend = int gen_ascend
(** Person's ascendants entry in the base *)

type dsk_union = int gen_union
(** Person's union entry in the base *)

type dsk_family = (int, int, int) gen_family
(** Family's entry in the base *)

type dsk_couple = int gen_couple
(** Family's couple entry in the base *)

type dsk_descend = int gen_descend
(** Family's descendants entry in the base *)

type dsk_title = int gen_title
(** Nobility title in the base *)

type 'a record_access = {
  (* Load array in the memory and cache it so it could be accessed
     instantly by other functions unless [clear_array] is called. *)
  load_array : unit -> unit;
  (* Get the nth element of array. In details, it searches for an element in
     the following order:
     - Search inside the pending patches
     - Search inside the commited patches
     - Search insede the loaded in memory array
     - Search inside the "base" file *)
  get : int -> 'a;
  (* Same as [get] but doesn't consider pending patches *)
  get_nopending : int -> 'a;
  (* Set the nth element of array *)
  set : int -> 'a -> unit;
  (* Return length of an array that by default takes into account
     commited patches *)
  mutable len : int;
  (* Output array with applied commited patches to the giving chanel *)
  output_array : out_channel -> unit;
  (* Remove array from the memory *)
  clear_array : unit -> unit;
}
(** Type that define the functions to use to access and manipulate with
    database arrays. *)

type string_person_index = {
  (* Find all person's ids that has giving surname/first name. *)
  find : int -> int list;
  (* Return surname's/first name's id. If it doen't present return id of the next
     name by alphabetical order *)
  cursor : string -> int;
  (* Return surname's/first name's id. If it doen't present return id of the next
     name by alphabetical order *)
  next : int -> int;
}
(** Data structure for optimised search throughout index by name
    (surname or first name). Considers also patched persons. *)

type visible_record_access = {
  v_write : unit -> unit;
  v_get : (dsk_person -> bool) -> int -> bool;
}

type perm = RDONLY | RDRW

type base_data = {
  (* Array of persons *)
  persons : dsk_person record_access;
  (* Array of persons' ascendants *)
  ascends : dsk_ascend record_access;
  (* Array of persons' unions *)
  unions : dsk_union record_access;
  (* unused by default *)
  visible : visible_record_access;
  (* Array of families *)
  families : dsk_family record_access;
  (* Array of families' couples *)
  couples : dsk_couple record_access;
  (* Array of families' descendants *)
  descends : dsk_descend record_access;
  (* Array of strings *)
  strings : string record_access;
  (* Array of autorised to use surname's particles *)
  particles_txt : string list;
  (* Regular expression that matches particles in [particles_txt]  *)
  particles : Re.re Lazy.t;
  (* Data base notes and extended page structure *)
  bnotes : Def.base_notes;
  (* Directory where database's files are stored *)
  bdir : string;
  perm : perm;
}
(** Data part of database *)

type base_func = {
  (* Return person's id from the giving key (first name, surname and occurene number).
     If person doesn't exists return None. Doesn't consider pending patches *)
  person_of_key : string -> string -> int -> int option;
  (* Return list of person ids that have giving name
     (could be one of the mix). Doesn't consider pending patches *)
  persons_of_name : string -> int list;
  (* Return list of surnames (string ids) that contain giving person's surname or surname substring.
     Consider also surnames of pathed persons. Doesn't consider pending patches *)
  strings_of_sname : string -> int list;
  (* Return list of first names (string ids) that contain giving person's first name or first name's
     substring. Consider also first names of pathed persons. Doesn't consider pending patches *)
  strings_of_fname : string -> int list;
  (* Return list of aliases (string ids) that contain giving person's alias substring.
     Consider also aliases of pathed persons. Doesn't consider pending patches *)
  strings_of_aname : string -> int list;
  (* Search functionalities throughout index by surname *)
  persons_of_surname : string_person_index;
  (* Search functionalities throughout index by first name *)
  persons_of_first_name : string_person_index;
  (* Search functionalities throughout index by alias *)
  persons_of_alias : string_person_index;
  (* Insert or modify person with a giving id (add to pending patches). *)
  patch_person : int -> dsk_person -> unit;
  (* Insert or modify ascendants of a person with a giving id (add to pending patches). *)
  patch_ascend : int -> dsk_ascend -> unit;
  (* Insert or modify union of a person with a giving id (add to pending patches). *)
  patch_union : int -> dsk_union -> unit;
  (* Insert or modify family with a giving id (add to pending patches). *)
  patch_family : int -> dsk_family -> unit;
  (* Insert or modify couple of a family with a giving id (add to pending patches). *)
  patch_couple : int -> dsk_couple -> unit;
  (* Insert or modify descendants of a family with a giving id (add to pending patches). *)
  patch_descend : int -> dsk_descend -> unit;
  (* Associate person to [name] inside the index.
     Added directly inside commited patches. *)
  patch_name : string -> int -> unit;
  (* Insert new string inside the pending patches and returns its id.
     If string already exists return its id. *)
  insert_string : string -> int;
  (* Commit pending patches and write a patches' new state inside "patches"
     file. "nb_persons" is also updated. *)
  commit_patches : unit -> unit;
  (* Update content (second arg) of the notes' file (first arg) if exists. *)
  commit_notes : string -> string -> unit;
  (* Close every opened channel. *)
  cleanup : unit -> unit;
  (* Returns real number of persons inside the base (without empty persons).
     Pending patches aren't considered. *)
  nb_of_real_persons : unit -> int;
  (* Tells if person with giving id exists in the base.
     Pending patches are also considered. *)
  iper_exists : int -> bool;
  (* Tells if family with giving id exists in the base.
     Pending patches are also considered. *)
  ifam_exists : int -> bool;
}
(** Functionality part of database. Every modification of the base is stored in {i patches} file.
    Note that, every modification firstly is pendent and should be commited
    to apply them and to update {i patches} file with [commit_patches]. *)

(** Geneweb database version *)
type base_version = GnWb0020 | GnWb0021 | GnWb0022 | GnWb0023 | GnWb0024

type dsk_base = { data : base_data; func : base_func; version : base_version }
(** Database representation: data and basic requests over this data. *)
