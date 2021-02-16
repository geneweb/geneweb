#define FN_strings "0"
#define FN_p_access "1"
#define FN_p_aliases "2"
#define FN_p_consang "3"
#define FN_p_first_name_aliases "4"
#define FN_p_firstname "5"
#define FN_p_image "6"
#define FN_p_lastname "7"
#define FN_p_notes "8"
#define FN_p_occ "9"
#define FN_p_occupation "A"
#define FN_p_parents "B"
#define FN_p_pevents "C"
#define FN_p_psources "D"
#define FN_p_public_name "E"
#define FN_p_qualifiers "F"
#define FN_p_related "G"
#define FN_p_rparents "H"
#define FN_p_sex "I"
#define FN_p_surnames_aliases "J"
#define FN_p_titles "K"
#define FN_p_unions "L"
#define FN_f_comment "M"
#define FN_f_fevents "N"
#define FN_f_fsources "O"
#define FN_f_origin_file "P"
#define FN_f_couple "Q"
#define FN_f_children "R"

#define FN_p_deleted "S"
#define FN_f_deleted "T"

#define FN_idx_npoc "U"
#define FN_particles "V"

type ifam = int
type iper = int
type istr = int

let iper_of_string = int_of_string
let ifam_of_string = int_of_string
let istr_of_string = int_of_string
let string_of_ifam = string_of_int
let string_of_iper = string_of_int
let string_of_istr = string_of_int
let is_empty_string = (=) 0
let is_quest_string = (=) 1
let empty_string = 0
let quest_string = 1
let eq_istr = Int.equal
let dummy_ifam = -1
let dummy_iper = -1

type fam_event = (iper, istr) Def.gen_fam_event
type pers_event = (iper, istr) Def.gen_pers_event
type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title

type person =
  { access : Def.access Lazy.t
  ; aliases : istr list Lazy.t
  ; first_names_aliases : istr list Lazy.t
  ; firstname : istr Lazy.t
  ; image : istr Lazy.t
  ; iper : iper
  ; lastname : istr Lazy.t
  ; note : istr Lazy.t
  ; occ : int Lazy.t
  ; occupation : istr Lazy.t
  ; parents : ifam option Lazy.t
  ; consang : Adef.fix Lazy.t
  ; pevents : pers_event list Lazy.t
  ; psources : istr Lazy.t
  ; public_name : istr Lazy.t
  ; qualifiers : istr list Lazy.t
  ; related : iper list Lazy.t
  ; rparents : relation list Lazy.t
  ; sex : Def.sex Lazy.t
  ; surnames_aliases : istr list Lazy.t
  ; titles : title list Lazy.t
  ; unions : ifam array Lazy.t

  ; birth : pers_event option Lazy.t
  ; baptism : pers_event option Lazy.t
  ; death : pers_event option Lazy.t
  ; burial : pers_event option Lazy.t
  }

type family =
  { comment : istr Lazy.t
  ; fevents : fam_event list Lazy.t
  ; fsources : istr Lazy.t
  ; ifam : ifam Lazy.t
  ; origin_file : istr Lazy.t
  ; couple : iper array Lazy.t
  ; children : iper array Lazy.t

  ; relation : fam_event option Lazy.t
  ; separation : fam_event option Lazy.t
  }

type string_person_index

type base_version = GnWbLazy0000

type base =
  { bdir : string
  ; in_bin : (string, in_channel) Hashtbl.t
  ; open_in_bin : string -> in_channel
  ; out_bin : (string, out_channel) Hashtbl.t
  ; open_out_bin : string -> out_channel
  ; cache_sou : (int, string) Hashtbl.t
  ; particles : string list Lazy.t
  }

(* TODO: create an array header so we can read all as array *)
let output_array_dat_inx bdir name get arr =
  let oc_dat = open_out_bin @@ Filename.concat bdir @@ name ^ ".dat" in
  let oc_inx = open_out_bin @@ Filename.concat bdir @@ name ^ ".inx" in
  prerr_endline @@ Filename.concat bdir @@ name ^ ".dat" ;
  Array.iteri begin fun i x ->
    assert (pos_out oc_inx = i * 4) ;
    output_binary_int oc_inx (pos_out oc_dat) ;
    Marshal.to_channel oc_dat (get x) [ Marshal.No_sharing ] ;
  end arr ;
  close_out oc_dat ;
  close_out oc_inx

(* TODO: create an array header so we can read all as array *)
let output_array_dat bdir name get arr =
  let oc_dat = open_out_bin @@ Filename.concat bdir @@ name ^ ".dat" in
  prerr_endline @@ Filename.concat bdir @@ name ^ ".dat" ;
  Array.iteri begin fun i x ->
    assert (pos_out oc_dat = i * 4) ;
    output_binary_int oc_dat (get x) ;
  end arr ;
  close_out oc_dat

let read_dat_inx_aux ic_dat ic_inx i =
  seek_in ic_inx (i * 4) ;
  seek_in ic_dat (input_binary_int ic_inx) ;
  Marshal.from_channel ic_dat

let read_dat_inx base name =
  read_dat_inx_aux
    (base.open_in_bin @@ name ^ ".dat")
    (base.open_in_bin @@ name ^ ".inx")

let read_dat base name i =
  let ic_dat = base.open_in_bin @@ name ^ ".dat" in
  seek_in ic_dat (i * 4) ;
  let r = input_binary_int ic_dat in
  r

let open_base bname =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  let aux ht fn =
    fun s -> match Hashtbl.find_opt ht s with
      | Some c -> c
      | None ->
        let c = fn @@ Filename.concat bdir s in
        Hashtbl.add ht s c ;
        c
  in
  let in_bin = Hashtbl.create 0 in
  let out_bin = Hashtbl.create 0 in
  let open_in_bin = aux in_bin open_in_bin in
  let open_out_bin = aux out_bin open_out_bin in
  let out_bin = Hashtbl.create 0 in
  let cache_sou = Hashtbl.create 0 in
  let particles =
    lazy (let ic = open_in_bin @@ Filename.concat bdir FN_particles in
          let r = Marshal.from_channel ic in
          close_in ic ;
          r
         )
  in
  { bdir ; in_bin ; open_in_bin ; out_bin ; open_out_bin ; cache_sou ; particles }

let nb_of_persons b =
  in_channel_length (b.open_in_bin @@ FN_p_firstname ^ ".dat") / 4

let nb_of_families b =
  in_channel_length (b.open_in_bin @@ FN_f_comment ^ ".dat") / 4

let make_key_index p strings =
  let a =
    Array.mapi
      (fun i p -> ( Name.crush_lower strings.(p.Def.first_name)
                  , Name.crush_lower strings.(p.Def.surname)
                  , p.occ), i)
      p
  in
  Array.sort compare a ;
  a

let make bname particles ((p, a, u), (f, c, d), strings, bnotes) =
  let bdir =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  Mutil.mkdir_p bdir ;
  (* strings *)
  output_array_dat_inx bdir FN_strings (fun x -> x) strings ;
  (* persons *)
  output_array_dat bdir FN_p_access (fun x -> Obj.magic x.Def.access) p ;
  output_array_dat bdir FN_p_consang (fun x -> Obj.magic x.Def.consang) a ;
  output_array_dat bdir FN_p_firstname (fun x -> x.Def.first_name) p ;
  output_array_dat bdir FN_p_image (fun x -> x.Def.image) p ;
  output_array_dat bdir FN_p_lastname (fun x -> x.Def.surname) p ;
  output_array_dat bdir FN_p_notes (fun x -> x.Def.notes) p ;
  output_array_dat bdir FN_p_occ (fun x -> x.Def.occ) p ;
  output_array_dat bdir FN_p_occupation (fun x -> x.Def.occupation) p ;
  output_array_dat bdir FN_p_parents (fun x -> match x.Def.parents with Some x -> x | _ -> -1) a ;
  output_array_dat bdir FN_p_public_name (fun x -> x.Def.public_name) p ;
  output_array_dat bdir FN_p_psources (fun x -> x.Def.psources) p ;
  output_array_dat bdir FN_p_sex (fun x -> Obj.magic x.Def.sex) p ;
  output_array_dat_inx bdir FN_p_aliases (fun x -> x.Def.aliases) p ;
  output_array_dat_inx bdir FN_p_first_name_aliases (fun x -> x.Def.first_names_aliases) p ;
  output_array_dat_inx bdir FN_p_pevents (fun x -> x.Def.pevents) p ;
  output_array_dat_inx bdir FN_p_qualifiers (fun x -> x.Def.qualifiers) p ;
  output_array_dat_inx bdir FN_p_related (fun x -> x.Def.related) p ;
  output_array_dat_inx bdir FN_p_rparents (fun x -> x.Def.rparents) p ;
  output_array_dat_inx bdir FN_p_surnames_aliases (fun x -> x.Def.surnames_aliases) p ;
  output_array_dat_inx bdir FN_p_titles (fun x -> x.Def.titles) p ;
  output_array_dat_inx bdir FN_p_unions (fun x -> x.Def.family) u ;
  (* families *)
  output_array_dat bdir FN_f_comment (fun x -> x.Def.comment) f ;
  output_array_dat bdir FN_f_fsources (fun x -> x.Def.fsources) f ;
  output_array_dat bdir FN_f_origin_file (fun x -> x.Def.origin_file) f ;
  output_array_dat_inx bdir FN_f_fevents (fun x -> x.Def.fevents) f ;
  output_array_dat_inx bdir FN_f_couple (fun x -> [| Adef.father x ; Adef.mother x |]) c ;
  output_array_dat_inx bdir FN_f_children (fun x -> x.Def.children) d ;
  (* particles *)
  (let oc = open_out_bin @@ Filename.concat bdir FN_particles in
   Marshal.to_channel oc particles [ Marshal.No_sharing ] ;
   close_out oc) ;
  (* indexes *)
  output_array_dat_inx bdir FN_idx_npoc (fun x -> x) (make_key_index p strings) ;
  open_base bname

let sync ?scratch:_ b =
  Hashtbl.iter (fun _ -> flush) b.out_bin

let find_events g b x e =
  List.find_opt (fun x' -> List.mem (g x') x) e

let find_event g b x e =
  List.find_opt (fun x' -> g x' = x) e

let istr_of_option = function None -> -1 | Some x -> x

let istr_to_option = function -1 -> None | x -> Some x

let sou b i =
  try Hashtbl.find b.cache_sou i
  with _ ->
    let x = read_dat_inx b FN_strings i in
    Hashtbl.add b.cache_sou i x ;
    x

let poi b i =
  let pevents = lazy (read_dat_inx b FN_p_pevents i) in
  let find_event = find_event (fun e -> e.Def.epers_name) in
  let find_events = find_events (fun e -> e.Def.epers_name) in
  { access = lazy (Obj.magic @@ read_dat b FN_p_access i)
  ; aliases = lazy (read_dat_inx b FN_p_aliases i)
  ; first_names_aliases = lazy (read_dat_inx b FN_p_first_name_aliases i)
  ; firstname = lazy (read_dat b FN_p_firstname i)
  ; image = lazy (read_dat b FN_p_image i)
  ; iper = i
  ; lastname = lazy (read_dat b FN_p_lastname i)
  ; note = lazy (read_dat b FN_p_notes i)
  ; occ = lazy (read_dat b FN_p_occ i)
  ; occupation = lazy (read_dat b FN_p_occupation i)
  ; parents = lazy (istr_to_option @@ read_dat b FN_p_parents i)
  ; consang = lazy (Obj.magic @@ read_dat b FN_p_consang i)
  ; pevents
  ; psources = lazy (read_dat b FN_p_psources i)
  ; public_name = lazy (read_dat b FN_p_public_name i)
  ; qualifiers = lazy (read_dat_inx b FN_p_qualifiers i)
  ; related = lazy (read_dat_inx b FN_p_related i)
  ; rparents = lazy (read_dat_inx b FN_p_rparents i)
  ; sex = lazy (Obj.magic read_dat b FN_p_sex i)
  ; surnames_aliases = lazy (read_dat_inx b FN_p_surnames_aliases i)
  ; titles = lazy (read_dat_inx b FN_p_titles i)
  ; unions = lazy (read_dat_inx b FN_p_unions i)

  ; birth = lazy (find_event b Def.Epers_Birth @@ Lazy.force pevents)
  ; baptism = lazy (find_event b Def.Epers_Baptism @@ Lazy.force pevents)
  ; death = lazy (find_event b Def.Epers_Death @@ Lazy.force pevents)
  ; burial = lazy (find_events b [ Def.Epers_Burial ; Def.Epers_Cremation ] @@ Lazy.force pevents)
  }

let foi b i =
  let fevents = lazy (read_dat_inx b FN_f_fevents i) in
  let find_events = find_events (fun e -> e.Def.efam_name) in
  { comment = lazy (read_dat b FN_f_comment i)
  ; fevents
  ; fsources = lazy (read_dat b FN_f_fsources i)
  ; ifam = lazy (if Obj.magic @@ read_dat b FN_f_deleted i then dummy_ifam else i)
  ; origin_file = lazy (read_dat b FN_f_origin_file i)
  ; couple = lazy (read_dat_inx b FN_f_couple i)
  ; children = lazy (read_dat_inx b FN_f_children i)

  (* FIXME: choose the right one *)
  ; relation = lazy (find_events b
                       [ Def.Efam_Marriage ; Def.Efam_NoMarriage ; Def.Efam_Engage
                       ; Def.Efam_NoMention ; Def.Efam_MarriageBann ; Def.Efam_MarriageContract
                       ; Def.Efam_MarriageLicense ; Def.Efam_PACS ; Def.Efam_Residence ]
                     @@ Lazy.force fevents)
  ; separation = lazy (find_events b [ Def.Efam_Divorce ; Def.Efam_Separated ] @@ Lazy.force fevents)
  }

let map fn = function Some x -> fn x | None -> None
let default d = function Some x -> x | None -> d
let map_default d fn = function Some x -> fn x | None -> d

let get_access p = Lazy.force p.access
let get_aliases p = Lazy.force p.aliases
let get_baptism p = map_default Adef.cdate_None (fun e -> e.Def.epers_date) (Lazy.force p.baptism)
let get_baptism_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.baptism)
let get_baptism_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.baptism)
let get_baptism_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.baptism)
let get_birth p = map_default Adef.cdate_None (fun e -> e.Def.epers_date) (Lazy.force p.birth)
let get_birth_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.birth)
let get_birth_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.birth)
let get_birth_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.birth)
let get_first_name p = Lazy.force p.firstname
let get_surname p = Lazy.force p.lastname
let get_iper p = p.iper
let get_burial p =
  match Lazy.force p.burial with
  | Some { Def.epers_name = Epers_Burial ; epers_date ; _ } -> Def.Buried epers_date
  | Some { Def.epers_name = Epers_Cremation ; epers_date ; _ } -> Def.Cremated epers_date
  | _ -> Def.UnknownBurial
let get_burial_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.burial)
let get_burial_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.burial)
let get_burial_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.burial)
let get_death p = (* FIXME *)
  match Lazy.force p.death with
  | Some { Def.epers_date ; _ } ->
    if epers_date <> Adef.cdate_None then Def.Death (Def.Unspecified, epers_date)
    else Def.OfCourseDead
  | _ -> Def.NotDead
let get_death_note p = map_default empty_string (fun e -> e.Def.epers_note) (Lazy.force p.death)
let get_death_place p = map_default empty_string (fun e -> e.Def.epers_place) (Lazy.force p.death)
let get_death_src p = map_default empty_string (fun e -> e.Def.epers_src) (Lazy.force p.death)
let get_consang p = Lazy.force p.consang
let get_first_names_aliases p = Lazy.force p.first_names_aliases
let get_family p = Lazy.force p.unions
let get_image p = Lazy.force p.image
let get_notes p = Lazy.force p.note
let get_occ p = Lazy.force p.occ
let get_occupation p = Lazy.force p.occupation
let get_origin_file p = Lazy.force p.origin_file
let get_parents p = Lazy.force p.parents
let get_pevents p = Lazy.force p.pevents
let get_psources p = Lazy.force p.psources
let get_public_name p = Lazy.force p.public_name
let get_qualifiers p = Lazy.force p.qualifiers
let get_related p = Lazy.force p.related
let get_rparents p = Lazy.force p.rparents
let get_sex p = Lazy.force p.sex
let get_surnames_aliases p = Lazy.force p.surnames_aliases
let get_titles p = Lazy.force p.titles

let get_children f = Lazy.force f.children
let get_comment f = Lazy.force f.comment
let get_divorce f =
  match Lazy.force f.separation with
  | None -> Def.NotDivorced
  | Some { Def.efam_name = Def.Efam_Divorce ; efam_date } -> Def.Divorced efam_date
  | Some { Def.efam_name = Def.Efam_Separated } -> Def.Separated
  | _ -> assert false
let get_father f = Array.unsafe_get (Lazy.force f.couple) 0
let get_fevents f = Lazy.force f.fevents
let get_fsources f = Lazy.force f.fsources
let get_ifam f = Lazy.force f.ifam
let get_marriage f = map_default Adef.cdate_None (fun e -> e.Def.efam_date) (Lazy.force f.relation)
let get_marriage_note f = map_default empty_string (fun e -> e.Def.efam_note) (Lazy.force f.relation)
let get_marriage_place f = map_default empty_string (fun e -> e.Def.efam_place) (Lazy.force f.relation)
let get_marriage_src f = map_default empty_string (fun e -> e.Def.efam_src) (Lazy.force f.relation)
let get_mother f = Array.unsafe_get (Lazy.force f.couple) 1
let get_parent_array f = Lazy.force f.couple
let get_witnesses f =
  match Lazy.force f.relation with
  | Some e -> Array.map fst e.Def.efam_witnesses
  | None -> [||]
let get_relation f =
  match Lazy.force f.relation with
  | Some { Def.efam_name = Def.Efam_Marriage } -> Def.Married
  | Some { Def.efam_name = Def.Efam_NoMarriage } -> Def.NotMarried
  | Some { Def.efam_name = Def.Efam_Engage } -> Def.Engaged
  | Some { Def.efam_name = Def.Efam_NoMention } -> Def.NoMention
  | Some { Def.efam_name = Def.Efam_MarriageBann } -> Def.MarriageBann
  | Some { Def.efam_name = Def.Efam_MarriageContract } -> Def.MarriageContract
  | Some { Def.efam_name = Def.Efam_MarriageLicense } -> Def.MarriageLicense
  | Some { Def.efam_name = Def.Efam_PACS } -> Def.Pacs
  | Some { Def.efam_name = Def.Efam_Residence } -> Def.Residence
  | _ -> assert false

let load_ascends_array _ = ()
let load_couples_array _ = ()
let load_descends_array _ = ()
let load_families_array _ = ()
let load_persons_array _ = ()
let load_strings_array _ = ()
let load_unions_array _ = ()
let clear_ascends_array _ = ()
let clear_couples_array _ = ()
let clear_descends_array _ = ()
let clear_families_array _ = ()
let clear_persons_array _ = ()
let clear_strings_array _ = ()
let clear_unions_array _ = ()
let base_particles b = Lazy.force b.particles

let base_strings_of_first_name _ = assert false
let base_strings_of_surname _ = assert false
let base_visible_get _ = assert false
let base_visible_write _ = assert false
let base_wiznotes_dir _ = assert false
let bname _ = assert false
let close_base _ = assert false
let commit_notes _ = assert false
let commit_patches _ = assert false
let date_of_last_change _ = assert false
let delete_ascend _ = assert false
let delete_couple _ = assert false
let delete_descend _ = assert false
let delete_family _ = assert false
let delete_person _ = assert false
let delete_union _ = assert false
let empty_family _ = assert false
let empty_person _ = assert false
let families ?select:_ _ = assert false
let family_of_gen_family _ = assert false
let gen_ascend_of_person _ = assert false
let gen_couple_of_family _ = assert false
let gen_descend_of_family _ = assert false
let gen_family_of_family _ = assert false
let gen_person_of_person _ = assert false
let gen_union_of_person _ = assert false
let insert_ascend _ = assert false
let insert_couple _ = assert false
let insert_descend _ = assert false
let insert_family _ = assert false
let insert_person _ = assert false
let insert_string _ = assert false
let insert_union _ = assert false

let nb_of_real_persons _ = assert false
let new_ifam _ = assert false
let new_iper _ = assert false
let no_ascend = { Def.parents = None ; consang = Adef.no_consang }
let no_couple = Adef.couple dummy_iper dummy_iper
let no_descend = { Def.children = [||] }
let no_family ifam =
  { Def.marriage = Adef.cdate_None
  ; marriage_place = empty_string
  ; marriage_note = empty_string
  ; marriage_src = empty_string
  ; witnesses = [||]
  ; relation = Def.NoMention
  ; divorce = Def.NotDivorced
  ; fevents = []
  ; comment = empty_string
  ; origin_file = empty_string
  ; fsources = empty_string
  ; fam_index = ifam
  }
let no_person ip =
  { Def.first_name = empty_string
  ; surname = empty_string
  ; occ = 0
  ; image = empty_string
  ; first_names_aliases = []
  ; surnames_aliases = []
  ; public_name = empty_string
  ; qualifiers = []
  ; titles = []
  ; rparents = []
  ; related = []
  ; aliases = []
  ; occupation = empty_string
  ; sex = Def.Neuter
  ; access = Def.Private
  ; birth = Adef.cdate_None
  ; birth_place = empty_string
  ; birth_note = empty_string
  ; birth_src = empty_string
  ; baptism = Adef.cdate_None
  ; baptism_place = empty_string
  ; baptism_note = empty_string
  ; baptism_src = empty_string
  ; death = Def.DontKnowIfDead
  ; death_place = empty_string
  ; death_note = empty_string
  ; death_src = empty_string
  ; burial = Def.UnknownBurial
  ; burial_place = empty_string
  ; burial_note = empty_string
  ; burial_src = empty_string
  ; pevents = []
  ; notes = empty_string
  ; psources = empty_string
  ; key_index = ip }
let no_union = { Def.family = [||] }

let patch_ascend _ = assert false
let patch_couple _ = assert false
let patch_descend _ = assert false
let patch_family _ = assert false
let patch_person _ = assert false
let patch_union _ = assert false
let person_of_gen_person _ =   assert false
let persons _ = assert false
let persons_of_first_name _ = assert false
let persons_of_name _ = assert false
let persons_of_surname _ = assert false

let spi_find _ = assert false
let spi_first _ = assert false
let spi_next _ = assert false

module IDX = struct

  (* Return the index of an element using cmp *)
  let binary_search cmp get len =
    let rec aux low high =
      if high <= low then
        if cmp (get low) = 0 then low
        else raise Not_found
      else
        let mid = (low + high) / 2 in
        let c = cmp (get mid) in
        if c < 0 then
          aux low (mid - 1)
        else if c > 0 then
          aux (mid + 1) high
        else
          mid
    in aux 0 len

  (* Return the index of the first element matching, or the which would come after if unbound *)
  let binary_search_or_next cmp get len =
    let rec aux acc low high =
      if high <= low then
        if cmp (get low) <= 0 then low
        else match acc with Some x -> x | None -> raise Not_found
      else
        let mid = (low + high) / 2 in
        let c = cmp (get mid) in
        if c < 0 then
          aux (Some mid) low (mid - 1)
        else if c > 0 then
          aux acc (mid + 1) high
        else
          mid
    in aux None 0 len

  (* Return the index of the element comming after, using cmp *)
  let binary_search_next cmp get len =
    let rec aux acc low high =
      if high <= low then
        if cmp (get low) < 0 then low
        else match acc with Some x -> x | None -> raise Not_found
      else
        let mid = (low + high) / 2 in
        let c = cmp (cmp mid) in
        if c < 0 then
          aux (Some mid) low (mid - 1)
        else
          aux acc (mid + 1) high
    in aux None 0 len


end

let search_npoc b key =
  let ic_dat = b.open_in_bin @@ FN_idx_npoc ^ ".dat" in
  let ic_inx = b.open_in_bin @@ FN_idx_npoc ^ ".inx" in
  let get = read_dat_inx_aux ic_dat ic_inx in
  IDX.binary_search (fun (key', _) -> compare key key') get (in_channel_length ic_inx / 4)
  |> get
  |> snd

let person_of_key b p n o =
  try Some (search_npoc b (Name.crush_lower p, Name.crush_lower n, o))
  with e -> None


(* Copied from gwc1 *)

module Collection = struct

  type 'a t =
    { length : int
    ; get : int -> 'a option
    }

  let map (fn : 'a -> 'b) c =
    { length = c.length
    ; get = (fun i ->  match c.get i with Some x -> Some (fn x) | None -> None)
    }

  let length { length ; _ } = length

  let iter fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn x | None -> () done

  let iteri fn { get ; length } =
    for i = 0 to length - 1 do match get i with Some x -> fn i x | None -> () done

  let fold ?from ?until fn acc { get ; length } =
    let from = match from with Some x -> x | None -> 0 in
    let until = match until with Some x -> x + 1 | None -> length in
    let rec loop acc i =
      if i = until then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc from

  let fold_until continue fn acc { get ; length } =
    let rec loop acc i =
      if not (continue acc) || i = length then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc 0

  let iterator { get ; length } =
    let cursor = ref 0 in
    let rec next () =
      if !cursor < length then
        match get !cursor with
        | None -> incr cursor ; next ()
        |  v -> incr cursor ; v
      else None
    in
    next

end

module Marker = struct

  type ('k, 'v) t =
    { get : 'k -> 'v
    ; set : 'k -> 'v -> unit
    }

  let make (k : 'a -> 'k) (c : 'a Collection.t) (i : 'v) : ('a, 'v) t =
    let a = Array.make c.Collection.length i in
    { get = (fun x -> Array.get a (k x) )
    ; set = (fun x v -> Array.set a (k x) v) }

  let get ({ get ; _ } : _ t) k = get k
  let set ({ set ; _ } : _ t) k = set k

end

let persons base =
  { Collection.length = nb_of_persons base
  ; get = (fun i -> Some (poi base i))
  }

let ipers base =
  { Collection.length = nb_of_persons base
  ; get = (fun i -> Some i)
  }

let iper_marker c i = Marker.make (fun i -> i) c i

let ifams ?(select = fun _ -> true) base =
  { Collection.length = nb_of_families base
  ; get = begin fun i ->
      if select i
      then
        if get_ifam (foi base i) = dummy_ifam
        then None else Some i
      else None
    end
  }

let families ?(select = fun _ -> true) base =
  { Collection.length = nb_of_families base
  ; get = begin fun i ->
      let f = foi base i in
      if get_ifam f <> dummy_ifam && select f
      then Some f
      else None
    end
  }

let dummy_collection _ =
  { Collection.length = -1
  ; get = fun _ -> None
  }

let ifam_marker c i = Marker.make (fun i -> i) c i

let dummy_marker (_ : 'a) (v : 'b) : ('a, 'b) Marker.t =
  { Marker.get = begin fun _ -> v end
  ; set = begin fun _ _ -> () end
  }

let bfname base fname =
  Filename.concat base.bdir fname

module NLDB = struct

  let magic = "GWNL0010"

  let read base =
    let fname = bfname base "notes_links" in
    match try Some (open_in_bin fname) with Sys_error _ -> None with
    | Some ic ->
      let r =
        if Mutil.check_magic magic ic
        then (input_value ic : (iper, iper) Def.NLDB.t)
        else failwith "unsupported nldb format"
      in
      close_in ic ; r
    | None -> []

  let write base db =
    let fname_tmp = bfname base "1notes_links" in
    let fname_def = bfname base "notes_links" in
    let fname_back = bfname base "notes_links~" in
    let oc = open_out_bin fname_tmp in
    output_string oc magic ;
    output_value oc (db : (iper, ifam) Def.NLDB.t) ;
    close_out oc ;
    Mutil.rm fname_back;
    Mutil.mv fname_def fname_back ;
    Sys.rename fname_tmp fname_def

end

let read_nldb = NLDB.read
let write_nldb = NLDB.write

let base_notes_origin_file _ = "" (* FIXME *)
let base_notes_dir b = Filename.concat b.bdir "notes_d"
let base_wiznotes_dir b = Filename.concat b.bdir "wiznotes"

let base_notes_read_aux base fnotes mode =
  let fname =
    if fnotes = "" then "notes"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  try
    let ic = Secure.open_in @@ Filename.concat base.bdir fname in
    let str =
      match mode with
      | Def.RnDeg -> if in_channel_length ic = 0 then "" else " "
      | Def.Rn1Ln -> (try input_line ic with End_of_file -> "")
      | Def.RnAll -> Mutil.input_file_ic ic
    in
    close_in ic ;
    str
  with Sys_error _ -> ""

let base_notes_read base fnotes = base_notes_read_aux base fnotes Def.RnAll
let base_notes_read_first_line base fnotes = base_notes_read_aux base fnotes Def.Rn1Ln
let base_notes_are_empty base fnotes = base_notes_read_aux base fnotes Def.RnDeg = ""
