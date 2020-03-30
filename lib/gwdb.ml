open Def

type base = Gwdb_driver.base

type istr = Gwdb_driver.istr
type ifam = Gwdb_driver.ifam
type iper = Gwdb_driver.iper

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

let string_of_iper = Gwdb_driver.string_of_iper
let string_of_ifam = Gwdb_driver.string_of_ifam
let string_of_istr = Gwdb_driver.string_of_istr

let iper_of_string = Gwdb_driver.iper_of_string
let ifam_of_string = Gwdb_driver.ifam_of_string
let istr_of_string = Gwdb_driver.istr_of_string

let dummy_iper = Gwdb_driver.dummy_iper
let dummy_ifam = Gwdb_driver.dummy_ifam
let empty_string = Gwdb_driver.empty_string
let quest_string = Gwdb_driver.quest_string

let eq_istr = Gwdb_driver.eq_istr
let is_quest_string = Gwdb_driver.is_quest_string
let is_empty_string = Gwdb_driver.is_empty_string

let cache f a get set x =
  match get x with
  | Some v -> v
  | None -> let v = f a in set x (Some v) ; v

(** Persons *)

type person =
  { base : base
  ; iper : iper
  ; mutable p : (iper, iper, istr) gen_person option
  ; mutable a : ifam gen_ascend option
  ; mutable u : ifam gen_union option }

let cache_per f ({ base ; iper ; _ } as p)  =
  f (cache (Gwdb_driver.get_person base) iper (fun p -> p.p) (fun p v -> p.p <- v) p)

let cache_asc f ({ base ; iper ; _ } as p) =
  f (cache (Gwdb_driver.get_ascend base) iper (fun p -> p.a) (fun p v -> p.a <- v) p)

let cache_uni f ({ base ; iper ; _ } as p) =
  f (cache (Gwdb_driver.get_union base) iper (fun p -> p.u) (fun p v -> p.u <- v) p)

let dsk_person_of_person = cache_per (fun p -> p)
let gen_person_of_person = cache_per (fun p -> p)
let gen_ascend_of_person = cache_asc (fun p -> p)
let gen_union_of_person = cache_uni (fun p -> p)
let get_access = cache_per (fun p -> p.Def.access)
let get_aliases = cache_per (fun p -> p.Def.aliases)
let get_baptism = cache_per (fun p -> p.Def.baptism)
let get_baptism_note = cache_per (fun p -> p.Def.baptism_note)
let get_baptism_place = cache_per (fun p -> p.Def.baptism_place)
let get_baptism_src = cache_per (fun p -> p.Def.baptism_src)
let get_birth = cache_per (fun p -> p.Def.birth)
let get_birth_note = cache_per (fun p -> p.Def.birth_note)
let get_birth_place = cache_per (fun p -> p.Def.birth_place)
let get_birth_src = cache_per (fun p -> p.Def.birth_src)
let get_burial = cache_per (fun p -> p.Def.burial)
let get_burial_note = cache_per (fun p -> p.Def.burial_note)
let get_burial_place = cache_per (fun p -> p.Def.burial_place)
let get_burial_src = cache_per (fun p -> p.Def.burial_src)
let get_consang = cache_asc (fun a -> a.Def.consang)
let get_death = cache_per (fun p -> p.Def.death)
let get_death_note = cache_per (fun p -> p.Def.death_note)
let get_death_place = cache_per (fun p -> p.Def.death_place)
let get_death_src = cache_per (fun p -> p.Def.death_src)
let get_family = cache_uni (fun u -> u.Def.family)
let get_first_name = cache_per (fun p -> p.Def.first_name)
let get_first_names_aliases = cache_per (fun p -> p.Def.first_names_aliases)
let get_image = cache_per (fun p -> p.Def.image)
let get_iper = cache_per (fun p -> p.Def.key_index)
let get_notes = cache_per (fun p -> p.Def.notes)
let get_occ = cache_per (fun p -> p.Def.occ)
let get_occupation = cache_per (fun p -> p.Def.occupation)
let get_parents = cache_asc (fun a -> a.Def.parents)
let get_pevents = cache_per (fun p -> p.Def.pevents)
let get_psources = cache_per (fun p -> p.Def.psources)
let get_public_name = cache_per (fun p -> p.Def.public_name)
let get_qualifiers = cache_per (fun p -> p.Def.qualifiers)
let get_related = cache_per (fun p -> p.Def.related)
let get_rparents = cache_per (fun p -> p.Def.rparents)
let get_sex = cache_per (fun p -> p.Def.sex)
let get_surname = cache_per (fun p -> p.Def.surname)
let get_surnames_aliases = cache_per (fun p -> p.Def.surnames_aliases)
let get_titles = cache_per (fun p -> p.Def.titles)

(** Families *)

type family =
  { base : base
  ; ifam : ifam
  ; mutable f : (iper, ifam, istr) gen_family option
  ; mutable c : iper gen_couple option
  ; mutable d : iper gen_descend option
  }

let cache_fam f ({ base ; ifam ; _ } as fam)  =
  f (cache (Gwdb_driver.get_family base) ifam (fun f -> f.f) (fun f v -> f.f <- v) fam)

let cache_cpl f ({ base ; ifam ; _ } as fam) =
  f (cache (Gwdb_driver.get_couple base) ifam (fun f -> f.c) (fun f v -> f.c <- v) fam)

let cache_des f ({ base ; ifam ; _ } as fam) =
  f (cache (Gwdb_driver.get_descend base) ifam (fun f -> f.d) (fun f v -> f.d <- v) fam)

let gen_couple_of_family = cache_cpl (fun c -> c)
let gen_descend_of_family = cache_des (fun d -> d)
let gen_family_of_family = cache_fam (fun f -> f)
let get_children = cache_des (fun d -> d.Def.children)
let get_comment = cache_fam (fun f -> f.Def.comment)
let get_ifam = cache_fam (fun f -> f.Def.fam_index)
let get_divorce = cache_fam (fun f -> f.Def.divorce)
let get_father = cache_cpl (fun c -> Adef.father c)
let get_fevents = cache_fam (fun f -> f.Def.fevents)
let get_fsources = cache_fam (fun f -> f.Def.fsources)
let get_marriage = cache_fam (fun f -> f.Def.marriage)
let get_marriage_note = cache_fam (fun f -> f.Def.marriage_note)
let get_marriage_place = cache_fam (fun f -> f.Def.marriage_place)
let get_marriage_src = cache_fam (fun f -> f.Def.marriage_src)
let get_mother = cache_cpl (fun c -> Adef.mother c)
let get_origin_file = cache_fam (fun f -> f.Def.origin_file)
let get_parent_array = cache_cpl (fun c -> Adef.parent_array c)
let get_relation = cache_fam (fun f -> f.Def.relation)
let get_witnesses = cache_fam (fun f -> f.Def.witnesses)

let no_person ip =
  { first_name = quest_string
  ; surname = quest_string
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
  ; sex = Neuter
  ; access = Private
  ; birth = Adef.cdate_None
  ; birth_place = empty_string
  ; birth_note = empty_string
  ; birth_src = empty_string
  ; baptism = Adef.cdate_None
  ; baptism_place = empty_string
  ; baptism_note = empty_string
  ; baptism_src = empty_string
  ; death = DontKnowIfDead
  ; death_place = empty_string
  ; death_note = empty_string
  ; death_src = empty_string
  ; burial = UnknownBurial
  ; burial_place = empty_string
  ; burial_note = empty_string
  ; burial_src = empty_string
  ; pevents = []
  ; notes = empty_string
  ; psources = empty_string
  ; key_index = ip }

let no_ascend = { parents = None ; consang = Adef.no_consang }

let no_union = { family = [||] }

let empty_person base iper =
  { base
  ; iper
  ; p = Some (no_person iper)
  ; a = Some no_ascend
  ; u = Some no_union
  } [@ocaml.warning "-42"]

let person_of_gen_person base (p, a, u) =
  { base
  ; iper = p.key_index
  ; p = Some p
  ; a = Some a
  ; u = Some u
  } [@ocaml.warning "-42"]

let family_of_gen_family base (f, c, d) =
  { base
  ; ifam = f.fam_index
  ; f = Some f
  ; c = Some c
  ; d = Some d
  } [@ocaml.warning "-42"]

let poi base iper =
  if iper = dummy_iper then empty_person base iper
  else { base ; iper ; p = None ; a = None ; u = None } [@ocaml.warning "-42"]

let poi_batch base = List.map (poi base)

let no_family ifam =
  { marriage = Adef.cdate_None
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

let no_couple = Adef.couple dummy_iper dummy_iper

let no_descend = { Def.children = [||] }

let empty_family base ifam =
  { base
  ; ifam
  ; f = Some (no_family ifam)
  ; c = Some no_couple
  ; d = Some no_descend }

let foi base ifam =
  if ifam = dummy_ifam then empty_family base ifam
  else { base ; ifam ; f = None ; c = None ; d = None }

let foi_batch base = List.map (foi base) (* FIXME *)

(** Cache loading *)

let load_ascends_array = Gwdb_driver.load_ascends_array
let load_unions_array = Gwdb_driver.load_unions_array
let load_couples_array = Gwdb_driver.load_couples_array
let load_descends_array = Gwdb_driver.load_descends_array
let load_strings_array = Gwdb_driver.load_strings_array
let load_persons_array = Gwdb_driver.load_persons_array
let load_families_array = Gwdb_driver.load_families_array

let clear_ascends_array = Gwdb_driver.clear_ascends_array
let clear_unions_array = Gwdb_driver.clear_unions_array
let clear_couples_array = Gwdb_driver.clear_couples_array
let clear_descends_array = Gwdb_driver.clear_descends_array
let clear_strings_array = Gwdb_driver.clear_strings_array
let clear_persons_array = Gwdb_driver.clear_persons_array
let clear_families_array = Gwdb_driver.clear_families_array

(** INSERT *)

let insert_string = Gwdb_driver.insert_string

(** [insert_person base per]
    Add a new person with the same properties as [per] in [base],
    returning the fresh new {!type:iper} for this person.
    [per] SHOULD be defined using [dummy_iper].
*)
let insert_person base p a u =
  let iper = Gwdb_driver.new_iper base in
  let p = { p with key_index = iper } in
  Gwdb_driver.insert_ascend base iper a ;
  Gwdb_driver.insert_union base iper u ;
  Gwdb_driver.insert_person base iper p ;
  iper

(** [insert_family base fam]
    Add a new family with the same properties as [fam] in [base],
    returning the fresh new {!type:ifam} for this family.
    [fam] SHOULD be defined using [dummy_ifam].
*)
let insert_family base f c d =
  let ifam = Gwdb_driver.new_ifam base in
  Gwdb_driver.insert_family base ifam f ;
  Gwdb_driver.insert_couple base ifam c ;
  Gwdb_driver.insert_descend base ifam d ;
  ifam

(** UPDATE *)

let _patch_person base i p a u =
  Gwdb_driver.patch_person base i p ;
  Gwdb_driver.patch_ascend base i a ;
  Gwdb_driver.patch_union base i u

let patch_person = Gwdb_driver.patch_person
let patch_ascend = Gwdb_driver.patch_ascend
let patch_union = Gwdb_driver.patch_union

let _patch_family base i f c d =
  Gwdb_driver.patch_family base i f ;
  Gwdb_driver.patch_couple base i c ;
  Gwdb_driver.patch_descend base i d

let patch_family = Gwdb_driver.patch_family
let patch_couple = Gwdb_driver.patch_couple
let patch_descend = Gwdb_driver.patch_descend

(** DELETE *)

let rec delete_person base ip =
  let spouse c =
    let f = Adef.father c in
    if ip = f then Adef.mother c else f
  in
  let a = Gwdb_driver.get_ascend base ip in
  let ipers, ifams =
    match a.parents with
    | Some ifam ->
      Gwdb_driver.delete_ascend base ip ;
      let children =
        (Gwdb_driver.get_descend base ifam).children
        |> Mutil.array_except ip
      in
      Gwdb_driver.patch_descend base ifam { children } ;
      if children = [| ip |]
      then
        let c = Gwdb_driver.get_couple base ifam in
        let fath = Adef.father c in
        let moth = Adef.mother c in
        if is_empty_p base fath ~ifam && is_empty_p base moth ~ifam
        then [ fath ; moth ], [ ifam ]
        else [], []
      else [], []
    | None -> [], []
  in
  let del, ipers, ifams =
    let u = Gwdb_driver.get_union base ip in
    if u.family = [||] then (true, [], [])
    else
      Array.fold_left begin fun (del, ipers, ifams) ifam ->
        (* let f = Gwdb_driver.get_family base ifam in *)
        let d = Gwdb_driver.get_descend base ifam in
        if Array.length d.children > 1 then (false, ipers, ifams)
        else begin
          let sp = spouse @@ Gwdb_driver.get_couple base ifam in
          if is_empty_p base sp ~ifam then (del, sp :: ipers, ifam :: ifams)
          else (false, ipers, ifams)
        end
      end (true, ipers, ifams) u.family
  in
  if del then begin
    print_endline @@ Printf.sprintf "%s: %s" __LOC__ (string_of_iper ip) ;
    Gwdb_driver.delete_person base ip
  end
  else Gwdb_driver.patch_person base ip (no_person ip) ;
  List.iter (delete_person base) ipers ;
  List.iter (delete_family base) ifams

and is_empty_p ?ifam base sp =
  Gwdb_driver.get_ascend base sp = no_ascend
  && (Gwdb_driver.get_union base sp).family = (match ifam with Some i -> [| i |] | None -> [||])
  && (Gwdb_driver.get_person base sp) = no_person sp

and delete_family base ifam =
  let fam = foi base ifam in
  let fath = get_father fam in
  let moth = get_mother fam in
  let children = get_children fam in
  rm_union base ifam fath ;
  rm_union base ifam moth ;
  Array.iter (fun i -> patch_ascend base i no_ascend) children ;
  Gwdb_driver.delete_family base ifam ;
  if is_empty_p base fath ~ifam then delete_person base fath ;
  if is_empty_p base moth ~ifam then delete_person base moth ;
  Array.iter (fun i -> if is_empty_p base i then delete_person base i) children

and rm_union base ifam iper =
  { family = (Gwdb_driver.get_union base iper).family
             |> Mutil.array_except ifam
  }
  |> Gwdb_driver.patch_union base iper

(**/**)
(** Misc *)

(* Restrict file *)

let verbose = Mutil.verbose

(* FIXME: these values should not be global *)
let visible_ref : (iper, bool) Hashtbl.t option ref = ref None

let read_or_create_visible base =
  let fname = Filename.concat (Gwdb_driver.bname base) "restrict" in
  let visible =
    try
      let ic = Secure.open_in fname in
      let visible = input_value ic in
        close_in ic ;
        visible
    with Sys_error _ -> Hashtbl.create (Gwdb_driver.nb_of_persons base)
  in
  visible_ref := Some visible ;
  visible

let base_visible_write base =
  let fname = Filename.concat (Gwdb_driver.bname base) "restrict" in
  match !visible_ref with
  | Some visible ->
    let oc = Secure.open_out fname in
    output_value oc visible;
    close_out oc
  | None -> ()

let base_visible_get base fct i =
  let visible =
    match !visible_ref with
    | Some visible -> visible
    | None -> read_or_create_visible base
  in
  match Hashtbl.find_opt visible i with
  | None ->
    let status = fct (poi base i) in
    Hashtbl.add visible i status ;
    visible_ref := Some visible;
    status
  | Some b -> b

let base_particles = Gwdb_driver.base_particles
let base_strings_of_first_name = Gwdb_driver.base_strings_of_first_name
let base_strings_of_surname = Gwdb_driver.base_strings_of_surname

let base_notes_read = Gwdb_driver.base_notes_read
let base_notes_read_first_line = Gwdb_driver.base_notes_read_first_line
let base_notes_are_empty = Gwdb_driver.base_notes_are_empty
let base_notes_origin_file = Gwdb_driver.base_notes_origin_file
let base_notes_dir = Gwdb_driver.base_notes_dir
let base_wiznotes_dir = Gwdb_driver.base_wiznotes_dir

let sou = Gwdb_driver.sou

let commit_patches = Gwdb_driver.commit_patches
let commit_notes = Gwdb_driver.commit_notes
let person_of_key = Gwdb_driver.person_of_key

let nobtit base allowed_titles denied_titles p =
  let list = get_titles p in
  match Lazy.force allowed_titles with
  | [] -> list
  | allowed_titles ->
    let list =
      List.fold_right
        (fun t l ->
           let id = Name.lower (sou base t.t_ident) in
           let pl = Name.lower (sou base t.t_place) in
           if pl = "" then
             if List.mem id allowed_titles then t :: l else l
           else if
             List.mem (id ^ "/" ^ pl) allowed_titles ||
             List.mem (id ^ "/*") allowed_titles
           then
             t :: l
           else l)
        list []
    in
    match Lazy.force denied_titles with
      [] -> list
    | denied_titles ->
      List.filter
        (fun t ->
           let id = Name.lower (sou base t.t_ident) in
           let pl = Name.lower (sou base t.t_place) in
           if List.mem (id ^ "/" ^ pl) denied_titles ||
              List.mem ("*/" ^ pl) denied_titles
           then
             false
           else true)
        list

let p_first_name base p = Mutil.nominative (sou base (get_first_name p))
let p_surname base p = Mutil.nominative (sou base (get_surname p))

let husbands base gp =
  let p = poi base gp.key_index in
  Array.map
    (fun ifam ->
       let fam = foi base ifam in
       let husband = poi base (get_father fam) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) (get_surnames_aliases husband)
       in
       husband_surname, husband_surnames_aliases)
    (get_family p)

let father_titles_places base p nobtit =
  match get_parents (poi base p.key_index) with
  | Some ifam ->
    let fam = foi base ifam in
    let fath = poi base (get_father fam) in
    List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> []

let gen_gen_person_misc_names base p nobtit nobtit_fun =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) nobtit)
    (if p.sex = Female then Array.to_list (husbands base p) else [])
    (father_titles_places base p nobtit_fun)

let gen_person_misc_names base p nobtit =
  gen_gen_person_misc_names base p (nobtit p)
    (fun p -> nobtit (gen_person_of_person p))

let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit

  (* patch_misc_names base ip p ;
   * Array.iter
   *   begin fun i ->
   *     let cpl = base.data.couples.get i in
   *     let m = Adef.mother cpl in
   *     let f = Adef.father cpl in
   *     patch_misc_names base m (gen_person_of_person @@ poi base m) ;
   *     patch_misc_names base f (gen_person_of_person @@ poi base f) ;
   *     Array.iter
   *       begin
   *         fun i -> patch_misc_names base i (gen_person_of_person @@ poi base i)
   *       end
   *       (base.data.descends.get i).children
   *   end
   *   (base.data.unions.get ip).Def.family *)

let nb_of_persons : base -> int = Gwdb_driver.nb_of_persons
let nb_of_families : base -> int = Gwdb_driver.nb_of_families
let nb_of_real_persons : base -> int = Gwdb_driver.nb_of_real_persons

let date_of_last_change = Gwdb_driver.date_of_last_change

module type Collection = sig

  (** Collections are sets of elements you want to traverse. *)
  type 'a t

  (** Return the number of elements of a colletion *)
  val length : 'a t -> int

  (** [map fn c]
      Return a collection corresponding to [c]
      where [fn] would have been applied to each of its elements.
  *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [iter fn c]
      Apply [fn] would have been applied to each elements of [c].
  *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** [iter fn c]
      Apply [fn i] would have been applied to each elements of [c]
      where [i] is the index (starting with 0) of the element.
  *)
  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  (** [fold fn acc c]
      Combine each element of [c] into a single value using [fn].
      [fn] first argument is the result computed so far as we traverse the
      collection, and second element is the current element being combined.
      [acc] is the starting combined value.
      Start at [from]-nth and finish with [until]-nth element (included).
  *)
  val fold : ?from:int -> ?until:int -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** [fold_until continue fn acc c]
      Same as [fold fn acc c], but computation stops as soon as [continue]
      is not satisfied by combined value anymore.
  *)
  val fold_until : ('a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** [iterator c]
      Return a function returning [Some next_element] when it is called,
      or [None] if you reached the end of the collection.
  *)
  val iterator : 'a t -> (unit -> 'a option)

end

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

let ipers base =
  let cursor : Gwdb_driver.iper Gwdb_driver.cursor = Gwdb_driver.persons base in
  let length = cursor.Gwdb_driver.length in
  let get i = cursor.Gwdb_driver.get i in
  { Collection.length ; get }

let persons base =
  Collection.map (poi base) (ipers base)

let ifams base =
  let cursor : Gwdb_driver.ifam Gwdb_driver.cursor = Gwdb_driver.families base in
  let length = cursor.Gwdb_driver.length in
  let get i = cursor.Gwdb_driver.get i in
  { Collection.length ; get }

let families base =
  Collection.map (foi base) (ifams base)

(** [dummy_collection x] create a dummy collection with no element.
    [x] is only used for typing.
    Useful for placeholders or for typing purpose. *)
let dummy_collection _ =
  { Collection.length = -1
  ; get = fun _ -> None
  }

module type Marker = sig

  (** Markers are way to annotate (add extra information to) elements of a {!val:Collection.t}. *)
  type ('k, 'v) t

  val make : int -> 'v -> ('k, 'v) t

  (** [get marker key]
      Return the annotation associated to [key].
  *)
  val get : ('k, 'v) t -> 'k -> 'v

  (** [set marker key value]
      Set [value] as annotation associated to [key].
  *)
  val set : ('k, 'v) t -> 'k -> 'v -> unit

end

module Marker : Marker = struct
  type ('k, 'v) t = { default : 'v ; table : ('k, 'v) Hashtbl.t }
  let make nb default = { default ; table = Hashtbl.create nb }
  let get { default ; table } k = try Hashtbl.find table k with Not_found -> default
  let set { table ; _ } k v = Hashtbl.replace table k v
end

(** [dummy_marker k v] create a dummy collection with no element.
    [k] and [v] are only used for typing.
    Useful for placeholders or for typing purpose. *)
let dummy_marker (_ : 'a) (v : 'b) : ('a, 'b) Marker.t =
  Marker.make 32 v

let persons_of_name = Gwdb_driver.persons_of_name
let persons_of_surname = Gwdb_driver.persons_of_surname
let persons_of_first_name = Gwdb_driver.persons_of_first_name

let spi_find = Gwdb_driver.spi_find
let spi_first = Gwdb_driver.spi_first
let spi_next = Gwdb_driver.spi_next

let open_base = Gwdb_driver.open_base
let close_base = Gwdb_driver.close_base

let make = Gwdb_driver.make

let read_nldb = Gwdb_driver.read_nldb
let write_nldb = Gwdb_driver.write_nldb

let sync = Gwdb_driver.sync

let bname = Gwdb_driver.bname
