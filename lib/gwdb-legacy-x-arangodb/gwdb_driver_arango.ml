open Def

module J = struct

  include Json_jsonaf

  (* Json -> GeneWeb *)

  let dmy_of_json prec json =
    { day = get_int "day"json
    ; month = get_int "month" json
    ; year = get_int "year" json
    ; delta = get_int "delta" json
    ; prec
    }

  let dmy2_of_json json =
    { day2 = get_int "day" json
    ; month2 = get_int "month" json
    ; year2 = get_int "year" json
    ; delta2 = get_int "delta" json
    }

  let date_of_json = function
    | `String t -> Dtext t
    | json ->
      let prec = match get_string "prec" json with
        | "sure" -> Sure
        | "about" -> About
        | "maybe" -> Maybe
        | "before" -> Before
        | "after" -> After
        | "or" -> OrYear (dmy2_of_json @@ member "dmy2" json)
        | "between" -> YearInt (dmy2_of_json @@ member "dmy2" json)
        | _ -> assert false
      in
      let d = dmy_of_json prec (member "dmy1" json) in
      match get_string "calendar" json with
      | "gregorian" -> Dgreg (d, Dgregorian)
      | "julian" -> Dgreg (d, Djulian)
      | "french" -> Dgreg (d, Dfrench)
      | "hebrew" -> Dgreg (d, Dhebrew)
      | _ -> assert false

  let cdate_of_json = function
    | `Null -> Adef.cdate_of_od None
    | json -> Adef.cdate_of_od @@ Some (date_of_json json)

  let pevent_name_of_string = function
    | "birth" -> Epers_Birth
    | "baptism" -> Epers_Baptism
    | "death" -> Epers_Death
    | "burial" -> Epers_Burial
    | "cremation" -> Epers_Cremation
    | "accomplishment" -> Epers_Accomplishment
    | "aquisition" -> Epers_Acquisition
    | "adhesion" -> Epers_Adhesion
    | "baptismlds" -> Epers_BaptismLDS
    | "barmitzvah" -> Epers_BarMitzvah
    | "batmitzvah" -> Epers_BatMitzvah
    | "benediction" -> Epers_Benediction
    | "changename" -> Epers_ChangeName
    | "circumcision" -> Epers_Circumcision
    | "confirmation" -> Epers_Confirmation
    | "confirmationlds" -> Epers_ConfirmationLDS
    | "decoration" -> Epers_Decoration
    | "demobilisationmilitaire" -> Epers_DemobilisationMilitaire
    | "diploma" -> Epers_Diploma
    | "distinction" -> Epers_Distinction
    | "dotation" -> Epers_Dotation
    | "dotationlds" -> Epers_DotationLDS
    | "education" -> Epers_Education
    | "election" -> Epers_Election
    | "emigration" -> Epers_Emigration
    | "excommunication" -> Epers_Excommunication
    | "familylinklds" -> Epers_FamilyLinkLDS
    | "firstcommunion" -> Epers_FirstCommunion
    | "funeral" -> Epers_Funeral
    | "graduate" -> Epers_Graduate
    | "hospitalisation" -> Epers_Hospitalisation
    | "illness" -> Epers_Illness
    | "immigration" -> Epers_Immigration
    | "listepassenger" -> Epers_ListePassenger
    | "militarydistinction" -> Epers_MilitaryDistinction
    | "militarypromotion" -> Epers_MilitaryPromotion
    | "militaryservice" -> Epers_MilitaryService
    | "mobilisationmilitaire" -> Epers_MobilisationMilitaire
    | "naturalisation" -> Epers_Naturalisation
    | "occupation" -> Epers_Occupation
    | "ordination" -> Epers_Ordination
    | "property" -> Epers_Property
    | "recensement" -> Epers_Recensement
    | "residence" -> Epers_Residence
    | "retired" -> Epers_Retired
    | "scellentchildlds" -> Epers_ScellentChildLDS
    | "scellentparentlds" -> Epers_ScellentParentLDS
    | "scellentspouselds" -> Epers_ScellentSpouseLDS
    | "ventebien" -> Epers_VenteBien
    | "will" -> Epers_Will
    | s -> Epers_Name s

  let pevent_witness_of_json json =
    ( get_string "person" json
    , match member "type" json with
    | `String "godparent" -> Def.Witness_GodParent
    | `String "witness" -> Def.Witness
    | `String "officer" -> Def.Witness_Officer
    | _ -> assert false )

  let pevent_of_json json =
    { epers_place = get_string "place" json
    ; epers_reason = get_string "reason" json
    ; epers_note = get_string "note" json
    ; epers_src = get_string "src" json
    ; epers_name = pevent_name_of_string (get_string "name" json)
    ; epers_date = cdate_of_json (member "date" json)
    ; epers_witnesses = Array.of_list (get_list "witnesses" pevent_witness_of_json json)
    }

  let relation_type_of_json js = match to_string js with
    | "adoption" -> Adoption
    | "recognition" -> Recognition
    | "candidate_parent" -> CandidateParent
    | "god_parent" -> GodParent
    | "foster_parent" -> FosterParent
    | _ -> assert false

  let rparent_of_json json =
    { r_type = relation_type_of_json (member "type" json)
    ; r_fath = (member "father" json |> to_opt to_string)
    ; r_moth = (member "mother" json |> to_opt to_string)
    ; r_sources = get_string "source" json
    }

  let divorce_of_json = function
    | `False -> NotDivorced
    | `True -> Separated
    | date -> Divorced (cdate_of_json date)

  let fevent_name_of_string = function
    | "marriage" -> Efam_Marriage
    | "no_marriage" -> Efam_NoMarriage
    | "no_mention" -> Efam_NoMention
    | "engaged" -> Efam_Engage
    | "divorce" -> Efam_Divorce
    | "separated" -> Efam_Separated
    | "annulation" -> Efam_Annulation
    | "marriage_bann" -> Efam_MarriageBann
    | "marriage_contract" -> Efam_MarriageContract
    | "marriage_license" -> Efam_MarriageLicense
    | "pacs" -> Efam_PACS
    | "residence" -> Efam_Residence
    | s -> Efam_Name s

  let relation_kind_of_json js = match to_string js with
    | "married" -> Married
    | "not_married" -> NotMarried
    | "engaged" -> Engaged
    | "no_sexes_check_not_married" -> NoSexesCheckNotMarried
    | "no_mention" -> NoMention
    | "no_sexes_check_married" -> NoSexesCheckMarried
    | "marriage_bann" -> MarriageBann
    | "marriage_contract" -> MarriageContract
    | "marriage_license" -> MarriageLicense
    | "pacs" -> Pacs
    | "residence" -> Residence
    | s -> failwith s

  let fevent_witness_of_json = pevent_witness_of_json

  let fevent_of_json json =
    { efam_place = get_string "place" json
    ; efam_reason = get_string "reason" json
    ; efam_note = get_string "note" json
    ; efam_src = get_string "src" json
    ; efam_name = fevent_name_of_string (get_string "name" json)
    ; efam_date = cdate_of_json (member "date" json)
    ; efam_witnesses = Array.of_list (get_list "witnesses" fevent_witness_of_json json)
    }

end

module HTTP = struct include Http_curl end

let curl =
  let state = ref @@ Angstrom.Buffered.parse Jsonaf.parse in
  let write data =
    begin match !state with
      | Angstrom.Buffered.Partial k -> state := k (`String data)
      | _ -> assert false
    end ;
    String.length data
  in
  let cnt = ref 0 in
  fun ?(header = []) m url ->
    incr cnt ;
    state := Angstrom.Buffered.parse Jsonaf.parse ;
    HTTP.send header m url write ;
    match Angstrom.Buffered.state_to_result !state with
    | Ok js -> js
    | Error msg -> failwith msg

type iper = string
type ifam = string
type istr = string

type person = (iper, iper, istr) Def.gen_person
type ascend = ifam Def.gen_ascend
type union = ifam Def.gen_union
type family = (iper, ifam, istr) Def.gen_family
type couple = iper Def.gen_couple
type descend = iper Def.gen_descend

type ('iper, 'ifam, 'per, 'asc, 'uni, 'fam, 'cou, 'des) base =
  { bname : string
  ; mutable nb_of_persons : int
  ; mutable nb_of_real_persons : int
  ; mutable nb_of_families : int
  ; cache_person : (iper, person) Hashtbl.t
  ; cache_ascend : (iper, ascend) Hashtbl.t
  ; cache_union : (iper, union) Hashtbl.t
  ; cache_family : (ifam, family) Hashtbl.t
  ; cache_couple : (ifam, couple) Hashtbl.t
  ; cache_descend : (ifam, descend) Hashtbl.t
  ; mutable patch_person : ('iper * 'per) list
  ; mutable patch_ascend : ('iper * 'asc) list
  ; mutable patch_union : ('iper * 'uni) list
  ; mutable patch_family : ('ifam * 'fam) list
  ; mutable patch_couple : ('ifam * 'cou) list
  ; mutable patch_descend : ('ifam * 'des) list
  ; mutable insert_person : 'iper list
  ; mutable insert_family : 'ifam list
  ; mutable delete_person : 'iper list
  ; mutable delete_family : 'ifam list
  ; get : (string -> J.t)
  ; put : (string -> string -> J.t)
  ; post : (string -> string -> J.t)
  ; delete : (string -> string -> J.t)
  }

let open_base bdir =
  { bname = Filename.(let s = basename bdir in
                      if check_suffix s ".gwb" then chop_suffix s ".gwb" else s)
  ; nb_of_persons = -1
  ; nb_of_real_persons = -1
  ; nb_of_families = -1
  ; cache_person = Hashtbl.create 32
  ; cache_ascend = Hashtbl.create 32
  ; cache_union = Hashtbl.create 32
  ; cache_family = Hashtbl.create 32
  ; cache_couple = Hashtbl.create 32
  ; cache_descend = Hashtbl.create 32
  ; patch_person = []
  ; patch_ascend = []
  ; patch_union = []
  ; patch_family = []
  ; patch_couple = []
  ; patch_descend = []
  ; insert_person = []
  ; insert_family = []
  ; delete_person = []
  ; delete_family = []
  ; get = begin fun request -> curl `GET @@ Gwdb_driver_env.url ^ request end
  ; put = begin fun request data -> curl (`PUT data) @@ Gwdb_driver_env.url ^ request end
  ; post = begin fun request data -> curl (`POST data) @@ Gwdb_driver_env.url ^ request end
  ; delete = begin fun request data -> curl (`DELETE data) @@ Gwdb_driver_env.url ^ request end
  }

let dummy_iper : iper = "-1" (* FIXME *)
let dummy_ifam : ifam = "-1" (* FIXME *)

let patch_person base iper p =
  base.patch_person <- (iper, p) :: base.patch_person

let patch_ascend base iper a =
  base.patch_ascend <- (iper, a) :: base.patch_ascend

let patch_union base iper u =
  base.patch_union <- (iper, u) :: base.patch_union

let patch_couple base ifam c =
  base.patch_couple <- (ifam, c) :: base.patch_couple

let patch_descend base ifam d =
  base.patch_descend <- (ifam, d) :: base.patch_descend

let patch_family base ifam f =
  base.patch_family <- (ifam, f) :: base.patch_family

let insert_person base iper p =
  base.insert_person <- iper :: base.insert_person ;
  patch_person base iper p

let insert_ascend = patch_ascend

let insert_union = patch_union

let insert_family base ifam f =
  base.insert_family <- ifam :: base.insert_family ;
  patch_family base ifam f

let insert_couple = patch_couple

let insert_descend = patch_descend

let delete_person base iper =
  base.insert_person <- List.filter ((<>) iper) base.insert_person ;
  base.patch_person <- List.remove_assoc iper base.patch_person ;
  base.patch_ascend <- List.remove_assoc iper base.patch_ascend ;
  base.patch_union <- List.remove_assoc iper base.patch_union ;
  base.delete_person <- iper :: base.delete_person
let delete_ascend _ _ = ()
let delete_union _ _ = ()

let delete_family base ifam =
  base.insert_family <- List.filter ((<>) ifam) base.insert_family ;
  base.patch_family <- List.remove_assoc ifam base.patch_family ;
  base.patch_couple <- List.remove_assoc ifam base.patch_couple ;
  base.patch_descend <- List.remove_assoc ifam base.patch_descend ;
  base.delete_family <- ifam :: base.delete_family
let delete_couple _ _ = ()
let delete_descend _ _ = ()


(* FIXME: see Gwdb_arangoimport.print_pevent_witness *)
let commit_patches_p base sou get_person get_ascend get_union =
  let ipers =
    List.map fst base.patch_person
    |> List.rev_append (List.map fst base.patch_union)
    |> List.rev_append (List.map fst base.patch_ascend)
    |> List.sort_uniq compare
  in
  let patch iper =
    let aux patches def =
      match List.assoc_opt iper patches with
      | None -> def iper
      | Some p -> p
    in
    let p = aux base.patch_person get_person in
    let p = Futil.map_person_ps (fun i -> i) (fun i -> sou i) p in
    let a = aux base.patch_ascend get_ascend in
    let u = aux base.patch_union get_union in
    let bname = base.bname in
    `Assoc [ ("_key", Json.key_of_iper bname iper)
            ; ("basename", `String bname)
            ; ("person", Json.json_of_person bname p a u)
            ]
  in
  let data = Yojson.Basic.to_string @@ `List (List.map patch ipers) in
  base.put
    "_api/document/geneweb_persons?waitForSync=true&ignoreRevs=true&returnOld=false&returnNew=false"
    data
  |> ignore

let commit_patches_f base sou get_family get_couple get_descend =
  let ifams =
    List.map fst base.patch_family
    |> List.rev_append (List.map fst base.patch_couple)
    |> List.rev_append (List.map fst base.patch_descend)
    |> List.sort_uniq compare
  in
  let patch ifam =
    let aux patches def =
      match List.assoc_opt ifam patches with
      | None -> def ifam
      | Some p -> p
    in
    let f = aux base.patch_family get_family in
    let f = Futil.map_family_ps (fun i -> i) (fun i -> i) sou f in
    let c = aux base.patch_couple get_couple in
    let d = aux base.patch_descend get_descend in
    let bname = base.bname in
    `Assoc [ ("family", Json.json_of_family bname f c d)
           ; ("basename", `String bname)
           ; ("_key", Json.key_of_ifam bname ifam) ]
  in
  let data = Yojson.Basic.to_string @@ `List (List.map patch ifams) in
  ignore @@
  base.put
    "_api/document/geneweb_families?waitForSync=true&ignoreRevs=true&returnOld=false&returnNew=false"
    data

let commit_patches_r base get_person get_ascend get_union get_couple =
  let bname = base.bname in
  let edge from_ to_ kind payload =
    `Assoc begin
      [ ("_from", from_)
      ; ("_to", to_)
      ; ("kind", `String kind)
      ; ("basename", `String bname)
      ] |> fun acc ->
      if payload = `Null then acc else ("payload", payload) :: acc
    end
  in
  let unions =
    let ipers = List.map fst base.patch_union in
    List.map begin fun i ->
      let iph = Json.handler_of_iper bname i in
      Array.fold_left begin fun acc ifam ->
        let f = get_couple ifam in
        let from_, to_ =
          if i = Adef.father f
          then iph, Json.handler_of_iper bname (Adef.mother f)
          else Json.handler_of_iper bname (Adef.father f), iph
        in
        edge from_ to_ "union" (Json.handler_of_ifam bname ifam) :: acc
      end [] (get_union i).family
    end ipers
  in
  let ascends =
    let ipers = List.map fst base.patch_ascend in
    List.map begin fun i ->
      let iph = Json.handler_of_iper bname i in
      match (get_ascend i).parents with
      | None -> []
      | Some ifam ->
        let f = get_couple ifam in
        let i = Json.handler_of_ifam bname ifam in
        [ edge (Json.handler_of_iper bname @@ Adef.father f) iph "ascend" i
        ; edge (Json.handler_of_iper bname @@ Adef.mother f) iph "ascend" i ]
    end ipers
  in
  let persons =
    let ipers = List.map fst base.patch_person in
    List.map begin fun i ->
      let p = get_person i in
      let iph = Json.handler_of_iper bname i in
      let r =
        List.map begin fun i ->
          edge iph (Json.handler_of_iper bname i) "related" `Null
        end p.related
      in
      let witness_kind = function
        | Def.Witness -> "witness"
        | Witness_GodParent -> "godparent"
        | Witness_Officer -> "officer"
      in
      List.fold_left begin fun acc e ->
        Array.fold_left begin fun acc (i, wk) ->
          let e =
          edge iph (Json.handler_of_iper bname i) (witness_kind wk) `Null in
          e :: acc
        end acc e.Def.epers_witnesses
      end r (p.pevents)
    end ipers
  in
  let () =
    let ipers =
      List.rev_map fst base.patch_union
      |> List.rev_append (List.rev_map fst base.patch_ascend)
      |> List.rev_append (List.rev_map fst base.patch_person)
      |> List.rev_map (Json.handler_of_iper bname)
    in
    if ipers <> [] then
      let query =
        let ipers = Yojson.Basic.to_string (`List ipers) in
        {|FOR r IN geneweb_relations FILTER r.basename == "|} ^ bname
        ^ {|" AND (r._from IN |} ^ ipers ^ {| OR r._to IN |} ^ ipers ^ {|)|}
        ^ {| REMOVE r._key IN geneweb_relations|}
      in
      `Assoc [ "query", `String query ; "ttl", `Int 0 ]
      |> Yojson.Basic.to_string
      |> base.post "_api/cursor"
      |> ignore
  in
  let data = Yojson.Basic.to_string @@ `List (List.flatten @@ persons @ ascends @ unions) in
  ignore @@
  base.post
    "_api/document/geneweb_relations?waitForSync=true&ignoreRevs=true&returnOld=false&returnNew=false"
    data

let insert_p base =
  if base.insert_person <> [] then
    `List (List.map
             (fun i -> `Assoc [ ("_key", Json.key_of_iper base.bname i ) ])
             base.insert_person)
    |> Yojson.Basic.to_string
    |> base.post
      "_api/document/geneweb_persons?waitForSync=true&ignoreRevs=true&returnOld=false&returnNew=false"
    |> ignore

let insert_f base =
  if base.insert_family <> [] then
    `List (List.map
             (fun i -> `Assoc [ ("_key", Json.key_of_ifam base.bname i) ])
             base.insert_family)
    |> Yojson.Basic.to_string
    |> base.post
      "_api/document/geneweb_families?waitForSync=true&ignoreRevs=true&returnOld=false&returnNew=false"
    |> ignore

let delete_p base =
  if base.delete_person <> [] then
    `List (List.map
             (fun i -> `Assoc [ ("_key", Json.key_of_iper base.bname i ) ])
             base.delete_person)
    |> Yojson.Basic.to_string
    |> base.delete
      "_api/document/geneweb_persons?waitForSync=true&ignoreRevs=true&returnOld=false&returnNew=false"
    |> ignore

let delete_f base =
  if base.delete_family <> [] then
    `List (List.map
             (fun i -> `Assoc [ ("_key", Json.key_of_ifam base.bname i ) ])
             base.delete_family)
    |> Yojson.Basic.to_string
    |> base.delete
      "_api/document/geneweb_families?waitForSync=true&ignoreRevs=true&returnOld=false&returnNew=false"
    |> ignore

(* TODO: use transaction *)
let commit_patches base sou get_person get_ascend get_union get_family get_couple get_descend =
  delete_p base ;
  delete_f base ;
  insert_p base ;
  insert_f base ;
  commit_patches_r base get_person get_ascend get_union get_couple ;
  commit_patches_p base sou get_person get_ascend get_union ;
  commit_patches_f base sou get_family get_couple get_descend


(* READING *)

let title_of_json json =
  { Def.t_name = (match J.member "name" json with `Null -> Tnone | `String "" -> Tmain | `String s -> Tname s | _ -> assert false)
  ; t_ident = J.get_string "ident" json
  ; t_place = J.get_string "place" json
  ; t_date_start = J.cdate_of_json @@ J.member "date_start" json
  ; t_date_end = J.cdate_of_json @@ J.member "date_end" json
  ; t_nth = J.get_int "nth" json
  }

let person_of_json (json : J.t) =
  let iper = J.to_string (J.member "_key" json) in
  let json = J.member "person" json in
  let access = Array.get [| IfTitles ; Public ; Private |] @@ J.get_int "access" json in
  let aliases = J.get_list "aliases" J.to_string json in
  let pevents = J.get_list "pevents" J.pevent_of_json json in
  let get_event_aux names def fn =
    let rec loop = function
      | [] -> None
      | e :: _ when List.mem e.epers_name names -> Some e
      | _ :: tl -> loop tl
    in
    match loop pevents with
    | Some e -> (fn e , e.epers_place, e.epers_note, e.epers_src)
    | None -> (def, "", "", "")
  in
  let baptism, baptism_place, baptism_note, baptism_src =
    get_event_aux [ Epers_Baptism ] Adef.cdate_None @@ fun e -> e.epers_date
  in
  let birth, birth_place, birth_note, birth_src =
    get_event_aux [ Epers_Birth ] Adef.cdate_None @@ fun e -> e.epers_date
  in
  let burial, burial_place, burial_note, burial_src =
    get_event_aux [ Epers_Cremation ; Epers_Burial ] UnknownBurial @@
    function
    | { epers_name = Epers_Cremation ; epers_date } -> Cremated epers_date
    | { epers_name = Epers_Burial ; epers_date } -> Buried epers_date
    | _ -> assert false
  in
  let death_date, death_place, death_note, death_src =
    get_event_aux [ Epers_Death ] Adef.cdate_None @@ fun e -> e.epers_date
  in
  let death = match J.get_string "death" json with
    | "notdead" -> Def.NotDead
    | "killed" -> Death (Killed, death_date)
    | "murdered" -> Death (Murdered, death_date)
    | "executed" -> Death (Executed, death_date)
    | "disappeared" -> Death (Disappeared, death_date)
    | "unspecified" -> Death (Unspecified, death_date)
    | "deadyoung" -> DeadYoung
    | "deaddontknowwhen" -> DeadDontKnowWhen
    | "dontknowifdead" -> DontKnowIfDead
    | "ofcoursedead" -> OfCourseDead
    | _ -> assert false
  in
  let occupation = J.get_string "occupation" json in
  let first_name =
    match J.member "firstname" json with
    | `String s -> s
    | _ -> ""
  in
  let first_names_aliases = J.get_list "first_names_aliases" J.to_string json in
  let image = J.get_string "image" json in
  let notes = J.get_string "note" json in
  let occ = J.get_int "occ" json in
  let psources = J.get_string "psources" json in
  let public_name = J.get_string "public_name" json in
  let qualifiers = J.get_list "qualifiers" J.to_string json in
  let related = J.get_list "related" J.to_string json in
  let rparents = J.get_list "rparents" J.rparent_of_json json in
  let sex = Array.get [| Def.Neuter ; Def.Male ; Def.Female |] @@ J.get_int "sex" json in
  let surname = J.get_string "lastname" json in
  let surnames_aliases = J.get_list "surnames_aliases" J.to_string json in
  let titles = J.get_list "titles" title_of_json json in
  { first_name
  ; surname
  ; occ
  ; image
  ; public_name
  ; qualifiers
  ; aliases
  ; first_names_aliases
  ; surnames_aliases
  ; titles
  ; rparents
  ; related
  ; occupation
  ; sex
  ; access
  ; birth
  ; birth_place
  ; birth_note
  ; birth_src
  ; baptism
  ; baptism_place
  ; baptism_note
  ; baptism_src
  ; death
  ; death_place
  ; death_note
  ; death_src
  ; burial
  ; burial_place
  ; burial_note
  ; burial_src
  ; pevents
  ; notes
  ; psources
  ; key_index = iper
  }

let union_of_json = function
  | `Array [ `String iper ; `Array json ] ->
    iper, { family = Array.of_list @@ List.map (function `String x -> x | _ -> assert false) json }
  | `Array [ `String iper ; `Null ] -> iper, { family = [||] }
  | _ -> assert false

let no_ascend = { parents = None ; consang = Adef.no_consang }

let consang_of_json js =
  match J.member "consang" @@ J.member "person" js with
  | `Null -> Adef.no_consang
  | `Number f -> Adef.fix @@ int_of_string f
  | _ -> assert false

let cache_pau_js base js =
  let p = person_of_json js in
  let iper = p.key_index in
  let a =
    match J.member "parents" @@ J.member "person" js with
    | `String x -> { parents = Some x ; consang = consang_of_json js }
    | _ -> { no_ascend with consang = consang_of_json js }
  in
  let u =
    match J.member "unions" @@ J.member "person" js with
    | `Array x -> { family = Array.of_list @@ List.map (function `String x -> x
                                                               | _ -> assert false) x
      }
    | _ -> { family = [||] }
  in
  Hashtbl.replace base.cache_person iper p ;
  Hashtbl.replace base.cache_ascend iper a ;
  Hashtbl.replace base.cache_union iper u ;
  iper

let cache_pau base iper =
  cache_pau_js base @@
  base.get @@ "_api/document/geneweb_persons/" ^ iper

let aux_pau base cache iper =
  if iper = dummy_iper then raise Not_found ;
  match Hashtbl.find_opt cache iper with
  | Some x -> x
  | None -> Hashtbl.find cache @@ cache_pau base iper

let get_person base iper =
  aux_pau base base.cache_person iper

let get_ascend base iper =
  aux_pau base base.cache_ascend iper

let get_union base iper =
  aux_pau base base.cache_union iper

let family_of_json json =
  let ifam = J.to_string (J.member "_key" json) in
  let json = J.member "family" json in
  { marriage = J.cdate_of_json @@ J.member "marriage" json
  ; marriage_place = J.get_string "marriage_place" json
  ; marriage_note = J.get_string "marriage_note" json
  ; marriage_src = J.get_string "marriage_src" json
  ; witnesses = Array.of_list (J.get_list "witnesses" J.to_string json)
  ; relation = J.member "relation_kind" json |> J.relation_kind_of_json
  ; divorce = J.divorce_of_json (J.member "divorce" json)
  ; fevents = J.get_list "fevents" J.fevent_of_json json
  ; comment = J.get_string "comment" json
  ; origin_file = J.get_string "origin_file" json
  ; fsources = J.get_string "fsources" json
  ; fam_index = ifam
  }

let couple_of_json js =
  match J.to_list js with
  | [ `String ifam ; parents ] ->
    begin match J.to_list_of J.to_string parents with
      | [ father ; mother ] -> ifam, Adef.couple father mother
      | _ -> assert false
    end
  | _ -> assert false

let descend_of_json = function
  | `Array list -> { children = Array.of_list @@ List.map J.to_string list }
  | `Null -> { children = [||] }
  | _ -> assert false

let cache_fcd_js base js =
  let f = family_of_json js in
  let ifam = f.fam_index in
  let c =
    match J.member "parents" @@ J.member "family" js with
    | `Array [ `String father ; `String mother ] -> Adef.couple father mother
    | _ -> Adef.couple dummy_iper dummy_iper
  in
  let d =
    match J.member "children" @@ J.member "family" js with
    | `Array list -> { children = Array.of_list @@ List.map J.to_string list }
    | _ -> { children = [||] }
  in
  Hashtbl.replace base.cache_family ifam f ;
  Hashtbl.replace base.cache_couple ifam c ;
  Hashtbl.replace base.cache_descend ifam d ;
  ifam

let cache_fcd base ifam =
  cache_fcd_js base @@ base.get @@ "_api/document/geneweb_families/" ^ ifam

let aux_fcd base cache ifam =
  if ifam = dummy_ifam then raise Not_found ;
  match Hashtbl.find_opt cache ifam with
  | Some x -> x
  | None -> Hashtbl.find cache @@ cache_fcd base ifam

let get_family base ifam =
  aux_fcd base base.cache_family ifam

let get_couple base ifam =
  aux_fcd base base.cache_couple ifam

let get_descend base ifam =
  aux_fcd base base.cache_descend ifam

let nb_of base collection filter =
    let query =
      {|RETURN LENGTH (FOR x IN |}
      ^ collection ^ {| FILTER x.basename == "|} ^ base.bname ^ {|"|} ^ filter ^ {| RETURN 1)|}
    in
    J.show @@ J.assoc [ ("query", J.string query) ]
    |> base.post "_api/cursor"
    |> J.member "result"
    |> J.to_list
    |> List.hd
    |> J.to_int

let nb_of_families base =
  if base.nb_of_families = -1 then base.nb_of_families <- nb_of base "geneweb_families" "" ;
  base.nb_of_families

let nb_of_persons base =
  if base.nb_of_persons = -1 then base.nb_of_persons <- nb_of base "geneweb_persons" "" ;
  base.nb_of_persons

let nb_of_real_persons base =
  if base.nb_of_real_persons = -1
  then base.nb_of_real_persons <-
      nb_of base "geneweb_persons"
        {| AND (x.person.firstname != "?" OR x.person.lastname != "?") |} ;
  base.nb_of_real_persons

let cursor_next js =
  ( (if J.to_bool @@ J.member "hasMore" js
     then Some (J.to_string @@ J.member "id" js)
     else None)
  , J.member "result" js)

let cursor base batchsize query =
  let data = J.assoc [ ("query", J.string query) ; ("batchSize", J.int batchsize) ] in
  let data = J.show data in
  let init = base.post "_api/cursor" data in
  cursor_next init

type 'a cursor = { length : int ; get : int -> 'a option }

let cursor_loop cursor fn =
  let rec loop (next, current) =
    begin match current with `Array array -> fn array | _ -> assert false end ;
    match next with Some url -> loop (cursor_next @@ curl (`PUT "") url) | None -> ()
  in loop cursor

let cursor base collection length =
  let length = length base in
  let query =
    {| FOR x IN |} ^ collection ^ {| FILTER x.basename == "|} ^ base.bname ^ {|" RETURN x |}
  in
  let (next, current) = cursor base 10000 query in
  let next = ref next in
  let current = ref @@ match current with `Array array -> array | _ -> assert false in
  let previous = ref (-1) in
  let rec get i =
    assert (!previous = i - 1) ;
    match !current with
    | hd :: tl -> current := tl ; incr previous ; Some hd
    | [] -> match !next with
      | Some id ->
        let (next', current') = cursor_next @@ base.put ("_api/cursor/" ^ id) "" in
        next := next' ;
        current := (match current' with `Array array -> array | _ -> assert false) ;
        get i
      | None -> None
  in
  { length ; get }

let persons base = cursor base "geneweb_persons" nb_of_persons
let families base = cursor base "geneweb_families" nb_of_families
