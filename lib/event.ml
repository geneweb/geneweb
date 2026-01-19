type 'a event_name =
  | Pevent of 'a Def.gen_pers_event_name
  | Fevent of 'a Def.gen_fam_event_name

let pevent_name s = Pevent s
let fevent_name s = Fevent s

type 'a event_item =
  | PE of Gwdb.pers_event * 'a event_name
  | FE of Gwdb.fam_event * 'a event_name * Gwdb.iper option
  | DPE of (Gwdb.iper, Gwdb.istr) Def.gen_pers_event * 'a event_name
  | DFE of
      (Gwdb.iper, Gwdb.istr) Def.gen_fam_event
      * 'a event_name
      * Gwdb.iper option

let wrap p f defp deff (e : 'a event_item) =
  match e with
  | PE (e, _) -> p e
  | FE (e, _, _) -> f e
  | DPE (e, _) -> defp e
  | DFE (e, _, _) -> deff e

let get_name = function
  | PE (_, name) | FE (_, name, _) -> name
  | DPE (_, name) | DFE (_, name, _) -> name

let get_date ei =
  wrap Gwdb.get_pevent_date Gwdb.get_fevent_date
    (fun e -> e.Def.epers_date)
    (fun e -> e.Def.efam_date)
    ei

let get_place ei =
  wrap Gwdb.get_pevent_place Gwdb.get_fevent_place
    (fun e -> e.Def.epers_place)
    (fun e -> e.Def.efam_place)
    ei

let get_note ei =
  wrap Gwdb.get_pevent_note Gwdb.get_fevent_note
    (fun e -> e.Def.epers_note)
    (fun e -> e.Def.efam_note)
    ei

let get_src ei =
  wrap Gwdb.get_pevent_src Gwdb.get_fevent_src
    (fun e -> e.Def.epers_src)
    (fun e -> e.Def.efam_src)
    ei

let get_witnesses ei =
  wrap Gwdb.get_pevent_witnesses Gwdb.get_fevent_witnesses
    (fun e -> Array.map (fun (a, b, _) -> (a, b)) e.Def.epers_witnesses)
    (fun e -> Array.map (fun (a, b, _) -> (a, b)) e.Def.efam_witnesses)
    ei

let get_witness_notes ei =
  wrap Gwdb.get_pevent_witness_notes Gwdb.get_fevent_witness_notes
    (fun e -> Array.map (fun (_, _, n) -> n) e.Def.epers_witnesses)
    (fun e -> Array.map (fun (_, _, n) -> n) e.Def.efam_witnesses)
    ei

let get_witnesses_and_notes ei =
  wrap Gwdb.get_pevent_witnesses_and_notes Gwdb.get_fevent_witnesses_and_notes
    (fun e -> e.Def.epers_witnesses)
    (fun e -> e.Def.efam_witnesses)
    ei

let get_spouse_iper ei =
  match ei with PE _ | DPE _ -> None | FE (_, _, sp) | DFE (_, _, sp) -> sp

let has_witnesses ei =
  let nb_witnesses =
    match ei with
    | PE _ | FE (_, _, _) -> Array.length (get_witnesses ei)
    | DPE (e, _) -> Array.length e.Def.epers_witnesses
    | DFE (e, _, _) -> Array.length e.Def.efam_witnesses
  in
  nb_witnesses > 0

let has_witness_note ei =
  match ei with
  | PE (e, _) ->
      Array.exists
        (fun n -> not (Gwdb.is_empty_string n))
        (Gwdb.get_pevent_witness_notes e)
  | FE (e, _, _) ->
      Array.exists
        (fun n -> not (Gwdb.is_empty_string n))
        (Gwdb.get_fevent_witness_notes e)
  | DPE (e, _) ->
      Array.exists
        (fun (_, _, n) -> not (Gwdb.is_empty_string n))
        e.Def.epers_witnesses
  | DFE (e, _, _) ->
      Array.exists
        (fun (_, _, n) -> not (Gwdb.is_empty_string n))
        e.Def.efam_witnesses

let event_item_of_pevent pe = PE (pe, pevent_name (Gwdb.get_pevent_name pe))

let event_item_of_fevent ~sp fe =
  FE (fe, fevent_name (Gwdb.get_fevent_name fe), sp)

let event_item_of_gen_pevent evt = DPE (evt, pevent_name evt.Def.epers_name)

let event_item_of_gen_fevent ~sp evt =
  DFE (evt, fevent_name evt.Def.efam_name, sp)

(*
   On ignore les événements personnalisés.
   Dans l'ordre de priorité :
     birth, baptism, ..., death, funeral, burial/cremation.
   Pour les évènements familiaux, cet ordre est envisageable :
     engage, PACS, marriage bann, marriage contract, marriage, ...,
     separate, divorce
*)
let compare_event_name name1 name2 =
  match (name1, name2) with
  | Pevent Def.Epers_Birth, _ -> -1
  | _, Pevent Def.Epers_Birth -> 1
  | ( Pevent Def.Epers_Baptism,
      Pevent
        ( Def.Epers_Death | Def.Epers_Funeral | Def.Epers_Burial
        | Def.Epers_Cremation ) ) ->
      -1
  | ( Pevent
        ( Def.Epers_Death | Def.Epers_Funeral | Def.Epers_Burial
        | Def.Epers_Cremation ),
      Pevent Def.Epers_Baptism ) ->
      1
  | Pevent Def.Epers_Cremation, Pevent Def.Epers_Burial -> -1
  | Pevent (Def.Epers_Burial | Def.Epers_Cremation), _ -> 1
  | _, Pevent (Def.Epers_Burial | Def.Epers_Cremation) -> -1
  | Pevent Def.Epers_Funeral, _ -> 1
  | _, Pevent Def.Epers_Funeral -> -1
  | Pevent Def.Epers_Death, _ -> 1
  | _, Pevent Def.Epers_Death -> -1
  | _, _ -> 0

let int_of_fevent_name = function
  | Def.Efam_NoMarriage -> 0
  | Def.Efam_PACS -> 1
  | Def.Efam_Engage -> 2
  | Def.Efam_MarriageBann -> 3
  | Def.Efam_MarriageContract -> 4
  | Def.Efam_MarriageLicense -> 5
  | Def.Efam_Marriage -> 6
  | Def.Efam_Residence -> 7
  | Def.Efam_Separated -> 8
  | Def.Efam_Annulation -> 9
  | Def.Efam_Divorce -> 10
  | Def.Efam_NoMention -> 11
  | Def.Efam_Name _ -> 12

let compare_fevent_name name1 name2 =
  let i1 = int_of_fevent_name name1 in
  let i2 = int_of_fevent_name name2 in
  i1 - i2

let better_compare_event_name name1 name2 =
  let c = compare_event_name name1 name2 in
  if c <> 0 then c
  else
    match (name1, name2) with
    (* put Fevent after Pevent *)
    | Fevent _, Pevent _ -> 1
    | Pevent _, Fevent _ -> -1
    (* this is to make event order stable; depends on type definition order! *)
    | Fevent e1, Fevent e2 -> compare_fevent_name e1 e2
    | Pevent e1, Pevent e2 -> compare e1 e2

(* try to handle the fact that events are not well ordered *)
let sort_events get_name get_date events =
  let dated, undated =
    List.fold_left
      (fun (dated, undated) e ->
        match Date.cdate_to_dmy_opt (get_date e) with
        | None -> (dated, e :: undated)
        | Some _d -> (e :: dated, undated))
      ([], []) events
  in
  (* we need this to keep the input with same date ordered
     by their creation order *)
  let dated, undated = (List.rev dated, List.rev undated) in

  (* this do not define a preorder (no transitivity);
     can not be used to sort a list
     ex:
      let a,b,c events with
        a.date = Some 2022;
        b.date = None;
        c.date = Some 2000;
      we can have a <= b and b <= c because of event name.
      but we do not have a <= c
  *)
  let cmp e1 e2 =
    let cmp_name e1 e2 =
      better_compare_event_name (get_name e1) (get_name e2)
    in
    match Date.cdate_to_dmy_opt (get_date e1) with
    | None -> cmp_name e1 e2
    | Some d1 -> (
        match Date.cdate_to_dmy_opt (get_date e2) with
        | None -> cmp_name e1 e2
        | Some d2 ->
            let x = Date.compare_dmy d1 d2 in
            if x = 0 then cmp_name e1 e2 else x)
  in

  (* sort events with dates separately to make sure
     that dates are in correct order *)
  let l1 = List.stable_sort cmp dated in
  let l2 = List.stable_sort cmp undated in
  List.merge cmp l1 l2

let events conf base p =
  if not (Person.is_visible conf base p) then []
  else
    let pevents = List.map event_item_of_pevent (Gwdb.get_pevents p) in
    let events =
      (* append fevents *)
      Array.fold_right
        (fun ifam events ->
          let fam = Gwdb.foi base ifam in
          let isp = Gutil.spouse (Gwdb.get_iper p) fam in
          (* filter family event with contemporary spouse *)
          let m_auth = Person.is_visible conf base (Util.pget conf base isp) in
          if not m_auth then events
          else
            List.fold_right
              (fun fe events ->
                event_item_of_fevent ~sp:(Some isp) fe :: events)
              (Gwdb.get_fevents fam) events)
        (Gwdb.get_family p) pevents
    in
    events

let sorted_events conf base p =
  let unsorted_events = events conf base p in
  sort_events get_name get_date unsorted_events

let other_events conf base p =
  let is_other_event e =
    match get_name e with
    | Pevent Epers_Birth
    | Pevent Epers_Baptism
    | Pevent Epers_Death
    | Pevent Epers_Burial
    | Fevent Efam_Marriage ->
        false
    | Fevent
        ( Efam_NoMarriage | Efam_NoMention | Efam_Engage | Efam_Divorce
        | Efam_Separated | Efam_Annulation | Efam_MarriageBann
        | Efam_MarriageContract | Efam_MarriageLicense | Efam_PACS
        | Efam_Residence | Efam_Name _ )
    | Pevent
        ( Epers_Cremation | Epers_Accomplishment | Epers_Acquisition
        | Epers_Adhesion | Epers_BaptismLDS | Epers_BarMitzvah
        | Epers_BatMitzvah | Epers_Benediction | Epers_ChangeName
        | Epers_Circumcision | Epers_Confirmation | Epers_ConfirmationLDS
        | Epers_Decoration | Epers_DemobilisationMilitaire | Epers_Diploma
        | Epers_Distinction | Epers_Dotation | Epers_DotationLDS
        | Epers_Education | Epers_Election | Epers_Emigration
        | Epers_Excommunication | Epers_FamilyLinkLDS | Epers_FirstCommunion
        | Epers_Funeral | Epers_Graduate | Epers_Hospitalisation | Epers_Illness
        | Epers_Immigration | Epers_ListePassenger | Epers_MilitaryDistinction
        | Epers_MilitaryPromotion | Epers_MilitaryService
        | Epers_MobilisationMilitaire | Epers_Naturalisation | Epers_Occupation
        | Epers_Ordination | Epers_Property | Epers_Recensement
        | Epers_Residence | Epers_Retired | Epers_ScellentChildLDS
        | Epers_ScellentParentLDS | Epers_ScellentSpouseLDS | Epers_VenteBien
        | Epers_Will | Epers_Name _ | Epers_Adoption ) ->
        true
  in
  p |> events conf base |> List.filter is_other_event

type ('string, 'person) witness = {
  person : 'person;
  kind : Def.witness_kind;
  note : 'string;
}

type ('string, 'person) union = {
  kind : Def.relation_kind;
  date : Adef.cdate;
  place : 'string;
  note : 'string;
  source : 'string;
  witnesses : ('string, 'person) witness array;
}

type ('string, 'person) main_family_events = {
  main_union : ('string, 'person) union option;
  main_separation : Def.divorce option;
}

let get_main_family_events fevents =
  (* On tri les évènements pour être sûr. *)
  let fevents =
    sort_events
      (fun evt -> Fevent evt.Def.efam_name)
      (fun evt -> evt.efam_date)
      fevents
  in
  let found_marriage = ref None in
  let found_divorce = ref None in
  let mk_marr evt kind =
    let e =
      Some
        {
          kind;
          date = evt.Def.efam_date;
          place = evt.efam_place;
          note = evt.efam_note;
          source = evt.efam_src;
          witnesses =
            Array.map
              (fun (person, kind, note) -> { person; kind; note })
              evt.efam_witnesses;
        }
    in
    match Option.map (fun (union : _ union) -> union.kind) !found_marriage with
    | None -> found_marriage := e
    | Some (Def.NoMention | Residence)
      when kind <> NoMention && kind <> Residence ->
        found_marriage := e
    | Some Married when kind <> Married -> ()
    | _ -> if kind = Married then found_marriage := e
  in
  let mk_div kind =
    match !found_divorce with
    | None -> found_divorce := Some kind
    | Some _ -> ()
  in
  (* Marriage is more important than any other relation.
     For other cases, latest event is the most important,
     except for NotMention and Residence. *)
  (* FIXME: For now, we ignore Annulation since it gives a wrong date
     (relation on [annulation date] makes no sense) *)
  let rec loop = function
    | [] -> ()
    | evt :: l -> (
        match evt.Def.efam_name with
        | Efam_Engage ->
            mk_marr evt Engaged;
            loop l
        | Efam_Marriage ->
            mk_marr evt Married;
            loop l
        | Efam_MarriageContract ->
            mk_marr evt MarriageContract;
            loop l
        | Efam_NoMention ->
            mk_marr evt NoMention;
            loop l
        | Efam_MarriageBann ->
            mk_marr evt MarriageBann;
            loop l
        | Efam_MarriageLicense ->
            mk_marr evt MarriageLicense;
            loop l
        | Efam_PACS ->
            mk_marr evt Pacs;
            loop l
        | Efam_Residence ->
            mk_marr evt Residence;
            loop l
        | Efam_NoMarriage ->
            mk_marr evt NotMarried;
            loop l
        | Efam_Divorce ->
            mk_div (Def.Divorced evt.efam_date);
            loop l
        | Efam_Separated ->
            mk_div Separated;
            loop l
        | Efam_Annulation -> loop l
        | Efam_Name _ -> loop l)
  in
  loop (List.rev fevents);
  { main_union = !found_marriage; main_separation = !found_divorce }
