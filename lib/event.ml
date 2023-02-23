open Def
open Gwdb

type per
type fam

type 'a event_name =
  | Pevent of 'a Def.gen_pers_event_name
  | Fevent of 'a Def.gen_fam_event_name

let pevent_name s = Pevent s
let fevent_name s = Fevent s

type 'a event_item = {
  name :'a event_name;
  date : cdate;
  place : istr;
  note : istr;
  src : istr;
  witnesses : (iper * witness_kind) array;
  witness_notes : istr array option;
  spouse : iper option
}

let get_name : 'a event_item -> 'a event_name = fun ei -> ei.name
let get_date : 'a event_item -> Def.cdate = fun ei -> ei.date
let get_place : 'a event_item -> istr = fun ei -> ei.place
let get_note : 'a event_item -> istr = fun ei -> ei.note
let get_src : 'a event_item -> istr = fun ei -> ei.src
let get_witnesses : 'a event_item -> (iper * Def.witness_kind) array = fun ei -> ei.witnesses
let get_witness_notes : 'a event_item ->  istr array option = fun ei -> ei.witness_notes
let get_spouse_iper : 'a event_item -> iper option = fun ei -> ei.spouse

let has_witnesses : 'a event_item -> bool = fun ei -> Array.length ei.witnesses > 0

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
  | Pevent Epers_Birth, _ -> -1
  | _, Pevent Epers_Birth -> 1
  | ( Pevent Epers_Baptism,
      Pevent (Epers_Death | Epers_Funeral | Epers_Burial | Epers_Cremation) ) ->
      -1
  | ( Pevent (Epers_Death | Epers_Funeral | Epers_Burial | Epers_Cremation),
      Pevent Epers_Baptism ) ->
      1
  | Pevent Epers_Cremation, Pevent Epers_Burial -> -1
  | Pevent (Epers_Burial | Epers_Cremation), _ -> 1
  | _, Pevent (Epers_Burial | Epers_Cremation) -> -1
  | Pevent Epers_Funeral, _ -> 1
  | _, Pevent Epers_Funeral -> -1
  | Pevent Epers_Death, _ -> 1
  | _, Pevent Epers_Death -> -1
  | _ -> 0
(*TODO Fevent??*)

let compare get_name get_date e1 e2 =
  match Date.cdate_to_dmy_opt (get_date e1) with
  | None -> compare_event_name (get_name e1) (get_name e2)
  | Some d1 -> (
      match Date.cdate_to_dmy_opt (get_date e2) with
      | None -> compare_event_name (get_name e1) (get_name e2)
      | Some d2 -> (
          match Date.compare_dmy_opt ~strict:false d1 d2 with
          | Some 0 | None -> compare_event_name (get_name e1) (get_name e2)
          | Some x -> x))

let sort_events get_name get_date events =
  List.stable_sort (fun e1 e2 -> compare get_name get_date e1 e2) events

let events conf base p =
  if not (Util.authorized_age conf base p) then []
  else
    let pevents =
      List.fold_right
        (fun evt events ->
          let name = Pevent (get_pevent_name evt) in
          let date = get_pevent_date evt in
          let place = get_pevent_place evt in
          let note = get_pevent_note evt in
          let src = get_pevent_src evt in
          let witnesses = get_pevent_witnesses evt in
          let witness_notes = Some (get_pevent_witness_notes evt) in
          let event_item = {name; date; place; note; src; witnesses; witness_notes; spouse = None} in
          event_item  :: events)
        (get_pevents p) []
    in
    let fevents =
      Array.fold_right
        (fun ifam fevents ->
          let fam = foi base ifam in
          let isp = Gutil.spouse (get_iper p) fam in
          let m_auth =
            Util.authorized_age conf base (Util.pget conf base isp)
          in
          let fam_fevents =
            if m_auth then
              List.fold_right
                (fun evt fam_fevents ->
                  let name = Fevent (get_fevent_name evt) in
                  let date = get_fevent_date evt in
                  let place = get_fevent_place evt in
                  let note = get_fevent_note evt in
                  let src = get_fevent_src evt in
                  let witnesses = get_fevent_witnesses evt in
                  let witness_notes = Some (get_fevent_witness_notes evt) in
                  let spouse = Some isp in
                  let event_item = {name; date; place; note; src; witnesses; witness_notes; spouse} in
                  event_item :: fam_fevents)
                (get_fevents fam) []
            else []
          in
          fam_fevents @ fevents)
        (get_family p) []
    in
    pevents @ fevents

let sorted_events conf base p =
  let unsorted_events = events conf base p in
  sort_events get_name get_date unsorted_events
