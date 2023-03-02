open Def
open Gwdb

type per
type fam

type 'a event_name =
  | Pevent of 'a Def.gen_pers_event_name
  | Fevent of 'a Def.gen_fam_event_name

let pevent_name s = Pevent s
let fevent_name s = Fevent s

(*type 'a event_item = {
  name :'a event_name;
  date : cdate;
  place : istr;
  note : istr;
  src : istr;
  witnesses : (iper * witness_kind) array;
  witness_notes : istr array option;
  spouse : iper option
  }*)

type 'a event_item =
    PE of Gwdb.pers_event * 'a event_name
  | FE of Gwdb.fam_event * 'a event_name * iper option

let wrap p f (e : 'a event_item) = match e with
  | PE (e, _) -> p e
  | FE (e, _, _) -> f e

let get_name ei = match ei with
    PE (_, name) | FE (_, name, _) -> name

let get_date ei = wrap get_pevent_date get_fevent_date ei
let get_place ei = wrap get_pevent_place get_fevent_place ei
let get_note ei = wrap get_pevent_note get_fevent_note ei
let get_src ei = wrap get_pevent_src get_fevent_src ei
let get_witnesses ei = wrap get_pevent_witnesses get_fevent_witnesses ei
let get_witness_notes ei = wrap get_pevent_witness_notes get_fevent_witness_notes ei
let get_witnesses_and_notes ei =
  wrap get_pevent_witnesses_and_notes get_fevent_witnesses_and_notes ei
let get_spouse_iper ei = match ei with
  | PE _ -> None
  | FE (_, _, sp) -> sp
(*let get_witnesses_and_notes ei =
  let get_notes i = match ei.witness_notes with
    | Some notes when Array.length notes > 0 -> notes.(i)
    | _ -> empty_string
  in
  Array.init (Array.length ei.witnesses) (fun i ->
      let ip, wk = ei.witnesses.(i) in
      ip, wk, get_notes i
    )
*)
let has_witnesses ei =
  let witnesses = get_witnesses ei in
  Array.length witnesses > 0

let event_item_of_pevent pe = PE (pe, pevent_name (Gwdb.get_pevent_name pe))
let event_item_of_fevent ?sp fe = FE (fe, fevent_name (Gwdb.get_fevent_name fe), sp)

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
    let pevents = List.map event_item_of_pevent (get_pevents p) in
    let events =
      Array.fold_right (fun ifam events ->
          let fam = foi base ifam in
          let isp = Gutil.spouse (get_iper p) fam in
          let m_auth = Util.authorized_age conf base (Util.pget conf base isp) in
          if not m_auth then events
          else List.fold_right (fun fe events -> event_item_of_fevent ~sp:isp fe :: events) (get_fevents fam) events
        ) (get_family p) pevents
    in
    events

let sorted_events conf base p =
  let unsorted_events = events conf base p in
  sort_events get_name get_date unsorted_events
