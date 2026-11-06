open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

type 'a event_name =
  | Pevent of 'a gen_pers_event_name
  | Fevent of 'a gen_fam_event_name

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

let life_events =
  [
    Epers_Birth;
    Epers_Baptism;
    Epers_Death;
    Epers_Funeral;
    Epers_Cremation;
    Epers_Burial;
  ]

let life_rank name =
  let rec loop i l =
    match l with
    | [] -> None
    | n :: l -> if n = name then Some i else loop (i + 1) l
  in
  loop 0 life_events

let compare_names name1 name2 =
  match (name1, name2) with
  | Pevent n1, Pevent n2 -> (
      match (life_rank n1, life_rank n2) with
      | Some r1, Some r2 -> Int.compare r1 r2
      | (Some _ | None), _ -> 0)
  | Pevent _, Fevent _ | Fevent _, (Pevent _ | Fevent _) ->
      compare_event_name name1 name2

let compare get_name get_date e1 e2 =
  match Date.cdate_to_gregorian_dmy_opt (get_date e1) with
  | None -> compare_names (get_name e1) (get_name e2)
  | Some d1 -> (
      match Date.cdate_to_gregorian_dmy_opt (get_date e2) with
      | None -> compare_names (get_name e1) (get_name e2)
      | Some d2 -> (
          match Date.compare_dmy_period d1 d2 with
          | 0 -> compare_names (get_name e1) (get_name e2)
          | x -> x))

let pevent_key e = Hasher.SHA256.(feeder_to_hasher (gen_pers_event iper istr)) e
let fevent_key e = Hasher.SHA256.(feeder_to_hasher (gen_fam_event iper istr)) e

let sort_events get_name get_date events =
  List.stable_sort (fun e1 e2 -> compare get_name get_date e1 e2) events

type 'a event_item =
  'a event_name
  * Adef.cdate
  * Driver.istr
  * Driver.istr
  * Driver.istr
  * (Driver.iper * witness_kind) array
  * Driver.iper option

let events conf base p =
  if not (Util.authorized_age conf base p) then []
  else
    let pevents =
      List.fold_right
        (fun evt events ->
          let name = Pevent evt.epers_name in
          let date = evt.epers_date in
          let place = evt.epers_place in
          let note = evt.epers_note in
          let src = evt.epers_src in
          let wl = evt.epers_witnesses in
          let x = (name, date, place, note, src, wl, None) in
          x :: events)
        (Driver.get_pevents p) []
    in
    let fevents =
      Array.fold_right
        (fun ifam fevents ->
          let fam = Driver.foi base ifam in
          let isp = Gutil.spouse (Driver.get_iper p) fam in
          let m_auth =
            Util.authorized_age conf base (Util.pget conf base isp)
          in
          let fam_fevents =
            if m_auth then
              List.fold_right
                (fun evt fam_fevents ->
                  let name = Fevent evt.efam_name in
                  let date = evt.efam_date in
                  let place = evt.efam_place in
                  let note = evt.efam_note in
                  let src = evt.efam_src in
                  let wl = evt.efam_witnesses in
                  let x = (name, date, place, note, src, wl, Some isp) in
                  x :: fam_fevents)
                (Driver.get_fevents fam) []
            else []
          in
          fam_fevents @ fevents)
        (Driver.get_family p) []
    in
    pevents @ fevents

let sorted_events conf base p =
  let unsorted_events = events conf base p in
  let get_name (n, _, _, _, _, _, _) = n in
  let get_date (_, date, _, _, _, _, _) = date in
  sort_events get_name get_date unsorted_events
