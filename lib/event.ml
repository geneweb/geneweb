open Def
open Gwdb

type 'a event_name =
    Pevent of 'a gen_pers_event_name
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
  match name1, name2 with
  | Pevent Epers_Birth, _ -> -1
  | _, Pevent Epers_Birth -> 1
  | Pevent Epers_Baptism, Pevent ( Epers_Death
                               | Epers_Funeral
                               | Epers_Burial
                               | Epers_Cremation ) ->
    -1
  | Pevent ( Epers_Death
          | Epers_Funeral
          | Epers_Burial
          | Epers_Cremation ), Pevent Epers_Baptism ->
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
  match Adef.od_of_cdate (get_date e1) with
  | Some (Dgreg (d1, _)) -> begin
      match Adef.od_of_cdate (get_date e2) with
      | Some (Dgreg (d2, _)) ->
        begin match Date.compare_dmy_opt ~strict:true d1 d2 with
          | Some 0 | None -> compare_event_name (get_name e1) (get_name e2)
          | Some x -> x
        end
      | Some (Dtext _) | None -> compare_event_name (get_name e1) (get_name e2)
    end
  | Some (Dtext _) | None -> compare_event_name (get_name e1) (get_name e2)

let sort_events get_name get_date events =
  List.stable_sort (fun e1 e2 -> compare get_name get_date e1 e2) events

type 'a event_item =
  'a event_name * cdate * istr * istr * istr * (iper * witness_kind) array *
    iper option

let events conf base p =
  if not (Util.authorized_age conf base p) then []
  else
    let pevents =
      List.fold_right (fun evt events ->
        let name = Pevent evt.epers_name in
        let date = evt.epers_date in
        let place = evt.epers_place in
        let note = evt.epers_note in
        let src = evt.epers_src in
        let wl = evt.epers_witnesses in
        let x = name, date, place, note, src, wl, None in
        x :: events
      ) (get_pevents p) []
    in
    let fevents =
      Array.fold_right (fun ifam fevents ->
        let fam = foi base ifam in
        let isp = Gutil.spouse (get_iper p) fam in
        let m_auth = Util.authorized_age conf base (Util.pget conf base isp) in
        let fam_fevents =
          if m_auth then
            List.fold_right (fun evt fam_fevents ->
              let name = Fevent evt.efam_name in
              let date = evt.efam_date in
              let place = evt.efam_place in
              let note = evt.efam_note in
              let src = evt.efam_src in
              let wl = evt.efam_witnesses in
              let x = name, date, place, note, src, wl, Some isp in
              x :: fam_fevents
            ) (get_fevents fam) []
          else []
        in
        fam_fevents @ fevents
      ) (get_family p) []
    in
    (pevents @ fevents)

let sorted_events conf base p =
  let unsorted_events = events conf base p in
  let get_name (n, _, _, _, _, _, _) = n
  in
  let get_date (_, date, _, _, _, _, _) = date in
  sort_events get_name get_date unsorted_events
