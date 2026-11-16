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

let align ~name ~score l1 l2 =
  let a1 = Array.of_list l1 and a2 = Array.of_list l2 in
  let ranks a =
    let seen = Hashtbl.create 8 in
    Array.map
      (fun e ->
        let k = name e in
        let r = Option.value ~default:0 (Hashtbl.find_opt seen k) in
        Hashtbl.replace seen k (r + 1);
        r)
      a
  in
  let r1 = ranks a1 and r2 = ranks a2 in
  let candidates = ref [] in
  Array.iteri
    (fun i e1 ->
      Array.iteri
        (fun j e2 ->
          if name e1 = name e2 then
            candidates :=
              (-score e1 e2, abs (r1.(i) - r2.(j)), i, j) :: !candidates)
        a2)
    a1;
  let p1 = Array.make (Array.length a1) None in
  let p2 = Array.make (Array.length a2) None in
  List.iter
    (fun (_, _, i, j) ->
      if Option.is_none p1.(i) && Option.is_none p2.(j) then (
        p1.(i) <- Some j;
        p2.(j) <- Some i))
    (List.sort Stdlib.compare !candidates);
  let n2 = Array.length a2 in
  let removed = Array.make (n2 + 1) [] in
  let next = ref n2 in
  for i = Array.length a1 - 1 downto 0 do
    match p1.(i) with
    | Some j -> next := j
    | None -> removed.(!next) <- i :: removed.(!next)
  done;
  let with_removed j acc =
    List.fold_right (fun i acc -> (Some a1.(i), None) :: acc) removed.(j) acc
  in
  let acc = ref (with_removed n2 []) in
  for j = n2 - 1 downto 0 do
    acc :=
      with_removed j ((Option.map (Array.get a1) p2.(j), Some a2.(j)) :: !acc)
  done;
  !acc

let count_true l = List.fold_left (fun n b -> if b then n + 1 else n) 0 l

let align_pevents l1 l2 =
  align
    ~name:(fun e -> e.epers_name)
    ~score:(fun e1 e2 ->
      count_true
        [
          e1.epers_date = e2.epers_date;
          e1.epers_place = e2.epers_place;
          e1.epers_reason = e2.epers_reason;
          e1.epers_note = e2.epers_note;
          e1.epers_src = e2.epers_src;
          e1.epers_witnesses = e2.epers_witnesses;
        ])
    l1 l2

let align_fevents l1 l2 =
  align
    ~name:(fun e -> e.efam_name)
    ~score:(fun e1 e2 ->
      count_true
        [
          e1.efam_date = e2.efam_date;
          e1.efam_place = e2.efam_place;
          e1.efam_reason = e2.efam_reason;
          e1.efam_note = e2.efam_note;
          e1.efam_src = e2.efam_src;
          e1.efam_witnesses = e2.efam_witnesses;
        ])
    l1 l2

let sort_events get_name get_date events =
  List.stable_sort (fun e1 e2 -> compare get_name get_date e1 e2) events

type 'a event_item =
  'a event_name
  * Adef.cdate
  * Driver.istr
  * Driver.istr
  * Driver.istr
  * (Driver.iper * witness_kind * Driver.istr) array
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
  let anchored, _, _ =
    List.fold_left
      (fun (l, src, prev) ((_, date, _, _, _, _, isp) as e) ->
        let prev = if src = Some isp then prev else Date.cdate_None in
        let d =
          match Date.cdate_to_gregorian_dmy_opt date with
          | Some _ -> date
          | None -> prev
        in
        ((d, e) :: l, Some isp, d))
      ([], None, Date.cdate_None)
      (events conf base p)
  in
  List.rev anchored
  |> List.stable_sort (compare (fun (_, (n, _, _, _, _, _, _)) -> n) fst)
  |> List.map snd
