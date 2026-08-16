open Alcotest
open Def
module Driver = Geneweb_db.Driver
module Cache = Geneweb.Place_cache

(* --- Minimal in-memory base scaffolding (mirrors merge_test.ml) --- *)

let empty_string = 0
let quest_string = 1

(* Indices into [strings] below. *)
let s_paris = 2
let s_lyon = 3
let iper (i : int) : Driver.iper = Obj.magic i
let ascend parents = { Driver.no_ascend with Def.parents }
let union family = { Def.family }

(* A blank person we clone; [base_person.birth] gives us a valid "no date"
   cdate without depending on the exact name of the empty-date constant. *)
let base_person = Mutil.empty_person empty_string quest_string
let empty_cdate = base_person.birth

let pevent name place =
  {
    epers_name = name;
    epers_date = empty_cdate;
    epers_place = place;
    epers_reason = empty_string;
    epers_note = empty_string;
    epers_src = empty_string;
    epers_witnesses = [||];
  }

let person i pevents = { base_person with occ = i; key_index = i; pevents }

let base_notes =
  { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) }

(* index 0 = "", 1 = "?", 2 = "Paris", 3 = "Lyon" *)
let strings = [| ""; "?"; "Paris"; "Lyon" |]

let make_base name persons f =
  let n = Array.length persons in
  let ascends = Array.make n (ascend None) in
  let unions = Array.make n (union [||]) in
  let families = [||] in
  let couples = [||] in
  let descends = [||] in
  let data =
    ( (persons, ascends, unions),
      (families, couples, descends),
      strings,
      base_notes )
  in
  Driver.make name [] data f

let empty_cache () =
  {
    Cache.persons = Hashtbl.create 16;
    families = Hashtbl.create 16;
    iper_places = Hashtbl.create 16;
    ifam_places = Hashtbl.create 16;
  }

(* Sorted ids currently indexed under [place] in the forward index. *)
let place_ipers cache place =
  match Hashtbl.find_opt cache.Cache.persons place with
  | None -> []
  | Some l ->
      List.map (fun (_, i) -> Driver.Iper.to_string i) l |> List.sort compare

(* Sorted places [ip] currently contributes to, per the reverse index. *)
let reverse cache ip =
  match Hashtbl.find_opt cache.Cache.iper_places (iper ip) with
  | None -> None
  | Some l -> Some (List.sort compare l)

let has_place cache place = Hashtbl.mem cache.Cache.persons place

(* --- Tests --- *)

(* [update_iper] on a fresh cache indexes a person's event place in both the
   forward and reverse indexes. *)
let index_persons () =
  let persons =
    [|
      person 0 [ pevent Epers_Birth s_paris ];
      person 1 [ pevent Epers_Birth s_paris ];
      person 2 [ pevent Epers_Birth s_lyon ];
    |]
  in
  make_base "place_cache_index_base" persons @@ fun base ->
  let cache = empty_cache () in
  Cache.update_iper cache base (iper 0);
  Cache.update_iper cache base (iper 1);
  Cache.update_iper cache base (iper 2);
  (check (list string))
    "Paris has p0,p1" [ "0"; "1" ]
    (place_ipers cache "Paris");
  (check (list string)) "Lyon has p2" [ "2" ] (place_ipers cache "Lyon");
  (check (option (list string)))
    "reverse index for p0 is [Paris]" (Some [ "Paris" ]) (reverse cache 0);
  ()

(* Re-running [update_iper] against an edited base must move a person from the
   old place to the new one (exercising remove + add + reverse index), and a
   person whose place was cleared must disappear entirely. *)
let move_and_clear () =
  (* Base 1: p0,p1 in Paris, p2 in Lyon -> index all three. *)
  let cache =
    let persons =
      [|
        person 0 [ pevent Epers_Birth s_paris ];
        person 1 [ pevent Epers_Birth s_paris ];
        person 2 [ pevent Epers_Birth s_lyon ];
      |]
    in
    make_base "place_cache_move_base1" persons @@ fun base ->
    let cache = empty_cache () in
    Cache.update_iper cache base (iper 0);
    Cache.update_iper cache base (iper 1);
    Cache.update_iper cache base (iper 2);
    cache
  in
  (* Base 2: p0 moved Paris -> Lyon, p1's place cleared, p2 unchanged. *)
  let persons2 =
    [|
      person 0 [ pevent Epers_Birth s_lyon ];
      person 1 [] (* place cleared *);
      person 2 [ pevent Epers_Birth s_lyon ];
    |]
  in
  make_base "place_cache_move_base2" persons2 @@ fun base ->
  Cache.update_iper cache base (iper 0);
  Cache.update_iper cache base (iper 1);
  (check bool) "Paris key removed once empty" false (has_place cache "Paris");
  (check (list string))
    "Lyon now has p0,p2" [ "0"; "2" ] (place_ipers cache "Lyon");
  (check (option (list string)))
    "reverse index for p0 is now [Lyon]" (Some [ "Lyon" ]) (reverse cache 0);
  (check (option (list string)))
    "reverse index for cleared p1 is gone" None (reverse cache 1);
  ()

let v =
  [
    ( "place-cache-index",
      [ test_case "update_iper indexes persons" `Quick index_persons ] );
    ( "place-cache-move",
      [ test_case "update_iper moves and clears" `Quick move_and_clear ] );
  ]
