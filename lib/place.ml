(* Copyright (c) 1998-2007 INRIA *)

open Util

let src = Logs.Src.create ~doc:"Place" "PLAC"

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let normalize_place_parens = Place_cache.normalize_place_parens

let suburb_aux sub nosub s =
  let len = String.length s in
  if len = 0 then nosub ""
  else if String.unsafe_get s 0 = '[' then
    match String.index_opt s ']' with
    | None -> nosub s
    | Some i -> (
        match
          let rec loop b i =
            if i = len then None
            else
              match Char.code s.[i] with
              | 0x20 -> loop b (i + 1)
              | 0x2D when not b -> loop true (i + 1) (* hyphen *)
              (* handle en and em dash as well *)
              | 0xE2
                when Char.code s.[i + 1] = 0x80
                     && (Char.code s.[i + 2] = 0x93
                        || Char.code s.[i + 2] = 0x94)
                     && not b ->
                  loop true (i + 3)
              | _ -> if b then Some i else None
          in
          loop false (i + 1)
        with
        | None -> nosub s
        | Some j -> sub s len i j)
  else nosub s

(** [split_suburb "[foo-bar] - boobar (baz)"] is ["foo-bar", "boobar (baz)")] *)
let split_suburb =
  suburb_aux
    (fun s len i j -> (String.sub s 1 (i - 1), String.sub s j (len - j)))
    (fun s -> ("", s))

(** [only_suburb "[foo-bar] - boobar (baz)"] is ["foo-bar"]
    [only_suburb "boobar (baz)"] is [""] *)
let only_suburb =
  suburb_aux (fun s _len i _j -> String.sub s 1 (i - 1)) (fun _ -> "")

(** [without_suburb "[foo-bar] - boobar (baz)"] is ["boobar (baz)"]
    [without_suburb "boobar (baz)"] is ["boobar (baz)"] *)
let without_suburb =
  suburb_aux (fun s len _i j -> String.sub s j (len - j)) (fun s -> s)

(* [Vother] boxes an arbitrary value into a template environment slot. Kept in
   this core module (it is pure and part of the public [Place] API) and consumed
   by the rendering code in {!PlaceDisplay}. *)
type 'a env = Vother of 'a

let get_vother (Vother x) = Some x
let set_vother x = Vother x

let normalize =
  suburb_aux
    (fun s len i j ->
      let b = Bytes.create (len - j + i + 1) in
      Bytes.blit_string s 1 b 0 (i - 1);
      Bytes.unsafe_set b (i - 1) ',';
      Bytes.unsafe_set b i ' ';
      Bytes.blit_string s j b (i + 1) (len - j);
      Bytes.unsafe_to_string b)
    (fun s -> s)

let compare_places s1 s2 =
  let ss1, s1 = split_suburb s1 in
  let ss2, s2 = split_suburb s2 in
  match
    Mutil.list_compare Gutil.alphabetic_order
      (String.split_on_char ',' s1)
      (String.split_on_char ',' s2)
  with
  | 0 -> Gutil.alphabetic_order ss1 ss2
  | x -> x

(* [String.length s > 0] is always true because we already tested [is_empty_string].
   If it is not true, then the base should be cleaned. *)
let fold_place_long inverted s =
  match String.length s with
  | 0 ->
      Log.warn (fun k -> k "Zero length string in fold_place_long!");
      ([], "")
  | _ ->
      let sub = only_suburb s in
      let s = without_suburb s in
      let len = String.length s in
      let rec loop iend list i ibeg =
        if i = iend then
          if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
        else
          let list, ibeg =
            match String.unsafe_get s i with
            | ',' ->
                let list =
                  if i > ibeg then String.sub s ibeg (i - ibeg) :: list
                  else list
                in
                (list, i + 1)
            | ' ' when i = ibeg -> (list, i + 1)
            | _ -> (list, ibeg)
          in
          loop iend list (i + 1) ibeg
      in
      ((if inverted then List.rev (loop len [] 0 0) else loop len [] 0 0), sub)

(** Predicate: should this person event be included given the current flags. *)
let person_event_selected ~add_birth ~add_baptism ~add_death ~add_burial
    ~add_pevents (name : Driver.istr Def.gen_pers_event_name) =
  match name with
  | Epers_Birth -> add_birth
  | Epers_Baptism -> add_baptism
  | Epers_Death -> add_death
  | Epers_Burial -> add_burial
  | Epers_Cremation -> add_burial
  | _ -> add_pevents

(** Predicate: should this family event be included given the current flags. *)
let family_event_selected ~add_marriage ~add_fevents
    (name : Driver.istr Def.gen_fam_event_name) =
  match name with
  | Efam_Marriage -> add_marriage
  | Efam_MarriageBann -> add_marriage
  | Efam_MarriageContract -> add_marriage
  | Efam_MarriageLicense -> add_marriage
  | Efam_PACS -> add_marriage
  | Efam_Residence -> add_marriage
  | _ -> add_fevents

(** Convert the new flat cache format into the presentation format expected by
    [PlaceDisplay.print_html_places_surnames_*].

    The cache is unfiltered; filtering by event type, authorised age, and
    initial-letter [filter] is applied here at query time. *)
let cache_to_array conf base (cache : Place_cache.t) ~add_birth ~add_baptism
    ~add_death ~add_burial ~add_marriage ~add_pevents ~add_fevents fold_place
    filter =
  let ht : (string list * string, (string * Driver.iper list) list) Hashtbl.t =
    Hashtbl.create (Hashtbl.length cache.Place_cache.persons)
  in
  let add_iper key sn_str iper =
    if filter key then
      match Hashtbl.find_opt ht key with
      | None -> Hashtbl.add ht key [ (sn_str, [ iper ]) ]
      | Some snl ->
          let snl' =
            match List.assoc_opt sn_str snl with
            | None -> (sn_str, [ iper ]) :: snl
            | Some ipl ->
                if List.mem iper ipl then snl
                else (sn_str, iper :: ipl) :: List.remove_assoc sn_str snl
          in
          Hashtbl.replace ht key snl'
  in
  Hashtbl.iter
    (fun place events ->
      let key = Place_cache.normalize_place_parens place |> fold_place in
      List.iter
        (fun (evname, iper) ->
          if
            person_event_selected ~add_birth ~add_baptism ~add_death ~add_burial
              ~add_pevents evname
          then
            let p = Driver.poi base iper in
            if authorized_age conf base p then
              let sn = Driver.sou base (Driver.get_surname p) in
              add_iper key sn iper)
        events)
    cache.Place_cache.persons;
  Hashtbl.iter
    (fun place events ->
      let key = Place_cache.normalize_place_parens place |> fold_place in
      List.iter
        (fun (evname, ifam) ->
          if family_event_selected ~add_marriage ~add_fevents evname then
            let fam = Driver.foi base ifam in
            let fath = Driver.get_father fam in
            let moth = Driver.get_mother fam in
            let pf = Driver.poi base fath in
            let pm = Driver.poi base moth in
            if authorized_age conf base pf && authorized_age conf base pm then begin
              let sn_f = Driver.sou base (Driver.get_surname pf) in
              let sn_m = Driver.sou base (Driver.get_surname pm) in
              add_iper key sn_f fath;
              add_iper key sn_m moth
            end)
        events)
    cache.Place_cache.families;
  let len = Hashtbl.length ht in
  let dummy : (string list * string) * (string * Driver.iper list) list =
    (([], ""), [])
  in
  let arr = Array.make len dummy in
  let idx = ref 0 in
  Hashtbl.iter
    (fun k v ->
      Array.unsafe_set arr !idx (k, v);
      incr idx)
    ht;
  arr

(** Main entry point for the PPS index. Loads the cache from disk if valid,
    rebuilds it otherwise, then converts to the presentation format with event
    filtering applied. *)
let get_all_cached conf base ~add_birth ~add_baptism ~add_death ~add_burial
    ~add_marriage ~add_pevents ~add_fevents fold_place filter =
  let bdir = Driver.bdir base in
  let cache = Place_cache.get_or_build bdir conf base in
  cache_to_array conf base cache ~add_birth ~add_baptism ~add_death ~add_burial
    ~add_marriage ~add_pevents ~add_fevents fold_place filter
