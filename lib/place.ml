(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

let src = Logs.Src.create ~doc:"Place" "PLAC"

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

(* max number of persons for which a m=RLM graph will be computed *)
let max_rlm_nbr_default = 80

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

let max_rlm_nbr conf =
  match p_getenv conf.env "max_rlm_nbr" with
  | Some n -> (
      match int_of_string_opt n with
      | Some n -> n
      | None -> (
          match List.assoc_opt "max_rlm_nbr" conf.base_env with
          | Some n -> (
              match int_of_string_opt n with
              | Some n -> n
              | None -> max_rlm_nbr_default)
          | None -> max_rlm_nbr_default))
  | None -> (
      match List.assoc_opt "max_rlm_nbr" conf.base_env with
      | Some n -> (
          match int_of_string_opt n with
          | Some n -> n
          | None -> max_rlm_nbr_default)
      | None -> max_rlm_nbr_default)

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
      (* Trimm spaces after ',' and build reverse String.split_on_char ',' *)
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
      let list =
        if String.unsafe_get s (len - 1) = ')' then
          match String.rindex_opt s '(' with
          | Some i when i < len - 2 ->
              let j =
                let rec loop i =
                  if i >= 0 && String.unsafe_get s i = ' ' then loop (i - 1)
                  else i + 1
                in
                loop (i - 1)
              in
              String.sub s (i + 1) (len - i - 2) :: loop j [] 0 0
          | _ -> loop len [] 0 0
        else loop len [] 0 0
      in
      ((if inverted then List.rev list else list), sub)

let places_to_string inverse pl =
  (* TODO reverse ??*)
  let pl = if inverse then List.rev pl else pl in
  let rec loop acc first = function
    | p :: l -> loop (p ^ (if first then "" else ", ") ^ acc) false l
    | [] -> acc
  in
  loop "" true pl

(* Canonical place string for the m=L marker comparison. Runs the raw place
   through the exact same pipeline as the place key — [fold_place_long] (drops
   the suburb, splits on commas / a trailing "(xxx)", honours [places_inverted])
   then renders it child-first — so the value the markers test is byte-identical
   to the key in every case. Examples:
     "[Hameau Boileau] - Paris 16e" -> "Paris 16e"
     "Paris (75)"                   -> "Paris, 75" *)
let normalize_place inverted s =
  if s = "" then ""
  else places_to_string false (fst (fold_place_long inverted s))

let places_inverted conf =
  List.assoc_opt "places_inverted" conf.base_env = Some "yes"

exception List_too_long

let get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
    ~add_marriage (dummy_key : 'a) (dummy_value : 'c)
    (fold_place : string -> 'a) (filter : 'a -> bool)
    (mk_value : 'b option -> Driver.person -> 'b) (fn : 'b -> 'c)
    (max_length : int) : ('a * 'c) array =
  (* [fold_cache] is keyed by place [istr]; the total string population is a
     tight upper bound on the distinct place istrs, so pre-size it there to
     avoid rehashing during the scan. [ht] holds the (fewer) distinct folded
     place keys, whose count is unknown up front — start small and let it grow
     (amortised O(1)). *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create 2048 in
  (* Many people share the same place [istr]; fold each distinct place string
     once (sou + fold_place) instead of once per person. *)
  let fold_cache : (Driver.istr, 'a) Hashtbl.t =
    Hashtbl.create (Driver.nb_of_strings base)
  in
  let key_of istr =
    match Hashtbl.find_opt fold_cache istr with
    | Some key -> key
    | None ->
        let key = Driver.sou base istr |> fold_place in
        Hashtbl.add fold_cache istr key;
        key
  in
  let long = p_getenv conf.env "display" = Some "long" in
  let ht_add istr p =
    let key : 'a = key_of istr in
    if filter key then
      match Hashtbl.find_opt ht key with
      | Some _ as prev -> Hashtbl.replace ht key (mk_value prev p)
      | None ->
          Hashtbl.add ht key (mk_value None p);
          if Hashtbl.length ht > max_length && long then raise List_too_long
  in
  (if add_birth || add_death || add_baptism || add_burial then
     let aux b fn p =
       if b then
         let x = fn p in
         if not (Driver.Istr.is_empty x) then ht_add x p
     in
     Collection.iter
       (fun i ->
         let p = pget conf base i in
         if authorized_age conf base p then (
           aux add_birth Driver.get_birth_place p;
           aux add_baptism Driver.get_baptism_place p;
           aux add_death Driver.get_death_place p;
           aux add_burial Driver.get_burial_place p))
       (Geneweb_db.Driver.ipers base));
  if add_marriage then
    Collection.iter
      (fun i ->
        let fam = Driver.foi base i in
        let pl_ma = Driver.get_marriage_place fam in
        if not (Driver.Istr.is_empty pl_ma) then
          let fath = pget conf base (Driver.get_father fam) in
          let moth = pget conf base (Driver.get_mother fam) in
          if authorized_age conf base fath && authorized_age conf base moth then (
            ht_add pl_ma fath;
            ht_add pl_ma moth))
      (Geneweb_db.Driver.ifams base);
  let len = Hashtbl.length ht in
  let array = Array.make len (dummy_key, dummy_value) in
  let i = ref 0 in
  Hashtbl.iter
    (fun k v ->
      Array.unsafe_set array !i (k, fn v);
      incr i)
    ht;
  array

let find_in conf x ini =
  (* look at possibility to have ini=aaa, bbb or aaa (bbb) *)
  let word = p_getenv conf.env "word" = Some "on" in
  (* full words *)
  let case = p_getenv conf.env "case" = Some "on" in
  (* case sensitive *)
  let any = p_getenv conf.env "any" = Some "on" in
  (* anywhere in place list *)
  let low s = if not case then Name.lower s else s in
  let inil = String.split_on_char ',' ini in
  let inil =
    if List.length inil = 1 then
      match String.index_opt ini '(' with
      | Some index when index > 0 ->
          [
            String.sub ini 0 (index - 1);
            String.sub ini index (String.length ini - index);
          ]
      | Some _index -> [ ini ]
      | None -> [ ini ]
    else inil
  in
  List.fold_left
    (fun acc ini ->
      let ini = low ini in
      acc
      &&
      if any || List.length inil > 1 then
        List.fold_left
          (fun r p ->
            r || if word then low p = ini else Mutil.contains (low p) ini)
          false x
      else
        match x with
        | [] -> false
        | x :: _ when word -> low x = ini
        | x :: _ -> Mutil.contains (low x) ini)
    true inil
