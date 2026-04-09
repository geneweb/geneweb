(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util

let src = Logs.Src.create ~doc:"Place" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

let max_rlm_nbr_default = 80

(* ===== Suburb parsing ===== *)

let suburb_aux sub nosub s =
  let len = String.length s in
  if len = 0 then nosub ""
  else if String.unsafe_get s 0 = '[' then
    match String.index_opt s ']' with
    | None -> nosub s
    | Some i -> (
        let rec loop b i =
          if i = len then None
          else
            match Char.code s.[i] with
            | 0x20 -> loop b (i + 1)
            | 0x2D when not b -> loop true (i + 1) (* hyphen *)
            (* en-dash and em-dash *)
            | 0xE2
              when Char.code s.[i + 1] = 0x80
                   && (Char.code s.[i + 2] = 0x93 || Char.code s.[i + 2] = 0x94)
                   && not b ->
                loop true (i + 3)
            | _ -> if b then Some i else None
        in
        match loop false (i + 1) with
        | None -> nosub s
        | Some j -> sub s len i j)
  else nosub s

(** [split_suburb "[foo-bar] - boobar (baz)"] is [("foo-bar", "boobar (baz)")]
*)
let split_suburb =
  suburb_aux
    (fun s len i j -> (String.sub s 1 (i - 1), String.sub s j (len - j)))
    (fun s -> ("", s))

(** [only_suburb "[foo-bar] - boobar (baz)"] is ["foo-bar"] *)
let only_suburb =
  suburb_aux (fun s _len i _j -> String.sub s 1 (i - 1)) (fun _ -> "")

(** [without_suburb "[foo-bar] - boobar (baz)"] is ["boobar (baz)"] *)
let without_suburb =
  suburb_aux (fun s len _i j -> String.sub s j (len - j)) (fun s -> s)

let has_suburb s = String.length s > 0 && String.unsafe_get s 0 = '['

(* ===== Template environment ===== *)

type 'a env =
  | Vlist_data of (string * (string * int) list) list
  | Vlist_ini of string list
  | Vlist_value of (string * (string * int) list) list
  | Venv_keys of (string * int) list
  | Vint of int
  | Vstring of string
  | Vbool of bool
  | Vother of 'a
  | Vnone

let get_vother = function Vother x -> Some x | _ -> None
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

(* ===== Place folding ===== *)

(** Shared inner parser: splits [s] on commas (trimming leading spaces after
    each comma) and handles the [(xxx)] shorthand for the last component.
    Returns the list in the order components appear in [s]. *)
let fold_place_parts s =
  let len = String.length s in
  let rec loop iend acc i ibeg =
    if i = iend then
      if i > ibeg then String.sub s ibeg (i - ibeg) :: acc else acc
    else
      match String.unsafe_get s i with
      | ',' ->
          let acc =
            if i > ibeg then String.sub s ibeg (i - ibeg) :: acc else acc
          in
          loop iend acc (i + 1) (i + 1)
      | ' ' when i = ibeg -> loop iend acc (i + 1) (i + 1)
      | _ -> loop iend acc (i + 1) ibeg
  in
  if len = 0 then []
  else if String.unsafe_get s (len - 1) = ')' then
    match String.rindex_opt s '(' with
    | Some i when i < len - 2 ->
        (* trim trailing spaces before '(' *)
        let j =
          let rec trim i =
            if i >= 0 && String.unsafe_get s i = ' ' then trim (i - 1)
            else i + 1
          in
          trim (i - 1)
        in
        String.sub s (i + 1) (len - i - 2) :: loop j [] 0 0
    | _ -> loop len [] 0 0
  else loop len [] 0 0

(** [fold_place_long inverted s] parses [s] into [(place_list, suburb)]. When
    [inverted = true] the list is reversed so the largest unit comes first
    (country → … → parish). *)
let fold_place_long inverted s =
  match String.length s with
  | 0 ->
      Log.warn (fun k -> k "Zero length string in fold_place_long!");
      ([], "")
  | _ ->
      let sub = only_suburb s in
      let s = without_suburb s in
      let lst = fold_place_parts s in
      ((if inverted then List.rev lst else lst), sub)

(** Like [fold_place_long] but ignores the suburb [bracket] notation. *)
let fold_place_long_v6 inverted s =
  match String.length s with
  | 0 ->
      Log.warn (fun k -> k "Zero length string in fold_place_long_v6!");
      []
  | _ ->
      let lst = fold_place_parts s in
      if inverted then List.rev lst else lst

let fold_place_short inverted s =
  if inverted then
    match String.index_opt s ',' with Some i -> String.sub s 0 i | None -> s
  else
    let len = String.length s in
    let default () =
      let i =
        match String.rindex_opt s ',' with
        | Some i ->
            let rec skip_spaces i =
              if i < len && String.unsafe_get s i = ' ' then skip_spaces (i + 1)
              else i
            in
            skip_spaces (i + 1)
        | None -> 0
      in
      let i = if i = len then 0 else i in
      String.sub s i (len - i)
    in
    if String.unsafe_get s (len - 1) = ')' then
      match String.rindex_opt s '(' with
      | Some i when i < len - 2 -> String.sub s (i + 1) (len - i - 2)
      | _ -> default ()
    else default ()

(** Join a place list into a display string in list order. *)
let places_to_string pl = String.concat ", " pl

(* ===== Config helpers ===== *)

let max_rlm_nbr conf =
  let from_base () =
    match List.assoc_opt "max_rlm_nbr" conf.base_env with
    | Some n -> Option.value ~default:max_rlm_nbr_default (int_of_string_opt n)
    | None -> max_rlm_nbr_default
  in
  match p_getenv conf.env "max_rlm_nbr" with
  | Some n -> Option.value ~default:(from_base ()) (int_of_string_opt n)
  | None -> from_base ()

(** Build the persistent URL option string from all filter checkboxes, including
    "case" which was previously missing. *)
let get_opt conf =
  let on s = if p_getenv conf.env s = Some "on" then "&" ^ s ^ "=on" else "" in
  String.concat ""
    (List.map on
       [
         "bi";
         "ba";
         "de";
         "bu";
         "ma";
         "f_sort";
         "up";
         "a_sort";
         "lower";
         "word";
         "any";
         "case";
       ])

(* ===== Core data collection ===== *)

exception List_too_long

(** Iterate over all persons and families, collecting place → value mappings.
    Returns an unsorted array of (folded_place, computed_value) pairs. Raises
    [List_too_long] in long-display mode when [max_length] is exceeded. *)
let get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
    ~add_marriage (fold_place : string -> 'a) (filter : 'a -> bool)
    (mk_value : 'b option -> Driver.person -> 'b) (fn : 'b -> 'c)
    (max_length : int) : ('a * 'c) array =
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create 2048 in
  let long = p_getenv conf.env "display" = Some "long" in
  let ht_add istr p =
    let key = fold_place (Driver.sou base istr) in
    if filter key then begin
      Hashtbl.replace ht key (mk_value (Hashtbl.find_opt ht key) p);
      if long && Hashtbl.length ht > max_length then raise List_too_long
    end
  in
  if add_birth || add_baptism || add_death || add_burial then begin
    let aux add get_place p =
      if add then
        let pl = get_place p in
        if not (Driver.Istr.is_empty pl) then ht_add pl p
    in
    Collection.iter
      (fun i ->
        let p = pget conf base i in
        if authorized_age conf base p then begin
          aux add_birth Driver.get_birth_place p;
          aux add_baptism Driver.get_baptism_place p;
          aux add_death Driver.get_death_place p;
          aux add_burial Driver.get_burial_place p
        end)
      (Driver.ipers base)
  end;
  if add_marriage then
    Collection.iter
      (fun i ->
        let fam = Driver.foi base i in
        let pl = Driver.get_marriage_place fam in
        if not (Driver.Istr.is_empty pl) then begin
          let fath = pget conf base (Driver.get_father fam) in
          let moth = pget conf base (Driver.get_mother fam) in
          if authorized_age conf base fath && authorized_age conf base moth then begin
            ht_add pl fath;
            ht_add pl moth
          end
        end)
      (Driver.ifams base);
  Hashtbl.fold (fun k v acc -> (k, fn v) :: acc) ht [] |> Array.of_list

let rec sort_place_utf8 k1 k2 =
  match (k1, k2) with
  | ([], sub1), ([], sub2) -> Gutil.alphabetic_order sub1 sub2
  | _, ([], _) -> 1
  | ([], _), _ -> -1
  | (p1 :: pl1, sub1), (p2 :: pl2, sub2) ->
      let c = Gutil.alphabetic_order p1 p2 in
      if c = 0 then sort_place_utf8 (pl1, sub1) (pl2, sub2) else c

(* ===== Search/filter ===== *)

let find_in conf x ini =
  let word = p_getenv conf.env "word" = Some "on" in
  let case = p_getenv conf.env "case" = Some "on" in
  let any = p_getenv conf.env "any" = Some "on" in
  let low s = if case then s else Name.lower s in
  (* Split on comma and trim spaces so that a k value like "Yvelines, Buc"
     round-trips correctly through URL encoding → decoding → matching. *)
  let inil = String.split_on_char ',' ini |> List.map String.trim in
  (* Also support "parish (county)" as two-part query when there is no comma *)
  let inil =
    match inil with
    | [ s ] -> (
        match String.index_opt s '(' with
        | Some i when i > 0 ->
            [
              String.trim (String.sub s 0 i);
              String.sub s i (String.length s - i);
            ]
        | _ -> inil)
    | _ -> inil
  in
  let match_one ini_part place =
    if word then low place = ini_part else Mutil.contains (low place) ini_part
  in
  let multi = match inil with [ _ ] -> false | _ -> true in
  List.for_all
    (fun ini_part ->
      let ini_part = low ini_part in
      if any || multi then List.exists (match_one ini_part) x
      else match x with [] -> false | hd :: _ -> match_one ini_part hd)
    inil

(* ===== IP-list helpers ===== *)

let get_ip_list snl = List.concat_map snd snl |> List.sort_uniq compare

(** Print the person-count badge; builds the full URL in a Buffer to avoid O(n²)
    string concatenation for large lists. *)
let print_ip_list conf places opt link_to_ind ipl =
  let len = List.length ipl in
  if len > max_rlm_nbr conf && link_to_ind then Output.printf conf "(%d)" len
  else
    let places = (Mutil.encode places :> string) in
    let buf = Buffer.create (64 + (len * 16)) in
    Buffer.add_string buf
      (Printf.sprintf "&nbsp;(<a href=\"%sm=L&data=place%s&k=%s&nb=%d"
         (commd conf :> string)
         opt places len);
    List.iteri
      (fun i ip ->
        Buffer.add_string buf
          (Printf.sprintf "&i%d=%s&p%d=%s" i (Driver.Iper.to_string ip) i places))
      ipl;
    Buffer.add_string buf
      (Printf.sprintf "\" title=\"%s\">%d</a>)"
         (Utf8.capitalize (transl conf "summary book ascendants"))
         len);
    Output.print_sstring conf (Buffer.contents buf)

(** Build an m=PPS navigation link. *)
let pps_call conf opt long keep k places =
  Printf.sprintf "<a href=\"%sm=PPS%s&display=%s&keep=%d&k=%s\">%s</a>"
    (commd conf :> string)
    opt
    (if long then "long" else "short")
    keep k
    (String.concat ", " places)

(* ===== Short-display helpers ===== *)

(** Keep only the first [keep] elements of [pll]. *)
let strip_pl keep pll =
  let rec loop acc i = function
    | [] -> List.rev acc
    | _ when i > keep -> List.rev acc
    | x :: xs -> loop (x :: acc) (i + 1) xs
  in
  loop [] 1 pll

let print_html_places_surnames_short conf _base _link_to_ind
    (arry : ((string list * string) * (string * Driver.iper list) list) array) =
  let long = p_getenv conf.env "display" = Some "long" in
  let keep =
    let base_keep = Option.value ~default:1 (p_getint conf.env "keep") in
    match p_getenv conf.env "k" with
    | Some ini when ini <> "" ->
        let depth =
          List.length (String.split_on_char ',' ini |> List.map String.trim)
        in
        max base_keep (depth + 1)
    | _ -> base_keep
  in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let opt = get_opt conf in
  (* When places_inverted=yes, fold_place_long returns keys in large→small
     order for display. For m=L's k= parameter we need storage order
     (small→large) so that k matches birth_place_raw / death_place_raw etc.
     in the template's event-marker comparison. *)
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let storage_order pl = if inverted then pl else List.rev pl in
  (* Group entries by their keep-depth prefix.
     Each group maps prefix-key → (full_pl * iper_list) list. *)
  let groups : (string list, (string list * Driver.iper list) list) Hashtbl.t =
    Hashtbl.create 64
  in
  Array.iter
    (fun ((pl, _sub), snl) ->
      let key = strip_pl keep pl in
      let entry = (pl, get_ip_list snl) in
      Hashtbl.replace groups key
        (entry :: Option.value ~default:[] (Hashtbl.find_opt groups key)))
    arry;
  (* Convert to list; sort groups *)
  let groups_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) groups [] in
  let group_total entries =
    List.concat_map snd entries |> List.sort_uniq compare |> List.length
  in
  let cmp_key (k1, _) (k2, _) = sort_place_utf8 (k1, "") (k2, "") in
  let sorted =
    if f_sort then
      List.sort
        (fun (_, e1) (_, e2) ->
          let c = compare (group_total e1) (group_total e2) in
          if up then c else -c)
        groups_list
    else if a_sort then List.sort cmp_key groups_list
    else List.sort (fun a b -> cmp_key b a) groups_list
  in
  (* Print one link per group.
     The link text and k= parameter use the group key (the keep-depth prefix),
     not the full place — so keep=1 shows "Yvelines", not "Yvelines, Buc".
     The m=L link and count always cover all persons in the entire group. *)
  let print_group first (key, entries) =
    if not first then Output.print_sstring conf ", ";
    let all_ipl = List.concat_map snd entries |> List.sort_uniq compare in
    let total = List.length all_ipl in
    (* Display order (large→small when inverted): used for link text and
       for the PPS k= parameter so the next drill-down level filters correctly. *)
    let str = String.concat ", " key in
    let str2 = (Mutil.encode str :> string) in
    (* Storage order (small→large when inverted): used for m=L k= so that it
       matches birth_place_raw / death_place_raw in the template comparison. *)
    let str_k = String.concat ", " (storage_order key) in
    let str_k2 = (Mutil.encode str_k :> string) in
    Output.printf conf "<a href=\"%sm=PPS%s&display=%s&keep=%d&k=%s\">%s</a>"
      (commd conf :> string)
      opt
      (if long then "long" else "short")
      (keep + 1) str2 str;
    if total < max_rlm_nbr conf then begin
      (* Map each iper to its own storage-order place string.
         When a person appears in multiple sub-entries (e.g. born and died
         in different cities within the same group), the last entry wins.
         This is used to emit p{n}= per person so that in m=L the template's
         so=p{n} matches birth_place_raw / death_place_raw exactly. *)
      let iper_place : (Driver.iper, string) Hashtbl.t =
        Hashtbl.create (total * 2)
      in
      List.iter
        (fun (pl, ipl) ->
          let p_str = String.concat ", " (storage_order pl) in
          List.iter (fun ip -> Hashtbl.replace iper_place ip p_str) ipl)
        entries;
      let buf = Buffer.create (64 + (total * 32)) in
      Buffer.add_string buf
        (Printf.sprintf "&nbsp;(<a href=\"%sm=L&data=place%s&k=%s&nb=%d"
           (commd conf :> string)
           opt str_k2 total);
      List.iteri
        (fun n ip ->
          let p_str =
            Option.value ~default:str_k (Hashtbl.find_opt iper_place ip)
          in
          Buffer.add_string buf
            (Printf.sprintf "&i%d=%s&p%d=%s" n (Driver.Iper.to_string ip) n
               (Mutil.encode p_str :> string)))
        all_ipl;
      Buffer.add_string buf
        (Printf.sprintf "\" title=\"%s\">%d</a>)"
           (Utf8.capitalize (transl conf "summary book ascendants"))
           total);
      Output.print_sstring conf (Buffer.contents buf)
    end
    else Output.printf conf "&nbsp;(%d)" total
  in
  Mutil.list_iter_first print_group sorted;
  Output.print_sstring conf "<p>"

(* ===== Long-display ===== *)

let print_html_places_surnames_long conf base link_to_ind
    (arry : ((string list * string) * (string * Driver.iper list) list) array) =
  let k =
    (Mutil.encode (Option.value ~default:"" (p_getenv conf.env "k")) :> string)
  in
  let keep =
    let base_keep = Option.value ~default:1 (p_getint conf.env "keep") in
    match p_getenv conf.env "k" with
    | Some ini when ini <> "" ->
        let depth =
          List.length (String.split_on_char ',' ini |> List.map String.trim)
        in
        max base_keep (depth + 1)
    | _ -> base_keep
  in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let opt = get_opt conf in
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let storage_order pl = if inverted then pl else List.rev pl in
  let count_snl snl =
    List.fold_left (fun acc (_, ipl) -> acc + List.length ipl) 0 snl
  in
  (* Default: ascending alphabetical; a_sort=on keeps that order;
     a_sort=off gives descending — consistent with short-display semantics. *)
  Array.sort (fun (k1, _) (k2, _) -> sort_place_utf8 k1 k2) arry;
  let l = Array.to_list arry in
  let l =
    if f_sort then
      List.sort
        (fun (_, s1) (_, s2) ->
          let c = compare (count_snl s1) (count_snl s2) in
          if up then c else -c)
        l
    else if a_sort then l (* already ascending from Array.sort *)
    else List.sort (fun (k1, _) (k2, _) -> sort_place_utf8 k2 k1) l
  in
  let print_sn (sn, ips) (pl, sub) =
    let ips = List.sort_uniq compare ips in
    (* m=L k= must be in storage order to match birth_place_raw etc.
       Suburb (if any) is the first component in storage order, matching
       what perso.ml's normalize produces for "[suburb] - place" strings. *)
    let places =
      let lst = storage_order pl in
      places_to_string (if sub <> "" then sub :: lst else lst)
    in
    if link_to_ind then (
      match ips with
      | [ ip ] ->
          Output.printf conf "<a href=\"%s" (commd conf :> string);
          Output.print_string conf (acces conf base @@ pget conf base @@ ip);
          Output.printf conf "\" title=\"%s\">%s</a>"
            (Driver.sou base (Driver.get_first_name (Driver.poi base ip)))
            sn
      | _ ->
          Output.printf conf "<a href=\"%s" (commd conf :> string);
          Output.printf conf "m=N&v=%s" (sn :> string);
          Output.printf conf "\">%s</a>" sn)
    else Output.printf conf "%s" (sn :> string);
    print_ip_list conf places opt link_to_ind ips
  in
  let print_sn_list (pl, sub) (snl : (string * Driver.iper list) list) =
    Output.printf conf "<li>%s\n" (if sub <> "" then sub else "");
    let snl =
      if f_sort then
        List.sort
          (fun (_, ipl1) (_, ipl2) ->
            let c = compare (List.length ipl1) (List.length ipl2) in
            if up then c else -c)
          snl
      else
        List.sort
          (fun (p1, _) (p2, _) ->
            if a_sort then Gutil.alphabetic_order p2 p1
            else Gutil.alphabetic_order p1 p2)
          snl
    in
    Mutil.list_iter_first
      (fun first x ->
        if not first then Output.printf conf ",\n";
        print_sn x (pl, sub))
      snl;
    Output.printf conf "\n";
    Output.print_sstring conf "</li>\n"
  in
  let rec loop prev = function
    | ((pl, sub), snl) :: rest ->
        let rec open_close prev (pl, sub) =
          match (prev, pl) with
          | [], l2 ->
              List.iter
                (fun x ->
                  Output.printf conf "<li>%s<ul>\n"
                    (pps_call conf opt true keep k [ x ]))
                l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then open_close l1 (l2, sub)
              else begin
                List.iter
                  (fun _ -> Output.print_sstring conf "</ul></li>\n")
                  (x1 :: l1);
                open_close [] (x2 :: l2, sub)
              end
          | _ -> Output.print_sstring conf "</ul></li>\n"
        in
        open_close prev (pl, sub);
        print_sn_list (pl, sub) snl;
        loop pl rest
    | [] -> List.iter (fun _ -> Output.print_sstring conf "</ul></li>\n") prev
  in
  Output.print_sstring conf "<ul>\n";
  loop [] l;
  Output.print_sstring conf "</ul>\n"

(* ===== Top-level entry point ===== *)

let print_all_places_surnames_aux conf base ~add_birth ~add_baptism ~add_death
    ~add_burial ~add_marriage max_length short filter =
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let arry =
    get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      ~add_marriage (fold_place_long inverted) filter
      (fun prev p ->
        let value = (Driver.get_surname p, Driver.get_iper p) in
        match prev with Some l -> value :: l | None -> [ value ])
      (fun v ->
        let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
        let rec loop acc = function
          | [] -> acc
          | (sn, iper) :: tl -> (
              match acc with
              | (sn', ipers) :: tl_acc when Driver.sou base sn = sn' ->
                  loop ((sn', iper :: ipers) :: tl_acc) tl
              | _ -> loop ((Driver.sou base sn, [ iper ]) :: acc) tl)
        in
        loop [] v)
      max_length
  in
  Array.sort (fun (k1, _) (k2, _) -> sort_place_utf8 k1 k2) arry;
  let title _ =
    Output.printf conf "%s / %s"
      (Utf8.capitalize (transl_nth conf "place/places" 0))
      (Utf8.capitalize (transl_nth conf "surname/surnames" 0))
  in
  let opt = get_opt conf in
  let long = p_getenv conf.env "display" = Some "long" in
  let keep = Option.value ~default:1 (p_getint conf.env "keep") in
  Hutil.header conf title;
  let ifun =
    Templ.
      {
        eval_var = (fun _ -> raise Not_found);
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = (fun _ -> raise Not_found);
      }
  in
  Templ.output conf ifun Templ.Env.empty
    (Driver.empty_person base Driver.Iper.dummy)
    "buttons_places";
  Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.command;
  let link_to_ind =
    List.assoc_opt "place_surname_link_to_ind" conf.base_env = Some "yes"
  in
  let t =
    if short then Utf8.capitalize (transl conf "v7 list too long") else ""
  in
  let href =
    Printf.sprintf "href=\"%sm=PPS%s&display=%s&keep=%d%s\" title=\"%s\""
      (commd conf :> string)
      opt
      (if long then "short" else "long")
      keep
      (match p_getenv conf.env "k" with
      | Some ini -> "&k=" ^ (Mutil.encode ini :> string)
      | None -> "")
      t
  in
  Output.printf conf "<p>\n<a %s>%s</a>" href
    (Utf8.capitalize
       (transl conf (if long then "short display" else "long display")));
  if short then Output.printf conf " (%s)\n" t;
  Output.printf conf "<p>\n";
  if arry <> [||] then
    if long then print_html_places_surnames_long conf base link_to_ind arry
    else print_html_places_surnames_short conf base link_to_ind arry;
  Output.printf conf "</form>\n";
  Hutil.trailer conf

let print_all_places_surnames conf base =
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "ba" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let lim =
    try int_of_string @@ List.assoc "short_place_threshold" conf.base_env
    with _ -> 500
  in
  let filter =
    match p_getenv conf.env "k" with
    | Some ini when ini <> "" -> fun (x, _) -> find_in conf x ini
    | _ -> fun _ -> true
  in
  try
    print_all_places_surnames_aux conf base ~add_birth ~add_baptism ~add_death
      ~add_burial ~add_marriage lim false filter
  with List_too_long ->
    let conf =
      {
        conf with
        env =
          ("display", Adef.encoded "short")
          :: List.remove_assoc "display" conf.env;
      }
    in
    print_all_places_surnames_aux conf base ~add_birth ~add_baptism ~add_death
      ~add_burial ~add_marriage lim true filter

let print_list conf _base =
  let ifun =
    Templ.
      {
        eval_var = (fun _ -> raise Not_found);
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = (fun _ -> raise Not_found);
      }
  in
  Templ.output conf ifun Templ.Env.empty () "list"
