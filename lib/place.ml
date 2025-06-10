(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Logs = Geneweb_logs.Logs
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

let has_suburb s = String.unsafe_get s 0 = '['

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
      Logs.syslog `LOG_WARNING "Zero length string in fold_place_long!";
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

(* TODO see how to merge these two fold_place_long *)
let fold_place_long_v6 inverted s =
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
              if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
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
  if inverted then List.rev list else list

let fold_place_short inverted s =
  if inverted then
    match String.index_opt s ',' with Some i -> String.sub s 0 i | None -> s
  else
    let len = String.length s in
    let default () =
      let i =
        match String.rindex_opt s ',' with
        | Some i ->
            let rec l i =
              if i < len && String.unsafe_get s i = ' ' then l (i + 1) else i
            in
            l (i + 1)
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

let places_to_string inverse pl =
  (* TODO reverse ??*)
  let pl = if inverse then List.rev pl else pl in
  let rec loop acc first = function
    | p :: l -> loop (p ^ (if first then "" else ", ") ^ acc) false l
    | [] -> acc
  in
  loop "" true pl

exception List_too_long

let get_opt conf =
  let to_url_param s =
    if p_getenv conf.env s = Some "on" then Printf.sprintf "&%s=on" s else ""
  in
  let l =
    List.map to_url_param
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
      ]
  in
  String.concat "" l

let get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
    ~add_marriage (dummy_key : 'a) (dummy_value : 'c)
    (fold_place : string -> 'a) (filter : 'a -> bool)
    (mk_value : 'b option -> Driver.person -> 'b) (fn : 'b -> 'c)
    (max_length : int) : ('a * 'c) array =
  let ht_size = 2048 in
  (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let long = p_getenv conf.env "display" = Some "long" in
  let ht_add istr p =
    let key : 'a = Driver.sou base istr |> fold_place in
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

let rec sort_place_utf8 k1 k2 =
  match (k1, k2) with
  | ([], sub1), ([], sub2) -> Gutil.alphabetic_order sub1 sub2
  | _, ([], _) -> 1
  | ([], _), _ -> -1
  | (p1 :: pl1, sub1), (p2 :: pl2, sub2) ->
      if Gutil.alphabetic_order p1 p2 = 0 then
        sort_place_utf8 (pl1, sub1) (pl2, sub2)
      else Gutil.alphabetic_order p1 p2

let clean_ps ps =
  let len = String.length ps in
  if ps.[0] = '(' && ps.[len - 1] = ')' then String.sub ps 1 (len - 2) else ps

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

let get_ip_list (snl : (string * Driver.iper list) list) =
  List.map snd snl |> List.flatten |> List.sort_uniq compare

(* TODO clean-up pi (place) and qi (suburb??) *)

(** print the number of items in ip_list and a call to m=L for them **)
let print_ip_list conf places opt link_to_ind ipl =
  let len = List.length ipl in
  if len > max_rlm_nbr conf && link_to_ind then Output.printf conf "(%d)" len
  else
    let places = (Mutil.encode places :> string) in
    let head =
      Printf.sprintf "&nbsp;(<a href=\"%sm=L&data=place%s&k=%s&nb=%d&p0=%s"
        (commd conf :> string)
        opt places len places
    in
    let body =
      let rec loop i acc = function
        | [] -> acc
        | ip :: ipl ->
            loop (i + 1)
              (Printf.sprintf "&i%d=%s" i (Driver.Iper.to_string ip) ^ acc)
              ipl
      in
      loop 0 "" ipl
    in
    let tail =
      Printf.sprintf "\" title=\"%s\">%d</a>)"
        (Utf8.capitalize (transl conf "summary book ascendants"))
        (List.length ipl)
    in
    Output.print_sstring conf (head ^ body ^ tail)

(** print a call to m=PPS with a new k value *)
let pps_call conf opt long keep k places =
  Printf.sprintf "<a href=\"%sm=PPS%s&display=%s&keep=%s&k=%s\">%s</a>"
    (commd conf :> string)
    opt
    (if long then "long" else "short")
    (string_of_int keep) k
    (String.concat ", " places)

(* build ip list for all entries having same first element in places *)
let get_new_l l =
  let new_l =
    let rec loop prev ipl acc l =
      match l with
      | [] -> acc
      | ((pl :: _pll, _), snl) :: l when pl = prev ->
          loop prev (get_ip_list snl :: ipl) acc l
      | ((pl :: _pll, _), _snl) :: l ->
          loop pl [] ((prev, List.flatten ipl) :: acc) l
      | ((_, _), _snl) :: l -> loop "" [] ((prev, List.flatten ipl) :: acc) l
    in
    loop "" [] [] l
  in
  new_l

(* conserve only keep elements of pll *)
let strip_pl keep pll =
  if List.length pll <= keep then pll
  else
    let rec loop acc i pll =
      match pll with
      | [] -> List.rev acc
      | _ when i > keep -> List.rev acc
      | pl :: pll -> loop (pl :: acc) (i + 1) pll
    in
    loop [] 1 pll

let print_html_places_surnames_short conf _base _link_to_ind
    (arry : ((string list * string) * (string * Driver.iper list) list) array) =
  (* (sub_places_list * suburb) * (surname * ip_list) list *)
  let long = p_getenv conf.env "display" = Some "long" in
  let keep = match p_getint conf.env "keep" with Some t -> t | None -> 1 in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let opt = get_opt conf in
  Array.sort (fun (k1, _) (k2, _) -> sort_place_utf8 k1 k2) arry;
  let l = Array.to_list arry in
  (* build new list of (places, ipl) *)
  (* accumulate snl according to keep *)
  let new_l =
    let rec loop prev_pl acc_snl acc_l = function
      | ((pl, _sub), snl) :: l when prev_pl = strip_pl keep pl ->
          loop (strip_pl keep pl) (get_ip_list snl :: acc_snl) acc_l l
      | ((pl, _sub), snl) :: l when acc_snl <> [] ->
          let acc_snl = List.sort_uniq compare (List.flatten acc_snl) in
          loop (strip_pl keep pl)
            [ get_ip_list snl ]
            ((prev_pl, acc_snl) :: acc_l)
            l
      | ((pl, _sub), snl) :: l ->
          loop (strip_pl keep pl) [ get_ip_list snl ] acc_l l
      | [] ->
          let acc_snl = List.sort_uniq compare (List.flatten acc_snl) in
          (prev_pl, acc_snl) :: acc_l
    in
    loop [] [] [] l
  in
  (* sort *)
  let new_l =
    if a_sort then
      List.sort
        (fun (pl1, _) (pl2, _) -> sort_place_utf8 (pl1, "") (pl2, ""))
        new_l
    else
      List.sort
        (fun (pl1, _) (pl2, _) -> sort_place_utf8 (pl2, "") (pl1, ""))
        new_l
  in
  let new_l =
    if f_sort then
      List.sort
        (fun (_, ipl1) (_, ipl2) ->
          if up then List.length ipl1 - List.length ipl2
          else List.length ipl2 - List.length ipl1)
        new_l
    else new_l
  in
  (* accumulate snl entries with same pl value *)
  let new_l =
    let rec loop prev acc_snl acc_l new_l =
      match (new_l, prev) with
      | (pl, snl) :: l, prev when prev = strip_pl keep pl ->
          loop pl ((pl, snl) :: acc_snl) acc_l l
      | (pl, snl) :: l, prev when prev <> strip_pl keep pl ->
          loop pl
            [ (pl, snl) ]
            (if acc_snl <> [] then acc_snl :: acc_l else acc_l)
            l
      | (pl, snl) :: l, _ -> loop pl [] ([ (pl, snl) ] :: acc_l) l
      | [], _ -> if acc_snl <> [] then acc_snl :: acc_l else acc_l
    in
    loop [ "" ] [] [] new_l
  in
  let print_one_entry l =
    let len = List.fold_left (fun acc (_, ipl) -> acc + List.length ipl) 0 l in
    let rec loop0 l =
      match l with
      | [] -> ()
      | (pl, ipl) :: l ->
          let str = places_to_string true pl in
          let str2 = (Mutil.encode str :> string) in
          Output.printf conf
            "<a href=\"%sm=PPS%s&display=%s&keep=%s&k=%s\">%s</a>"
            (commd conf :> string)
            opt
            (if long then "long" else "short")
            (string_of_int (keep + 1))
            str2 str;
          if len < max_rlm_nbr conf then (
            Output.printf conf "&nbsp;(<a href=\"%sm=L&data=place%s&k=%s&nb=%d"
              (commd conf :> string)
              opt str2 len;
            let rec loop1 i = function
              | [] -> ()
              | (pl, ipl) :: l ->
                  let rec loop2 i = function
                    | [] -> loop1 i l
                    | ip :: ipl ->
                        Output.printf conf "&i%d=%s%s" i
                          (Driver.Iper.to_string ip)
                          (Printf.sprintf "&p%d=%s" i
                             (places_to_string false pl));
                        loop2 (i + 1) ipl
                  in
                  loop2 i ipl
            in
            loop1 0 ((pl, ipl) :: l);
            Output.printf conf "\" title=\"%s\">%d</a>)"
              (Utf8.capitalize (transl conf "summary book ascendants"))
              len)
          else Output.printf conf "&nbsp;(%d)" len;
          loop0 l
    in
    loop0 l
  in
  let rec loop first = function
    | l1 :: l ->
        Output.print_sstring conf (if first then "" else ", ");
        print_one_entry l1;
        loop false l
    | [] -> ()
  in
  loop true new_l;
  Output.print_sstring conf "<p>"

let print_html_places_surnames_long conf base link_to_ind
    (arry : ((string list * string) * (string * Driver.iper list) list) array) =
  (* (sub_places_list * suburb) * (surname * ip_list) list *)
  let k =
    (Mutil.encode (match p_getenv conf.env "k" with Some s -> s | _ -> "")
      :> string)
  in
  let keep = match p_getint conf.env "keep" with Some t -> t | None -> 1 in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let opt = get_opt conf in
  Array.sort (fun (k1, _) (k2, _) -> sort_place_utf8 k1 k2) arry;
  let l = Array.to_list arry in
  (* sort global list according to a_sort, f_sort *)
  let l =
    if f_sort then
      List.sort
        (fun (_, ipl1) (_, ipl2) ->
          if up then List.length ipl1 - List.length ipl2
          else List.length ipl2 - List.length ipl1)
        l
    else if a_sort then
      List.sort (fun (p1, _) (p2, _) -> sort_place_utf8 p2 p1) l
    else l
  in
  let print_sn (sn, ips) (pl, _sub) =
    (* Warn : do same sort_uniq in short mode *)
    let ips = List.sort_uniq compare ips in
    let places = places_to_string true pl in
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
      (* sort surname list according to a_sort, f_sort *)
      if f_sort then
        List.sort
          (fun (_, ipl1) (_, ipl2) ->
            if up then List.length ipl1 - List.length ipl2
            else List.length ipl2 - List.length ipl1)
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
    | ((pl, sub), snl) :: l ->
        let rec loop1 prev (pl, sub) =
          match (prev, pl) with
          | [], l2 ->
              List.iter
                (fun x ->
                  Output.printf conf "<li>%s<ul>\n"
                    (pps_call conf opt true keep k [ x ]))
                l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then loop1 l1 (l2, sub)
              else (
                List.iter
                  (fun _ -> Output.print_sstring conf "</ul></li>\n")
                  (x1 :: l1);
                loop1 [] (x2 :: l2, sub))
          | _ -> Output.print_sstring conf "</ul></li>\n"
          (* FIXME was assert false!! *)
        in
        loop1 prev (pl, sub);
        print_sn_list (pl, sub) snl;
        loop pl l
    | [] -> List.iter (fun _ -> Output.print_sstring conf "</ul></li>\n") prev
  in
  Output.print_sstring conf "<ul>\n";
  loop [] l;
  Output.print_sstring conf "</ul>\n"

let print_all_places_surnames_aux conf base _ini ~add_birth ~add_baptism
    ~add_death ~add_burial ~add_marriage max_length short filter =
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let arry =
    get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      ~add_marriage ([], "") [] (fold_place_long inverted) filter
      (fun prev p ->
        (* add one ip to a list flagged by surname *)
        let value = (Driver.get_surname p, Driver.get_iper p) in
        match prev with Some l -> value :: l | None -> [ value ])
      (fun v ->
        let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
        let rec loop acc l =
          match (l, acc) with
          | [], _ -> acc
          | (sn, iper) :: tl_list, (sn', iper_list) :: tl_acc
            when Driver.sou base sn = sn' ->
              loop ((sn', iper :: iper_list) :: tl_acc) tl_list
          | (sn, iper) :: tl_list, _ ->
              loop ((Driver.sou base sn, [ iper ]) :: acc) tl_list
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
  let keep = match p_getint conf.env "keep" with Some t -> t | None -> 1 in
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
    match List.assoc_opt "place_surname_link_to_ind" conf.base_env with
    | Some "yes" -> true
    | _ -> false
  in
  let t =
    if short then
      Printf.sprintf "%s" (Utf8.capitalize (transl conf "v7 list too long"))
    else ""
  in
  let href =
    Printf.sprintf "href=\"%sm=PPS%s&display=%s&keep=%s%s\" title=\"%s\""
      (commd conf :> string)
      opt
      (if long then "short" else "long")
      (string_of_int keep)
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
  let ini, filter =
    match p_getenv conf.env "k" with
    | Some ini ->
        ( ini,
          if ini = "" then fun _ -> true else fun (x, _) -> find_in conf x ini
        )
    | None -> ("", fun _ -> true)
  in
  try
    print_all_places_surnames_aux conf base ini ~add_birth ~add_baptism
      ~add_death ~add_burial ~add_marriage lim false filter
  with List_too_long ->
    let conf =
      {
        conf with
        env =
          ("display", Adef.encoded "short")
          :: List.remove_assoc "display" conf.env;
      }
    in
    print_all_places_surnames_aux conf base ini ~add_birth ~add_baptism
      ~add_death ~add_burial ~add_marriage lim true filter

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
