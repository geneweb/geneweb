(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Hutil
open Util

let rec alphabetic_order_list l1 l2 =
  if l1 = [] || l2 = [] then
    if l1 <> [] && l2 = [] then 1 else if l1 = [] && l2 <> [] then -1 else 0
  else
    let sort = (Gutil.alphabetic_utf_8 (List.hd l1) (List.hd l2)) in
    if sort = 0 then
      alphabetic_order_list (List.tl l1) (List.tl l2)
    else sort

let suburb_aux sub nosub s =
  let len = String.length s in
  if len = 0 then nosub ""
  else begin
    if String.unsafe_get s 0 = '[' then begin
      match String.index_opt s ']' with
      | None -> nosub s
      | Some i ->
        match
          let rec loop b i =
            if i = len then None
            else match Char.code s.[i] with
              | 0x20 -> loop b (i + 1)
              | 0x2D when not b -> loop true (i + 1) (* hyphen *)
              (* handle en and em dash as well *)
              | 0xE2 when Char.code s.[i+1] = 0x80 && 
                         (Char.code s.[i+2] = 0x93 ||
                          Char.code  s.[i+2] = 0x94) &&
                          not b -> loop true (i + 3)
              | _ -> if b then Some i else None
          in loop false (i + 1)
        with
        | None -> nosub s
        | Some j -> sub s len i j
    end else nosub s
  end

(** [split_suburb "[foo-bar] - boobar (baz)"] is ["foo-bar", "boobar (baz)")] *)
let split_suburb =
  suburb_aux
    begin fun s len i j -> String.sub s 1 (i - 1), String.sub s j (len - j) end
    begin fun s -> "", s end

(** [only_suburb "[foo-bar] - boobar (baz)"] is ["foo-bar"]
    [only_suburb "boobar (baz)"] is [""] *)
let only_suburb =
  suburb_aux
    begin fun s _len i _j -> String.sub s 1 (i - 1) end
    begin fun _ -> "" end

(** [without_suburb "[foo-bar] - boobar (baz)"] is ["boobar (baz)"]
    [without_suburb "boobar (baz)"] is ["boobar (baz)"] *)
let without_suburb =
  suburb_aux
    begin fun s len _i j -> String.sub s j (len - j) end
    begin fun s -> s end

let has_suburb s = String.unsafe_get s 0 = '['

type 'a env =
    Vlist_data of (string * (string * int) list) list
  | Vlist_ini of string list
  | Vlist_value of (string * (string * int) list) list
  | Venv_keys of (string * int) list
  | Vint of int
  | Vstring of string
  | Vbool of bool
  | Vother of 'a
  | Vnone

let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let normalize =
  suburb_aux
    begin fun s len i j ->
      let b = Bytes.create (len - j + i + 1) in
      Bytes.blit_string s 1 b 0 (i - 1) ;
      Bytes.unsafe_set b (i - 1) ',' ;
      Bytes.unsafe_set b i ' ' ;
      Bytes.blit_string s j b (i + 1) (len - j) ;
      Bytes.unsafe_to_string b
    end
    begin fun s -> s end

let compare_places s1 s2 =
  let ss1, s1 = split_suburb s1 in
  let ss2, s2 = split_suburb s2 in
  match
    Mutil.list_compare
      Gutil.alphabetic_order
      (String.split_on_char ',' s1)
      (String.split_on_char ',' s2)
  with
  | 0 -> Gutil.alphabetic_order ss1 ss2
  | x -> x

let max_rlm_nbr conf =
  match p_getenv conf.env "max_rlm_nbr" with
  | Some n -> if n = "" then
      match p_getenv conf.base_env "max_rlm_nbr" with
      | Some n ->
        if n = "" then 80 else int_of_string n
      | None -> 80
      else int_of_string n
  | None ->
      match p_getenv conf.base_env "max_rlm_nbr" with
      | Some n ->
        if n = "" then 80 else int_of_string n
      | None -> 80

(* [String.length s > 0] is always true because we already tested [is_empty_string].
   If it is not true, then the base should be cleaned. *)
let fold_place_long inverted s =
  let sub = only_suburb s in
  let s = without_suburb s in
  let len = String.length s in
  (* Trimm spaces after ',' and build reverse String.split_on_char ',' *)
  let rec loop iend list i ibeg =
    if i = iend
    then if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
    else
      let (list, ibeg) =
        match String.unsafe_get s i with
        | ',' ->
          let list =
            if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
          in
          list, i + 1
        | ' ' when i = ibeg -> (list, i + 1)
        | _ -> list, ibeg
      in
      loop iend list (i + 1) ibeg
  in
  let list =
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        let j =
          let rec loop i =
            if i >= 0 && String.unsafe_get s i = ' ' then loop (i - 1) else i + 1
          in
          loop (i - 1)
        in
        String.sub s (i + 1) (len - i - 2) :: loop j [] 0 0
      | _ -> loop len [] 0 0
    else loop len [] 0 0
  in
  ((if inverted then List.rev list else list), sub)

let places_to_string pl =
  (* TODO reverse ??*)
  let pl = List.rev pl in
  let rec loop acc first =
    function
    | p :: l -> loop (p ^ (if first then "" else ", ") ^ acc) false l
    | [] -> acc
  in loop "" true pl

exception List_too_long

let get_opt conf =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "ba" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let lower = p_getenv conf.env "lower" = Some "on" in
  let word = p_getenv conf.env "word" = Some "on" in
  let any = p_getenv conf.env "any" = Some "on" in
  (if add_birth then "&bi=on" else "") ^
  (if add_baptism then "&ba=on" else "") ^
  (if add_death then "&de=on" else "") ^
  (if add_burial then "&bu=on" else "") ^
  (if add_marriage then "&ma=on" else "") ^
  (if f_sort then "&f_sort=on" else "") ^
  (if up then "&up=on" else "") ^
  (if a_sort then "&a_sort=on" else "") ^
  (if lower then "&lower=on" else "") ^
  (if word then "&word=on" else "") ^
  (if any then "&any=on" else "")

let get_all =
  fun conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
    (dummy_key : 'a)
    (dummy_value : 'c)
    (fold_place : string -> 'a)
    (filter : 'a -> bool)
    (mk_value : 'b option -> person -> 'b)
    (fn : 'b -> 'c)
    (max_length : int) : ('a * 'c)
    array ->
    let ht_size = 2048 in (* FIXME: find the good heuristic *)
    let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
    let long = p_getenv conf.env "display" = Some "long" in
    let ht_add istr p =
      let key : 'a = sou base istr |> fold_place in
      if filter key then begin
        begin match Hashtbl.find_opt ht key with
          | Some _ as prev -> Hashtbl.replace ht key (mk_value prev p)
          | None ->
            Hashtbl.add ht key (mk_value None p) ;
            if Hashtbl.length ht > max_length && long then raise List_too_long
        end
      end
    in
    if add_birth || add_death || add_baptism || add_burial then begin
      let aux b fn p =
        if b then let x = fn p in if not (is_empty_string x) then ht_add x p
      in
      Gwdb.Collection.iter (fun i ->
          let p = pget conf base i in
          if authorized_age conf base p then begin
            aux add_birth get_birth_place p ;
            aux add_baptism get_baptism_place p ;
            aux add_death get_death_place p ;
            aux add_burial get_burial_place p ;
          end)
        (Gwdb.ipers base) ;
    end ;
    if add_marriage then begin
      Gwdb.Collection.iter (fun i ->
          let fam = foi base i in
          let pl_ma = get_marriage_place fam in
          if not (is_empty_string pl_ma) then
            let fath = pget conf base (get_father fam) in
            let moth = pget conf base (get_mother fam) in
            if authorized_age conf base fath
            && authorized_age conf base moth
            then begin
              ht_add pl_ma fath ;
              ht_add pl_ma moth
            end)
        (Gwdb.ifams base) ;
    end ;
    let len = Hashtbl.length ht in
    let array = Array.make len (dummy_key, dummy_value) in
    let i = ref 0 in
    Hashtbl.iter
      (fun k v ->
         Array.unsafe_set array !i (k, fn v) ;
         incr i)
      ht ;
    array

let rec sort_place_utf8 k1 k2 =
  match k1, k2 with
  | ([], sub1), ([], sub2) -> Gutil.alphabetic_order sub1 sub2
  | _, ([], _) -> 1
  | ([], _), _ -> -1
  | (p1 :: pl1, sub1), (p2 :: pl2, sub2) -> 
      if Gutil.alphabetic_order p1 p2 = 0 then
        sort_place_utf8 (pl1, sub1) (pl2, sub2)
      else Gutil.alphabetic_order p1 p2

let clean_ps ps =
  let len = String.length ps in
  if ps.[0] = '(' && ps.[len - 1] = ')' then
    String.sub ps 1 (len - 2)
  else ps

let find_in conf x ini =
  (* look at possibility to have ini=aaa, bbb or aaa (bbb) *)
  let word = p_getenv conf.env "word" = Some "on" in (* full words *)
  let case = p_getenv conf.env "case" = Some "on" in (* case sensitive *)
  let any = p_getenv conf.env "any" = Some "on" in (* anywhere in place list *)
  let low s = if not case then Name.lower s else s in
  let inil = String.split_on_char ',' ini in
  let inil =
    if List.length inil = 1 then
      match String.index_opt ini '(' with
      | Some index ->
        [(String.sub ini 0 (index - 1));
         (String.sub ini index (String.length ini - index))]
      | None -> [ini]
    else inil
  in
  List.fold_left (fun acc ini ->
      let ini = low ini in
      acc &&
      (if any || List.length inil > 1 then 
         List.fold_left (fun r p ->
             r || (if word then low p = ini else Mutil.contains (low p) ini))
           false x
       else
       if word then low (List.hd x) = ini
       else Mutil.contains (low (List.hd x)) ini
      )) true inil

let get_ip_list (snl : (string * iper list) list) =
  List.map snd snl |> List.flatten |> List.sort_uniq compare

(** print the number of items in ip_list and a call to m=L for them **)
(* TODO clean-up pi (place) and qi (suburb??) *)
let print_ip_list conf places sub opt link_to_ind ipl =
  let len = List.length ipl in
  if len > (max_rlm_nbr conf) && link_to_ind then
    Output.printf conf "(%d)" len
  else begin
    let head = 
      Printf.sprintf " (<a href=\"%sm=L&k=%s%s&nb=%d&p0=%s&q0=%s"
        (commd conf) (Mutil.encode sub) opt len places sub
    in
    let body =
      let rec loop i acc =
      function
      | [] -> acc
      | ip :: ipl ->
          loop (i+1) ((Printf.sprintf "&i%d=%s" i (Gwdb.string_of_iper ip)) ^ acc) ipl
      in loop 0 "" ipl
    in
    let tail =  
      Printf.sprintf "\" title=\"%s\">%d</a>)"
        (Utf8.capitalize (transl conf "summary book ascendants")) (List.length ipl)
    in
    Output.print_sstring conf (head ^ body ^ tail)
  end

(** print a call to m=PPS with a new k value *)
let pps_call conf opt long k places =
  Printf.sprintf "<a href=\"%sm=PPS%s&display=%s&k=%s\">%s</a>\n"
    (commd conf) opt (if long then "long" else "short") k
    (List.fold_left (fun acc p ->
      acc ^ (if acc <> "" then ", " else "") ^ p) "" places)

(* build ip list for all entries having same List.hd pl *)
let get_new_list list =
  let new_list =
    let rec loop prev ipl acc =
      function
      | ((pl, _), snl) :: l when List.hd pl = prev ->
          loop prev ((get_ip_list snl) :: ipl) acc l
      | ((pl, _), snl) :: l ->
          loop (List.hd pl) [] ((prev, (List.flatten ipl)) :: acc) l
      | [] -> acc
    in
    loop "" [] [] list
  in
  new_list

let print_html_places_surnames_short conf _base link_to_ind
  (array : ((string list * string) * (string * iper list) list) array) =
  let very = p_getenv conf.env "very" = Some "on" in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let opt = get_opt conf in
  let list = Array.to_list array in
  let list = List.sort
      (fun (k1, _) (k2, _) -> sort_place_utf8 k1 k2) list
  in
  let new_list =
    let rec loop prev_pl acc acc_l =
      function
      | ((pl, sub), snl) :: list when (not very && prev_pl =  pl) ||
            (very && List.hd prev_pl = List.hd pl)->
          loop pl ((get_ip_list snl) :: acc) acc_l list
      | ((pl, sub), snl) :: list when acc <> [] ->
          let acc = List.sort_uniq compare (List.flatten acc) in
          loop pl [get_ip_list snl] ((prev_pl, acc) :: acc_l) list
      | ((pl, sub), snl) :: list ->
          loop pl [get_ip_list snl] acc_l list
      | [] ->
          let acc = List.sort_uniq compare (List.flatten acc) in
          (prev_pl, acc) :: acc_l
    in loop [""] [] [] list
  in
  let new_list =
    if a_sort then List.sort (fun (pl1, _) (pl2, _) ->
      sort_place_utf8 (pl2, "") (pl1, "")) new_list
    else List.sort (fun (pl1, _) (pl2, _) ->
      sort_place_utf8 (pl1, "") (pl2, "")) new_list
  in
  let new_list =
    if f_sort then List.sort (fun (_, ipl1) (_, ipl2) ->
      if up then ((List.length ipl1) - (List.length ipl2))
      else ((List.length ipl2) - (List.length ipl1))) new_list
    else new_list
  in
  let rec loop first =
    function
    | [] -> ()
    | (pl, ipl) :: list ->
        Output.print_sstring conf (if first then "" else "; ");
        Output.print_sstring conf
          (pps_call conf opt true (List.hd pl) 
            [(if very then (List.hd pl) else (places_to_string pl))]);
        print_ip_list conf (places_to_string pl) "sub" opt link_to_ind ipl;
        loop false list
  in loop true new_list;
  Output.print_sstring conf "<p>"

let print_html_places_surnames_long conf base link_to_ind
  (array : ((string list * string) * (string * iper list) list) array) =
  (* (sub_places_list * suburb) * (surname * ip_list) list *)
  let k = match p_getenv conf.env "k" with | Some s -> s | _ -> "" in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let opt = get_opt conf in
  let list = Array.to_list array in
  let list = List.sort
      (fun (k1, _) (k2, _) -> sort_place_utf8 k1 k2) list
  in
  let print_sn (sn, ips) (pl, sub) =
    (* Warn : do same sort_uniq in short mode *)
    let ips = List.sort_uniq compare ips in
    let len = List.length ips in
    let places = places_to_string pl in
    let sub = "[" ^ sub ^ "]" in
    Output.printf conf "<a href=\"%s" (commd conf);
    if link_to_ind && len = 1
    then Output.print_string conf (acces conf base @@ pget conf base @@ List.hd ips)
    else Output.printf conf "m=N&v=%s" (Mutil.encode sn);
    Output.printf conf "\">%s</a>" sn;
    print_ip_list conf places sub opt link_to_ind ips

  in
  let print_sn_list (pl, sub) (snl : (string * iper list) list) =
    if List.length pl = 1 then Output.print_sstring conf "<ul>\n";
    if sub <> "" then Output.printf conf "<li>%s<ul>\n" sub;
    let snl = if f_sort then
        List.sort
          (fun (_, ipl1) (_, ipl2) ->
            if up then ((List.length ipl1) - (List.length ipl2))
            else ((List.length ipl2) - (List.length ipl1))) snl
      else
        List.sort
          (fun (p1, _) (p2, _) ->
            if a_sort then Gutil.alphabetic_order p2 p1
            else Gutil.alphabetic_order p1 p2) snl
    in
    Mutil.list_iter_first (fun first x ->
      if not first then Output.printf conf ",\n" ; print_sn x (pl, sub)) snl ;
    Output.printf conf "\n";
    if sub <> "" then Output.print_sstring conf "</ul>\n";
    if List.length pl = 1 then Output.print_sstring conf "</ul>\n"
  in
  let rec loop prev =
    function
    | ((pl, sub), snl) :: list ->
        let rec loop1 prev (pl, sub) =
          match prev, pl with
          | [], l2 -> List.iter (fun x ->
              Output.printf conf "<li>%s<ul>\n" (pps_call conf opt true k [x])) l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then loop1 l1 (l2, sub)
              else
                begin
                  List.iter (fun _ -> Output.print_sstring conf "</ul></li>\n")
                    (x1 :: l1);
                  loop1 [] ((x2 :: l2), sub)
                end
          | _ -> () (* FIXME was assert false!! *)
        in
        loop1 prev (pl, sub);
        print_sn_list (pl, sub) snl;
        loop pl list
    | [] -> List.iter (fun _ -> Output.print_sstring conf "</ul></li>\n") prev
  in
  Output.print_sstring conf "<ul>\n";
  loop [] list;
  Output.print_sstring conf "</ul>\n"

let print_all_places_surnames_aux conf base _ini ~add_birth ~add_baptism
  ~add_death ~add_burial ~add_marriage max_length short filter =
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    get_all conf base ~add_birth ~add_baptism ~add_death
      ~add_burial ~add_marriage
      ([], "")
      []
      (fold_place_long inverted)
      filter
      (fun prev p -> (* add one ip to a list flagged by surname *)
         let value = (get_surname p, get_iper p) in
         match prev with Some list -> value :: list | None -> [ value ])
      (fun v ->
         let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
         let rec loop acc list = match list, acc with
           | [], _ -> acc
           | (sn, iper) :: tl_list, (sn', iper_list) :: tl_acc
                when (sou base sn) = sn' ->
                  loop ((sn', iper :: iper_list) :: tl_acc) tl_list
           | (sn, iper) :: tl_list, _ ->
             loop ((sou base sn, [iper]) :: acc) tl_list
         in
         loop [] v)
      max_length
  in
  Array.sort (fun (k1, _) (k2, _) -> sort_place_utf8 k1 k2) array ;
  let title _ =
    Output.printf conf "%s / %s" (Utf8.capitalize (transl conf "place"))
      (Utf8.capitalize (transl_nth conf "surname/surnames" 0))
  in
  let opt = get_opt conf in
  let long = p_getenv conf.env "display" = Some "long" in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  Hutil.interp_no_header conf "buttons_places"
    {Templ.eval_var = (fun _ -> raise Not_found);
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = fun _ -> raise Not_found}
    []
    (Gwdb.empty_person base Gwdb.dummy_iper);
  Output.printf conf "<form method=\"get\" action=\"%s\">\n" conf.command;
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
    | Some "yes" -> true
    | _ -> false
  in
  let t = if short then (Printf.sprintf "%s" (Utf8.capitalize
    (transl conf "v7 list too long"))) else ""
  in
  let href =
    if short then ""
    else
      Printf.sprintf "href=\"%sm=PPS%s%s%s\" title=\"%s\"" (commd conf) opt
      (if long then "&display=short" else "&display=long")
      (  match p_getenv conf.env "k" with
        | Some ini -> "&k=" ^ ini
        | None -> ""
      ) t
  in
  Output.printf conf "<p>\n<a %s>%s</a>" href
    (Utf8.capitalize (transl conf
      (if long then "short display" else "long display"))) ;
  if short then Output.printf conf " (%s)\n" t;
  Output.printf conf "<p>\n";
  if array <> [||] then
    if long then
      print_html_places_surnames_long conf base link_to_ind array
    else
      print_html_places_surnames_short conf base link_to_ind array;
  Output.printf conf "</form>\n";
  Hutil.trailer conf

let print_all_places_surnames conf base =
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "ba" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let lim = 
    try int_of_string @@ List.assoc "short_place_threshold"
      conf.base_env with _ -> 500
  in
  let (ini, filter) =
    match p_getenv conf.env "k" with
    | Some ini ->
        (ini, (if ini = "" then fun _ -> true else fun (x, _) ->
          find_in conf x ini))
    | None -> ("", (fun _ -> true))
  in
  try
    print_all_places_surnames_aux conf base ini ~add_birth ~add_baptism
      ~add_death ~add_burial ~add_marriage lim false filter
  with List_too_long ->
    let conf = {conf with env = ("display", "short") :: conf.env} in
    print_all_places_surnames_aux conf base ini ~add_birth ~add_baptism
      ~add_death ~add_burial ~add_marriage lim true filter

let print_list conf base =
    Hutil.interp conf "list"
      {Templ.eval_var = (fun _ -> raise Not_found);
       Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
       Templ.eval_predefined_apply = (fun _ -> raise Not_found);
       Templ.get_vother = get_vother; Templ.set_vother = set_vother;
       Templ.print_foreach = fun _ -> raise Not_found}
      [] ()
