(* Copyright (c) 1998-2007 INRIA *)

open Geneweb.Config
open Geneweb.Gwdb
open Geneweb.Util

module Gutil = Geneweb.Gutil
module Gwdb = Geneweb.Gwdb
module Hutil = Geneweb.Hutil
module Templ = Geneweb.Templ

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
            else match String.unsafe_get s i with
              | ' ' -> loop b (i + 1)
              | '-' when not b -> loop true (i + 1)
              | _ -> if b then Some i else None
          in loop false (i + 1)
        with
        | None -> nosub s
        | Some j -> sub s len i j
    end else nosub s
  end

(** [split_suburb "[foo-bar] - boobar (baz)"] is [9"foo-bar", "boobar (baz)")] *)
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

(** Transform ["[foo-bar] - boobar (baz)"] into ["foo-bar, boobar (baz)"] *)

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

(* [String.length s > 0] is always true because we already tested [is_empty_string].
   If it is not true, then the base should be cleaned. *)
let fold_place_long inverted s =
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
  (s, if inverted then List.rev list else list)

let fold_place_short inverted s =
  if inverted
  then match String.index_opt s ',' with
    | Some i -> String.sub s 0 i
    | None -> s
  else begin
    let len = String.length s in
    let default () =
      let i =
        match String.rindex_opt s ',' with
        | Some i ->
          let rec l i = if i < len && String.unsafe_get s i = ' ' then l (i + 1) else i in l (i + 1)
        | None -> 0
      in
      let i = if i = len then 0 else i in
      String.sub s i (len - i)
    in
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        String.sub s (i + 1) (len - i - 2)
      | _ -> default ()
    else default ()
  end

exception List_too_long

let get_opt conf =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
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
  (if add_baptism then "&bp=on" else "") ^
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
    (max_length : int) :
    ('a * 'c) array ->
    let ht_size = 2048 in (* FIXME: find the good heuristic *)
    let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
    let long = p_getenv conf.env "display" = Some "long" in
    let ht_add istr p =
      let key : 'a = sou base istr |> normalize |> fold_place in
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

let rec sort_place_utf8 pl1 pl2 =
  match pl1, pl2 with
    _, [] -> 1
  | [], _ -> -1
  | s1 :: pl11, s2 :: pl22 ->
    if Gutil.alphabetic_order s1 s2 = 0 then sort_place_utf8 pl11 pl22
    else Gutil.alphabetic_order s1 s2

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

let get_new_list list =
  let list1 =
    let rec loop acc =
      function
      | ((so, pl), snl) :: l ->
        let pln = if List.length pl > 0 then List.tl pl else [] in
        loop (((so, pln), pl, snl) :: acc) l
      | [] -> acc
    in
    loop [] list
  in
  let new_list =
    let rec loop cnt acc acc_ip =
      function
      | ((so, []), plo, snl) :: l ->
        let ipl = get_ip_list snl in
        let add = List.length ipl in
        loop 0 (((so, []), plo, (cnt + add), (ipl :: acc_ip)) :: acc) [] l
      | ((so, pl), plo, snl) :: l ->
        let ipl = get_ip_list snl in
        let add = List.length ipl in
        if (List.hd pl) <> "" &&
           (List.hd pl) <>
           (if (List.length l > 0) then
              (let ((_, pl1), _, _) = List.hd l in
               if List.length pl1 > 0 then List.hd pl1 else "") else "")
        then (* current pl item is <> from "" and from next pl item *)
          loop 0 (((so, pl), plo, (cnt + add), (ipl :: acc_ip)) :: acc) [] l
        else (* accumulate count and ip list *)
          loop (cnt + add) acc (ipl :: acc_ip) l
      | [] -> acc
    in
    loop 0 [] [] list1
  in
  new_list

let print_place_list conf opt long link_to_ind max_rlm_nbr pl_l =
  let title = transl conf "long/short display" in
  let max_rlm = match p_getenv conf.env "max_rlm_nbr"
    with | Some s -> s | _ -> ""
  in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let pl_l = if f_sort then
    List.sort
      (fun pl1 pl2 ->
        let ipl1 = List.fold_left (fun cnt (_, _, ipl) ->
          cnt + (List.length ipl)) 0 pl1
        in
        let ipl2 = List.fold_left (fun cnt (_, _, ipl) ->
          cnt + (List.length ipl)) 0 pl2
        in
        if up then ipl1 - ipl2 else ipl2 - ipl1) pl_l
  else
    List.sort
      (fun pl1 pl2 ->
        let (_, p1, _) = List.hd pl1 in
        let (_, p2, _) = List.hd pl2 in
        if a_sort then alphabetic_order_list p2 p1
        else alphabetic_order_list p1 p2) pl_l
  in
  let rec loop1 first =
    function
    | pl :: t_pl_l ->
        (* we need total nb of persons in list ahead of time *)
        let (cnt, so, p) =
          let rec loop2 cnt so p =
          function
          | (so, p, ipl) :: t_pl ->
              loop2 (cnt + List.length ipl) so p t_pl
          | _ -> (cnt, so, p)
          in loop2 0 "" [] pl;
        in
        if not first then Wserver.printf ", " ;
        let p1 =  List.hd p in
        let p2 = p1 ^
          (if List.length p > 1 then (", " ^ (List.hd (List.tl p))) else "")
        in
        Wserver.printf
          "<a href=\"%sm=PS%s%s%s\" title=\"%s\">%s</a>"
            (commd conf) opt ("&k=" ^ (Mutil.encode p2))
            (if not long then "&display=long" else "&display=short") title (p1) ;
        if link_to_ind && cnt < max_rlm_nbr then
          begin
          Wserver.printf " (<a href=\"%sm=L%s%s&nb=%d%s" (commd conf)
            ("&k=" ^ (Mutil.encode so)) opt cnt
            (if max_rlm <> "" then "&max_rlm_nbr=" ^ max_rlm else "") ;
          let rec loop3 cnt =
            function
            | (so, _, ipl) :: t_pl ->
                Wserver.printf "&p%d=%s" cnt so ;
                List.iteri (fun i ip ->
                  Wserver.printf "&i%d=%s" (i + cnt) (Gwdb.string_of_iper ip))
                ipl ;
                loop3 (cnt + List.length ipl) t_pl
            | _ -> ()
          in loop3 0 pl ;
          Wserver.printf "\" title=\" %s\">%d</a>)"
            (Utf8.capitalize (transl conf "summary book ascendants")) cnt
          end
        else
          Wserver.printf " (%d)" cnt ;
        loop1 false t_pl_l
    | _ -> ()
    in
  loop1 true pl_l

let print_html_places_surnames_short conf _base max_rlm_nbr link_to_ind
  (array : ((string * string list) * (string * iper list) list) array) =
  let long = p_getenv conf.env "display" = Some "long" in
  let k = match p_getenv conf.env "k" with | Some s -> s | _ -> "" in
  let opt = get_opt conf in
  let pl_sn_list = Array.to_list array in
  let new_list = get_new_list pl_sn_list in
  let new_list =
    List.sort
      (fun (_, plo1, _, _) (_, plo2, _, _) ->
        let ps11 = clean_ps
          (if List.length plo1 > 0 then List.hd plo1 else "")
        in
        let ps12 = clean_ps
          (if List.length plo2 > 0 then List.hd plo2 else "")
        in
        (* FIXME utf 8 ?? *)
        compare ps11 ps12) new_list
  in
  (* in new_list, ps is a string, pl was a list of strings *)
  if k = "" then
    let pl =
      let rec loop1 acc1 acc2 prev =
        function
        | ((so, _), p1 :: _, _, ipl) :: t_list when p1 <> prev ->
          let ipl = List.flatten ipl in
          loop1 [(so, [p1], ipl)]
          (if acc1 <> [] then (acc1 :: acc2) else acc2) p1 t_list
        | ((so, _), p1 :: _, _, ipl) :: t_list when p1 = prev ->
          let ipl = List.flatten ipl in
          loop1 ((so, [p1], ipl) :: acc1) acc2 p1 t_list
        | _ -> (acc1 :: acc2)
      in loop1 [] [] "" new_list
    in
    print_place_list conf opt long link_to_ind max_rlm_nbr pl
  else
    let rec loop2 acc prev =
      function
      | ((so, _), p1 :: t_pl, _, ipl) :: t_list when p1 <> prev ->
        if acc <> [] then
          Wserver.printf "<li>%s<br>" prev;
          print_place_list conf opt long link_to_ind max_rlm_nbr acc ;
        let p2 = if List.length t_pl > 0 then [(List.hd t_pl); p1] else [p1] in
        let ipl = List.flatten ipl in
        loop2 [[(so, p2, ipl)]] p1 t_list
      | ((so, _), p1 :: t_pl, _, ipl) :: t_list when p1 = prev ->
        let p2 = if List.length t_pl > 0 then [(List.hd t_pl); p1] else [p1] in
        let ipl = List.flatten ipl in
        loop2 ([(so, p2, ipl)] :: acc) p1 t_list
      | _ ->
        if acc <> [] then
          Wserver.printf "<li>%s<br>" prev;
          print_place_list conf opt long link_to_ind max_rlm_nbr acc
    in
    Wserver.printf "<ul>\n";
    loop2 [] "" new_list;
    Wserver.printf "</ul>\n"

let print_html_places_surnames conf base max_rlm_nbr link_to_ind
  (array : ((string * string list) * (string * iper list) list) array) =
  let k = match p_getenv conf.env "k" with | Some s -> s | _ -> "" in
  let a_sort = p_getenv conf.env "a_sort" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  let up = p_getenv conf.env "up" = Some "on" in
  let opt = get_opt conf in
  let list = Array.to_list array in
  let list = List.sort
      (fun ((_, pl1), _) ((_, pl2), _) ->
        sort_place_utf8 pl1 pl2) list
  in
  let list = if a_sort then List.rev list else list in
  let print_sn (sn, ips) so =
    (* Warn : do same sort_uniq in short mode *)
    let ips = List.sort_uniq compare ips in
    let len = List.length ips in
    Wserver.printf "<a href=\"%s" (commd conf);
    if link_to_ind && len = 1
    then Wserver.print_string (acces conf base @@ pget conf base @@ List.hd ips)
    else Wserver.printf "m=N&v=%s" (Mutil.encode sn);
    Wserver.printf "\">%s</a>" sn;
    if link_to_ind && List.length ips < max_rlm_nbr then
      begin
        Wserver.printf " (<a href=\"%sm=L%s%s&nb=%d" (commd conf)
          ("&k=" ^ (Mutil.encode so))
          opt len ;
        Wserver.printf "&p0=%s" so ;
        List.iteri (fun i ip ->
          Wserver.printf "&i%d=%s" i (Gwdb.string_of_iper ip))
        ips ;
        Wserver.printf "\" title=\"%s\">%d</a>)"
          (Utf8.capitalize (transl conf "summary book ascendants")) (List.length ips)
      end
    else
      Wserver.printf " (%d)" len
  in
  let print_sn_list (snl : (string * iper list) list) so =
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
    Wserver.printf "<li>\n";
    Mutil.list_iter_first (fun first x ->
      if not first then Wserver.printf ",\n" ; print_sn x so) snl ;
    Wserver.printf "\n";
    Wserver.printf "</li>\n"
  in
  let r = Str.regexp "\"" in
  let rec loop prev =
    function
    | ((so, pl), snl) :: list ->
        let so = Str.global_replace r "&quot;" so in
        let rec loop1 prev pl =
          match prev, pl with
          | [], l2 -> List.iter (fun x ->
              let str = Printf.sprintf "<a href=\"%sm=PS%s%s\">%s</a>\n"
                (commd conf) ("&k=" ^ k) opt x in
              Wserver.printf "<li>%s<ul>\n" str) l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then loop1 l1 l2
              else
                begin
                  List.iter (fun _ -> Wserver.printf "</ul></li>\n")
                    (x1 :: l1);
                  loop1 [] (x2 :: l2)
                end
          | _ -> () (* FIXME was assert false!! *)
        in
        loop1 prev pl;
        if List.length pl = 1 then Wserver.printf "<ul>\n";
        print_sn_list snl so;
        if List.length pl = 1 then Wserver.printf "</ul>\n";
        loop pl list
    | [] -> List.iter (fun _ -> Wserver.printf "</ul></li>\n") prev
  in
  Wserver.printf "<ul>\n";
  loop [] list ;
  Wserver.printf "</ul>\n"

let print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage =
    (if add_birth then "&bi=on" else "") ^
    (if add_baptism then "&bp=on" else "") ^
    (if add_death then "&de=on" else "") ^
    (if add_burial then "&bu=on" else "") ^
    (if add_marriage then "&ma=on" else "")

let print_aux conf title fn =
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  fn () ;
  Hutil.trailer conf

let print_all_places_surnames_short conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage =
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    get_all
      conf base ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage
      "" 0
      (fold_place_short inverted)
      (fun _ -> true)
      (fun prev _ -> match prev with Some n -> n + 1 | None -> 1)
      (fun x -> x)
      max_int
  in
  Array.sort (fun (s1, _) (s2, _) -> Gutil.alphabetic_order s1 s2) array ;
  let title _ = Wserver.print_string (Utf8.capitalize (transl conf "place")) in
  print_aux conf title begin fun () ->
    let opt = print_aux_opt ~add_birth ~add_baptism ~add_death ~add_burial ~add_marriage in
    Wserver.printf
      "<p><a href=\"%sm=PS%s&display=long\">%s</a></p><p>"
      (commd conf) opt (transl conf "long display") ;
    let last = Array.length array - 1 in
    Array.iteri
      (fun i (s, x) ->
         Wserver.printf "<a href=\"%sm=PS%s&k=%s\">%s</a> (%d)%s"
           (commd conf) opt (Mutil.encode s) s x (if i = last then "" else ",\n"))
      array ;
    Wserver.printf "</p>\n"
  end

let print_buttons conf _base =
  Hutil.interp_no_header conf "buttons_places"
    {Templ.eval_var = (fun _ -> raise Not_found);
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = (fun _ -> raise Not_found)}
    [] ()

let print_all_places_surnames_long conf base _ini ~add_birth ~add_baptism
  ~add_death ~add_burial ~add_marriage max_length short filter =
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    get_all conf base ~add_birth ~add_baptism ~add_death
      ~add_burial ~add_marriage
      ("", []) [] (fold_place_long inverted) filter
      (fun prev p ->
         let value = (get_surname p, get_iper p) in
         match prev with Some list -> value :: list | None -> [ value ])
      (fun v ->
         let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
         let rec loop acc list = match list, acc with
           | [], _ -> acc
           | (sn, iper) :: tl_list, (sn', iper_list) ::
              tl_acc when (sou base sn) = sn' ->
                loop ((sn', iper:: iper_list) :: tl_acc) tl_list
           | (sn, iper) :: tl_list, _ ->
             loop ((sou base sn, [iper]) :: acc) tl_list
         in
         loop [] v)
      max_length
  in
  Array.sort (fun ((_, pl1), _) ((_, pl2), _) ->
    sort_place_utf8 pl1 pl2) array ;
  let title _ =
    Wserver.printf "%s / %s" (Utf8.capitalize (transl conf "place"))
      (Utf8.capitalize (transl_nth conf "surname/surnames" 0))
  in
  let opt = get_opt conf in
  let long = p_getenv conf.env "display" = Some "long" in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  print_buttons conf base;
  let max_rlm_nbr =
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
  in
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
    | Some "yes" -> true
    | _ -> false
  in
  let t = if short then (Printf.sprintf "%s" (Utf8.capitalize
    (transl conf "list too long"))) else ""
  in
  let href =
    if short then ""
    else
      Printf.sprintf "href=\"%sm=PS%s%s%s\" title=\"%s\"" (commd conf) opt
      (if long then "&display=short" else "&display=long")
      (  match p_getenv conf.env "k" with
        | Some ini -> "&k=" ^ ini
        | None -> ""
      ) t
  in
  Wserver.printf "<p>\n<a %s>%s</a>" href
    (Utf8.capitalize (transl conf
      (if long then "short display" else "long display"))) ;
  if short then Wserver.printf " (%s)\n" t;
  Wserver.printf "<p>\n";
  if array <> [||] then
    if long then
      print_html_places_surnames conf base max_rlm_nbr link_to_ind array
    else
      print_html_places_surnames_short conf base max_rlm_nbr link_to_ind array;
  Hutil.trailer conf

let print_all_places_surnames conf base =
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let lim = 
    try int_of_string @@ List.assoc "short_place_threshold"
      conf.base_env with _ -> 500
  in
  let (ini, filter) =
    match p_getenv conf.env "k" with
    | Some ini ->
        (ini, (if ini = "" then fun _ -> true else fun (_, x) ->
          find_in conf x ini))
    | None -> ("", (fun _ -> true))
  in
  try
    print_all_places_surnames_long conf base ini ~add_birth ~add_baptism
      ~add_death ~add_burial ~add_marriage lim false filter
  with List_too_long ->
    let conf = {conf with env = ("display", "short") :: conf.env} in
    print_all_places_surnames_long conf base ini ~add_birth ~add_baptism
      ~add_death ~add_burial ~add_marriage lim true filter
