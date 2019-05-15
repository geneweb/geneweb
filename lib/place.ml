(* $Id: place.ml,v 5.21 2007-09-18 19:12:08 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util
open TemplAst

let normalize =
  (* petit hack en attendant une vraie gestion des lieux transforme
     "[foo-bar] - boobar (baz)" en "[foo-bar], boobar (baz)"
     On remet les [ ] pour retrouver le lien vers le dictionnaire!
     La convention [aaa] - bbb permet de trier les lieux par rapport à bbb
     Cette fonction normalize n'est utilisée que dans le module place
     la séquence [-–—] distingue les trois tirets possibles
  *)
  let r = Str.regexp "^\\[\\([^]]+\\)\\] *\\(-\\|–\\|—\\) *\\(.*\\)" in
  fun s -> Str.global_replace r "[\\1], \\3" s

(* [String.length s > 0] is always true because we already tested [is_empty_string].
   If it is not true, then the base should be cleaned. *)
let fold_place_long inverted s =
  let len = String.length s in
  (* Trimm spaces after ',' and build reverse String.split_on_char ',' *)
  let rec loop iend list i ibeg bracket=
    if i = iend
    then if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
    else
      let (list, ibeg, bracket) =
        match String.unsafe_get s i with
        | '[' -> list, ibeg, true
        | ']' -> list, ibeg, false
        | ',' ->
          if bracket then list, ibeg, true
          else
            let list =
              if i > ibeg then String.sub s ibeg (i - ibeg) :: list else list
            in
            list, i + 1, false
        | ' ' when i = ibeg -> (list, i + 1, bracket)
        | _ -> list, ibeg, bracket
      in
      loop iend list (i + 1) ibeg bracket
  in
  let (iend, rest) =
    if String.unsafe_get s (len - 1) = ')'
    then match String.rindex_opt s '(' with
      | Some i when i < len - 2 ->
        let j =
          let rec loop i =
            if i >= 0 && String.unsafe_get s i = ' '
            then loop (i - 1) else i + 1
          in
          loop (i - 1)
        in
        j, [ String.sub s (i) (len - i) ] (* +1 and -2 to strip () *)
      | _ -> len, []
    else len, []
  in
  let list = List.rev_append rest @@ loop iend [] 0 0 false in
  if inverted then List.rev list else list

let unfold_place_long inverted s =
  let pl = List.rev (fold_place_long inverted s) in
  let s = List.fold_left
    (fun acc p -> acc ^ (if acc = "" then "" else ", ") ^ p) "" pl
  in s

let fold_place_short s =
  let len = String.length s in
  let default () =
    let i =
      match String.rindex_opt s ',' with
      | Some i ->
        let rec l i =
          if i < len && String.unsafe_get s i = ' '
          then l (i + 1) else i in l (i + 1)
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

let get_all =
  fun conf base ~add_birth ~add_baptism ~add_death ~add_burial
    (dummy_key : 'a)
    (dummy_value : 'c)
    (fold_place : string -> 'a)
    (filter : 'a -> bool)
    (mk_value : 'b option -> person -> 'b)
    (fn : 'b -> 'c) :
    ('a * 'c) array ->
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let ht_size = 2048 in (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let ht_add istr p =
    let key : 'a = sou base istr |> normalize |> fold_place in
    if filter key then
      match Hashtbl.find_opt ht key with
      | Some _ as prev -> Hashtbl.replace ht key (mk_value prev p)
      | None -> Hashtbl.add ht key (mk_value None p)
  in
  if add_birth || add_death || add_baptism || add_burial then begin
    let len = nb_of_persons base in
    let aux b fn p =
      if b then let x = fn p in if not (is_empty_string x) then ht_add x p
    in
    let rec loop i =
      if i < len then begin
        let p = pget conf base (Adef.iper_of_int i) in
        if authorized_age conf base p then begin
          aux add_birth get_birth_place p ;
          aux add_baptism get_baptism_place p ;
          aux add_death get_death_place p ;
          aux add_burial get_burial_place p ;
        end ;
        loop (i + 1)
      end
    in
    loop 0 ;
  end ;
  if add_marriage then begin
    let rec loop i =
      let len = nb_of_families base in
      if i < len then begin
        let fam = foi base (Adef.ifam_of_int i) in
        if not @@ is_deleted_family fam then begin
          let pl_ma = get_marriage_place fam in
          if not (is_empty_string pl_ma) then
            let fath = pget conf base (get_father fam) in
            let moth = pget conf base (get_mother fam) in
            if authorized_age conf base fath
            && authorized_age conf base moth
            then begin
              ht_add pl_ma fath ;
              ht_add pl_ma moth
            end
        end ;
        loop (i + 1) ;
      end
    in
    loop 0 ;
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

let get_opt conf =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let add_marriage = p_getenv conf.env "ma" = Some "on" in
  let f_sort = p_getenv conf.env "f_sort" = Some "on" in
  (if add_birth then "&bi=on" else "") ^
  (if add_baptism then "&bp=on" else "") ^
  (if add_death then "&de=on" else "") ^
  (if add_burial then "&bu=on" else "") ^
  (if add_marriage then "&ma=on" else "") ^
  (if f_sort then "&f_sort=on" else "") ^
  "&dates=on"

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

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x
let bool_val x = VVbool x
let str_val x = VVstring x

let string_to_list str =
  let rec loop acc =
    function
      s ->
        if String.length s > 0 then
          let nbc = Name.nbc s.[0] in
          let c = String.sub s 0 nbc in
          let s1 = String.sub s nbc (String.length s - nbc) in
          loop (c :: acc) s1
        else acc
  in loop [] str

let get_ip_list (snl : (string * Adef.iper list) list) =
  List.map snd snl |> List.flatten |> List.sort_uniq compare

let sort_list _conf list =
  let list_n =
    let rec loop cnt1 cnt2 acc1 acc2 prev1 prev2 =
      function
      | (pl, snl) :: l ->
          let k1 = if List.length pl > 0 then List.hd pl else "" in
          let k2 = if List.length pl > 1 then List.hd (List.tl pl) else "" in
          if k1 = prev1 && k2 = prev2 then
            loop (cnt1 + (List.length snl))
            (cnt2 + (List.length snl))
            ((pl, snl) :: acc1) acc2 prev1 k2 l
          else
            loop (List.length snl) (List.length snl) [(pl, snl)]
              (List.fold_left 
                (fun acc (pl, snl) ->
                  (cnt1, cnt2, pl, snl) :: acc) acc2 acc1)
              k1 k2 l
      | [] -> (List.fold_left 
                (fun acc (pl, snl) ->
                  (cnt1, (List.length snl), pl, snl) :: acc) acc2 acc1)
    in loop 0 0 [] [] "" "" list
  in
  let list_n = List.sort (fun (c1, c3, _, _) (c2, c4, _, _) -> 
    if c1 = c2 then c3 - c4 else c1 - c2) list_n in
  List.fold_left (fun acc (_, _, pl, snl) -> (pl, snl) :: acc) [] list_n

let get_new_list conf list =
  let k1 = match p_getenv conf.env "k1" with | Some s -> s | _ -> "" in
  let list1 =
    let rec loop acc =
      function
      | (pl, snl) :: l ->
        let pln = if k1 = "" then pl else if List.length pl > 0 then List.tl pl else [] in
        loop ((pln, pl, snl) :: acc) l
      | [] -> acc
    in
    loop [] list
  in
  let (new_list, cntt) =
    let rec loop cntt cnt acc acc_ip =
      function
      | ([], plo, snl) :: l ->
          let ipl = get_ip_list snl in
          let add = List.length ipl in
          loop (cntt + add) 0 (([], plo, (cnt + add), [],
            (ipl :: acc_ip)) :: acc) [] l
      | (pl, plo, snl) :: l ->
          if (List.hd pl) <> "" &&
             (List.hd pl) <>
             (if (List.length l > 0) then
               (let (pl1, _, _) = List.hd l in
                if List.length pl1 > 0 then List.hd pl1 else "") else "")
          then
            let ipl = get_ip_list snl in
            let add = List.length ipl in
            loop (cntt + add) 0 ((pl, plo, (cnt + add),
              (if List.tl pl <> [] then List.tl pl else []),
              (ipl :: acc_ip)) :: acc) [] l
          else
            let ipl = get_ip_list snl in
            let add = List.length ipl in
            loop (cntt + add) (cnt + add)
            acc (ipl :: acc_ip) l
      | [] -> (acc, cntt)
    in
    loop 0 0 [] [] list1
  in
  (new_list, cntt)

let get_k3 plo =
  (* si on avait [xxx] - aaa, bbb, alors le s= se fera sur aaa *)
  let k4 = Util.code_varenv (if List.length plo > 1 && (List.hd (List.rev plo)).[0] = '['
    then List.hd (List.tl plo) else "")
  in
  let k3 =
    Util.code_varenv (List.fold_left
    (fun acc p -> p ^
      (if acc <> "" && acc.[0] = '(' then " "
        else if p.[0] = '['  then " - "
        else if acc <> "" then ", " 
        else "") ^ acc)
    "" plo)
  in
  (k3, k4)

let print_section conf opt ps1 f_sort =
  if not f_sort then
  Wserver.printf "</ul><h5><a href=\"%sm=PS%s%s%s\">%s</a></h5><ul>\n"
    (commd conf) opt "&long=on" ("&k1=" ^(Util.code_varenv ps1)) ps1
  
let clean_ps ps =
  let len = String.length ps in
  if ps.[0] = '(' && ps.[len - 1] = ')' then
    String.sub ps 1 (len - 2)
  else ps

let print_html_places_surnames_long conf _base
  (array : (string list * (string * Adef.iper list) list) array) =
  let max_rlm_nbr =
    match p_getenv conf.env "max_rlm_nbr" with
    | Some n -> int_of_string n
    | None -> 
        match p_getenv conf.base_env "max_rlm_nbr" with
        | Some n -> int_of_string n
        | None -> 80
  in
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
    | Some "yes" -> true
    | _ -> false
  in
  let opt = get_opt conf in
  let k1 = match p_getenv conf.env "k1" with | Some s -> s | _ -> "" in
  let k2 = match p_getenv conf.env "k2" with | Some s -> s | _ -> "" in
  let list = Array.to_list array in
  let long =
    match p_getenv conf.env "long" with
    | Some "on" -> true
    | _ -> false
  in
  let f_sort =
    match p_getenv conf.env "f_sort" with
    | Some "on" -> true
    | _ -> false
  in
  let print_sn ((sn, ips), _pl, plo) =
    let len = List.length ips in
    let ps1 = if List.length plo > 0 then List.hd plo else "" in
    let ps1 = clean_ps ps1 in
    let ps2 = if List.length plo > 1 then List.hd (List.tl plo) else "" in
    let (k3, k4) = get_k3 plo in
    Wserver.printf "<a href=\"%sm=N&v=%s\">%s</a>" (commd conf)
        (code_varenv sn) sn ;
    if link_to_ind && len < max_rlm_nbr then
      begin
        Wserver.printf " (<a href=\"%sm=L&surn=%s&nb=%d&nbs=3"
          (commd conf) sn (List.length ips) ;
        List.iteri (fun i ip ->
          Wserver.printf "&i%d=%d" i (Adef.int_of_iper ip))
        ips ;
        Wserver.printf "%s%s%s%s%s\">%d</a>)"
          (if ps1 <> "" then "&k1=" ^ Util.code_varenv (ps1) else "")
          (if ps2 <> "" then "&k2=" ^ Util.code_varenv (ps2) else "")
          (if k3 <> "" then "&k3=" ^ k3 else "")
          (if k4 <> "" then "&k4=" ^ k4 else "") opt len
      end
    else Wserver.printf " (%d)" len
  in
  let print_sn_list ((snl : (string * Adef.iper list) list), pl, plo) =
    let snl = List.sort
      (fun (sn1, _) (sn2, _) -> Gutil.alphabetic_order sn1 sn2) snl
    in
    let snl =
      if f_sort then
        List.rev
          (List.sort
            (fun (sn1, ipl1) (sn2, ipl2) ->
              let lipl1 = List.length ipl1 in
              let lipl2 = List.length ipl2 in
              if lipl1 = lipl2 then (Gutil.alphabetic_order sn1 sn2)
              else lipl1 - lipl2) snl)
      else snl
    in
    Wserver.printf "<li>\n" ;
    Mutil.list_iter_first
      (fun first x -> if not first then Wserver.printf ",\n" ;
        print_sn (x, pl, plo)) snl ;
    Wserver.printf "\n" ;
    Wserver.printf "</li>\n"
  in
  let title = transl conf "long/short display" in
  (* TODO sort list according to number of persons at k1 level *)
  (* as done in short *)
  let list = if f_sort then sort_list conf list else list in
  (* sort list removing the possible () around first item in place list *)
    let list =
    List.sort
      (fun (plo1, _) (plo2, _) -> 
        let ps11 = clean_ps
          (if List.length plo1 > 0 then List.hd plo1 else "")
        in
        let ps12 = clean_ps
          (if List.length plo2 > 0 then List.hd plo2 else "")
        in
        compare ps11 ps12) list
  in
  let rec loop prev =
    function
    | (plo, snl) :: list ->
        let pl =
          if k1 = "" then plo
          else if List.length plo > 0 then List.tl plo else []
        in
        let ps1 = if List.length plo > 0 then List.hd plo else "" in
        let ps1 = clean_ps ps1 in
        let ps2 = if List.length plo > 1 then List.hd (List.tl plo) else "" in
        let rec loop1 prev pl lvl =
          match prev, pl with
          | [], l2 ->
            if List.length l2 = 0
            then print_section conf opt ps1 f_sort;
            List.iteri
              (fun i x ->
                let x = clean_ps x in
                let href =
                  Printf.sprintf "%sm=PS%s%s%s%s"
                  (commd conf) opt
                  (if i >= 0 && ps1 <> "" then "&k1=" ^ Util.code_varenv (ps1) else "")
                  (if (k1 = "" && i + lvl -1 >= 1) || (  k1 <> "" && i >= 0 ) && ps2 <> ""
                    then "&k2=" ^ Util.code_varenv (ps2) else "")
                  (if k1 <> "" && k2 <> "" || long then "&long=on" else "")
                in
                Wserver.printf "<li><a href=\"%s\" title=\"%s\">%s</a><ul>\n"
                href title x)
              l2
          | x1 :: l1, x2 :: l2 ->
              if x1 = x2 then loop1 l1 l2 (lvl + 1)
              else
                begin
                  List.iter (fun _ -> Wserver.printf "</ul></li>\n")
                    (x1 :: l1) ;
                  loop1 [] (x2 :: l2) (lvl + 1)
                end
          | _ ->
              List.iter (fun _ -> Wserver.printf "</ul></li>\n") prev ;
              print_section conf opt ps1 f_sort
        in
        loop1 prev pl 0 ;
        let snl =
          List.fold_left
            (fun acc (sn, ipl) -> (sn, List.sort_uniq compare ipl) :: acc)
            [] snl
        in
        print_sn_list (snl, pl, plo) ;
        loop pl list
    | [] -> List.iter (fun _ -> Wserver.printf "</ul></li>\n") prev
  in
  Wserver.printf "<ul>\n" ;
  loop [] list ;
  Wserver.printf "</ul>\n"

let print_html_places_surnames_short conf _base
  (array : (string list * (string * Adef.iper list) list) array) =
  let max_rlm_nbr =
    match p_getenv conf.env "max_rlm_nbr" with
    | Some n -> int_of_string n
    | None -> 
        match p_getenv conf.base_env "max_rlm_nbr" with
        | Some n -> int_of_string n
        | None -> 80
  in
  let link_to_ind =
    match p_getenv conf.base_env "place_surname_link_to_ind" with
    | Some "yes" -> true
    | _ -> false
  in
  let k1 = match p_getenv conf.env "k1" with | Some s -> s | _ -> "" in
  let long = p_getenv conf.env "long" = Some "on" in
  let opt = get_opt conf in
  let pl_sn_list = Array.to_list array in
  let (new_list, _cntt) = get_new_list conf pl_sn_list in
  let new_list =
    if p_getenv conf.env "f_sort" = Some "on" then
      List.rev (List.sort
      (fun (_, _, cnt1, _, _) (_, _, cnt2, _, _) -> (cnt1 - cnt2)) new_list)
    else new_list
  in
  (* sort new_list removing the possible () around first item in place list *)
  let new_list =
    List.sort
      (fun (_, plo1, _, _, _) (_, plo2, _, _, _) -> 
        let ps11 = clean_ps
          (if List.length plo1 > 0 then List.hd plo1 else "")
        in
        let ps12 = clean_ps
          (if List.length plo2 > 0 then List.hd plo2 else "")
        in
        compare ps11 ps12) new_list
  in
  let title = transl conf "long/short display" in
  (* in new_list, ps is a string, pl was a list of strings *)
  Mutil.list_iter_first
    (fun first (_pl, plo, _cnt, _, ipl) ->
      let ps1 = if List.length plo > 0 then List.hd plo else "" in
      let ps1 = clean_ps ps1 in
      let ps2 = if List.length plo > 1 then List.hd (List.tl plo) else ps1 in
      let (k3, k4) = get_k3 plo in
      let ipl = List.flatten ipl |> List.sort_uniq compare in
      Wserver.printf
        "%s<a href=\"%sm=PS%s%s%s%s\" title=\"%s\">%s</a>"
        (if not first then ", \n" else "") (commd conf) opt
        ("&k1=" ^ Util.code_varenv ps1)
        (if k1 = "" then ""
         else if List.length plo > 1
          then "&k2=" ^ Util.code_varenv ps2 else "")
        (if not long then "&long=on" else "") title
        (if k1 = "" then ps1 else ps2) ;
      if link_to_ind && List.length ipl < max_rlm_nbr then
        begin
        Wserver.printf " (<a href=\"%sm=L%s%s%s%s%s&nb=%d&nbs=3" (commd conf)
          ("&k1=" ^ (Util.code_varenv ps1))
          (if k1 = "" then ""
           else "&k2=" ^ (Util.code_varenv ps2))
           (if k3 <> "" then "&k3=" ^ k3 else "")
           (if k4 <> "" then "&k4=" ^ k4 else "")
          opt (List.length ipl) ;
        List.iteri (fun i ip ->
          Wserver.printf "&i%d=%d" i (Adef.int_of_iper ip))
        ipl ;
        Wserver.printf "\" title=\"%s\">%d</a>)"
          (capitale (transl conf "summary book ascendants")) (List.length ipl)
        end
      else
        Wserver.printf " (%d)" (List.length ipl);
      )
    new_list

let print_searchl conf searchl =
  match p_getenv conf.env "search" with
    | Some "on" ->
        let searchl = List.sort_uniq compare searchl in
        let opt = get_opt conf in
        let print_pl pl =
          Wserver.printf "<li><a href=\"%sm=PS%s&k1=%s\">%s</a>"
            (commd conf) opt (List.hd pl) (List.hd pl) ;
          if List.length pl > 1 then
            let k1 = List.hd pl in
            let k2 = List.hd (List.tl pl) in
            Wserver.printf " > <a href=\"%sm=PS%s&k1=%s&k2=%s\">%s</a>"
              (commd conf) opt k1 k2 k2 ;
          if List.length pl > 2 then
            List.iter (fun p -> Wserver.printf " > %s" p)
              (List.tl (List.tl pl)) ;
        in
        List.iter (fun pl -> print_pl pl; Wserver.printf "</li>") searchl
    | _ -> ()

let print_places_surnames conf base array long searchl=
  let rec sort_place_utf8 pl1 pl2 =
    match pl1, pl2 with
    | _, [] -> 1
    | [], _ -> -1
    | s1 :: pl11, s2 :: pl22 ->
        match Gutil.alphabetic_order s1 s2 with
        | 0 -> sort_place_utf8 pl11 pl22
        | x -> x
  in
  Array.sort (fun (pl1, _) (pl2, _) -> sort_place_utf8 pl1 pl2) array ;
  let title _ =
    Wserver.printf "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  let k2_no = Array.for_all (fun (pl, _) -> pl = []) array in
  Hutil.header conf title ;
  Hutil.print_link_to_welcome conf true ;
  Hutil.interp_no_header conf "buttons_places"
    {Templ.eval_var = (fun _ -> raise Not_found);
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = (fun _ -> raise Not_found)}
    [] () ;
  if array <> [||] then
    if long || k2_no
    then print_html_places_surnames_long conf base array
    else print_html_places_surnames_short conf base array;
  if searchl <> [] then
    begin
      let k1 = match p_getenv conf.env "k1" with | Some s -> s | _ -> "" in
      let k2 = match p_getenv conf.env "k2" with | Some s -> s | _ -> "" in
      let opt = get_opt conf in
      let substr = p_getenv conf.env "substr" = Some "on" in
      let exact = p_getenv conf.env "exact" = Some "on" in
      let search =
        match p_getenv conf.env "search" with
        | Some "on" -> ""
        | _ -> "&search=on"
      in
      let search_on = p_getenv conf.env "search" = Some "on" in
      Wserver.printf "<hr>";
      if not search_on then
        Wserver.printf "<a href=\"%sm=PS%s%s%s%s%s%s\">%s %s “%s”</a>\n"
          (commd conf)
          (if k1 <> "" then "&k1=" ^ k1 else "")
          (if k2 <> "" then "&k2=" ^ k2 else "") opt
          (if substr then "&substr=on" else "")
          (if exact then "&exact=on" else "") search
          (capitale (transl_nth conf "visualize/show/hide/summary" 1))
          (transl conf "search results") k1
      else
        Wserver.printf "%s “%s”%s\n" (capitale (transl conf "search results"))
          k1 (transl conf ":");
      Wserver.printf "<ul class=\"list-unstyled my-2\">";
      print_searchl conf searchl;
      Wserver.printf "</ul>";
    end ;
  Hutil.trailer conf

let str_replace str c1 c2 =
  let bstr = Bytes.of_string str in
  let rec loop bstr i =
    if i < (Bytes.length bstr - 1) then
      begin
      if Bytes.get bstr i = c1 then Bytes.set bstr i c2;
      loop bstr (i+1)
      end
  in loop bstr 0;
  Bytes.to_string bstr

let match_place str1 str2 exact substr =
  match (str1, str2) with
  | ("", "") -> true
  | (s1, s2) ->
      let s1 = if exact then s1 else Name.lower (Some.name_unaccent s1) in
      let s1 = if exact then s1 else str_replace s1 '-' ' ' in
      let s2 = if exact then s2 else Name.lower (Some.name_unaccent s2) in
      let s2 = if exact then s2 else str_replace s2 '-' ' ' in
      if not substr then s1 = s2
      else if s2 = "" then false else Mutil.contains s1 s2

let filter_array array place i exact substr =
  if place <> "" then
    Array.of_list (Array.fold_left
      (fun acc (pl, snl) ->
        if i = 0 then
          if match_place (if List.length pl > 0 then List.hd pl else "")
           place exact substr
          then (pl, snl) :: acc else acc
        else
          if match_place
            (if List.length pl > 1 then (List.hd (List.tl pl)) else "")
            place exact substr
          then (pl, snl) :: acc else acc) [] array)
  else array

let search_array array k exact =
  if k <> "" then
    let k = if exact then k else Name.lower (Some.name_unaccent k) in
    let k = if exact then k else str_replace k '-' ' ' in
    let is_in_list k l =
      List.exists (fun p ->
        let p = if exact then p else (Name.lower (Some.name_unaccent p)) in
        let p = if exact then p else str_replace p '-' ' ' in
        Mutil.contains p k ) l
    in
    Array.fold_left
      (fun acc (pl, _) -> if is_in_list k pl then pl :: acc else acc) [] array
  else []

let print_places_surnames_some conf base array =
  let k1 = match p_getenv conf.env "k1" with | Some s -> s | _ -> "" in
  let k2 = match p_getenv conf.env "k2" with | Some s -> s | _ -> "" in
  let exact = p_getenv conf.env "exact" = Some "on" in
  let substr = p_getenv conf.env "substr" = Some "on" in
  let searchl = search_array array k1 exact in
  let array =
    if k1 <> "" then filter_array array k1 0 exact substr else array
  in
  let array =
    if k2 <> "" then filter_array array k2 1 exact substr else array
  in
  let long = p_getenv conf.env "long" = Some "on" in
  let k1 = p_getenv conf.env "k1" <> Some "" in
  let k2 =
    match p_getenv conf.env "k2" with
    | Some "" -> false
    | Some _ -> true
    | _ -> false
  in
  print_places_surnames conf base array
    (long || (k1 && k2)) searchl

let print_all_places_surnames conf base =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_baptism = p_getenv conf.env "bp" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let add_burial = p_getenv conf.env "bu" = Some "on" in
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes"
    with Not_found -> false
  in
  let array =
    get_all conf base ~add_birth ~add_baptism ~add_death ~add_burial
      [] [] (fold_place_long inverted) (fun _ -> true)
      (fun prev p ->
         let value = (get_surname p, get_key_index p) in
         match prev with Some list -> value :: list | None -> [ value ])
      (fun v ->
         let v = List.sort (fun (a, _) (b, _) -> compare a b) v in
         let rec loop acc list = match list, acc with
           | [], _ -> acc
           | (sn, iper) :: tl_list,
              (sn', iper_list) :: tl_acc when (sou base sn) = sn' ->
             loop ((sn', iper:: iper_list) :: tl_acc) tl_list
           | (sn, iper) :: tl_list, _ ->
             loop ((sou base sn, [iper]) :: acc) tl_list
         in
         loop [] v)
  in
  print_places_surnames_some conf base array


