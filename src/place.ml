(* camlp4r ./pa_html.cmo *)
(* $Id: place.ml,v 3.5 2000-04-02 12:50:02 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value fold_place inverted s =
  let rec loop list i ibeg =
    if i == String.length s then
      if i > ibeg then [String.sub s ibeg (i - ibeg) :: list] else list
    else if
      String.unsafe_get s i == '(' &&
      String.unsafe_get s (String.length s - 1) == ')'
    then
      let list =
        let j =
          loop i where rec loop i =
            if i > 0 && String.unsafe_get s (i - 1) == ' ' then loop (i - 1)
            else i
        in
        if j > ibeg then [String.sub s ibeg (j - ibeg) :: list] else list
      in
      let i =
        loop (i + 1) where rec loop i =
          if i < String.length s && String.unsafe_get s i == ' ' then
            loop (i + 1)
          else i
      in
      [String.sub s i (String.length s - i - 1) :: list]
    else
      let (list, ibeg) =
        match String.unsafe_get s i with
        [ ',' ->
            let list =
              if i > ibeg then [String.sub s ibeg (i - ibeg) :: list] else list
            in
            (list, i + 1)
        | ' ' -> if i = ibeg then (list, i + 1) else (list, ibeg)
        | _ -> (list, ibeg) ]
      in
      loop list (i + 1) ibeg
  in
  let list = loop [] 0 0 in
  if inverted then List.rev list else list
;

value get_all conf base =
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let inverted =
    try List.assoc "places_inverted" conf.base_env = "yes" with
    [ Not_found -> False ]
  in
  let ini = match p_getenv conf.env "k" with [ Some s -> s | None -> "" ] in
  let ht = Hashtbl.create 5003 in
  let ht_add istr p =
    let (cnt, _) =
      try Hashtbl.find ht (istr, p.surname) with
      [ Not_found ->
          let cnt = (ref 0, p.cle_index) in
          do Hashtbl.add ht (istr, p.surname) cnt; return cnt ]
    in
    incr cnt
  in
  let empty = base.func.index_of_string "" in
  loop 0 where rec loop i =
    if i = base.data.persons.len then
      let list = ref [] in
      let len = ref 0 in
      do Hashtbl.iter
           (fun (istr_pl, _) (cnt, ip) ->
              let s = fold_place inverted (sou base istr_pl) in
              if ini = "" || List.hd s = ini then
                do list.val := [(s, cnt.val, ip) :: list.val];
                   incr len;
                return ()
              else ())
           ht;
      return
      let list = Sort.list (fun (s1, _, _) (s2, _, _) -> s1 <= s2) list.val in
      (list, len.val)
    else
      let p = base.data.persons.get i in
      let pl_bi = if add_birth then p.birth_place else empty in
      let pl_de = if add_death then p.death_place else empty in
      do if pl_bi == empty && pl_de == empty || not (fast_auth_age conf p)
         then ()
         else
           do if pl_bi != empty then ht_add pl_bi p else ();
              if pl_de != empty then ht_add pl_de p else ();
           return ();
      return loop (i + 1)
;

value max_len = ref 2000;

value print_html_places_surnames conf base =
  do Wserver.wprint "<ul>\n"; return
  loop [] where rec loop prev =
    fun
    [ [(pl, snl) :: list] ->
        do loop prev pl where rec loop prev pl =
             match (prev, pl) with
             [ ([], [x2 :: l2]) ->
                 do Wserver.wprint "<li>%s\n" x2;
                    List.iter (fun p -> Wserver.wprint "<ul>\n<li>%s\n" p) l2;
                 return ()
             | ([x1], [x2 :: l2]) ->
                 do if x1 = x2 then ()
                    else Wserver.wprint "<li>%s\n" x2;
                    List.iter (fun p -> Wserver.wprint "<ul>\n<li>%s\n" p) l2;
                 return ()
             | ([x1 :: l1], [x2 :: l2]) ->
                 if x1 = x2 then loop l1 l2
                 else
                   do List.iter (fun _ -> Wserver.wprint "</ul>\n") l1;
                      Wserver.wprint "<li>%s\n" x2;
                      List.iter (fun p -> Wserver.wprint "<ul>\n<li>%s\n" p)
                        l2;
                   return ()
             | _ -> assert False ];
           Wserver.wprint "<ul>\n<li>\n";
           let snl =
             List.map
               (fun (len, ip) ->
                  let p = poi base ip in
                  let sn = sou base p.surname in
                  (len, p, sn))
               snl
           in
           let snl =
             Sort.list
               (fun (_, _, sn1) (_, _, sn2) ->
                  Iobase.name_key sn1 <= Iobase.name_key sn2)
               snl
           in
           List.iter
             (fun (len, p, sn) ->
                do Wserver.wprint "<a href=\"%s" (commd conf);
                   Wserver.wprint "%s" (acces conf base p);
                   Wserver.wprint "\">%s</a> (%d),\n" sn len;
                return ())
             snl;
           Wserver.wprint "</ul>\n";
        return loop pl list
    | [] -> List.iter (fun _ -> Wserver.wprint "</ul>\n") prev ]
;

value print_all_places_surnames_short conf list =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "place")) in
  let list =
    List.fold_left
      (fun list (pl, len, ip) ->
         let p = List.hd pl in
         match list with
         [ [(p1, len1, ip1) :: list1] when p1 = p ->
             [(p1, len1 + len, ip1) :: list1]
         | _ -> [(p, len, ip) :: list] ])
      [] (List.rev list)
  in
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  let opt =
    (if add_birth then ";bi=on" else "") ^
    (if add_death then ";de=on" else "")
  in
  do Util.header conf title;
     print_link_to_welcome conf True;
     stag "a" "href=\"%sm=PS%s;k=\"" (commd conf) opt begin
       Wserver.wprint "%s" (transl conf "long display");
     end;
     Wserver.wprint "\n<p>\n";
     List.iter
       (fun (s, len, ip) ->
          Wserver.wprint "<a href=\"%sm=PS%s;k=%s\">%s</a> (%d),\n"
            (commd conf) opt (Util.code_varenv s) s len)
       list;
     Util.trailer conf;
  return ()
;

value print_all_places_surnames_long conf base list =
  let list =
    List.fold_left
      (fun list (pl, len, ip) ->
         match list with
         [ [(pl1, lpl1) :: list1] when pl = pl1 ->
             [(pl1, [(len, ip) :: lpl1]) :: list1]
         | _ -> [(pl, [(len, ip)]) :: list] ])
      [] list
  in
  let list = Sort.list (fun (pl1, _) (pl2, _) -> pl1 <= pl2) list in
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  do Util.header conf title;
     print_link_to_welcome conf True;
     if list = [] then () else print_html_places_surnames conf base list;
     Util.trailer conf;
  return ()
;

value print_all_places_surnames conf base =
  let ini = p_getenv conf.env "k" in
  let (list, len) = get_all conf base in
  if ini = None && len > max_len.val then
    print_all_places_surnames_short conf list
  else
    print_all_places_surnames_long conf base list
;
