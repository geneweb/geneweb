(* camlp4r ./pa_html.cmo *)
(* $Id: place.ml,v 3.3 2000-03-31 15:13:35 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value fold_place s =
  loop [] 0 0 where rec loop list i ibeg =
    if i == String.length s then
      if i > ibeg then [String.sub s ibeg (i - ibeg) :: list] else list
    else if
      String.unsafe_get s i == '(' &&
      String.unsafe_get s (String.length s - 1) == ')'
    then
      let list =
        let j =
          if i > 0 && String.unsafe_get s (i - 1) == ' ' then i - 1 else i
        in
        if j > ibeg then [String.sub s ibeg (j - ibeg) :: list] else list
      in
      [String.sub s (i + 1) (String.length s - i - 2) :: list]
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
;

value get_list conf base =
(*
  let _ = base.data.persons.array () in
  let _ = base.data.strings.array () in
*)
  let add_birth = p_getenv conf.env "bi" = Some "on" in
  let add_death = p_getenv conf.env "de" = Some "on" in
  loop [] 0 where rec loop list i =
    if i = base.data.persons.len then list
    else
      let p = base.data.persons.get i in
      let s = sou base p.surname in
      let pl_bi =
        if add_birth then fold_place (sou base p.birth_place) else []
      in
      let pl_de =
        if add_death then fold_place (sou base p.death_place) else []
      in
      let list =
        if (pl_bi = [] && pl_de = []) || not (age_autorise conf base p) then
          list
        else
          let list = if pl_bi = [] then list else [(pl_bi, s, i) :: list] in
          let list =
            if pl_de = [] || pl_de = pl_bi then list
            else [(pl_de, s, i) :: list]
          in
          list
      in 
      loop list (i + 1)
;

value sort_by_place =
  Sort.list
    (fun (pl1, sn1, _) (pl2, sn2, _) ->
       if pl1 < pl2 then True
       else if pl1 > pl2 then False
       else sn1 <= sn2)
;

value uniq_place =
  fun
  [ [] -> []
  | [(pl, sn, i) :: l] ->
      loop [] [] pl sn i 1 l where rec loop list snl pl sn i n =
        fun
        [ [(pl2, sn2, i2) :: l] ->
            if pl2 = pl then
              if sn2 = sn then loop list snl pl sn i2 (n + 1) l
              else loop list [(sn, n, i) :: snl] pl sn2 i2 1 l
            else
              loop [(pl, List.rev [(sn, n, i) :: snl]) :: list] [] pl2 sn2 i2
                1 l
       | [] -> List.rev [(pl, List.rev [(sn, n, i) :: snl]) :: list] ] ]
;

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
             | _ -> failwith "bizarre" ];
           Wserver.wprint "<ul>\n<li>\n";
           List.iter
             (fun (sn, n, i) ->
                do Wserver.wprint "<a href=\"%s" (commd conf);
                   Wserver.wprint "%s"
                     (acces conf base (base.data.persons.get i));
                   Wserver.wprint "\">%s</a> (%d),\n" sn n;
                return ())
             snl;
           Wserver.wprint "</ul>\n";
        return loop pl list
    | [] -> List.iter (fun _ -> Wserver.wprint "</ul>\n") prev ]
;

value print_all_places_surnames conf base =
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "place"))
      (capitale (transl_nth conf "surname/surnames" 0))
  in
  let list = get_list conf base in
  let list = sort_by_place list in
  let list = uniq_place list in
  do Util.header conf title;
     print_link_to_welcome conf True;
     if list = [] then () else print_html_places_surnames conf base list;
     Util.trailer conf;
  return ()
;
