(* $Id: dag.ml,v 3.43 2001-01-26 02:18:44 ddr Exp $ *)

open Dag2html;
open Def;
open Config;
open Gutil;
open Util;
open Printf;

module Pset = Set.Make (struct type t = iper; value compare = compare; end);

(* testing *)

value map_dag f d =
  let a =
    Array.map (fun d -> {pare = d.pare; valu = f d.valu; chil = d.chil}) d.dag
  in
  {dag = a}
;

value tag_dag d =
  let c = ref 'A' in
  map_dag
    (fun v ->
       let v = c.val in
       do c.val :=
            if c.val = 'Z' then 'a'
            else if c.val = 'z' then '1'
            else Char.chr (Char.code c.val + 1);
       return v)
    d
;

(* input dag *)

value get_dag_elems conf base =
  loop None Pset.empty 1 where rec loop prev_po set i =
    let s = string_of_int i in
    let po = Util.find_person_in_env conf base s in
    let po = match po with [ None -> prev_po | x -> x ] in
    let so = Util.p_getenv conf.env ("s" ^ s) in
    match (po, so) with
    [ (Some p, Some s) ->
        let set =
          match Util.branch_of_sosa base p.cle_index (Num.of_string s) with
          [ Some ipsl ->
              List.fold_left (fun set (ip, _) -> Pset.add ip set) set ipsl
          | None -> set ]
        in
        loop po set (i + 1)
    | _ -> set ]
;

type sum 'a 'b = [ Left of 'a | Right of 'b ];

value make_dag conf base list =
  let module O = struct type t = iper; value compare = compare; end in
  let module M = Map.Make O in
  let nodes = Array.of_list list in
  let map =
    loop M.empty 0 where rec loop map i =
      if i = Array.length nodes then map
      else loop (M.add nodes.(i) (idag_of_int i) map) (i + 1)
  in
  let nodes =
    Array.map
      (fun ip ->
(*
do Printf.eprintf "\no %s\n" (denomination base (poi base ip)); flush stderr; return
*)
(*
do let p = poi base ip in Printf.eprintf "\no %s%s\n" (Util.person_title_text conf base p) (Date.short_dates_text conf base p); flush stderr; return
*)
         let pare =
           match (aoi base ip).parents with
           [ Some ifam ->
               let c = coi base ifam in
               let l = try [M.find c.mother map] with [ Not_found -> [] ] in
               try [M.find c.father map :: l] with [ Not_found -> l ]
           | None -> [] ]
         in
(*
do Printf.eprintf "parents\n"; flush stderr; return
do List.iter (fun id -> Printf.eprintf "- %s\n" (denomination base (poi base nodes.(int_of_idag id)))) pare; flush stderr; return
*)
         let chil =
           let u = uoi base ip in
           Array.fold_left
             (fun chil ifam ->
                let des = doi base ifam in
                Array.fold_left
                  (fun chil ip ->
                     try [M.find ip map :: chil] with [ Not_found -> chil ])
                  chil des.children)
             [] u.family
         in
         let chil = List.rev chil in
(*
do Printf.eprintf "children\n"; flush stderr; return
do List.iter (fun id -> Printf.eprintf "- %s\n" (denomination base (poi base nodes.(int_of_idag id)))) chil; flush stderr; return
*)
(*
do List.iter (fun id -> let p = poi base nodes.(int_of_idag id) in Printf.eprintf "- %s%s\n" (Util.person_title_text conf base p) (Date.short_dates_text conf base p)) chil; flush stderr; return
*)
         {pare = pare; valu = Left ip; chil = chil})
      nodes
  in
  {dag = nodes}
;

value image_normal_txt conf base p fname width height =
  let image_txt = capitale (transl_nth conf "image/images" 0) in
  let s = Unix.stat fname in
  let b = acces conf base p in
  let k = default_image_name base p in
  sprintf "<a href=\"%sm=IM;%s;k=/%s\">" (commd conf) b k ^
  sprintf "\
<img src=\"%sm=IM;d=%d;%s;k=/%s\" width=%d height=%d border=0 alt=\"%s\">"
        (commd conf)
        (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
        b k width height image_txt ^
  "</a>"
;

value image_url_txt conf base url height =
  let image_txt = capitale (transl_nth conf "image/images" 0) in
  sprintf "<a href=\"%s\">" url ^
  sprintf "<img src=\"%s\"\nheight=%d border=0 alt=\"%s\">" url
    height image_txt ^
  "</a>\n"
;

value image_txt conf base p =
  match p_getenv conf.env "image" with
  [ Some "on" ->
      match image_and_size conf base p (limited_image_size 100 75) with
      [ Some (f, Some (Some (wid, hei))) ->
          "<br>\n<center><table border=0><tr><td>\n" ^
          image_normal_txt conf base p f wid hei ^
          "</table></center>\n"
      | Some (url, None) ->
          "<br>\n<center><table border=0><tr><td>\n" ^
          image_url_txt conf base url 75 ^
          "</table></center>\n"
      | _ -> "" ]
  | _ -> "" ]
;

(* Print with HTML table tags: <table> <tr> <td> *)

value print_table conf hts =
  do Wserver.wprint "<center><table border=%d" conf.border;
     Wserver.wprint " cellspacing=0 cellpadding=0>\n";
     for i = 0 to Array.length hts - 1 do
       Wserver.wprint "<tr>\n";
       for j = 0 to Array.length hts.(i) - 1 do
         let (colspan, align, td) = hts.(i).(j) in
         do Wserver.wprint "<td";
(*
            if colspan > 1 then Wserver.wprint " colspan=%d" colspan else ();
*)
if colspan=1 && (td=TDstring "&nbsp;" || td=TDhr CenterA) then ()
else Wserver.wprint " colspan=%d" colspan;
(**)
            match (align, td) with
            [ (LeftA, TDhr LeftA) -> Wserver.wprint " align=left"
            | (LeftA, _) -> ()
            | (CenterA, _) -> Wserver.wprint " align=center"
            | (RightA, _) -> Wserver.wprint " align=right" ];
            Wserver.wprint ">";
            match td with
            [ TDstring s -> Wserver.wprint "%s" s
            | TDhr align ->
                do Wserver.wprint "<hr noshade size=1";
                   match align with
                   [ LeftA -> Wserver.wprint " width=\"50%%\" align=left"
                   | RightA -> Wserver.wprint " width=\"50%%\" align=right"
                   | _ -> () ];
                   Wserver.wprint ">";
                return () ];
            Wserver.wprint "</td>\n";
         return ();
       done;
     done;
     Wserver.wprint "</table></center>\n";
  return () 
;

(*
 * Print without HTML table tags: using <pre>
 *)

(* Machinery opering to 'displayed texts', i.e. strings where not all
   characters correspond to a displayed character (due to html tags or
   encoded characters) *)

(* Return next 'displayed character' location: can be on several 'string
   characters', like "&nbsp;" *)

value displayed_next_char s i =
  loop i where rec loop i =
    if i >= String.length s then None
    else
      match s.[i] with
      [ '<' ->
          loop1 (i + 1) where rec loop1 i =
            if i = String.length s then None
            else if s.[i] = '>' then loop (i + 1)
            else loop1 (i + 1)
      | '&' ->
          loop1 (i + 1) where rec loop1 j =
            if j = String.length s then Some (i, j)
            else
              match s.[j] with
              [ 'a'..'z' | 'A'..'Z' -> loop1 (j + 1)
              | ';' -> Some (i, j + 1)
              | _ -> Some (i, j) ]
      | _ -> Some (i, i + 1) ]
;

value buff_store_int s blen i j =
  loop blen i where rec loop blen i =
    if i = j then blen else loop (Buff.store blen s.[i]) (i + 1)
;

(* Remove empty tags, i.e. <a href=..></a> enclosing empty text, from s *)

value strip_empty_tags s =
  loop 0 None 0 where rec loop blen opened_tag i =
    if i >= String.length s then Buff.get blen
    else
      match s.[i] with
      [ '<' ->
          let j = i + 1 in
          let (tag_close, j) =
            match s.[j] with
            [ '/' -> (True, j + 1)
            | _ -> (False, j) ]
          in
          let (tag_name, j) =
            loop j where rec loop k =
              match s.[k] with
              [ 'a'..'z' | 'A'..'Z' -> loop (k + 1)
              | _ -> (String.sub s j (k - j), k) ]
          in
          let j =
            loop j where rec loop j =
              if s.[j] = '>' then j + 1
              else loop (j + 1)
          in
          match opened_tag with
          [ Some (opened_tag_name, k) ->
              if tag_close then
                if tag_name = opened_tag_name then loop blen None j
                else loop (buff_store_int s blen k j) None j
              else
                loop (buff_store_int s blen k i) (Some (tag_name, i)) j
          | None ->
              if tag_close then
                loop (buff_store_int s blen i j) None j
              else
                loop blen (Some (tag_name, i)) j ]
      | c ->
          let blen =
            match opened_tag with
            [ Some (_, k) -> buff_store_int s blen k i
            | None -> blen ]
          in
          loop (Buff.store blen c) None (i + 1) ]
;

value displayed_length s =
  loop 0 0 where rec loop len i =
    match displayed_next_char s i with
    [ Some (i, j) -> loop (len + 1) j
    | None -> len ]
;

value displayed_sub s ibeg ilen =
  loop 0 0 0 0 where rec loop blen di dlen i =
    match displayed_next_char s i with
    [ Some (j, k) ->
        let blen = buff_store_int s blen i j in
        let (blen, dlen) =
          if di >= ibeg && dlen < ilen then
            (buff_store_int s blen j k, dlen + 1)
          else (blen, dlen)
        in
        loop blen (di + 1) dlen k
   | None ->
        let s = Buff.get (buff_store_int s blen i (String.length s)) in
        strip_empty_tags s ]
;

value longuest_word_length s =
  loop 0 0 0 where rec loop maxlen len i =
    match displayed_next_char s i with
    [ Some (j, k) ->
        if s.[j] = ' ' then loop (max maxlen len) 0 k
        else loop maxlen (len + 1) k
    | None -> max maxlen len ]
;

value displayed_end_word s di i =
  loop di i where rec loop di i =
    match displayed_next_char s i with
    [ Some (j, k) -> if s.[j] = ' ' then (di, Some j) else loop (di + 1) k
    | None -> (di, None) ]
;

(* Strip 'displayed text' s by subtexts of limited size sz *)

value displayed_strip s sz =
  loop [] 0 0 0 where rec loop strl dibeg di i =
    let i =
      loop i where rec loop i =
        if i < String.length s && s.[i] = ' ' then loop (i + 1) else i
    in
    let (dj, j) = displayed_end_word s di i in
    match j with
    [ Some j ->
        if dj - dibeg > sz then
          loop [displayed_sub s dibeg (di - dibeg - 1) :: strl] di (dj + 1)
            (j + 1)
        else
          loop strl dibeg (dj + 1) (j + 1)
    | None ->
        let strl =
          if dj - dibeg > sz then
            let str2 = displayed_sub s dibeg (di - dibeg - 1) in
            let str1 = displayed_sub s di (dj - di) in
            [str1; str2 :: strl]
          else
            let str = displayed_sub s dibeg (dj - dibeg) in
            [str :: strl]
        in
        List.rev strl ]
;

(* Determine columns sizes; scan all table by increasing colspans *)

value gen_compute_columns_sizes size_fun hts ncol =
  let colsz = Array.make ncol 0 in
  do loop 1 where rec loop curr_colspan =
       let next_colspan = ref (ncol + 1) in
       do for i = 0 to Array.length hts - 1 do
            if i = Array.length hts then ()
            else
              loop 0 0 where rec loop col j =
                if j = Array.length hts.(i) then ()
                else
                  let (colspan, _, td) = hts.(i).(j) in
                  do match td with
                     [ TDstring s ->
                         if colspan = curr_colspan then
                           let len = size_fun s in
                           let currsz =
                             loop 0 col colspan
                             where rec loop currsz col cnt =
                               if cnt = 0 then currsz
                               else
                                 let currsz = currsz + colsz.(col) in
                                 loop currsz (col + 1) (cnt - 1)
                           in
                           if currsz >= len then ()
                           else
                             loop 1 col colspan where rec loop n col cnt =
                               if cnt = 0 then ()
                               else
                                 let inc_sz =
                                   n * (len - currsz) / colspan -
                                   (n - 1) * (len - currsz) / colspan
                                 in
                                 do colsz.(col) := colsz.(col) + inc_sz;
                                 return loop (n + 1) (col + 1) (cnt - 1)
                         else if colspan > curr_colspan then
                           next_colspan.val := min colspan next_colspan.val
                         else ()
                     | TDhr _ -> () ];
                  return loop (col + colspan) (j + 1);
          done;
       return
       if next_colspan.val > ncol then () else loop next_colspan.val;
  return colsz
;

value compute_columns_sizes =
  gen_compute_columns_sizes displayed_length;
value compute_columns_minimum_sizes =
  gen_compute_columns_sizes longuest_word_length;

(* Gadget to add a | to fill upper/lower part of a table data when
   preceded/followed by a |; not obligatory but nicer *)

value try_add_vbar stra_row stra_row_max hts i col =
  if stra_row < 0 then
    if i = 0 then ""
    else
      loop 0 0 where rec loop pcol pj =
        if pj >= Array.length hts.(i-1) then ""
        else
          let (colspan, _, td) = hts.(i-1).(pj) in
          if pcol = col then
            match td with
            [ TDstring "|" -> "|"
            | _ -> "" ]
          else loop (pcol + colspan) (pj + 1)
  else if stra_row >= stra_row_max then
    if i = Array.length hts - 1 then ""
    else
      loop 0 0 where rec loop ncol nj =
        if nj >= Array.length hts.(i+1) then ""
        else
          let (colspan, _, td) = hts.(i+1).(nj) in
          if ncol = col then
            match td with
            [ TDstring "|" -> "|"
            | _ -> "" ]
          else loop (ncol + colspan) (nj + 1)
  else ""
;

value strip_troublemakers s =
  loop 0 0 where rec loop len i =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
      [ '<' ->
          let j = i + 1 in
          let j =
            match s.[j] with
            [ '/' -> j + 1
            | _ -> j ]
          in
          let (tag_name, j) =
            loop j where rec loop k =
              match s.[k] with
              [ 'a'..'z' | 'A'..'Z' -> loop (k + 1)
              | _ -> (String.lowercase (String.sub s j (k - j)), k) ]
          in
          let j =
            loop j where rec loop j =
              if s.[j] = '>' then j + 1
              else loop (j + 1)
          in
          let len =
            match tag_name with
            [ "br" | "font" | "img" | "table" | "td" | "tr" | "center" -> len
            | _ -> buff_store_int s len i j ]
          in
          loop len j
      | '\n' | '\r' -> loop (Buff.store len ' ') (i + 1)
      | c -> loop (Buff.store len c) (i + 1) ]
;

value table_strip_troublemakers hts =
  for i = 0 to Array.length hts - 1 do
    for j = 0 to Array.length hts.(i) - 1 do
      match hts.(i).(j) with
      [ (colspan, align, TDstring s) ->
          hts.(i).(j) := (colspan, align, TDstring (strip_troublemakers s))
      | _ -> () ];
    done;
  done
;

value print_next_pos conf pos1 pos2 =
  match pos2 with
  [ Some pos2 ->
      let pos1 =
        match pos1 with
        [ Some pos1 -> pos1
        | None -> 0 ]
      in
      let dpos =
        match p_getint conf.env "dpos" with
        [ Some dpos -> dpos
        | None -> pos2 - pos1 - 10 ]
      in
      let env =
        List.fold_right
          (fun (k, v) env ->
             match k with
             [ "pos1" | "pos2" -> env
             | _ -> [(k, v) :: env] ])
          conf.env []
      in
      do Wserver.wprint "<div align=right>\n";
         Wserver.wprint "<a href=\"%s" (commd conf);
         List.iter (fun (k, v) -> Wserver.wprint "%s=%s;" k v) env;
         Wserver.wprint "pos1=%d;pos2=%d" (pos1 + dpos) (pos2 + dpos);
         Wserver.wprint "\">&gt;&gt;</a>\n";
         Wserver.wprint "</div>\n";
      return ()
  | None -> () ]
;

(* Main print table algorithm with <pre> *)

value print_table_pre conf hts =
  do table_strip_troublemakers hts; return
  let ncol =
    let hts0 = hts.(0) in
    loop 0 0 where rec loop ncol j =
      if j = Array.length hts0 then ncol
      else
        let (colspan, _, _) = hts0.(j) in
        loop (ncol + colspan) (j + 1)
  in
  let colsz = compute_columns_sizes hts ncol in
  let colminsz = compute_columns_minimum_sizes hts ncol in
  let tcol = Array.fold_left \+ 0 colsz in
  let tmincol = Array.fold_left \+ 0 colminsz in
  let dcol =
    let dcol =
      match p_getint conf.env "col" with
      [ Some i -> i
      | None -> 79 ]
    in
    max tmincol (min dcol tcol)
  in
(*
do for i = 0 to ncol - 1 do
     Wserver.wprint " %d(%d)" colsz.(i) colminsz.(i);
   done;
   Wserver.wprint " = %d(%d) -> %d<br>\n" tcol tmincol dcol;
return
*)
  do if tcol > tmincol then
       for i = 0 to ncol - 1 do
         colsz.(i) :=
           colminsz.(i)
           + (colsz.(i) - colminsz.(i)) * (dcol - tmincol) / (tcol - tmincol);
       done
     else ();
  return
(*
do for i = 0 to ncol - 1 do
     Wserver.wprint " %d(%d)" colsz.(i) colminsz.(i);
   done;
   let tcol = Array.fold_left \+ 0 colsz in
   Wserver.wprint " = %d(%d) -> %d<br>\n" tcol tmincol dcol;
return
*)
  let pos1 = p_getint conf.env "pos1" in
  let pos2 = p_getint conf.env "pos2" in
  do print_next_pos conf pos1 pos2;
     Wserver.wprint "<pre>\n";
     for i = 0 to Array.length hts - 1 do
       let (stra, max_row) =
         let (stral, max_row) =
           loop [] 1 0 0 where rec loop stral max_row col j =
             if j = Array.length hts.(i) then (stral, max_row)
             else
               let (colspan, _, td) = hts.(i).(j) in
               let stra =
                 match td with
                 [ TDstring s ->
                     let sz =
                       loop 0 colspan where rec loop sz k =
                         if k = 0 then sz else
                         loop (sz + colsz.(col + k - 1)) (k - 1)
                     in
                     Array.of_list (displayed_strip s sz)
                 | _ -> [| |] ]
               in
               loop [stra :: stral] (max max_row (Array.length stra))
                 (col + colspan) (j + 1)
         in
         (Array.of_list (List.rev stral), max_row)
       in
       for row = 0 to max_row - 1 do
         loop 0 0 0 where rec loop pos col j =
           if j = Array.length hts.(i) then Wserver.wprint "\n"
           else
             let (colspan, align, td) = hts.(i).(j) in
             let sz =
               loop 0 colspan where rec loop sz k =
                 if k = 0 then sz
                 else loop (sz + colsz.(col + k - 1)) (k - 1)
             in
             let outs =
               match td with
               [ TDstring s ->
                   let s =
                     let k =
                       let dk = (max_row - Array.length stra.(j)) / 2 in
                       row - dk
                     in
                     if k >= 0 && k < Array.length stra.(j) then stra.(j).(k)
                     else if s = "|" then s
                     else try_add_vbar k (Array.length stra.(j)) hts i col
                   in
                   let s = if s = "&nbsp;" then " " else s in
                   let len = displayed_length s in
                   String.make ((sz - len) / 2) ' ' ^ s ^
                   String.make (sz - (sz + len) / 2) ' '
               | TDhr LeftA ->
                   let len = (sz + 1) / 2 in
                   String.make len '-' ^ String.make (sz - len) ' '
               | TDhr RightA ->
                   let len = sz / 2 in
                   String.make (sz - len - 1) ' ' ^ String.make (len + 1) '-'
               | TDhr CenterA ->
                   String.make sz '-' ]
             in
             let clipped_outs =
               if pos1 = None && pos2 = None then outs
               else
                 let pos1 =
                   match pos1 with
                   [ Some pos1 -> pos1
                   | None -> pos ]
                 in
                 let pos2 =
                   match pos2 with
                   [ Some pos2 -> pos2
                   | None -> pos + sz ]
                 in
                 if pos + sz <= pos1 then ""
                 else if pos > pos2 then ""
                 else if pos2 >= pos + sz then
                   displayed_sub outs (pos1 - pos) (pos + sz - pos1)
                 else if pos1 < pos then
                   displayed_sub outs 0 (pos2 - pos)
                 else
                   displayed_sub outs (pos1 - pos) (pos2 - pos1)
             in
             do Wserver.wprint "%s" clipped_outs; return
             loop (pos + sz) (col + colspan) (j + 1);
       done;
     done;
     Wserver.wprint "</pre>\n";
  return ()
;

(* main *)

value print_html_table conf hts =
  if Util.p_getenv conf.env "notab" = Some "on"
  || Util.p_getenv conf.env "pos2" <> None
  || browser_doesnt_have_tables conf then
    print_table_pre conf hts
  else
    print_table conf hts
;

value print_only_dag conf base elem_txt spouse_on invert set spl d =
  let indi_txt n =
    match n.valu with
    [ Left ip ->
        let p = poi base ip in
        let txt = elem_txt conf base p in
        let txt =
          let spouses =
            if (spouse_on && n.chil <> [] || n.pare = []) && not invert then
              List.fold_left
                (fun list id ->
                   match d.dag.(int_of_idag id).valu with
                   [ Left cip ->
                       match (aoi base cip).parents with
                       [ Some ifam ->
                           let cpl = coi base ifam in
                           if ip == cpl.father then
                             if List.mem_assoc cpl.mother list then list
                             else [(cpl.mother, Some ifam) :: list]
                           else if ip == cpl.mother then
                             if List.mem_assoc cpl.father list then list
                             else [(cpl.father, Some ifam) :: list]
                           else list
                       | None -> list ]
                   | Right _ -> list ])
                [] n.chil
            else if n.chil = [] then
              try [List.assq ip spl] with [ Not_found -> [] ]
            else []
          in
          List.fold_left
            (fun txt (ips, ifamo) ->
               if Pset.mem ips set then txt
               else
                 let ps = poi base ips in
                 let d =
                   match ifamo with
                   [ Some ifam ->
                       Date.short_marriage_date_text conf base (foi base ifam)
                         p ps
                   | None -> "" ]
                 in
                 txt ^ "<br>\n&amp;" ^ d ^ " " ^
                 Util.referenced_person_title_text conf base ps ^
                 Date.short_dates_text conf base ps)
            txt spouses
        in
        let txt = txt ^ image_txt conf base p in
        txt
    | Right _ -> "&nbsp;" ]
  in
  let bd =
    match Util.p_getint conf.env "bd" with
    [ Some x -> x
    | _ -> 0 ]
  in
  let td =
    match Util.p_getenv conf.env "td" with
    [ Some x -> " " ^ x
    | _ -> "" ]
  in
  let indi_txt n =
    let (bd, td) =
      match n.valu with
      [ Left ip -> (bd, td)
      | _ -> (0, "") ]
    in
    if bd > 0 || td <> "" then
      sprintf "<table border=%d><tr><td align=center%s>%s</table>" bd td
        (indi_txt n)
    else indi_txt n
  in
  let phony n =
    match n.valu with
    [ Left _ -> False
    | Right _ -> True ]
  in
  let hts = html_table_of_dag indi_txt phony invert d in
  print_html_table conf hts
;

value gen_print_dag conf base spouse_on invert set spl d =
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "tree"))
  in
  let elem_txt conf base p =
    Util.referenced_person_title_text conf base p ^
    Date.short_dates_text conf base p
  in
  do Util.header_no_page_title conf title;
     print_only_dag conf base elem_txt spouse_on invert set spl d;
     Util.trailer conf;
  return ()
;

value print_dag conf base set spl d =
  let spouse_on =
    match Util.p_getenv conf.env "spouse" with
    [ Some "on" -> True
    | _ -> False ]
  in
  let invert =
    match Util.p_getenv conf.env "invert" with
    [ Some "on" -> True
    | _ -> False ]
  in
  gen_print_dag conf base spouse_on invert set spl d
;

value print conf base =
  let set = get_dag_elems conf base in
  let d = make_dag conf base (Pset.elements set) in
  print_dag conf base set [] d
;
