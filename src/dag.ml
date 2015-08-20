(* camlp5r ./pa_html.cmo *)
(* $Id: dag.ml,v 5.20 2007-09-12 09:58:44 ddr Exp $ *)

DEFINE OLD;

open Config;
open Dag2html;
open Def;
open Gutil;
open Gwdb;
open Mutil;
open Printf;
open TemplAst;
open Util;

module Pset =
 struct
   type t = list iper;
   type elt = iper;
   value add e s = if List.mem e s then s else [e :: s];
   value empty = [];
   value elements s = List.rev s;
   value mem = List.mem;
 end
;

(* testing *)

IFDEF TESTING THEN declare
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
       do {
         c.val :=
           if c.val = 'Z' then 'a'
           else if c.val = 'z' then '1'
           else Char.chr (Char.code c.val + 1);
         v
       })
    d
;
end END;

(* input dag *)

value get_dag_elems conf base =
  loop None Pset.empty 1 where rec loop prev_po set i =
    let s = string_of_int i in
    let po = Util.find_person_in_env conf base s in
    let po =
      match po with
      [ None -> prev_po
      | x -> x ]
    in
    let so = Util.p_getenv conf.env ("s" ^ s) in
    match (po, so) with
    [ (Some p, Some s) ->
        let set =
          match
            Util.branch_of_sosa conf base (get_key_index p) (Num.of_string s)
          with
          [ Some ipsl ->
              List.fold_left (fun set (ip, _) -> Pset.add ip set) set ipsl
          | None -> set ]
        in
        loop po set (i + 1)
    | _ -> set ]
;

type sum 'a 'b =
  [ Left of 'a
  | Right of 'b ]
;

value make_dag conf base set =
  let list = Pset.elements set in
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
         let pare =
           match get_parents (pget conf base ip) with
           [ Some ifam ->
               let c = foi base ifam in
               let l =
                 try [M.find (get_mother c) map] with [ Not_found -> [] ]
               in
               try [M.find (get_father c) map :: l] with [ Not_found -> l ]
           | None -> [] ]
         in
         let chil =
           let u = pget conf base ip in
           Array.fold_left
             (fun chil ifam ->
                let des = foi base ifam in
                Array.fold_left
                  (fun chil ip ->
                     try [M.find ip map :: chil] with [ Not_found -> chil ])
                  chil (get_children des))
             [] (get_family u)
         in
         let chil = List.rev chil in
         {pare = pare; valu = Left ip; chil = chil})
      nodes
  in
  let nodes =
    loop nodes (Array.length nodes) 0 where rec loop nodes n i =
      if i = Array.length nodes then nodes
      else
        match nodes.(i) with
        [ {valu = Left ip; chil = chil} ->
            let ifaml = Array.to_list (get_family (pget conf base ip)) in
            let (nodes, n) =
              loop nodes ifaml where rec loop nodes =
                fun
                [ [ifam :: ifaml] ->
                    let cpl = foi base ifam in
                    let isp = spouse ip cpl in
                    let jdo =
                      try Some (M.find isp map) with
                      [ Not_found -> None ]
                    in
                    match jdo with
                    [ Some jd ->
                        let j = int_of_idag jd in
                        if chil = [] && nodes.(j).chil = [] then
                          (* married but no child in the dag *)
                          let pare = [idag_of_int i; jd] in
                          let d = {pare = pare; valu = Right n; chil = []} in
                          let nodes = Array.append nodes [| d |] in
                          let nd = idag_of_int n in
                          do {
                            nodes.(i).chil := [nd];
                            nodes.(j).chil := [nd];
                            (nodes, n + 1)
                          }
                        else if chil <> nodes.(j).chil then
                          (* married; group their children even step ones *)
                          do {
                            List.iter
                              (fun nd ->
                                 if List.mem nd nodes.(j).chil then ()
                                 else do {
                                   let n = int_of_idag nd in
                                   nodes.(j).chil := [nd :: nodes.(j).chil];
                                   nodes.(n).pare := [jd :: nodes.(n).pare];
                                 })
                              chil;
                            List.iter
                              (fun nd ->
                                 if List.mem nd chil then ()
                                 else do {
                                   let id = idag_of_int i in
                                   let n = int_of_idag nd in
                                   nodes.(i).chil := [nd :: chil];
                                   nodes.(n).pare := [id :: nodes.(n).pare];
                                 })
                              nodes.(j).chil;
                            loop nodes ifaml
                          }
                        else loop nodes ifaml
                    | None -> loop nodes ifaml ]
                | [] -> (nodes, n) ]
            in
            loop nodes n (i + 1)
        | _ -> loop nodes n (i + 1) ]
  in
  {dag = nodes}
;

value image_normal_txt conf base p fname width height =
  let image_txt = capitale (transl_nth conf "image/images" 0) in
  let s = Unix.stat fname in
  let b = acces conf base p in
  let k = default_image_name base p in
  let r =
    sprintf "\
<img src=\"%sm=IM;d=%d;%s;k=/%s\"%s%s alt=\"%s\" title=\"%s\" style=\"%s %s\" />"
      (commd conf)
      (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int))) b k
      (if width = 0 then "" else " width=\"" ^ string_of_int width ^ "\"")
      (if height = 0 then "" else " height=\"" ^ string_of_int height ^ "\"")
      image_txt image_txt
      (if width = 0 then "" else " max-width:" ^ string_of_int width ^ "px;")
      (if height = 0 then "" else " max-height:" ^ string_of_int height ^ "px;")
  in
  if conf.cancel_links then r
  else sprintf "<a href=\"%sm=IM;%s;k=/%s\">" (commd conf) b k ^ r ^ "</a>"
;

value image_url_txt conf base url_p url height =
  let image_txt = capitale (transl_nth conf "image/images" 0) in
  sprintf "<a href=\"%s\">" url_p ^
    sprintf "<img src=\"%s\"\nheight=%d alt=\"%s\" title=\"%s\" style=\"%s\" />"
      url height image_txt image_txt
      (if height = 0 then "" else " max-height:" ^ string_of_int height ^ "px;") ^
    "</a>\n"
;

value image_url_txt_with_size conf base url_p url width height =
  let image_txt = capitale (transl_nth conf "image/images" 0) in
  sprintf "<a href=\"%s\">" url_p ^
    sprintf "<img src=\"%s\"\nwidth=%d height=\"%d\" alt=\"%s\" title=\"%s\" style=\"%s %s\" />"
      url width height image_txt image_txt
      (if width = 0 then "" else " max-width:" ^ string_of_int width ^ "px;")
      (if height = 0 then "" else " max-height:" ^ string_of_int height ^ "px;") ^
    "</a>\n"
;

value image_txt conf base p =
  match p_getenv conf.env "image" with
  [ Some "on" ->
      match image_and_size conf base p (limited_image_size 100 75) with
      [ Some (True, f, Some (wid, hei)) ->
          "<br" ^ conf.xhs ^
           ">\n<center><table border=\"0\"><tr align=\"left\"><td>\n" ^
            image_normal_txt conf base p f wid hei ^ "</td></tr></table></center>\n"
      | Some (True, f, None) ->
          "<br" ^ conf.xhs ^
          ">\n<center><table border=\"0\"><tr align=\"left\"><td>\n" ^
            image_normal_txt conf base p f 0 75 ^ "</td></tr></table></center>\n"
      | Some (False, url, Some (wid, hei)) ->
          let url_p = (commd conf) ^ (acces conf base p) in
          "<br" ^ conf.xhs ^
          ">\n<center><table border=\"0\"><tr align=\"left\"><td>\n" ^
            image_url_txt_with_size conf base url_p url wid hei ^
            "</td></tr></table></center>\n"
      | Some (False, url, None) ->
          let url_p = (commd conf) ^ (acces conf base p) in
          "<br" ^ conf.xhs ^
          ">\n<center><table border=\"0\"><tr align=\"left\"><td>\n" ^
            image_url_txt conf base url_p url 75 ^ "</td></tr></table></center>\n"
      | _ -> "" ]
  | _ -> "" ]
;

(* *)

type item = [ Item of person and string ];

value string_of_item conf base =
  fun
  [ Item p s ->
      Util.referenced_person_title_text conf base p ^
        Date.short_dates_text conf base p ^
      (if s = "" then "" else " " ^ s) ]
;

(* Print with HTML table tags: <table> <tr> <td> *)

IFDEF OLD THEN declare
value print_table conf hts =
  do {
    begin_centered conf;
    Wserver.wprint "<table border=\"%d\"" conf.border;
    Wserver.wprint " cellspacing=\"0\" cellpadding=\"0\">\n";
    for i = 0 to Array.length hts - 1 do {
      tag "tr" "align=\"left\"" begin
        for j = 0 to Array.length hts.(i) - 1 do {
          let (colspan, align, td) = hts.(i).(j) in
          Wserver.wprint "<td";
          if colspan = 1 && (td = TDnothing || td = TDhr CenterA) then
            ()
          else Wserver.wprint " colspan=\"%d\"" colspan;
          match (align, td) with
          [ (LeftA, TDhr LeftA) -> Wserver.wprint " align=\"%s\"" conf.left
          | (LeftA, _) -> ()
          | (CenterA, _) -> Wserver.wprint " align=\"center\""
          | (RightA, _) -> Wserver.wprint " align=\"%s\"" conf.right ];
          Wserver.wprint ">";
          match td with
          [ TDitem s -> Wserver.wprint "%s" s
          | TDtext s -> Wserver.wprint "%s" s
          | TDnothing -> Wserver.wprint "&nbsp;"
          | TDbar None -> Wserver.wprint "|"
          | TDbar (Some s) ->
              Wserver.wprint
                "<a style=\"text-decoration:none\" href=\"%s\">|</a>" s
          | TDhr align ->
              match align with
              [ LeftA ->
                  xtag "hr" "class=\"%s\"" conf.left
              | RightA ->
                  xtag "hr" "class=\"%s\"" conf.right
              | _ ->
                  xtag "hr" "class=\"full\"" ] ];
          Wserver.wprint "</td>\n"
        };
      end;
    };
    Wserver.wprint "</table>\n";
    end_centered conf;
  }
;
end END;

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
          let rec loop1 i =
            if i = String.length s then None
            else if s.[i] = '>' then loop (i + 1)
            else loop1 (i + 1)
          in
          loop1 (i + 1)
      | '&' ->
          let rec loop1 j =
            if j = String.length s then Some (i, j)
            else
              match s.[j] with
              [ 'a'..'z' | 'A'..'Z' -> loop1 (j + 1)
              | ';' -> Some (i, j + 1)
              | _ -> Some (i, j) ]
          in
          loop1 (i + 1)
      | c ->
          if utf_8_db.val then Some (i, i + max 1 (Name.nbc c))
          else Some (i, i + 1) ]
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
              if s.[j] = '>' then j + 1 else loop (j + 1)
          in
          match opened_tag with
          [ Some (opened_tag_name, k) ->
              if tag_close then
                if tag_name = opened_tag_name then loop blen None j
                else loop (buff_store_int s blen k j) None j
              else loop (buff_store_int s blen k i) (Some (tag_name, i)) j
          | None ->
              if tag_close then loop (buff_store_int s blen i j) None j
              else loop blen (Some (tag_name, i)) j ]
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
        else loop strl dibeg (dj + 1) (j + 1)
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
  let rec loop curr_colspan =
    let next_colspan = ref (ncol + 1) in
    do {
      for i = 0 to Array.length hts - 1 do {
        if i = Array.length hts then ()
        else
          let rec loop col j =
            if j = Array.length hts.(i) then ()
            else do {
              let (colspan, _, td) = hts.(i).(j) in
              match td with
              [ TDitem _ | TDtext _ | TDnothing ->
                  if colspan = curr_colspan then
                    let len =
                      match td with
                      [ TDitem s -> size_fun s
                      | TDtext s -> size_fun s
                      | _ -> 1 ]
                    in
                    let currsz =
                      loop 0 col colspan where rec loop currsz col cnt =
                        if cnt = 0 then currsz
                        else
                          let currsz = currsz + colsz.(col) in
                          loop currsz (col + 1) (cnt - 1)
                    in
                    if currsz >= len then ()
                    else
                      let rec loop n col cnt =
                        if cnt = 0 then ()
                        else do {
                          let inc_sz =
                            n * (len - currsz) / colspan -
                              (n - 1) * (len - currsz) / colspan
                          in
                          colsz.(col) := colsz.(col) + inc_sz;
                          loop (n + 1) (col + 1) (cnt - 1)
                        }
                      in
                      loop 1 col colspan
                  else if colspan > curr_colspan then
                    next_colspan.val := min colspan next_colspan.val
                  else ()
              | TDbar _ -> ()
              | TDhr _ -> () ];
              loop (col + colspan) (j + 1)
            }
          in
          loop 0 0
      };
      if next_colspan.val > ncol then () else loop next_colspan.val
    }
  in
  do { loop 1; colsz }
;

value compute_columns_sizes = gen_compute_columns_sizes displayed_length;
value compute_columns_minimum_sizes =
  gen_compute_columns_sizes longuest_word_length
;

(* Gadget to add a | to fill upper/lower part of a table data when
   preceded/followed by a |; not obligatory but nicer *)

value try_add_vbar stra_row stra_row_max hts i col =
  if stra_row < 0 then
    if i = 0 then ""
    else
      let rec loop pcol pj =
        if pj >= Array.length hts.(i - 1) then ""
        else
          let (colspan, _, td) = hts.(i - 1).(pj) in
          if pcol = col then
            match td with
            [ TDbar _ -> "|"
            | _ -> "" ]
          else loop (pcol + colspan) (pj + 1)
      in
      loop 0 0
  else if stra_row >= stra_row_max then
    if i = Array.length hts - 1 then ""
    else
      let rec loop ncol nj =
        if nj >= Array.length hts.(i + 1) then ""
        else
          let (colspan, _, td) = hts.(i + 1).(nj) in
          if ncol = col then
            match td with
            [ TDbar _ -> "|"
            | _ -> "" ]
          else loop (ncol + colspan) (nj + 1)
      in
      loop 0 0
  else ""
;

value strip_troublemakers s =
  loop False 0 0 where rec loop last_space len i =
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
              if s.[j] = '>' then j + 1 else loop (j + 1)
          in
          let len =
            match tag_name with
            [ "bdo" | "br" | "font" | "img" | "span" | "table" | "td" | "tr"
            | "center" -> len
            | _ -> buff_store_int s len i j ]
          in
          loop last_space len j
      | '\n' | '\r' | ' ' ->
          let len = if last_space then len else Buff.store len ' ' in
          loop True len (i + 1)
      | c -> loop False (Buff.store len c) (i + 1) ]
;

value table_strip_troublemakers hts =
  for i = 0 to Array.length hts - 1 do {
    for j = 0 to Array.length hts.(i) - 1 do {
      match hts.(i).(j) with
      [ (colspan, align, TDitem s) ->
          hts.(i).(j) := (colspan, align, TDitem (strip_troublemakers s))
      | _ -> () ]
    }
  }
;

value table_pre_dim conf hts =
  do {
    table_strip_troublemakers hts;
    let ncol =
      let hts0 = hts.(0) in
      let rec loop ncol j =
        if j = Array.length hts0 then ncol
        else
          let (colspan, _, _) = hts0.(j) in
          loop (ncol + colspan) (j + 1)
      in
      loop 0 0
    in
    let min_widths_tab = compute_columns_minimum_sizes hts ncol in
    let max_widths_tab = compute_columns_sizes hts ncol in
    let min_wid = Array.fold_left  \+ 0 min_widths_tab in
    let max_wid = Array.fold_left  \+ 0 max_widths_tab in
    (min_wid, max_wid, min_widths_tab, max_widths_tab, ncol)
  }
;

IFDEF OLD THEN declare
value print_next_pos conf pos1 pos2 tcol =
  let doit = p_getenv conf.env "notab" = Some "on" in
  if doit then do {
    let dpos =
      match p_getint conf.env "dpos" with
      [ Some dpos -> dpos
      | None -> 78 ]
    in
    let pos1 =
      match pos1 with
      [ Some pos1 -> pos1
      | None -> 0 ]
    in
    let pos2 =
      match pos2 with
      [ Some pos2 -> pos2
      | None -> dpos ]
    in
    let overlap =
      let overlap =
        match p_getint conf.env "overlap" with
        [ Some x -> x
        | None -> 10 ]
      in
      min overlap dpos
    in
    let env =
      List.fold_right
        (fun (k, v) env ->
           match k with
           [ "pos1" | "pos2" -> env
           | _ -> [(k, v) :: env] ])
        conf.env []
    in
    Wserver.wprint "<div style=\"text-align:right\">\n";
    if pos1 = 0 then Wserver.wprint "&nbsp;"
    else do {
      Wserver.wprint "<a href=\"%s" (commd conf);
      List.iter (fun (k, v) -> Wserver.wprint "%s=%s;" k v) env;
      Wserver.wprint "pos1=%d;pos2=%d" (pos1 + overlap - dpos)
        (pos1 + overlap);
      Wserver.wprint "\">&lt;&lt;</a>\n"
    };
    if pos2 >= tcol then Wserver.wprint "&nbsp;"
    else do {
      Wserver.wprint "<a href=\"%s" (commd conf);
      List.iter (fun (k, v) -> Wserver.wprint "%s=%s;" k v) env;
      Wserver.wprint "pos1=%d;pos2=%d" (pos2 - overlap)
        (pos2 - overlap + dpos);
      Wserver.wprint "\">&gt;&gt;</a>\n"
    };
    Wserver.wprint "</div>\n"
  }
  else ()
;

(* Main print table algorithm with <pre> *)

value print_table_pre conf hts =
  let (tmincol, tcol, colminsz, colsz, ncol) = table_pre_dim conf hts in
  let dcol =
    let dcol =
      match p_getint conf.env "width" with
      [ Some i -> i
      | None -> 79 ]
    in
    max tmincol (min dcol tcol)
  in
  do {
    if tcol > tmincol then
      for i = 0 to ncol - 1 do {
        colsz.(i) :=
          colminsz.(i) +
            (colsz.(i) - colminsz.(i)) * (dcol - tmincol) / (tcol - tmincol)
      }
    else ();
    let pos1 = p_getint conf.env "pos1" in
    let pos2 =
      match p_getint conf.env "pos2" with
      [ None -> p_getint conf.env "dpos"
      | x -> x ]
    in
    print_next_pos conf pos1 pos2 (Array.fold_left \+ 0 colsz);
    Wserver.wprint "<pre>\n";
    for i = 0 to Array.length hts - 1 do {
      let (stra, max_row) =
        let (stral, max_row) =
          loop [] 1 0 0 where rec loop stral max_row col j =
            if j = Array.length hts.(i) then (stral, max_row)
            else
              let (colspan, _, td) = hts.(i).(j) in
              let stra =
                match td with
                [ TDitem s | TDtext s ->
                    let sz =
                      loop 0 colspan where rec loop sz k =
                        if k = 0 then sz
                        else loop (sz + colsz.(col + k - 1)) (k - 1)
                    in
                    Array.of_list (displayed_strip s sz)
                | _ -> [| |] ]
              in
              loop [stra :: stral] (max max_row (Array.length stra))
                (col + colspan) (j + 1)
        in
        (Array.of_list (List.rev stral), max_row)
      in
      for row = 0 to max_row - 1 do {
        let rec loop pos col j =
          if j = Array.length hts.(i) then Wserver.wprint "\n"
          else do {
            let (colspan, align, td) = hts.(i).(j) in
            let sz =
              loop 0 colspan where rec loop sz k =
                if k = 0 then sz else loop (sz + colsz.(col + k - 1)) (k - 1)
            in
            let outs =
              match td with
              [ TDitem s | TDtext s ->
                  let s =
                    let k =
                      let dk = (max_row - Array.length stra.(j)) / 2 in
                      row - dk
                    in
                    if k >= 0 && k < Array.length stra.(j) then
                      let s = stra.(j).(k) in
                      if s = "&nbsp;" then " " else s
                    else try_add_vbar k (Array.length stra.(j)) hts i col
                  in
                  let len = displayed_length s in
                  String.make ((sz - len) / 2) ' ' ^ s ^
                    String.make (sz - (sz + len) / 2) ' '
              | TDnothing ->
                  String.make ((sz - 1) / 2) ' ' ^ "&nbsp;" ^
                    String.make (sz - (sz + 1) / 2) ' '
              | TDbar s ->
                  let s =
                    match s with
                    [ None | Some "" -> "|"
                    | Some s ->
                        sprintf
                          "<a style=\"text-decoration:none\" href=\"%s\">|</a>"
                          s ]
                  in
                  let len = displayed_length s in
                  String.make ((sz - len) / 2) ' ' ^ s ^
                    String.make (sz - (sz + len) / 2) ' '
              | TDhr LeftA ->
                  let len = (sz + 1) / 2 in
                  String.make len '-' ^ String.make (sz - len) ' '
              | TDhr RightA ->
                  let len = sz / 2 in
                  String.make (sz - len - 1) ' ' ^ String.make (len + 1) '-'
              | TDhr CenterA -> String.make sz '-' ]
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
                else if pos1 < pos then displayed_sub outs 0 (pos2 - pos)
                else displayed_sub outs (pos1 - pos) (pos2 - pos1)
            in
            Wserver.wprint "%s" clipped_outs;
            loop (pos + sz) (col + colspan) (j + 1)
          }
        in
        loop 0 0 0
      }
    };
    Wserver.wprint "</pre>\n"
  }
;

(* main *)

value print_html_table conf hts =
  do {
    if Util.p_getenv conf.env "notab" <> Some "on" then
      tag "p" begin
        Wserver.wprint "<div style=\"text-align:%s\"><a href=\"%s" conf.right
          (commd conf);
        List.iter (fun (k, v) -> Wserver.wprint "%s=%s;" k v) conf.env;
        Wserver.wprint "notab=on;slices=on";
        Wserver.wprint "\"><tt>//</tt></a></div>\n";
      end
    else ();
    if Util.p_getenv conf.env "notab" = Some "on" ||
       Util.p_getenv conf.env "pos2" <> None ||
       browser_doesnt_have_tables conf
    then
      print_table_pre conf hts
    else print_table conf hts
  }
;
end END;

value make_tree_hts conf base elem_txt vbar_txt invert set spl d =
  let no_group = p_getenv conf.env "nogroup" = Some "on" in
  let spouse_on =
    match Util.p_getenv conf.env "spouse" with
    [ Some "on" -> True
    | _ -> False ]
  in
  let bd =
    match Util.p_getint conf.env "bd" with
    [ Some x -> x
    | None -> 0 ]
  in
  let td_prop =
    match Util.p_getenv conf.env "td" with
    [ Some x -> " " ^ x
    | _ ->
        match Util.p_getenv conf.env "color" with
        [ None | Some "" -> ""
        | Some x -> " class=\"" ^ x ^ "\"" ] ]
  in
  let indi_txt n =
    match n.valu with
    [ Left ip ->
        let p = pget conf base ip in
        let txt =
          string_of_item conf base (elem_txt p) ^ image_txt conf base p
        in
        let txt =
          let spouses =
            if (spouse_on && n.chil <> [] || n.pare = []) && not invert then
              List.fold_left
                (fun list id ->
                   match d.dag.(int_of_idag id).valu with
                   [ Left cip ->
                       match get_parents (pget conf base cip) with
                       [ Some ifam ->
                           let cpl = foi base ifam in
                           if ip = get_father cpl then
                             if List.mem_assoc (get_mother cpl) list then list
                             else [(get_mother cpl, Some ifam) :: list]
                           else if ip = get_mother cpl then
                             if List.mem_assoc (get_father cpl) list then list
                             else [(get_father cpl, Some ifam) :: list]
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
                 let ps = pget conf base ips in
                 let d =
                   match ifamo with
                   [ Some ifam ->
                       Date.short_marriage_date_text conf base (foi base ifam)
                         p ps
                   | None -> "" ]
                 in
                 txt ^ "<br" ^ conf.xhs ^ ">\n&amp;" ^ d ^ " " ^
                   string_of_item conf base (elem_txt ps) ^
                     image_txt conf base ps)
            txt spouses
        in
        txt
    | Right _ -> "&nbsp;" ]
  in
  let indi_txt n =
    let (bd, td) =
      match n.valu with
      [ Left ip -> (bd, td_prop)
      | _ -> (0, "") ]
    in
    if bd > 0 || td <> "" then
      sprintf "\
<table border=\"%d\"><tr align=\"left\"><td align=\"center\"%s>%s</td></tr></table>"
        bd td (indi_txt n)
    else indi_txt n
  in
  let vbar_txt n =
    match n.valu with
    [ Left ip -> vbar_txt ip
    | _ -> "" ]
  in
  let phony n =
    match n.valu with
    [ Left _ -> False
    | Right _ -> True ]
  in
  let t = Dag2html.table_of_dag phony False invert no_group d in
  if Array.length t.table = 0 then [| |]
  else Dag2html.html_table_struct indi_txt vbar_txt phony d t
;

IFDEF OLD THEN declare
value print_slices_menu conf hts =
  let txt n =
    Util.capitale
      (transl_nth conf "display by slices/slice width/overlap/total width" n)
  in
  let title _ = Wserver.wprint "%s" (txt 0) in
  do {
    Hutil.header conf title;
    Hutil.print_link_to_welcome conf True;
    tag "form" "method=\"get\" action=\"%s\"" conf.command begin
      html_p conf;
      hidden_env conf;
      List.iter
        (fun (k, v) ->
           if k = "slices" then ()
           else
             Wserver.wprint
               "<input type=\"hidden\" name=\"%s\" value=\"%s\">\n"
               (decode_varenv k) (decode_varenv v))
        conf.env;
      tag "table" begin
        tag "tr" "align=\"left\"" begin
          tag "td" "align=\"right\"" begin
            Wserver.wprint "%s\n"
              (Util.capitale
                 (transl conf "don't group the common branches together"));
            Wserver.wprint
              "<input type=\"checkbox\" name=\"nogroup\" value=\"on\">\n";
          end;
        end;
        tag "tr" "align=\"left\"" begin
          tag "td" "align=\"right\"" begin
            Wserver.wprint "%s\n" (txt 1);
            Wserver.wprint "<input name=\"dpos\" size=\"5\" value=\"78\">\n";
          end;
        end;
        tag "tr" "align=\"left\"" begin
          tag "td" "align=\"right\"" begin
            Wserver.wprint "%s\n" (txt 2);
            Wserver.wprint
              "<input name=\"overlap\" size=\"5\" value=\"10\">\n";
          end;
        end;
        tag "tr" "align=\"left\"" begin
          tag "td" "align=\"right\"" begin
            Wserver.wprint "%s\n" (txt 3);
            let wid =
              let (min_wid, max_wid, _, _, _) = table_pre_dim conf hts in
              do {
                Wserver.wprint "(%d-%d)\n" min_wid max_wid;
                max min_wid (min max_wid 78)
              }
            in
            Wserver.wprint "<input name=\"width\" size=\"5\" value=\"%d\">\n"
              wid;
          end;
        end;
      end;
      html_p conf;
      Wserver.wprint "<input type=\"submit\" value=\"Ok\">\n";
    end;
    Hutil.trailer conf
  }
;

value print_dag_page conf base page_title hts next_txt =
  let conf =
    let doctype =
      (* changing doctype to transitional because use of
         <hr width=... align=...> *)
      match p_getenv conf.base_env "doctype" with
      [ Some ("html-4.01" | "html-4.01-trans") -> "html-4.01-trans"
      | _ -> "xhtml-1.0-trans" ]
    in
    {(conf) with base_env = [("doctype", doctype) :: conf.base_env]}
  in
  let title _ = Wserver.wprint "%s" page_title in
  do {
    Hutil.header_no_page_title conf title;
    print_html_table conf hts;
    if next_txt <> "" then
      tag "p" begin
        Wserver.wprint "<a href=\"%s%s\">&gt;&gt;</a>\n" (commd conf) next_txt;
      end
    else ();
    Hutil.trailer conf
  }
;
end END;

(* *)

type dag_item = string;
type dag_bar = string;

type env 'a =
  [ Vdag of (int * int * array int * array int * int)
  | Vdcell of (int * Dag2html.align * Dag2html.table_data dag_item dag_bar)
  | Vdcellp of string
  | Vdline of int
  | Vdlinep of (int * array (array string) * int * option int * option int)
  | Vlazy of Lazy.t (env 'a)
  | Vother of 'a
  | Vnone ]
;

value get_env v env =
  try
    match List.assoc v env with
    [ Vlazy l -> Lazy.force l
    | x -> x ]
  with
  [ Not_found -> Vnone ]
;
value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value rec eval_var conf page_title next_txt env xx loc =
  fun
  [ ["dag" :: sl] ->
      match get_env "dag" env with
      [ Vdag d -> eval_dag_var conf d sl
      | _ -> raise Not_found ]
  | ["dag_cell" :: sl] ->
      match get_env "dag_cell" env with
      [ Vdcell dcell -> eval_dag_cell_var conf dcell sl
      | _ -> raise Not_found ]
  | ["dag_cell_pre"] ->
      match get_env "dag_cell_pre" env with
      [ Vdcellp s -> VVstring s
      | _ -> raise Not_found ]
  | ["head_title"] -> VVstring page_title
  | ["link_next"] -> VVstring next_txt
  | _ -> raise Not_found ]
and eval_dag_var conf (tmincol, tcol, colminsz, colsz, ncol) =
  fun
  [ ["max_wid"] -> VVstring (string_of_int tcol)
  | ["min_wid"] -> VVstring (string_of_int tmincol)
  | ["ncol"] -> VVstring (string_of_int (Array.fold_left \+ 0 colsz))
  | _ -> raise Not_found ]
and eval_dag_cell_var conf (colspan, align, td) =
  fun
  [ ["align"] ->
      match align with
      [ LeftA -> VVstring conf.left
      | CenterA -> VVstring "center"
      | RightA -> VVstring conf.right ]
  | ["bar_link"] ->
       VVstring (match td with [ TDbar (Some s) -> s | _ -> "" ])
  | ["colspan"] -> VVstring (string_of_int colspan)
  | ["is_bar"] ->
       VVbool (match td with [ TDbar _ -> True | _ -> False ])
  | ["is_hr_left"] ->
       match td with
       [ TDhr LeftA -> VVbool True
       | _ -> VVbool False ]
  | ["is_hr_right"] ->
       match td with
       [ TDhr RightA -> VVbool True
       | _ -> VVbool False ]
  | ["is_nothing"] -> VVbool (td = TDnothing)
  | ["item"] ->
      match td with
      [ TDitem s -> VVstring s
      | _ -> VVstring "" ]
  | ["text"] ->
      match td with
      [ TDtext s -> VVstring s
      | _ -> VVstring "" ]
  | _ -> raise Not_found ]
;

value rec print_foreach conf hts print_ast eval_expr env () loc s sl el al =
  match [s :: sl] with
  [ ["dag_cell"] -> print_foreach_dag_cell conf hts print_ast env al
  | ["dag_cell_pre"] -> print_foreach_dag_cell_pre conf hts print_ast env al
  | ["dag_line"] -> print_foreach_dag_line conf print_ast env hts al
  | ["dag_line_pre"] -> print_foreach_dag_line_pre conf hts print_ast env al
  | _ -> raise Not_found ]
and print_foreach_dag_cell_pre conf hts print_ast env al =
  let i =
    match get_env "dag_line" env with
    [ Vdline i -> i
    | _ -> raise Not_found ]
  in
  let (_, _, _, colsz, _) =
    match get_env "dag" env with
    [ Vdag d -> d
    | _ -> raise Not_found ]
  in
  let (max_row, stra, row, pos1, pos2) =
    match get_env "dag_line_pre" env with
    [ Vdlinep x -> x
    | _ -> raise Not_found ]
  in
  let rec loop pos col j =
    if j = Array.length hts.(i) then ()
    else do {
      let (colspan, align, td) = hts.(i).(j) in
      let sz =
        loop 0 colspan where rec loop sz k =
          if k = 0 then sz else loop (sz + colsz.(col + k - 1)) (k - 1)
      in
      let outs =
        match td with
        [ TDitem s | TDtext s ->
            let s =
              let k =
                let dk = (max_row - Array.length stra.(j)) / 2 in
                row - dk
              in
              if k >= 0 && k < Array.length stra.(j) then
                let s = stra.(j).(k) in
                if s = "&nbsp;" then " " else s
              else try_add_vbar k (Array.length stra.(j)) hts i col
            in
            let len = displayed_length s in
            String.make ((sz - len) / 2) ' ' ^ s ^
              String.make (sz - (sz + len) / 2) ' '
        | TDnothing -> String.make sz ' '
        | TDbar s ->
            let s =
              match s with
              [ None | Some "" -> "|"
              | Some s ->
                  if conf.cancel_links then "|"
                  else
                    sprintf
                      "<a style=\"text-decoration:none\" href=\"%s\">|</a>" s ]
            in
            let len = displayed_length s in
            String.make ((sz - len) / 2) ' ' ^ s ^
              String.make (sz - (sz + len) / 2) ' '
        | TDhr LeftA ->
            let len = (sz + 1) / 2 in
            String.make len '-' ^ String.make (sz - len) ' '
        | TDhr RightA ->
            let len = sz / 2 in
            String.make (sz - len - 1) ' ' ^ String.make (len + 1) '-'
        | TDhr CenterA -> String.make sz '-' ]
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
          else if pos1 < pos then displayed_sub outs 0 (pos2 - pos)
          else displayed_sub outs (pos1 - pos) (pos2 - pos1)
      in
      if clipped_outs <> "" then do {
        let v = Vdcellp clipped_outs in
        let print_ast = print_ast [("dag_cell_pre", v) :: env] () in
        List.iter print_ast al;
      }
      else ();
      loop (pos + sz) (col + colspan) (j + 1)
    }
  in
  loop 0 0 0
and print_foreach_dag_cell conf hts print_ast env al =
  let i =
    match get_env "dag_line" env with
    [ Vdline i -> i
    | _ -> raise Not_found ]
  in
  for j = 0 to Array.length hts.(i) - 1 do {
    let print_ast = print_ast [("dag_cell", Vdcell hts.(i).(j)) :: env] () in
    List.iter print_ast al;
  }
and print_foreach_dag_line conf print_ast env hts al =
  for i = 0 to Array.length hts - 1 do {
    let print_ast = print_ast [("dag_line", Vdline i) :: env] () in
    List.iter print_ast al;
  }
and print_foreach_dag_line_pre conf hts print_ast env al =
  let i =
    match get_env "dag_line" env with
    [ Vdline i -> i
    | _ -> raise Not_found ]
  in
  let (tmincol, tcol, colminsz, colsz, ncol) =
    match get_env "dag" env with
    [ Vdag d -> d
    | _ -> raise Not_found ]
  in
  let (stra, max_row) =
    let (stral, max_row) =
      loop [] 1 0 0 where rec loop stral max_row col j =
        if j = Array.length hts.(i) then (stral, max_row)
        else
          let (colspan, _, td) = hts.(i).(j) in
          let stra =
            match td with
            [ TDitem s | TDtext s ->
                let sz =
                  loop 0 colspan where rec loop sz k =
                    if k = 0 then sz
                    else loop (sz + colsz.(col + k - 1)) (k - 1)
                in
                Array.of_list (displayed_strip s sz)
            | _ -> [| |] ]
          in
          loop [stra :: stral] (max max_row (Array.length stra))
            (col + colspan) (j + 1)
    in
    (Array.of_list (List.rev stral), max_row)
  in
  let pos1 = p_getint conf.env "pos1" in
  let pos2 =
    match p_getint conf.env "pos2" with
    [ None -> p_getint conf.env "dpos"
    | x -> x ]
  in
  for row = 0 to max_row - 1 do {
    let v = Vdlinep (max_row, stra, row, pos1, pos2) in
    let print_ast = print_ast [("dag_line_pre", v) :: env] () in
    List.iter print_ast al;
  }
;

IFDEF OLD THEN declare
value old_print_slices_menu_or_dag_page conf base page_title hts next_txt =
  if p_getenv conf.env "slices" = Some "on" then print_slices_menu conf hts
  else print_dag_page conf base page_title hts next_txt
;
end ELSE declare
value old_print_slices_menu_or_dag_page conf base page_title hts next_txt =
  incorrect_request conf
;
end END;

value print_slices_menu_or_dag_page conf base page_title hts next_txt =
(**)
  if p_getenv conf.env "old" = Some "on" then
    old_print_slices_menu_or_dag_page conf base page_title hts next_txt else
(**)
  let env =
    let table_pre_dim () =
      let (tmincol, tcol, colminsz, colsz, ncol) = table_pre_dim conf hts in
      let dcol =
        let dcol =
          match p_getint conf.env "width" with
          [ Some i -> i
          | None -> 79 ]
        in
        max tmincol (min dcol tcol)
      in
      do {
        if tcol > tmincol then
          for i = 0 to ncol - 1 do {
            colsz.(i) :=
              colminsz.(i) +
                (colsz.(i) - colminsz.(i)) * (dcol - tmincol) / (tcol - tmincol)
          }
        else ();
        Vdag (tmincol, tcol, colminsz, colsz, ncol)
      }
    in
    [("dag", Vlazy (Lazy.from_fun table_pre_dim))]
  in
  Hutil.interp conf base "dag"
    {Templ.eval_var = eval_var conf page_title next_txt;
     Templ.eval_transl _ = Templ.eval_transl conf;
     Templ.eval_predefined_apply _ = raise Not_found;
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = print_foreach conf hts}
     env ()
;

value make_and_print_dag conf base elem_txt vbar_txt invert set spl
  page_title next_txt
=
  let d = make_dag conf base set in
  let hts = make_tree_hts conf base elem_txt vbar_txt invert set spl d in
  print_slices_menu_or_dag_page conf base page_title hts next_txt
;

value print conf base =
  let set = get_dag_elems conf base in
  let elem_txt p = Item p "" in
  let vbar_txt ip = "" in
  let invert =
    match Util.p_getenv conf.env "invert" with
    [ Some "on" -> True
    | _ -> False ]
  in
  let page_title = Util.capitale (Util.transl conf "tree") in
  make_and_print_dag conf base elem_txt vbar_txt invert set [] page_title ""
;
