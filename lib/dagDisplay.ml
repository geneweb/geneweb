open Config
open Dag2html
open Def
open Util
open Dag
module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

let image_normal_txt conf base p fname width height =
  let image_txt = Utf8.capitalize_fst (transl_nth conf "image/images" 0) in
  let s = Unix.stat fname in
  let k = Image.default_image_filename "portraits" base p in
  let r =
    Format.sprintf
      {|<img src="%sm=IM&d=%s&%s&k=%s"%s%s alt="%s" title="%s"
        style="%s %s">|}
      (commd conf : Adef.escaped_string :> string)
      (string_of_int
      @@ int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
      (acces conf base p : Adef.escaped_string :> string)
      k
      (if width = 0 then "" else " width=" ^ string_of_int width)
      (if height = 0 then "" else " height=" ^ string_of_int height)
      image_txt image_txt
      (if width = 0 then "" else "max-width:" ^ string_of_int width ^ "px;")
      (if height = 0 then "" else "max-height:" ^ string_of_int height ^ "px;")
  in
  (if p_getenv conf.env "cgl" = Some "on" then r
   else
     Format.sprintf {|<a href="%sm=IM&%s&k=%s">%s</a>|}
       (commd conf : Adef.escaped_string :> string)
       (acces conf base p : Adef.escaped_string :> string)
       k r)
  |> Adef.safe

let image_url_txt conf url_p url ~width ~height =
  let width = Option.value ~default:0 width in
  let image_txt = Utf8.capitalize_fst (transl_nth conf "image/images" 0) in
  let size =
    Format.sprintf "%s %s"
      (if width = 0 then "" else Format.sprintf {|width="%d"|} width)
      (if height = 0 then "" else Format.sprintf {|height="%d"|} height)
  in
  let style =
    Format.sprintf "%spx;%spx;"
      (if width = 0 then "" else "max-width:" ^ string_of_int width)
      (if height = 0 then "" else "max-height:" ^ string_of_int height)
  in
  let s =
    Format.sprintf {|<img src="%s" alt="%s" title="%s" %s style="%s">|}
      (url : Adef.escaped_string :> string)
      image_txt image_txt size style
  in
  Adef.safe
  @@
  if p_getenv conf.env "cgl" = Some "on" then s
  else
    Format.sprintf {|<a href="%s">%s</a>|}
      (url_p : Adef.escaped_string :> string)
      s

let image_txt conf base p =
  let img = Util.get_opt conf "im" true in
  Adef.safe
  @@
  if img then
    match Image.get_portrait_with_size conf base p with
    | None -> ""
    | Some (`Path s, size_opt) ->
        let max_w, max_h = (100, 75) in
        let w, h =
          match size_opt with
          | Some (w, h) -> Image.scale_to_fit ~max_w ~max_h ~w ~h
          | None -> (0, max_h)
        in
        Printf.sprintf
          {|
            <br>
            <center>
              <table border="0">
                <tr align="left"><td>%s</td></tr>
              </table>
            </center>|}
          (image_normal_txt conf base p s w h |> Adef.as_string)
    | Some (`Url url, Some (width, height)) ->
        let url_p = commd conf ^^^ acces conf base p in
        Printf.sprintf
          {|
            <br>
            <center>
              <table border="0">
                <tr align="left"><td>%s</td></tr>
              </table>
            </center>|}
          (image_url_txt conf url_p (Util.escape_html url) ~width:(Some width)
             ~height
          |> Adef.as_string)
    | Some (`Url url, None) ->
        let url_p = commd conf ^^^ acces conf base p in
        let height = 75 in
        (* La hauteur est ajoutée à la table pour que les textes soient alignés. *)
        Printf.sprintf
          {|
            <br>
            <center>
              <table border="0" style="height:%spx">
                <tr align="left"><td>%s</td></tr>
              </table>
            </center>|}
          (string_of_int height)
          (image_url_txt conf url_p (Util.escape_html url) ~width:None ~height
          |> Adef.as_string)
  else ""

type item = Item of Driver.person * Adef.safe_string

let string_of_item conf base = function
  | Item (p, s) ->
      Util.referenced_person_title_text conf base p
      ^^^ DateDisplay.short_dates_text conf base p
      ^^^ if (s :> string) = "" then Adef.safe "" else " " ^<^ s

(* Print with HTML table tags: <table> <tr> <td> *)

let print_table conf
    (hts :
      (int * Dag2html.align * Adef.safe_string Dag2html.table_data) array array)
    =
  begin_centered conf;
  Output.print_sstring conf {|<table border="|};
  Output.print_sstring conf (string_of_int conf.border);
  Output.print_sstring conf {|" cellspacing="0" cellpadding="0">|};
  for i = 0 to Array.length hts - 1 do
    Output.print_sstring conf "<tr align=\"left\">\n";
    for j = 0 to Array.length hts.(i) - 1 do
      let colspan, align, td = hts.(i).(j) in
      Output.print_sstring conf "<td";
      if colspan = 1 && (td = TDnothing || td = TDhr CenterA) then ()
      else (
        Output.print_sstring conf " colspan=\"";
        Output.print_sstring conf (string_of_int colspan);
        Output.print_sstring conf "\"");
      (match (align, td) with
      | LeftA, TDhr LeftA ->
          Output.print_sstring conf " align=\"";
          Output.print_sstring conf conf.left;
          Output.print_sstring conf "\""
      | LeftA, _ -> ()
      | CenterA, _ -> Output.print_sstring conf " align=\"center\""
      | RightA, _ ->
          Output.print_sstring conf " align=\"";
          Output.print_sstring conf conf.right;
          Output.print_sstring conf "\"");
      Output.print_sstring conf ">";
      (match td with
      | TDitem (_ip, s, _t) -> Output.print_string conf s
      | TDtext (_ip, s) -> Output.print_string conf s
      | TDnothing -> Output.print_sstring conf "&nbsp;"
      | TDbar None -> Output.print_sstring conf "|"
      | TDbar (Some (s : Adef.escaped_string)) ->
          if p_getenv conf.env "cgl" = Some "on" then Output.print_string conf s
          else (
            Output.print_sstring conf {|<a style="text-decoration:none" href="|};
            Output.print_string conf s;
            Output.print_sstring conf {|">|</a>|})
      | TDhr align -> (
          match align with
          | LeftA ->
              Output.print_sstring conf "<hr class=\"";
              Output.print_sstring conf conf.left;
              Output.print_sstring conf "\">"
          | RightA ->
              Output.print_sstring conf "<hr class=\"";
              Output.print_sstring conf conf.right;
              Output.print_sstring conf "\">"
          | _ -> Output.print_sstring conf {|<hr class="full">|}));
      Output.print_sstring conf "</td>"
    done;
    Output.print_sstring conf "</tr>"
  done;
  Output.print_sstring conf "</table>";
  end_centered conf

(*
 * Print without HTML table tags: using <pre>
 *)

(* Machinery opering to 'displayed texts', i.e. strings where not all
   characters correspond to a displayed character (due to html tags or
   encoded characters) *)

(* Return next 'displayed character' location: can be on several 'string
   characters', like "&nbsp;" *)

let displayed_next_char s i =
  let rec loop i =
    if i >= String.length s then None
    else
      match s.[i] with
      | '<' ->
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
              | 'a' .. 'z' | 'A' .. 'Z' -> loop1 (j + 1)
              | ';' -> Some (i, j + 1)
              | _ -> Some (i, j)
          in
          loop1 (i + 1)
      | c -> Some (i, i + Utf8.nbc c)
  in
  loop i

let buff_store_int s blen i j =
  let rec loop blen i =
    if i = j then blen else loop (Buff.store blen s.[i]) (i + 1)
  in
  loop blen i

(* Remove empty tags, i.e. <a href=..></a> enclosing empty text, from s *)

let strip_empty_tags s =
  let rec loop blen opened_tag i =
    if i >= String.length s then Buff.get blen
    else
      match s.[i] with
      | '<' -> (
          let j = i + 1 in
          let tag_close, j =
            match s.[j] with '/' -> (true, j + 1) | _ -> (false, j)
          in
          let tag_name, j =
            let rec loop k =
              match s.[k] with
              | 'a' .. 'z' | 'A' .. 'Z' -> loop (k + 1)
              | _ -> (String.sub s j (k - j), k)
            in
            loop j
          in
          let j =
            let rec loop j = if s.[j] = '>' then j + 1 else loop (j + 1) in
            loop j
          in
          match opened_tag with
          | Some (opened_tag_name, k) ->
              if tag_close then
                if tag_name = opened_tag_name then loop blen None j
                else loop (buff_store_int s blen k j) None j
              else loop (buff_store_int s blen k i) (Some (tag_name, i)) j
          | None ->
              if tag_close then loop (buff_store_int s blen i j) None j
              else loop blen (Some (tag_name, i)) j)
      | c ->
          let blen =
            match opened_tag with
            | Some (_, k) -> buff_store_int s blen k i
            | None -> blen
          in
          loop (Buff.store blen c) None (i + 1)
  in
  loop 0 None 0

let displayed_length s =
  let s = (s : Adef.safe_string :> string) in
  let rec loop len i =
    match displayed_next_char s i with
    | Some (_i, j) -> loop (len + 1) j
    | None -> len
  in
  loop 0 0

let displayed_sub s ibeg ilen =
  let rec loop blen di dlen i =
    match displayed_next_char s i with
    | Some (j, k) ->
        let blen = buff_store_int s blen i j in
        let blen, dlen =
          if di >= ibeg && dlen < ilen then (buff_store_int s blen j k, dlen + 1)
          else (blen, dlen)
        in
        loop blen (di + 1) dlen k
    | None ->
        let s = Buff.get (buff_store_int s blen i (String.length s)) in
        strip_empty_tags s
  in
  loop 0 0 0 0

let longuest_word_length s =
  let s = (s : Adef.safe_string :> string) in
  let rec loop maxlen len i =
    match displayed_next_char s i with
    | Some (j, k) ->
        if s.[j] = ' ' then loop (max maxlen len) 0 k
        else loop maxlen (len + 1) k
    | None -> max maxlen len
  in
  loop 0 0 0

let displayed_end_word s di i =
  let rec loop di i =
    match displayed_next_char s i with
    | Some (j, k) -> if s.[j] = ' ' then (di, Some j) else loop (di + 1) k
    | None -> (di, None)
  in
  loop di i

(* Strip 'displayed text' s by subtexts of limited size sz *)

let displayed_strip s sz =
  let s = (s : Adef.safe_string :> string) in
  let displayed_sub s b e = displayed_sub s b e |> Adef.safe in
  let rec loop strl dibeg di i =
    let i =
      let rec loop i =
        if i < String.length s && s.[i] = ' ' then loop (i + 1) else i
      in
      loop i
    in
    let dj, j = displayed_end_word s di i in
    match j with
    | Some j ->
        if dj - dibeg > sz then
          loop
            (displayed_sub s dibeg (di - dibeg - 1) :: strl)
            di (dj + 1) (j + 1)
        else loop strl dibeg (dj + 1) (j + 1)
    | None ->
        let strl =
          if dj - dibeg > sz then
            let str2 = displayed_sub s dibeg (di - dibeg - 1) in
            let str1 = displayed_sub s di (dj - di) in
            str1 :: str2 :: strl
          else
            let str = displayed_sub s dibeg (dj - dibeg) in
            str :: strl
        in
        List.rev strl
  in
  loop [] 0 0 0

(* Determine columns sizes; scan all table by increasing colspans *)

let gen_compute_columns_sizes size_fun
    (hts : (int * 'a * 'b Dag2html.table_data) array array) ncol =
  let colsz = Array.make ncol 0 in
  let rec loop curr_colspan =
    let next_colspan = ref (ncol + 1) in
    for i = 0 to Array.length hts - 1 do
      if i = Array.length hts then ()
      else
        let rec loop col j =
          if j = Array.length hts.(i) then ()
          else
            let colspan, _, td = hts.(i).(j) in
            (match td with
            | TDitem _ | TDtext _ | TDnothing ->
                if colspan = curr_colspan then
                  let len =
                    match td with
                    | TDitem (_ip, s, _t) -> size_fun s
                    | TDtext (_ip, s) -> size_fun s
                    | _ -> 1
                  in
                  let currsz =
                    let rec loop currsz col cnt =
                      if cnt = 0 then currsz
                      else
                        let currsz = currsz + colsz.(col) in
                        loop currsz (col + 1) (cnt - 1)
                    in
                    loop 0 col colspan
                  in
                  if currsz >= len then ()
                  else
                    let rec loop n col cnt =
                      if cnt = 0 then ()
                      else
                        let inc_sz =
                          (n * (len - currsz) / colspan)
                          - ((n - 1) * (len - currsz) / colspan)
                        in
                        colsz.(col) <- colsz.(col) + inc_sz;
                        loop (n + 1) (col + 1) (cnt - 1)
                    in
                    loop 1 col colspan
                else if colspan > curr_colspan then
                  next_colspan := min colspan !next_colspan
            | TDbar _ -> ()
            | TDhr _ -> ());
            loop (col + colspan) (j + 1)
        in
        loop 0 0
    done;
    if !next_colspan > ncol then () else loop !next_colspan
  in
  loop 1;
  colsz

let compute_columns_sizes = gen_compute_columns_sizes displayed_length

let compute_columns_minimum_sizes =
  gen_compute_columns_sizes longuest_word_length

(* Gadget to add a | to fill upper/lower part of a table data when
   preceded/followed by a |; not obligatory but nicer *)

let try_add_vbar stra_row stra_row_max hts i col =
  Adef.safe
  @@
  if stra_row < 0 then
    if i = 0 then ""
    else
      let rec loop pcol pj =
        if pj >= Array.length hts.(i - 1) then ""
        else
          let colspan, _, td = hts.(i - 1).(pj) in
          if pcol = col then match td with TDbar _ -> "|" | _ -> ""
          else loop (pcol + colspan) (pj + 1)
      in
      loop 0 0
  else if stra_row >= stra_row_max then
    if i = Array.length hts - 1 then ""
    else
      let rec loop ncol nj =
        if nj >= Array.length hts.(i + 1) then ""
        else
          let colspan, _, td = hts.(i + 1).(nj) in
          if ncol = col then match td with TDbar _ -> "|" | _ -> ""
          else loop (ncol + colspan) (nj + 1)
      in
      loop 0 0
  else ""

let strip_troublemakers s =
  let s = (s : Adef.safe_string :> string) in
  let rec loop last_space len i =
    if i = String.length s then Adef.safe (Buff.get len)
    else
      match s.[i] with
      | '<' ->
          let j = i + 1 in
          let j = match s.[j] with '/' -> j + 1 | _ -> j in
          let tag_name, j =
            let rec loop k =
              match s.[k] with
              | 'a' .. 'z' | 'A' .. 'Z' -> loop (k + 1)
              | _ -> (String.lowercase_ascii (String.sub s j (k - j)), k)
            in
            loop j
          in
          let j =
            let rec loop j = if s.[j] = '>' then j + 1 else loop (j + 1) in
            loop j
          in
          let len =
            match tag_name with
            | "bdo" | "br" | "font" | "img" | "span" | "table" | "td" | "tr"
            | "center" ->
                len
            | _ -> buff_store_int s len i j
          in
          loop last_space len j
      | '\n' | '\r' | ' ' ->
          let len = if last_space then len else Buff.store len ' ' in
          loop true len (i + 1)
      | c -> loop false (Buff.store len c) (i + 1)
  in
  loop false 0 0

let table_strip_troublemakers hts =
  for i = 0 to Array.length hts - 1 do
    for j = 0 to Array.length hts.(i) - 1 do
      match hts.(i).(j) with
      | colspan, align, TDitem (ip, s, _t) ->
          hts.(i).(j) <-
            (colspan, align, TDitem (ip, strip_troublemakers s, Adef.safe ""))
      | _ -> ()
    done
  done

let table_pre_dim (hts : (int * 'a * 'b Dag2html.table_data) array array) =
  table_strip_troublemakers hts;
  let ncol =
    let hts0 = hts.(0) in
    let rec loop ncol j =
      if j = Array.length hts0 then ncol
      else
        let colspan, _, _ = hts0.(j) in
        loop (ncol + colspan) (j + 1)
    in
    loop 0 0
  in
  let min_widths_tab = compute_columns_minimum_sizes hts ncol in
  let max_widths_tab = compute_columns_sizes hts ncol in
  let min_wid = Array.fold_left ( + ) 0 min_widths_tab in
  let max_wid = Array.fold_left ( + ) 0 max_widths_tab in
  (min_wid, max_wid, min_widths_tab, max_widths_tab, ncol)

let print_next_pos conf pos1 pos2 tcol =
  let doit = p_getenv conf.env "notab" = Some "on" in
  let cgl = p_getenv conf.env "cgl" = Some "on" in
  if doit then (
    let dpos =
      match p_getint conf.env "dpos" with Some dpos -> dpos | None -> 78
    in
    let pos1 = match pos1 with Some pos1 -> pos1 | None -> 0 in
    let pos2 = match pos2 with Some pos2 -> pos2 | None -> dpos in
    let overlap =
      let overlap =
        match p_getint conf.env "overlap" with Some x -> x | None -> 10
      in
      min overlap dpos
    in
    let env =
      List.fold_right
        (fun (k, v) env ->
          match k with "pos1" | "pos2" -> env | _ -> (k, v) :: env)
        conf.env []
    in
    let aux env =
      List.iter
        (fun (k, v) ->
          Output.print_sstring conf k;
          Output.print_sstring conf "=";
          Output.print_string conf v;
          Output.print_sstring conf ";")
        env
    in
    Output.print_sstring conf {|<div style="text-align:right">|};
    if pos1 = 0 then Output.print_sstring conf "&nbsp;"
    else if cgl then Output.print_sstring conf "&lt;&lt;"
    else (
      Output.print_sstring conf "<a href=\"";
      Output.print_string conf (commd conf);
      Output.print_sstring conf "\"";
      aux env;
      Output.print_sstring conf "pos1=";
      Output.print_sstring conf (string_of_int @@ (pos1 + overlap - dpos));
      Output.print_sstring conf "&pos2=";
      Output.print_sstring conf (string_of_int @@ (pos1 + overlap));
      Output.print_sstring conf "\">&lt;&lt;</a>");
    if pos2 >= tcol then Output.print_sstring conf "&nbsp;"
    else if cgl then Output.print_sstring conf "&gt;&gt;"
    else (
      Output.print_sstring conf "<a href=\"";
      Output.print_string conf (commd conf);
      aux env;
      Output.print_sstring conf "pos1=";
      Output.print_sstring conf (string_of_int @@ (pos2 - overlap));
      Output.print_sstring conf "&pos2=";
      Output.print_sstring conf (string_of_int @@ (pos2 - overlap + dpos));
      Output.print_sstring conf "\">&gt;&gt;</a>");
    Output.print_sstring conf "</div>")

(* Main print table algorithm with <pre> *)

let print_table_pre conf hts =
  let cgl = p_getenv conf.env "cgl" = Some "on" in
  let displayed_sub s b e =
    displayed_sub (s : Adef.safe_string :> string) b e |> Adef.safe
  in
  let tmincol, tcol, colminsz, colsz, ncol = table_pre_dim hts in
  let dcol =
    let dcol =
      match p_getint conf.env "width" with Some i -> i | None -> 79
    in
    max tmincol (min dcol tcol)
  in
  if tcol > tmincol then
    for i = 0 to ncol - 1 do
      colsz.(i) <-
        colminsz.(i)
        + ((colsz.(i) - colminsz.(i)) * (dcol - tmincol) / (tcol - tmincol))
    done;
  let pos1 = p_getint conf.env "pos1" in
  let pos2 =
    match p_getint conf.env "pos2" with
    | None -> p_getint conf.env "dpos"
    | x -> x
  in
  print_next_pos conf pos1 pos2 (Array.fold_left ( + ) 0 colsz);
  Output.print_sstring conf "<pre>\n";
  for i = 0 to Array.length hts - 1 do
    let stra, max_row =
      let stral, max_row =
        let rec loop stral max_row col j =
          if j = Array.length hts.(i) then (stral, max_row)
          else
            let colspan, _, td = hts.(i).(j) in
            let stra =
              let aux s =
                let rec loop sz k =
                  if k = 0 then sz else loop (sz + colsz.(col + k - 1)) (k - 1)
                in
                loop 0 colspan |> displayed_strip s |> Array.of_list
              in
              match td with
              | TDitem (_ip, s, _t) -> aux s
              | TDtext (_ip, s) -> aux s
              | _ -> [||]
            in
            loop (stra :: stral)
              (max max_row (Array.length stra))
              (col + colspan) (j + 1)
        in
        loop [] 1 0 0
      in
      (Array.of_list (List.rev stral), max_row)
    in
    for row = 0 to max_row - 1 do
      let rec loop pos col j =
        if j = Array.length hts.(i) then Output.print_sstring conf "\n"
        else
          let colspan, _, td = hts.(i).(j) in
          let sz =
            let rec loop sz k =
              if k = 0 then sz else loop (sz + colsz.(col + k - 1)) (k - 1)
            in
            loop 0 colspan
          in
          let outs =
            match td with
            | TDitem _ | TDtext _ ->
                let s =
                  let k =
                    let dk = (max_row - Array.length stra.(j)) / 2 in
                    row - dk
                  in
                  if k >= 0 && k < Array.length stra.(j) then
                    let s = stra.(j).(k) in
                    if (s :> string) = "&nbsp;" then Adef.safe " " else s
                  else try_add_vbar k (Array.length stra.(j)) hts i col
                in
                let len = displayed_length s in
                String.make ((sz - len) / 2) ' '
                ^<^ (s : Adef.safe_string)
                ^>^ String.make (sz - ((sz + len) / 2)) ' '
            | TDnothing ->
                Adef.safe
                @@ String.make ((sz - 1) / 2) ' '
                ^ "&nbsp;"
                ^ String.make (sz - ((sz + 1) / 2)) ' '
            | TDbar s ->
                let s =
                  match s with
                  | None -> Adef.safe "|"
                  | Some s ->
                      let s = (s : Adef.escaped_string :> string) in
                      if s = "" || cgl then Adef.safe "|"
                      else
                        Adef.safe
                        @@ Printf.sprintf
                             "<a style=\"text-decoration:none\" \
                              href=\"%s\">|</a>"
                             s
                in
                let len = displayed_length s in
                String.make ((sz - len) / 2) ' '
                ^<^ s
                ^>^ String.make (sz - ((sz + len) / 2)) ' '
            | TDhr LeftA ->
                let len = (sz + 1) / 2 in
                Adef.safe (String.make len '-' ^ String.make (sz - len) ' ')
            | TDhr RightA ->
                let len = sz / 2 in
                Adef.safe
                  (String.make (sz - len - 1) ' ' ^ String.make (len + 1) '-')
            | TDhr CenterA -> Adef.safe (String.make sz '-')
          in
          let clipped_outs =
            if pos1 = None && pos2 = None then outs
            else
              let pos1 = match pos1 with Some pos1 -> pos1 | None -> pos in
              let pos2 =
                match pos2 with Some pos2 -> pos2 | None -> pos + sz
              in
              if pos + sz <= pos1 then Adef.safe ""
              else if pos > pos2 then Adef.safe ""
              else if pos2 >= pos + sz then
                displayed_sub outs (pos1 - pos) (pos + sz - pos1)
              else if pos1 < pos then displayed_sub outs 0 (pos2 - pos)
              else displayed_sub outs (pos1 - pos) (pos2 - pos1)
          in
          Output.print_string conf clipped_outs;
          loop (pos + sz) (col + colspan) (j + 1)
      in
      loop 0 0 0
    done
  done;
  Output.print_sstring conf "</pre>\n"

(* main *)

let print_html_table conf hts =
  let cgl = p_getenv conf.env "cgl" = Some "on" in
  if Util.p_getenv conf.env "notab" <> Some "on" then
    if cgl then Output.print_sstring conf "<tt>//</tt>"
    else (
      Output.print_sstring conf {|<p><div style="text-align:|};
      Output.print_sstring conf conf.right;
      Output.print_sstring conf {|"><a href="|};
      Output.print_string conf (commd conf);
      List.iter
        (fun (k, v) ->
          Output.print_sstring conf k;
          Output.print_sstring conf "=";
          Output.print_string conf v;
          Output.print_sstring conf ";")
        conf.env;
      Output.print_sstring conf
        {|notab=on&slices=on"><tt>//</tt></a></div></p>|});
  if
    Util.p_getenv conf.env "notab" = Some "on"
    || Util.p_getenv conf.env "pos2" <> None
    || browser_doesnt_have_tables conf
  then print_table_pre conf hts
  else print_table conf hts

let make_tree_hts conf base elem_txt vbar_txt invert set spl d =
  let no_group = p_getenv conf.env "nogroup" = Some "on" in
  let spouse_on =
    match (Util.p_getenv conf.env "sp", Util.p_getenv conf.env "spouse") with
    | Some ("off" | "0"), _ | _, Some "off" -> false
    | _, _ -> true
  in
  let bd = match Util.p_getint conf.env "bd" with Some x -> x | None -> 0 in
  let indi_ip n =
    match n.valu with Left ip -> ip | Right _ -> Driver.Iper.dummy
  in
  let indi_txt n =
    match n.valu with
    | Left ip ->
        let p = pget conf base ip in
        let txt =
          image_txt conf base p ^^^ string_of_item conf base (elem_txt p)
        in
        let spouses =
          if ((spouse_on && n.chil <> []) || n.pare = []) && not invert then
            List.fold_left
              (fun list id ->
                match d.dag.(int_of_idag id).valu with
                | Left cip -> (
                    match Driver.get_parents (pget conf base cip) with
                    | Some ifam ->
                        let cpl = Driver.foi base ifam in
                        if ip = Driver.get_father cpl then
                          if List.mem_assoc (Driver.get_mother cpl) list then
                            list
                          else (Driver.get_mother cpl, Some ifam) :: list
                        else if ip = Driver.get_mother cpl then
                          if List.mem_assoc (Driver.get_father cpl) list then
                            list
                          else (Driver.get_father cpl, Some ifam) :: list
                        else list
                    | None -> list)
                | Right _ -> list)
              [] n.chil
          else if n.chil = [] then
            try [ List.assq ip spl ] with Not_found -> []
          else []
        in
        List.fold_left
          (fun txt (ips, ifamo) ->
            if Pset.mem ips set then txt
            else
              let ps = pget conf base ips in
              let auth =
                authorized_age conf base p && authorized_age conf base ps
              in
              let d =
                match ifamo with
                | Some ifam when auth ->
                    DateDisplay.short_marriage_date_text conf base
                      (Driver.foi base ifam) p ps
                | _ -> Adef.safe ""
              in
              txt ^^^ "<br>&amp;" ^<^ d ^^^ " "
              ^<^ string_of_item conf base (elem_txt ps)
              ^^^ image_txt conf base ps)
          txt spouses
    | Right _ -> Adef.safe "&nbsp;"
  in
  let indi_txt n : Adef.safe_string =
    let bd = match n.valu with Left _ -> bd | _ -> 0 in
    if bd > 0 then
      {|<table border="|} ^<^ string_of_int bd
      ^<^ {|"><tr align="left"><td align="center">|} ^<^ indi_txt n
      ^>^ {|</td></tr></table>|}
    else indi_txt n
  in
  let vbar_txt n =
    match n.valu with Left ip -> vbar_txt ip | _ -> Adef.escaped ""
  in
  let phony n = match n.valu with Left _ -> false | Right _ -> true in
  let t = Dag2html.table_of_dag phony false invert no_group d in
  if Array.length t.table = 0 then [||]
  else Dag2html.html_table_struct indi_ip indi_txt vbar_txt phony d t

let print_slices_menu conf hts =
  let cgl = p_getenv conf.env "cgl" = Some "on" in
  let header n =
    transl_nth conf "display by slices/slice width/overlap/total width" n
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  let title _ = header 0 in
  Hutil.header conf title;
  if cgl then () else Templ.output_simple conf Templ.Env.empty "buttons_rel";
  Output.print_sstring conf {|<form method="get" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|"><p>|};
  hidden_env conf;
  List.iter
    (fun (k, v) -> if k <> "slices" then Util.hidden_input conf k v)
    conf.env;
  Output.print_sstring conf {|</p><table><tr align="left"><td align="right">|};
  transl conf "don't group the common branches together"
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf
    {|<input type="checkbox" name="nogroup" value="on"></td></tr><tr align="left"><td align="right">|};
  header 1;
  Output.print_sstring conf
    {|<input name="dpos" size="5" value="78"></td></tr><tr align="left"><td align="right">|};
  header 2;
  Output.print_sstring conf
    {|<input name="overlap" size="5" value="10"></td></tr><tr align="left"><td align="right">|};
  header 3;
  let wid =
    let min_wid, max_wid, _, _, _ = table_pre_dim hts in
    Output.printf conf "(%d-%d)\n" min_wid max_wid;
    max min_wid (min max_wid 78)
  in
  Output.printf conf "<input name=\"width\" size=\"5\" value=\"%d\">\n" wid;
  Output.print_sstring conf {|</td></tr></table><p>|};
  Output.print_sstring conf
    {|<p><button type="submit" class="btn btn-primary btn-lg">|};
  transl_nth conf "validate/delete" 0
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf {|</button></p></form>|};
  Hutil.trailer conf

let print_dag_page conf page_title hts next_txt =
  let cgl = p_getenv conf.env "cgl" = Some "on" in
  let title _ = if cgl then Output.print_string conf page_title else () in
  Hutil.header conf title;
  (* title goes into <title> ... </title> *)
  (* page <h1> title is handled by buttons_rel!! *)
  if cgl then () else Templ.output_simple conf Templ.Env.empty "buttons_rel";
  print_html_table conf hts;
  if (next_txt : Adef.escaped_string :> string) <> "" then
    if cgl then Output.print_sstring conf {|">&gt;&gt;</p>|}
    else (
      Output.print_sstring conf {|<p><a href="|};
      Output.print_string conf (commd conf);
      Output.print_string conf next_txt;
      Output.print_sstring conf {|">&gt;&gt;</a></p>|});
  Hutil.trailer conf

(* *)

type 'a env =
  | Vbool of bool
  | Vcnt of int ref
  | Vdag of (int * int * int array * int array * int)
  | Vdcell of (int * Dag2html.align * Adef.safe_string Dag2html.table_data)
  | Vdcellp of string
  | Vdline of int
  | Vdlinep of
      (int * Adef.safe_string array array * int * int option * int option)
  | Vint of int
  | Vsstring of Adef.safe_string
  | Vestring of Adef.escaped_string
  | Vind of Driver.person
  | Vlazy of 'a env Lazy.t
  | Vother of 'a
  | Vnone
  | Vvars of (string * string) list ref

let get_env v env =
  try match Templ.Env.find v env with Vlazy l -> Lazy.force l | x -> x
  with Not_found -> Vnone

let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

(* TODO should vl be Vint list and f = max|min, not a string?*)
let eval_predefined_apply f vl =
  let vl =
    List.map
      (function
        | Templ.VVstring "" -> (
            match f with
            | "min" -> max_int
            | "max" -> 0 - max_int
            | _ -> raise Not_found)
        | VVstring s -> int_of_string s
        | _ -> raise Not_found)
      vl
  in
  let f, first_element, l =
    match (f, vl) with
    | "min", s :: sl -> (min, s, sl)
    | "max", s :: sl -> (max, s, sl)
    | _ -> raise Not_found
  in
  try
    let m = List.fold_left (fun acc s -> f acc s) first_element l in
    string_of_int m
  with Failure _ ->
    Logs.syslog `LOG_WARNING "Incorrect parameter for eval_predefined_apply";
    raise Not_found

let parents_access_aux conf base td get_parent =
  match td with
  | TDitem (ip, _, _) | TDtext (ip, _) -> (
      match Driver.get_parents (Driver.poi base ip) with
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          Templ.VVstring
            (Util.acces conf base (Driver.poi base (get_parent cpl)) :> string)
      | None -> VVstring "")
  | _ -> VVstring ""

let has_sibling_aux base td next_or_prev =
  match td with
  | TDitem (ip, _, _) | TDtext (ip, _) -> (
      match Driver.get_parents (Driver.poi base ip) with
      | Some ifam ->
          let sib = Driver.get_children (Driver.foi base ifam) in
          (* array *)
          let i = ref (-1) in
          let _ = Array.iteri (fun n s -> if ip = s then i := n else ()) sib in
          let cond =
            if next_or_prev then !i >= 0 && !i < Array.length sib - 1
            else !i >= 1
          in
          if cond then Some sib.(!i + if next_or_prev then 1 else -1) else None
      | None -> None)
  | _ -> None

let sibling_access_aux conf base td next_or_prev =
  match has_sibling_aux base td next_or_prev with
  | Some s_ip ->
      Templ.VVstring (Util.acces conf base (Driver.poi base s_ip) :> string)
  | None -> raise Not_found

let rec eval_var conf base env _xx _loc = function
  | [ "browsing_with_sosa_ref" ] -> (
      match (Util.p_getenv conf.env "pz", Util.p_getenv conf.env "nz") with
      | Some _, Some _ -> Templ.VVbool true
      | _, _ -> VVbool false)
  | [ "cell_nbr" ] -> (
      match get_env "cell_nbr" env with
      | Vint i -> Templ.VVstring (string_of_int i)
      | _ -> raise Not_found)
  | "dag" :: sl -> (
      match get_env "dag" env with
      | Vdag d -> eval_dag_var conf d sl
      | _ -> raise Not_found)
  | "dag_cell" :: sl -> (
      match get_env "dag_cell" env with
      | Vdcell dcell -> eval_dag_cell_var conf base env dcell sl
      | _ -> raise Not_found)
  | [ "dag_cell_pre" ] -> (
      match get_env "dag_cell_pre" env with
      | Vdcellp s -> VVstring s
      | _ -> raise Not_found)
  | [ "head_title" ] -> (
      match get_env "p_title" env with
      | Vsstring s -> VVstring (s :> string)
      | _ -> VVstring "")
  | [ "is_first" ] -> (
      match get_env "first" env with Vbool b -> VVbool b | _ -> VVbool false)
  | [ "is_last" ] -> (
      match get_env "last" env with Vbool b -> VVbool b | _ -> VVbool false)
  | [ "line_nbr" ] -> (
      match get_env "line_nbr" env with
      | Vint i -> VVstring (string_of_int i)
      | _ -> raise Not_found)
  | [ "link_next" ] -> (
      match get_env "next_txt" env with
      | Vestring s -> VVstring (s :> string)
      | _ -> VVstring "")
  | [ "person_index" ] -> (
      match find_person_in_env conf base "" with
      | Some p -> VVstring (Driver.Iper.to_string (Driver.get_iper p))
      | None -> VVstring "")
  (* person_index.x -> i=, p=, n=, oc= *)
  (* person_index.1 -> i1=, p1=, n1=, oc1= *)
  (* person_index.2 -> i2=, p2=, n2=, oc2= *)
  (* person_index.e -> ei=, ep=, en=, eoc= *)
  | [ "person_index"; x; sl ] -> (
      let find_person =
        match x with "e" -> find_person_in_env_pref | _ -> find_person_in_env
      in
      let s = if x = "x" then "" else x in
      match find_person conf base s with
      | Some p -> eval_person_var conf base p sl
      | None -> VVstring "")
  | [ "person_index"; x ] -> (
      let find_person =
        match x with "e" -> find_person_in_env_pref | _ -> find_person_in_env
      in
      let s = if x = "x" then "" else x in
      match find_person conf base s with
      | Some p -> VVstring (Driver.Iper.to_string (Driver.get_iper p))
      | None -> VVstring "")
  | [ "get_var"; name ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          let vv =
            try List.assoc name !lv with Not_found -> raise Not_found
          in
          VVstring vv
      | _ -> raise Not_found)
  | [ "set_var"; name; value ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          if List.mem_assoc name !lv then lv := List.remove_assoc name !lv;
          lv := (name, value) :: !lv;
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          VVstring ""
      | _ -> raise Not_found)
  (* TODO set real values *)
  | [ "static_max_anc_level" ] -> VVstring "10"
  | [ "static_max_desc_level" ] -> VVstring "10"
  | _ -> raise Not_found

and eval_person_var _conf base p = function
  | "surname" -> VVstring (Driver.sou base (Driver.get_surname p))
  | "first_name" -> VVstring (Driver.sou base (Driver.get_first_name p))
  | _ -> raise Not_found

and eval_dag_var _conf (tmincol, tcol, _colminsz, colsz, _ncol) = function
  | [ "max_wid" ] -> VVstring (string_of_int tcol)
  | [ "min_wid" ] -> VVstring (string_of_int tmincol)
  | [ "ncol" ] -> VVstring (string_of_int (Array.fold_left ( + ) 0 colsz))
  | _ -> raise Not_found

and eval_dag_cell_var conf base env (colspan, align, td) = function
  | [ "access" ] -> (
      match td with
      | TDtext (ip, _s) ->
          VVstring (Util.acces conf base (Driver.poi base ip) :> string)
      | _ -> VVstring "")
  | [ "align" ] -> (
      match align with
      | LeftA -> VVstring conf.left
      | CenterA -> VVstring "center"
      | RightA -> VVstring conf.right)
  | [ "bar_link" ] ->
      VVstring
        (match td with
        | TDbar (Some s) -> (s : Adef.escaped_string :> string)
        | _ -> "")
  | [ "colspan" ] -> VVstring (string_of_int colspan)
  | [ "father"; "access" ] -> parents_access_aux conf base td Driver.get_father
  | [ "has_next_sibling" ] -> (
      match has_sibling_aux base td true with
      | Some _ -> VVbool true
      | None -> VVbool false)
  | [ "has_prev_sibling" ] -> (
      match has_sibling_aux base td false with
      | Some _ -> VVbool true
      | None -> VVbool false)
  | [ "index" ] -> (
      match td with
      | TDitem (ip, _, _) | TDtext (ip, _) ->
          VVstring (Driver.Iper.to_string ip)
      | _ -> VVstring "")
  | [ "is_bar" ] -> VVbool (match td with TDbar _ -> true | _ -> false)
  | [ "is_hr" ] -> (
      match td with
      | TDhr RightA | TDhr LeftA | TDhr CenterA -> VVbool true
      | _ -> VVbool false)
  | [ "is_hr_center" ] -> (
      match td with TDhr CenterA -> VVbool true | _ -> VVbool false)
  | [ "is_hr_left" ] -> (
      match td with TDhr LeftA -> VVbool true | _ -> VVbool false)
  | [ "is_hr_right" ] -> (
      match td with TDhr RightA -> VVbool true | _ -> VVbool false)
  | [ "is_nothing" ] -> VVbool (td = TDnothing)
  | [ "item" ] -> (
      match td with
      | TDitem (_ip, s, _t) -> VVstring (s : Adef.safe_string :> string)
      | _ -> VVstring "")
  | [ "mother"; "access" ] -> parents_access_aux conf base td Driver.get_mother
  | [ "next_sibling"; "access" ] -> sibling_access_aux conf base td true
  | [ "prev_sibling"; "access" ] -> sibling_access_aux conf base td false
  | [ "text" ] -> (
      match td with
      | TDtext (_ip, s) -> VVstring (s : Adef.safe_string :> string)
      | _ -> VVstring "")
  | [ "get_var"; name ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          let vv =
            try List.assoc name !lv with Not_found -> raise Not_found
          in
          VVstring vv
      | _ -> VVstring "")
  | [ "set_var"; name; value ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          if List.mem_assoc name !lv then lv := List.remove_assoc name !lv;
          lv := (name, value) :: !lv;
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          VVstring ""
      | _ -> raise Not_found)
  | _ -> raise Not_found

let rec print_foreach conf hts print_ast _eval_expr env () _loc s sl _el al =
  match s :: sl with
  | [ "dag_cell" ] -> print_foreach_dag_cell hts print_ast env al
  | [ "dag_cell_pre" ] -> print_foreach_dag_cell_pre conf hts print_ast env al
  | [ "dag_line" ] -> print_foreach_dag_line print_ast env hts al
  | [ "dag_line_pre" ] -> print_foreach_dag_line_pre conf hts print_ast env al
  | _ -> raise Not_found

and print_foreach_dag_cell_pre conf hts print_ast env al =
  let i =
    match get_env "dag_line" env with Vdline i -> i | _ -> raise Not_found
  in
  let _, _, _, colsz, _ =
    match get_env "dag" env with Vdag d -> d | _ -> raise Not_found
  in
  let max_row, stra, row, pos1, pos2 =
    match get_env "dag_line_pre" env with
    | Vdlinep x -> x
    | _ -> raise Not_found
  in
  let rec loop pos col j =
    if j = Array.length hts.(i) then ()
    else
      let colspan, _, td = hts.(i).(j) in
      let sz =
        let rec loop sz k =
          if k = 0 then sz else loop (sz + colsz.(col + k - 1)) (k - 1)
        in
        loop 0 colspan
      in
      let outs =
        match td with
        | TDitem _ | TDtext _ ->
            let s =
              let k =
                let dk = (max_row - Array.length stra.(j)) / 2 in
                row - dk
              in
              if k >= 0 && k < Array.length stra.(j) then
                let s = stra.(j).(k) in
                if (s : Adef.safe_string :> string) = "&nbsp;" then
                  Adef.safe " "
                else s
              else try_add_vbar k (Array.length stra.(j)) hts i col
            in
            let len = displayed_length s in
            String.make ((sz - len) / 2) ' '
            ^ (s :> string)
            ^ String.make (sz - ((sz + len) / 2)) ' '
        | TDnothing -> String.make sz ' '
        | TDbar s ->
            let s =
              match (s : Adef.escaped_string option :> string option) with
              | None | Some "" -> "|"
              | Some s ->
                  if p_getenv conf.env "cgl" = Some "on" then s
                  else
                    {|<a style="text-decoration:none" href="|} ^ s ^ {|">|</a>|}
            in
            let len = displayed_length (Adef.safe s) in
            String.make ((sz - len) / 2) ' '
            ^ s
            ^ String.make (sz - ((sz + len) / 2)) ' '
        | TDhr LeftA ->
            let len = (sz + 1) / 2 in
            String.make len '-' ^ String.make (sz - len) ' '
        | TDhr RightA ->
            let len = sz / 2 in
            String.make (sz - len - 1) ' ' ^ String.make (len + 1) '-'
        | TDhr CenterA -> String.make sz '-'
      in
      let clipped_outs =
        if pos1 = None && pos2 = None then outs
        else
          let pos1 = Option.value ~default:pos pos1 in
          let pos2 = Option.value ~default:(pos + sz) pos2 in
          if pos + sz <= pos1 then ""
          else if pos > pos2 then ""
          else if pos2 >= pos + sz then
            displayed_sub outs (pos1 - pos) (pos + sz - pos1)
          else if pos1 < pos then displayed_sub outs 0 (pos2 - pos)
          else displayed_sub outs (pos1 - pos) (pos2 - pos1)
      in
      (if clipped_outs <> "" then
         let v = Vdcellp clipped_outs in
         let env = Templ.Env.add "dag_cell_pre" v env in
         let print_ast = print_ast env () in
         List.iter print_ast al);
      loop (pos + sz) (col + colspan) (j + 1)
  in
  loop 0 0 0

and print_foreach_dag_cell hts print_ast env al =
  match get_env "dag_line" env with
  | Vdline i ->
      for j = 0 to Array.length hts.(i) - 1 do
        let env =
          Templ.Env.(
            env
            |> add "dag_cell" (Vdcell hts.(i).(j))
            |> add "cell_nbr" (Vint j)
            |> add "first" (Vbool (j = 0))
            |> add "last" (Vbool (j = Array.length hts.(i) - 1)))
        in
        List.iter (print_ast env ()) al
      done
  | _ -> raise Not_found

and print_foreach_dag_line print_ast env hts al =
  for i = 0 to Array.length hts - 1 do
    let env =
      Templ.Env.(
        env |> add "dag_line" (Vdline i) |> add "line_nbr" (Vint i)
        |> add "first" (Vbool (i = 0))
        |> add "last" (Vbool (i = Array.length hts - 1)))
    in
    List.iter (print_ast env ()) al
  done

and print_foreach_dag_line_pre conf hts print_ast env al =
  let i =
    match get_env "dag_line" env with Vdline i -> i | _ -> raise Not_found
  in
  let _, _, _, colsz, _ =
    match get_env "dag" env with Vdag d -> d | _ -> raise Not_found
  in
  let stra, max_row =
    let stral, max_row =
      let rec loop stral max_row col j =
        if j = Array.length hts.(i) then (stral, max_row)
        else
          let colspan, _, td = hts.(i).(j) in
          let stra =
            let aux s =
              let sz =
                let rec loop sz k =
                  if k = 0 then sz else loop (sz + colsz.(col + k - 1)) (k - 1)
                in
                loop 0 colspan
              in
              Array.of_list (displayed_strip s sz)
            in
            match td with
            | TDitem (_ip, s, _t) -> aux (s :> Adef.safe_string)
            | TDtext (_ip, s) -> aux s
            | _ -> [||]
          in
          loop (stra :: stral)
            (max max_row (Array.length stra))
            (col + colspan) (j + 1)
      in
      loop [] 1 0 0
    in
    (Array.of_list (List.rev stral), max_row)
  in
  let pos1 = p_getint conf.env "pos1" in
  let pos2 =
    match p_getint conf.env "pos2" with
    | None -> p_getint conf.env "dpos"
    | x -> x
  in
  for row = 0 to max_row - 1 do
    let v = Vdlinep (max_row, stra, row, pos1, pos2) in
    let env = Templ.Env.add "dag_line_pre" v env in
    List.iter (print_ast env ()) al
  done

let old_print_slices_menu_or_dag_page conf page_title hts next_txt =
  if p_getenv conf.env "slices" = Some "on" then print_slices_menu conf hts
  else print_dag_page conf page_title hts next_txt

let print_slices_menu_or_dag_page conf base page_title hts next_txt =
  if p_getenv conf.env "old" = Some "on" then
    old_print_slices_menu_or_dag_page conf page_title hts next_txt
  else
    let p =
      match find_person_in_env conf base "" with
      | Some p -> p
      | None -> Driver.poi base Driver.Iper.dummy
    in
    let env =
      let table_pre_dim () =
        let tmincol, tcol, colminsz, colsz, ncol = table_pre_dim hts in
        let dcol =
          let dcol =
            match p_getint conf.env "width" with Some i -> i | None -> 79
          in
          max tmincol (min dcol tcol)
        in
        if tcol > tmincol then
          for i = 0 to ncol - 1 do
            colsz.(i) <-
              colminsz.(i)
              + (colsz.(i) - colminsz.(i))
                * (dcol - tmincol) / (tcol - tmincol)
          done;
        Vdag (tmincol, tcol, colminsz, colsz, ncol)
      in
      Templ.Env.(
        empty |> add "p" (Vind p)
        |> add "p_auth" (Vbool (authorized_age conf base p))
        |> add "count" (Vcnt (ref 0))
        |> add "count1" (Vcnt (ref 0))
        |> add "count2" (Vcnt (ref 0))
        |> add "count3" (Vcnt (ref 0))
        |> add "vars" (Vvars (ref []))
        |> add "dag" (Vlazy (Lazy.from_fun table_pre_dim))
        |> add "p_title" (Vsstring page_title)
        |> add "next_txt" (Vestring next_txt))
    in
    let ifun =
      Templ.
        {
          eval_var = eval_var conf base;
          eval_transl = (fun _ -> Templ.eval_transl conf);
          eval_predefined_apply = (fun _ -> eval_predefined_apply);
          get_vother;
          set_vother;
          print_foreach = print_foreach conf hts;
        }
    in
    Templ.output conf ifun env () "dag"

let make_and_print_dag conf base elem_txt vbar_txt invert set spl page_title
    next_txt =
  let d = make_dag conf base set in
  let hts = make_tree_hts conf base elem_txt vbar_txt invert set spl d in
  print_slices_menu_or_dag_page conf base page_title hts next_txt

let print conf base =
  let set = get_dag_elems conf base in
  let elem_txt p = Item (p, Adef.safe "") in
  let vbar_txt _ = Adef.escaped "" in
  let invert = Util.p_getenv conf.env "invert" = Some "on" in
  let page_title =
    Util.transl conf "tree" |> Utf8.capitalize_fst |> Adef.safe
  in
  make_and_print_dag conf base elem_txt vbar_txt invert set [] page_title
    (Adef.escaped "")
