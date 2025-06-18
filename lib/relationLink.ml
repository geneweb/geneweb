(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Logs = Geneweb_logs.Logs
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

(* Printing for browsers without tables *)

let pre_text_size txt =
  let txt = (txt : Adef.safe_string :> string) in
  let rec normal len i =
    if i = String.length txt then len
    else if txt.[i] = '<' then in_tag len (i + 1)
    else if txt.[i] = '&' then in_char (len + 1) (i + 1)
    else normal (len + 1) (i + 1)
  and in_tag len i =
    if i = String.length txt then len
    else if txt.[i] = '>' then normal len (i + 1)
    else in_tag len (i + 1)
  and in_char len i =
    if i = String.length txt then len
    else if txt.[i] = ';' then normal len (i + 1)
    else in_char len (i + 1)
  in
  normal 0 0

let print_pre_center conf sz txt =
  for _i = 1 to (sz - pre_text_size txt) / 2 do
    Output.print_sstring conf " "
  done;
  Output.print_string conf txt;
  Output.print_sstring conf "\n"

let print_pre_left conf sz txt =
  let tsz = pre_text_size txt in
  if tsz < (sz / 2) - 1 then
    for _i = 2 to ((sz / 2) - 1 - tsz) / 2 do
      Output.print_sstring conf " "
    done;
  Output.print_sstring conf " ";
  Output.print_string conf txt;
  Output.print_sstring conf "\n"

let print_pre_right conf sz txt =
  let tsz = pre_text_size txt in
  if tsz < (sz / 2) - 1 then (
    for _i = 1 to sz / 2 do
      Output.print_sstring conf " "
    done;
    for _i = 1 to ((sz / 2) - 1 - tsz) / 2 do
      Output.print_sstring conf " "
    done;
    ())
  else
    for _i = 1 to sz - pre_text_size txt - 1 do
      Output.print_sstring conf " "
    done;
  Output.print_sstring conf " ";
  Output.print_string conf txt;
  Output.print_sstring conf "\n"

(* Algorithm *)

type info = {
  ip : Driver.iper;
  sp : sex;
  ip1 : Driver.iper;
  ip2 : Driver.iper;
  b1 : (Driver.iper * sex) list;
  b2 : (Driver.iper * sex) list;
  c1 : int;
  c2 : int;
  pb1 : (Driver.iper * sex) list option;
  pb2 : (Driver.iper * sex) list option;
  nb1 : (Driver.iper * sex) list option;
  nb2 : (Driver.iper * sex) list option;
  sp1 : Driver.person option;
  sp2 : Driver.person option;
  bd : int;
  td_prop : Adef.safe_string;
}

type dist = { mutable dmin : int; mutable dmax : int; mark : bool }

let infinity = 1000
let threshold = ref 10
let phony_dist_tab = ((fun _ -> 0), fun _ -> infinity)

let tsort_leq tstab x y =
  if Collection.Marker.get tstab x = Collection.Marker.get tstab y then x >= y
  else Collection.Marker.get tstab x < Collection.Marker.get tstab y

let make_dist_tab conf base ia maxlev =
  if maxlev <= !threshold then phony_dist_tab
  else
    let tstab = Util.create_topological_sort conf base in
    let module Pq = Pqueue.Make (struct
      type t = Driver.iper

      let leq x y = not (tsort_leq tstab x y)
    end) in
    let default = { dmin = infinity; dmax = 0; mark = false } in
    let dist = Driver.iper_marker (Driver.ipers base) default in
    let q = ref Pq.empty in
    let add_children ip =
      let u = pget conf base ip in
      for i = 0 to Array.length (Driver.get_family u) - 1 do
        let des = Driver.foi base (Driver.get_family u).(i) in
        for j = 0 to Array.length (Driver.get_children des) - 1 do
          let k = (Driver.get_children des).(j) in
          let d = Collection.Marker.get dist k in
          if not d.mark then (
            Collection.Marker.set dist k
            @@ { dmin = infinity; dmax = 0; mark = true };
            q := Pq.add k !q)
        done
      done
    in
    Collection.Marker.set dist ia @@ { dmin = 0; dmax = 0; mark = true };
    add_children ia;
    while not (Pq.is_empty !q) do
      let k, nq = Pq.take !q in
      q := nq;
      match Driver.get_parents (pget conf base k) with
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          let dfath = Collection.Marker.get dist (Driver.get_father cpl) in
          let dmoth = Collection.Marker.get dist (Driver.get_mother cpl) in
          (Collection.Marker.get dist k).dmin <- min dfath.dmin dmoth.dmin + 1;
          (Collection.Marker.get dist k).dmax <- max dfath.dmax dmoth.dmax + 1;
          if (Collection.Marker.get dist k).dmin > maxlev then ()
          else add_children k
      | None -> ()
    done;
    ( (fun ip -> (Collection.Marker.get dist ip).dmin),
      fun ip -> (Collection.Marker.get dist ip).dmax )

let find_first_branch conf base (dmin, dmax) ia =
  let rec find br len ip sp =
    if ip = ia then if len = 0 then Some br else None
    else if len = 0 then None
    else if len < dmin ip || len > dmax ip then None
    else
      match Driver.get_parents (pget conf base ip) with
      | Some ifam -> (
          let cpl = Driver.foi base ifam in
          match
            find ((ip, sp) :: br) (len - 1) (Driver.get_father cpl) Male
          with
          | Some _ as r -> r
          | None ->
              find ((ip, sp) :: br) (len - 1) (Driver.get_mother cpl) Female)
      | None -> None
  in
  find []

let rec next_branch_same_len conf base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    | [] -> None
    | (ip, sp) :: ipl1 -> (
        match sa with
        | Female ->
            next_branch_same_len conf base dist true (missing + 1) ip sp ipl1
        | Male -> (
            match Driver.get_parents (pget conf base ip) with
            | Some ifam ->
                let cpl = Driver.foi base ifam in
                next_branch_same_len conf base dist false missing
                  (Driver.get_mother cpl) Female ipl
            | _ -> failwith "next_branch_same_len")
        | Neuter ->
            Logs.syslog `LOG_CRIT
              (Format.sprintf "sex of %s is Neuter!\n"
                 (Gutil.designation base (Driver.poi base ia)));
            assert false)
  else if missing = 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    next_branch_same_len conf base dist true missing ia sa ipl
  else
    match Driver.get_parents (pget conf base ia) with
    | Some ifam ->
        let cpl = Driver.foi base ifam in
        next_branch_same_len conf base dist false (missing - 1)
          (Driver.get_father cpl) Male ((ia, sa) :: ipl)
    | None -> next_branch_same_len conf base dist true missing ia sa ipl

let find_next_branch conf base dist ia sa ipl =
  let rec loop ia1 sa1 ipl =
    match next_branch_same_len conf base dist true 0 ia1 sa1 ipl with
    | Some (ia1, sa1, ipl) -> if ia = ia1 then Some ipl else loop ia1 sa1 ipl
    | _ -> None
  in
  loop ia sa ipl

let rec prev_branch_same_len conf base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    | [] -> None
    | (ip, sp) :: ipl1 -> (
        match sa with
        | Male ->
            prev_branch_same_len conf base dist true (missing + 1) ip sp ipl1
        | Female -> (
            match Driver.get_parents (pget conf base ip) with
            | Some ifam ->
                let cpl = Driver.foi base ifam in
                prev_branch_same_len conf base dist false missing
                  (Driver.get_father cpl) Male ipl
            | _ -> failwith "prev_branch_same_len")
        | Neuter -> assert false)
  else if missing = 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    prev_branch_same_len conf base dist true missing ia sa ipl
  else
    match Driver.get_parents (pget conf base ia) with
    | Some ifam ->
        let cpl = Driver.foi base ifam in
        prev_branch_same_len conf base dist false (missing - 1)
          (Driver.get_mother cpl) Female ((ia, sa) :: ipl)
    | None -> prev_branch_same_len conf base dist true missing ia sa ipl

let find_prev_branch conf base dist ia sa ipl =
  let rec loop ia1 sa1 ipl =
    match prev_branch_same_len conf base dist true 0 ia1 sa1 ipl with
    | Some (ia1, sa1, ipl) -> if ia = ia1 then Some ipl else loop ia1 sa1 ipl
    | _ -> None
  in
  loop ia sa ipl

(* Printing *)

let someone_text conf base ip =
  let p = pget conf base ip in
  referenced_person_title_text conf base p
  ^^^ DateDisplay.short_dates_text conf base p

let spouse_text conf base end_sp ip ipl =
  match (ipl, (p_getenv conf.env "sp", p_getenv conf.env "opt")) with
  | (ips, _) :: _, (None, _ | _, Some "spouse") -> (
      let a = pget conf base ips in
      match Driver.get_parents a with
      | Some ifam ->
          let fam = Driver.foi base ifam in
          let sp =
            if ip = Driver.get_father fam then Driver.get_mother fam
            else Driver.get_father fam
          in
          let d =
            DateDisplay.short_marriage_date_text conf base fam
              (pget conf base (Driver.get_father fam))
              (pget conf base (Driver.get_mother fam))
          in
          (someone_text conf base sp, d, Some sp)
      | _ -> (Adef.safe "", Adef.safe "", None))
  | [], _ -> (
      match end_sp with
      | Some p ->
          ( someone_text conf base (Driver.get_iper p),
            Adef.safe "",
            Some (Driver.get_iper p) )
      | _ -> (Adef.safe "", Adef.safe "", None))
  | _ -> (Adef.safe "", Adef.safe "", None)

let print_someone_and_spouse conf base info in_tab ip n ipl =
  let s, d, spo = spouse_text conf base n ip ipl in
  if in_tab && (info.bd > 0 || (info.td_prop :> string) <> "") then (
    Output.print_sstring conf {|<table style="border:|};
    Output.print_sstring conf (string_of_int info.bd);
    Output.print_sstring conf {|px solid"|};
    Output.print_string conf info.td_prop;
    Output.print_sstring conf {|><tr><td align="center">|});
  Output.print_string conf (someone_text conf base ip);
  Output.print_string conf (DagDisplay.image_txt conf base (pget conf base ip));
  if (s :> string) <> "" then (
    Output.print_sstring conf "<br>&amp;";
    Output.print_string conf d;
    Output.print_sstring conf " ";
    Output.print_string conf s;
    match spo with
    | Some ip ->
        Output.print_string conf
          (DagDisplay.image_txt conf base (pget conf base ip))
    | _ -> ());
  if in_tab && (info.bd > 0 || (info.td_prop :> string) <> "") then
    Output.print_sstring conf "</td></tr></table>"

let rec print_both_branches conf base info pl1 pl2 =
  if pl1 = [] && pl2 = [] then ()
  else
    let p1, pl1 =
      match pl1 with (p1, _) :: pl1 -> (Some p1, pl1) | [] -> (None, [])
    in
    let p2, pl2 =
      match pl2 with (p2, _) :: pl2 -> (Some p2, pl2) | [] -> (None, [])
    in
    Output.print_sstring conf {|<tr align="|};
    Output.print_sstring conf conf.left;
    Output.print_sstring conf {|">|};
    Output.print_sstring conf {|<td align="center">|};
    Output.print_sstring conf (if p1 <> None then "|" else "&nbsp;");
    Output.print_sstring conf {|</td><td>&nbsp;</td><td align="center">|};
    Output.print_sstring conf (if p2 <> None then "|" else "&nbsp;");
    Output.print_sstring conf {|</td></tr><tr align="|};
    Output.print_sstring conf conf.left;
    Output.print_sstring conf {|"><td valign="top" align="center">|};
    (match p1 with
    | Some p1 -> print_someone_and_spouse conf base info true p1 info.sp1 pl1
    | None -> Output.print_sstring conf "&nbsp;");
    Output.print_sstring conf
      {|</td><td>&nbsp;</td><td valign="top" align="center">|};
    (match p2 with
    | Some p2 -> print_someone_and_spouse conf base info true p2 info.sp2 pl2
    | None -> Output.print_sstring conf "&nbsp;");
    Output.print_sstring conf "</td></tr>";
    print_both_branches conf base info pl1 pl2

let rec print_both_branches_pre conf base info sz pl1 pl2 =
  if pl1 = [] && pl2 = [] then ()
  else
    let p1, pl1 =
      match pl1 with (p1, _) :: pl1 -> (Some p1, pl1) | [] -> (None, [])
    in
    let p2, pl2 =
      match pl2 with (p2, _) :: pl2 -> (Some p2, pl2) | [] -> (None, [])
    in
    let s1 = if p1 <> None then "|" else " " in
    let s2 = if p2 <> None then "|" else " " in
    print_pre_center conf sz (Adef.safe @@ s1 ^ String.make (sz / 2) ' ' ^ s2);
    (match p1 with
    | Some p1 ->
        print_pre_left conf sz (someone_text conf base p1);
        let s, d, _ = spouse_text conf base info.sp1 p1 pl1 in
        if (s : Adef.safe_string :> string) <> "" then
          print_pre_left conf sz ("&amp;" ^<^ d ^^^ " " ^<^ s)
    | None -> Output.print_sstring conf "\n");
    (match p2 with
    | Some p2 ->
        print_pre_right conf sz (someone_text conf base p2);
        let s, d, _ = spouse_text conf base info.sp2 p2 pl2 in
        if (s : Adef.safe_string :> string) <> "" then
          print_pre_right conf sz ("&amp;" ^<^ d ^^^ " " ^<^ s)
    | None -> Output.print_sstring conf "\n");
    print_both_branches_pre conf base info sz pl1 pl2

let include_marr conf base (n : Adef.escaped_string) =
  match find_person_in_env conf base (n :> string) with
  | Some p -> "&" ^<^ acces_n conf base n p
  | None -> Adef.escaped ""

let sign_text conf base sign info b1 b2 c1 c2 =
  let sps = Util.get_opt conf "sp" true in
  let img = Util.get_opt conf "im" true in
  let href =
    commd conf ^^^ "m=RL&"
    ^<^ acces_n conf base (Adef.escaped "1") (pget conf base info.ip1)
    ^^^ "&"
    ^<^ acces_n conf base (Adef.escaped "2") (pget conf base info.ip2)
    ^^^ "&b1="
    ^<^ Sosa.to_string (old_sosa_of_branch conf base ((info.ip, info.sp) :: b1))
    ^<^ "&b2="
    ^<^ Sosa.to_string (old_sosa_of_branch conf base ((info.ip, info.sp) :: b2))
    ^<^ "&c1=" ^<^ string_of_int c1 ^<^ "&c2=" ^<^ string_of_int c2
    ^<^ Adef.escaped (if sps then "" else "&sp=0")
    ^^^ Adef.escaped (if img then "" else "&im=0")
    ^^^ (match p_getenv conf.env "bd" with
        | None | Some ("0" | "") -> Adef.escaped ""
        | Some x -> "&bd=" ^<^ (Mutil.encode x :> Adef.escaped_string))
    ^^^ (match p_getenv conf.env "color" with
        | None | Some "" -> Adef.escaped ""
        | Some x -> "&color=" ^<^ (Mutil.encode x :> Adef.escaped_string))
    ^^^ include_marr conf base (Adef.escaped "3")
    ^^^ include_marr conf base (Adef.escaped "4")
  in
  "<a href=\""
  ^<^ (href : Adef.escaped_string :> Adef.safe_string)
  ^^^ "\">"
  ^<^ (sign : Adef.safe_string)
  ^>^ "</a>"

let prev_next_1_text conf base info pb nb =
  let s =
    match pb with
    | Some b1 ->
        sign_text conf base (Adef.safe "&lt;&lt;") info b1 info.b2 (info.c1 - 1)
          info.c2
        ^>^ "\n"
    | _ -> Adef.safe ""
  in
  let s =
    match (pb, nb) with
    | None, None -> s
    | _ ->
        s ^>^ "<span style=\"font-size:80%\">" ^ string_of_int info.c1
        ^ "</span>"
  in
  match nb with
  | Some b1 ->
      s ^^^ "\n"
      ^<^ sign_text conf base (Adef.safe "&gt;&gt;") info b1 info.b2
            (info.c1 + 1) info.c2
  | _ -> s

let prev_next_2_text conf base info pb nb =
  let s =
    match pb with
    | Some b2 ->
        sign_text conf base (Adef.safe "&lt;&lt;") info info.b1 b2 info.c1
          (info.c2 - 1)
        ^>^ "\n"
    | _ -> Adef.safe ""
  in
  let s =
    match (pb, nb) with
    | None, None -> s
    | _ ->
        s ^>^ "<span style=\"font-size:80%\">" ^ string_of_int info.c2
        ^ "</span>"
  in
  match nb with
  | Some b2 ->
      s ^^^ "\n"
      ^<^ sign_text conf base (Adef.safe "&gt;&gt;") info info.b1 b2 info.c1
            (info.c2 + 1)
  | _ -> s

let print_prev_next_1 conf base info pb nb =
  Output.print_string conf (prev_next_1_text conf base info pb nb);
  Output.print_sstring conf "\n"

let print_prev_next_2 conf base info pb nb =
  Output.print_string conf (prev_next_2_text conf base info pb nb);
  Output.print_sstring conf "\n"

let other_parent_text_if_same conf base info =
  match (info.b1, info.b2) with
  | (sib1, _) :: _, (sib2, _) :: _ -> (
      match
        ( Driver.get_parents (pget conf base sib1),
          Driver.get_parents (pget conf base sib2) )
      with
      | Some ifam1, Some ifam2 -> (
          let cpl1 = Driver.foi base ifam1 in
          let cpl2 = Driver.foi base ifam2 in
          let other_parent =
            if Driver.get_father cpl1 = info.ip then
              if Driver.get_mother cpl1 = Driver.get_mother cpl2 then
                Some (Driver.get_mother cpl1)
              else None
            else if Driver.get_father cpl1 = Driver.get_father cpl2 then
              Some (Driver.get_father cpl1)
            else None
          in
          match other_parent with
          | Some ip ->
              let d =
                DateDisplay.short_marriage_date_text conf base
                  (Driver.foi base ifam1)
                  (pget conf base (Driver.get_father cpl1))
                  (pget conf base (Driver.get_mother cpl1))
              in
              Some ("&amp;" ^<^ d ^^^ " " ^<^ someone_text conf base ip, ip)
          | _ -> None)
      | _ -> None)
  | _ -> None

let print_someone_and_other_parent_if_same conf base info =
  if info.bd > 0 || (info.td_prop :> string) <> "" then (
    Output.print_sstring conf {|<table style="border:|};
    Output.print_sstring conf (string_of_int info.bd);
    Output.print_sstring conf {|px solid"|};
    Output.print_string conf info.td_prop;
    Output.print_sstring conf {|><tr><td align="center">|});
  Output.print_string conf (someone_text conf base info.ip);
  Output.print_sstring conf "\n";
  Output.print_string conf
    (DagDisplay.image_txt conf base (pget conf base info.ip));
  (match other_parent_text_if_same conf base info with
  | Some (s, ip) ->
      Output.print_sstring conf "<br>";
      Output.print_string conf s;
      Output.print_string conf
        (DagDisplay.image_txt conf base (pget conf base ip))
  | None -> ());
  if info.bd > 0 || (info.td_prop :> string) <> "" then
    Output.print_sstring conf "</td></tr></table>"

let rec list_iter_hd_tl f = function
  | x :: l ->
      f x l;
      list_iter_hd_tl f l
  | [] -> ()

let print_one_branch_no_table conf base info =
  let b = if info.b1 = [] then info.b2 else info.b1 in
  let sp = if info.b1 = [] then info.sp2 else info.sp1 in
  Output.print_sstring conf "<div style=\"text-align:center\">\n";
  print_someone_and_spouse conf base info false info.ip sp b;
  Output.print_sstring conf "<br>\n";
  list_iter_hd_tl
    (fun (ip1, _) ipl1 ->
      Output.print_sstring conf "|";
      Output.print_sstring conf "<br>\n";
      print_someone_and_spouse conf base info false ip1 sp ipl1;
      Output.print_sstring conf "<br>\n")
    b;
  Output.print_sstring conf "</div>\n"

let print_one_branch_with_table conf base info =
  let b = if info.b1 = [] then info.b2 else info.b1 in
  let sp = if info.b1 = [] then info.sp2 else info.sp1 in
  Output.printf conf
    "<table border=\"%d\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%%\">\n"
    conf.border;
  Output.print_sstring conf "<tr>\n";
  Output.print_sstring conf "<td align=\"center\">\n";
  print_someone_and_spouse conf base info true info.ip sp b;
  Output.print_sstring conf "</td>\n";
  list_iter_hd_tl
    (fun (ip1, _) ipl1 ->
      Output.print_sstring conf "<tr>\n";
      Output.print_sstring conf "<td align=\"center\">\n";
      Output.print_sstring conf "|";
      Output.print_sstring conf "</td>\n";
      Output.print_sstring conf "</tr>\n";
      Output.print_sstring conf "<tr>\n";
      Output.print_sstring conf "<td align=\"center\">\n";
      print_someone_and_spouse conf base info true ip1 sp ipl1;
      Output.print_sstring conf "</td>\n";
      Output.print_sstring conf "</tr>\n")
    b;
  Output.print_sstring conf "</tr>\n";
  Output.print_sstring conf "</table>\n"

let print_two_branches_with_pre conf base info =
  let sz = 79 in
  Output.print_sstring conf "<pre>\n";
  print_pre_center conf sz (someone_text conf base info.ip);
  (match other_parent_text_if_same conf base info with
  | Some (s, _) -> print_pre_center conf sz s
  | None -> ());
  print_pre_center conf sz (Adef.safe "|");
  print_pre_center conf sz (Adef.safe @@ String.make (sz / 2) '_');
  print_both_branches_pre conf base info sz info.b1 info.b2;
  if
    info.pb1 <> None || info.nb1 <> None || info.pb2 <> None || info.nb2 <> None
  then (
    Output.print_sstring conf "\n";
    (if info.pb1 <> None || info.nb1 <> None then
       let s = prev_next_1_text conf base info info.pb1 info.nb1 in
       print_pre_left conf sz s);
    if info.pb2 <> None || info.nb2 <> None then
      let s = prev_next_2_text conf base info info.pb2 info.nb2 in
      print_pre_right conf sz s);
  Output.print_sstring conf "</pre>\n"

let print_two_branches_with_table conf base info =
  Output.printf conf
    "<table border=\"%d\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%%\">\n"
    conf.border;
  Output.printf conf "<tr align=\"%s\">\n" "left";
  Output.print_sstring conf "<td colspan=\"3\" align=\"center\">";
  print_someone_and_other_parent_if_same conf base info;
  Output.print_sstring conf "</td>";
  Output.print_sstring conf "</tr>\n";
  Output.printf conf "<tr align=\"%s\">\n" "left";
  Output.print_sstring conf "<td colspan=\"3\" align=\"center\">";
  Output.print_sstring conf "|";
  Output.print_sstring conf "</td>";
  Output.print_sstring conf "</tr>\n";
  Output.printf conf "<tr align=\"%s\">\n" "left";
  Output.printf conf "<td align=\"%s\">" conf.right;
  Output.printf conf "<hr class=\"%s\">\n" conf.right;
  Output.print_sstring conf "</td>\n";
  Output.print_sstring conf "<td>";
  Output.print_sstring conf "<hr class=\"full\">\n";
  Output.print_sstring conf "</td>\n";
  Output.printf conf "<td align=\"%s\">" conf.left;
  Output.printf conf "<hr class=\"%s\">\n" conf.left;
  Output.print_sstring conf "</td>\n";
  Output.print_sstring conf "</tr>\n";
  print_both_branches conf base info info.b1 info.b2;
  if
    info.pb1 <> None || info.nb1 <> None || info.pb2 <> None || info.nb2 <> None
  then (
    Output.print_sstring conf {|<tr align="left"><td>|};
    if info.pb1 <> None || info.nb1 <> None then (
      Output.print_sstring conf "<br>";
      print_prev_next_1 conf base info info.pb1 info.nb1)
    else Output.print_sstring conf "&nbsp;";
    Output.print_sstring conf "</td><td>&nbsp;</td><td>";
    if info.pb2 <> None || info.nb2 <> None then (
      Output.print_sstring conf "<br>";
      print_prev_next_2 conf base info info.pb2 info.nb2)
    else Output.print_sstring conf "&nbsp;";
    Output.print_sstring conf "</td></tr>");
  Output.print_sstring conf "</table>"

let print_relation_path conf base info =
  let with_table =
    match p_getenv conf.env "tab" with
    | Some "on" -> true
    | Some "off" -> false
    | _ -> not (browser_doesnt_have_tables conf)
  in
  if info.b1 = [] || info.b2 = [] then (
    if (info.bd > 0 || (info.td_prop :> string) <> "") && with_table then
      print_one_branch_with_table conf base info
    else print_one_branch_no_table conf base info;
    if
      info.pb1 <> None || info.nb1 <> None || info.pb2 <> None
      || info.nb2 <> None
    then (
      Output.print_sstring conf "<p>";
      if info.pb1 <> None || info.nb1 <> None then
        print_prev_next_1 conf base info info.pb1 info.nb1;
      if info.pb2 <> None || info.nb2 <> None then
        print_prev_next_2 conf base info info.pb2 info.nb2;
      Output.print_sstring conf "</p>"))
  else if with_table then print_two_branches_with_table conf base info
  else print_two_branches_with_pre conf base info

let print_relation_ok conf base info =
  let title _ =
    transl_nth conf "relationship link/relationship links" 0
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    (match (info.pb1, info.nb1) with
    | None, None -> ()
    | _ ->
        Output.print_sstring conf " ";
        Output.print_sstring conf (string_of_int info.c1));
    match (info.pb2, info.nb2) with
    | None, None -> ()
    | _ ->
        Output.print_sstring conf " ";
        Output.print_sstring conf (string_of_int info.c2)
  in
  Hutil.header conf title;
  (match p_getenv conf.env "cgl" with
  | Some "on" -> ()
  | _ ->
      let conf = { conf with is_printed_by_template = false } in
      Templ.output_simple conf Templ.Env.empty "buttons_rel");
  Output.print_sstring conf {|<p style="clear:both">|};
  print_relation_path conf base info;
  Hutil.trailer conf

let print_relation_no_dag conf base po ip1 ip2 =
  let params =
    match (po, p_getint conf.env "l1", p_getint conf.env "l2") with
    | Some p, Some l1, Some l2 ->
        let ip = Driver.get_iper p in
        let dist = make_dist_tab conf base ip (max l1 l2 + 1) in
        let b1 = find_first_branch conf base dist ip l1 ip1 Neuter in
        let b2 = find_first_branch conf base dist ip l2 ip2 Neuter in
        Some (ip, Driver.get_sex (pget conf base ip), dist, b1, b2, 1, 1)
    | _ -> (
        match (p_getenv conf.env "b1", p_getenv conf.env "b2") with
        | Some b1str, Some b2str -> (
            let n1 = Sosa.of_string b1str in
            let n2 = Sosa.of_string b2str in
            match
              ( old_branch_of_sosa conf base ip1 n1,
                old_branch_of_sosa conf base ip2 n2 )
            with
            | Some ((ia1, sa1) :: b1), Some ((ia2, _) :: b2) ->
                if ia1 = ia2 then
                  let c1 =
                    match p_getint conf.env "c1" with Some n -> n | None -> 0
                  in
                  let c2 =
                    match p_getint conf.env "c2" with Some n -> n | None -> 0
                  in
                  let dist =
                    if c1 > 0 || c2 > 0 then
                      let maxlev = max (List.length b1) (List.length b2) + 1 in
                      make_dist_tab conf base ia1 maxlev
                    else phony_dist_tab
                  in
                  Some (ia1, sa1, dist, Some b1, Some b2, c1, c2)
                else None
            | _ -> None)
        | _ -> None)
  in
  match params with
  | Some (ip, sp, dist, Some b1, Some b2, c1, c2) ->
      let pb1 =
        if c1 <= 1 then None else find_prev_branch conf base dist ip sp b1
      in
      let nb1 =
        if c1 = 0 then None else find_next_branch conf base dist ip sp b1
      in
      let pb2 =
        if c2 <= 1 then None else find_prev_branch conf base dist ip sp b2
      in
      let nb2 =
        if c2 = 0 then None else find_next_branch conf base dist ip sp b2
      in
      let sp1 = find_person_in_env conf base "3" in
      let sp2 = find_person_in_env conf base "4" in
      let bd = match p_getint conf.env "bd" with Some x -> x | None -> 0 in
      let td_prop =
        match Util.p_getenv conf.env "color" with
        | None | Some "" -> Adef.safe ""
        | Some x ->
            (" class=\"" ^<^ Mutil.encode x ^>^ "\"" :> Adef.safe_string)
      in
      let info =
        {
          ip;
          sp;
          ip1;
          ip2;
          b1;
          b2;
          c1;
          c2;
          pb1;
          pb2;
          nb1;
          nb2;
          sp1;
          sp2;
          bd;
          td_prop;
        }
      in
      print_relation_ok conf base info
  | _ ->
      Hutil.incorrect_request conf
        ~comment:"relationLink: print_relation_no_dag failed"

let print_relation_dag conf base a ip1 ip2 l1 l2 =
  let ia = Driver.get_iper a in
  let add_branches dist set n ip l =
    let b = find_first_branch conf base dist ia l ip Neuter in
    let rec loop set n b =
      if n > 100 then raise Exit
      else
        match b with
        | Some b ->
            let set =
              List.fold_left (fun set (ip, _) -> Dag.Pset.add ip set) set b
            in
            loop set (n + 1)
              (find_next_branch conf base dist ia (Driver.get_sex a) b)
        | None -> (set, n)
    in
    loop set n b
  in
  try
    let set =
      List.fold_left
        (fun set l1 ->
          List.fold_left
            (fun set l2 ->
              let dist = make_dist_tab conf base ia (max l1 l2 + 1) in
              let set, n = add_branches dist set 0 ip1 l1 in
              let set, _ = add_branches dist set n ip2 l2 in
              set)
            set l2)
        (Dag.Pset.add ia Dag.Pset.empty)
        l1
    in
    let spl =
      List.fold_right
        (fun (ip, s) spl ->
          match find_person_in_env conf base s with
          | Some sp -> (ip, (Driver.get_iper sp, None)) :: spl
          | None -> spl)
        [ (ip1, "3"); (ip2, "4") ]
        []
    in
    let elem_txt p = DagDisplay.Item (p, Adef.safe "") in
    let vbar_txt _ = Adef.escaped "" in
    let invert =
      match Util.p_getenv conf.env "invert" with
      | Some "on" -> true
      | _ -> false
    in
    let page_title =
      Util.transl conf "tree" |> Utf8.capitalize_fst |> Adef.safe
    in
    DagDisplay.make_and_print_dag conf base elem_txt vbar_txt invert set spl
      page_title (Adef.escaped "")
  with Exit ->
    Hutil.incorrect_request conf
      ~comment:"relationLink: print_relation_dag failed"

let int_list s =
  let rec loop i n =
    if i = String.length s then [ n ]
    else
      match s.[i] with
      | '0' .. '9' as d -> loop (i + 1) ((n * 10) + Char.code d - Char.code '0')
      | _ -> n :: loop (i + 1) 0
  in
  loop 0 0

let print_relation conf base p1 p2 =
  let l1 = p_getenv conf.env "l1" in
  let l2 = p_getenv conf.env "l2" in
  let po = find_person_in_env conf base "" in
  match (p_getenv conf.env "dag", po, l1, l2) with
  | Some "on", Some p, Some l1, Some l2 ->
      print_relation_dag conf base p (Driver.get_iper p1) (Driver.get_iper p2)
        (int_list l1) (int_list l2)
  | _ ->
      print_relation_no_dag conf base po (Driver.get_iper p1)
        (Driver.get_iper p2)

let print conf base =
  match
    (find_person_in_env conf base "1", find_person_in_env conf base "2")
  with
  | Some p1, Some p2 -> print_relation conf base p1 p2
  | _ -> Hutil.incorrect_request conf ~comment:"relationLink: p1, p2 missing"
