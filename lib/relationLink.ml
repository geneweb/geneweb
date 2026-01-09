(* Copyright (c) 1998-2007 INRIA *)

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
  ip : Gwdb.iper;
  sp : Def.sex;
  ip1 : Gwdb.iper;
  ip2 : Gwdb.iper;
  b1 : (Gwdb.iper * Def.sex) list;
  b2 : (Gwdb.iper * Def.sex) list;
  c1 : int;
  c2 : int;
  pb1 : (Gwdb.iper * Def.sex) list option;
  pb2 : (Gwdb.iper * Def.sex) list option;
  nb1 : (Gwdb.iper * Def.sex) list option;
  nb2 : (Gwdb.iper * Def.sex) list option;
  sp1 : Gwdb.person option;
  sp2 : Gwdb.person option;
  bd : int;
  td_prop : Adef.safe_string;
}

type dist = { mutable dmin : int; mutable dmax : int; mark : bool }

let infinity = 1000
let threshold = ref 10
let phony_dist_tab = ((fun _ -> 0), fun _ -> infinity)

let tsort_leq tstab x y =
  if Gwdb.Marker.get tstab x = Gwdb.Marker.get tstab y then x >= y
  else Gwdb.Marker.get tstab x < Gwdb.Marker.get tstab y

let make_dist_tab conf base ia maxlev =
  if maxlev <= !threshold then phony_dist_tab
  else
    let tstab = Util.create_topological_sort conf base in
    let module Pq = Pqueue.Make (struct
      type t = Gwdb.iper

      let leq x y = not (tsort_leq tstab x y)
    end) in
    let default = { dmin = infinity; dmax = 0; mark = false } in
    let dist = Gwdb.iper_marker (Gwdb.ipers base) default in
    let q = ref Pq.empty in
    let add_children ip =
      let u = Util.pget conf base ip in
      for i = 0 to Array.length (Gwdb.get_family u) - 1 do
        let des = Gwdb.foi base (Gwdb.get_family u).(i) in
        for j = 0 to Array.length (Gwdb.get_children des) - 1 do
          let k = (Gwdb.get_children des).(j) in
          let d = Gwdb.Marker.get dist k in
          if not d.mark then (
            Gwdb.Marker.set dist k @@ { dmin = infinity; dmax = 0; mark = true };
            q := Pq.add k !q)
        done
      done
    in
    Gwdb.Marker.set dist ia @@ { dmin = 0; dmax = 0; mark = true };
    add_children ia;
    while not (Pq.is_empty !q) do
      let k, nq = Pq.take !q in
      q := nq;
      match Gwdb.get_parents (Util.pget conf base k) with
      | Some ifam ->
          let cpl = Gwdb.foi base ifam in
          let dfath = Gwdb.Marker.get dist (Gwdb.get_father cpl) in
          let dmoth = Gwdb.Marker.get dist (Gwdb.get_mother cpl) in
          (Gwdb.Marker.get dist k).dmin <- min dfath.dmin dmoth.dmin + 1;
          (Gwdb.Marker.get dist k).dmax <- max dfath.dmax dmoth.dmax + 1;
          if (Gwdb.Marker.get dist k).dmin > maxlev then () else add_children k
      | None -> ()
    done;
    ( (fun ip -> (Gwdb.Marker.get dist ip).dmin),
      fun ip -> (Gwdb.Marker.get dist ip).dmax )

let find_first_branch conf base (dmin, dmax) ia =
  let rec find br len ip sp =
    if ip = ia then if len = 0 then Some br else None
    else if len = 0 then None
    else if len < dmin ip || len > dmax ip then None
    else
      match Gwdb.get_parents (Util.pget conf base ip) with
      | Some ifam -> (
          let cpl = Gwdb.foi base ifam in
          match
            find ((ip, sp) :: br) (len - 1) (Gwdb.get_father cpl) Def.Male
          with
          | Some _ as r -> r
          | None -> find ((ip, sp) :: br) (len - 1) (Gwdb.get_mother cpl) Female
          )
      | None -> None
  in
  find []

let rec next_branch_same_len conf base dist backward missing ia sa ipl =
  if backward then
    match ipl with
    | [] -> None
    | (ip, sp) :: ipl1 -> (
        match sa with
        | Def.Female ->
            next_branch_same_len conf base dist true (missing + 1) ip sp ipl1
        | Male -> (
            match Gwdb.get_parents (Util.pget conf base ip) with
            | Some ifam ->
                let cpl = Gwdb.foi base ifam in
                next_branch_same_len conf base dist false missing
                  (Gwdb.get_mother cpl) Female ipl
            | _ -> failwith "next_branch_same_len")
        | Neuter -> assert false)
  else if missing = 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    next_branch_same_len conf base dist true missing ia sa ipl
  else
    match Gwdb.get_parents (Util.pget conf base ia) with
    | Some ifam ->
        let cpl = Gwdb.foi base ifam in
        next_branch_same_len conf base dist false (missing - 1)
          (Gwdb.get_father cpl) Male ((ia, sa) :: ipl)
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
        | Def.Male ->
            prev_branch_same_len conf base dist true (missing + 1) ip sp ipl1
        | Female -> (
            match Gwdb.get_parents (Util.pget conf base ip) with
            | Some ifam ->
                let cpl = Gwdb.foi base ifam in
                prev_branch_same_len conf base dist false missing
                  (Gwdb.get_father cpl) Male ipl
            | _ -> failwith "prev_branch_same_len")
        | Neuter -> assert false)
  else if missing = 0 then Some (ia, sa, ipl)
  else if missing < fst dist ia || missing > snd dist ia then
    prev_branch_same_len conf base dist true missing ia sa ipl
  else
    match Gwdb.get_parents (Util.pget conf base ia) with
    | Some ifam ->
        let cpl = Gwdb.foi base ifam in
        prev_branch_same_len conf base dist false (missing - 1)
          (Gwdb.get_mother cpl) Female ((ia, sa) :: ipl)
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
  let p = Util.pget conf base ip in
  let open Def in
  NameDisplay.referenced_person_title_text conf base p
  ^^^ DateDisplay.short_dates_text conf base p

let spouse_text conf base end_sp ip ipl =
  match
    ( ipl,
      ( Util.p_getenv conf.Config.env "spouse",
        Util.p_getenv conf.Config.env "opt" ) )
  with
  | (ips, _) :: _, (Some "on", _ | _, Some "spouse") -> (
      let a = Util.pget conf base ips in
      match Gwdb.get_parents a with
      | Some ifam ->
          let fam = Gwdb.foi base ifam in
          let sp =
            if ip = Gwdb.get_father fam then Gwdb.get_mother fam
            else Gwdb.get_father fam
          in
          let d =
            DateDisplay.short_marriage_date_text conf base fam
              (Util.pget conf base (Gwdb.get_father fam))
              (Util.pget conf base (Gwdb.get_mother fam))
          in
          (someone_text conf base sp, d, Some sp)
      | _ -> (Adef.safe "", Adef.safe "", None))
  | [], _ -> (
      match end_sp with
      | Some p ->
          ( someone_text conf base (Gwdb.get_iper p),
            Adef.safe "",
            Some (Gwdb.get_iper p) )
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
  Output.print_string conf
    (DagDisplay.image_txt conf base (Util.pget conf base ip));
  if (s :> string) <> "" then (
    Output.print_sstring conf "<br>&amp;";
    Output.print_string conf d;
    Output.print_sstring conf " ";
    Output.print_string conf s;
    match spo with
    | Some ip ->
        Output.print_string conf
          (DagDisplay.image_txt conf base (Util.pget conf base ip))
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
    Output.print_sstring conf conf.Config.left;
    Output.print_sstring conf {|">|};
    Output.print_sstring conf {|<td align="center">|};
    Output.print_sstring conf (if p1 <> None then "|" else "&nbsp;");
    Output.print_sstring conf {|</td><td>&nbsp;</td><td align="center">|};
    Output.print_sstring conf (if p2 <> None then "|" else "&nbsp;");
    Output.print_sstring conf {|</td></tr><tr align="|};
    Output.print_sstring conf conf.Config.left;
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
          let open Def in
          print_pre_left conf sz ("&amp;" ^<^ d ^^^ " " ^<^ s)
    | None -> Output.print_sstring conf "\n");
    (match p2 with
    | Some p2 ->
        print_pre_right conf sz (someone_text conf base p2);
        let s, d, _ = spouse_text conf base info.sp2 p2 pl2 in
        if (s : Adef.safe_string :> string) <> "" then
          let open Def in
          print_pre_right conf sz ("&amp;" ^<^ d ^^^ " " ^<^ s)
    | None -> Output.print_sstring conf "\n");
    print_both_branches_pre conf base info sz pl1 pl2

let include_marr conf base (n : Adef.escaped_string) =
  match Util.find_person_in_env conf base (n :> string) with
  | Some p ->
      let open Def in
      "&" ^<^ Util.acces_n conf base n p
  | None -> Adef.escaped ""

let sign_text conf base sign info b1 b2 c1 c2 =
  let href =
    let open Def in
    Util.commd conf ^^^ "m=RL&"
    ^<^ Util.acces_n conf base (Adef.escaped "1") (Util.pget conf base info.ip1)
    ^^^ "&"
    ^<^ Util.acces_n conf base (Adef.escaped "2") (Util.pget conf base info.ip2)
    ^^^ "&b1="
    ^<^ Sosa.to_string
          (Util.old_sosa_of_branch conf base ((info.ip, info.sp) :: b1))
    ^<^ "&b2="
    ^<^ Sosa.to_string
          (Util.old_sosa_of_branch conf base ((info.ip, info.sp) :: b2))
    ^<^ "&c1=" ^<^ string_of_int c1 ^<^ "&c2=" ^<^ string_of_int c2
    ^<^ Adef.escaped
          (if Util.p_getenv conf.Config.env "spouse" = Some "on" then
             "&spouse=on"
           else "")
    ^^^ Adef.escaped
          (if Util.p_getenv conf.Config.env "image" = Some "off" then
             "&image=off"
           else "")
    ^^^ (match Util.p_getenv conf.Config.env "bd" with
      | None | Some ("0" | "") -> Adef.escaped ""
      | Some x -> "&bd=" ^<^ (Mutil.encode x :> Adef.escaped_string))
    ^^^ (match Util.p_getenv conf.Config.env "color" with
      | None | Some "" -> Adef.escaped ""
      | Some x -> "&color=" ^<^ (Mutil.encode x :> Adef.escaped_string))
    ^^^ include_marr conf base (Adef.escaped "3")
    ^^^ include_marr conf base (Adef.escaped "4")
  in
  let open Def in
  "<a href=\""
  ^<^ (href : Adef.escaped_string :> Adef.safe_string)
  ^^^ "\">"
  ^<^ (sign : Adef.safe_string)
  ^>^ "</a>"

let prev_next_1_text conf base info pb nb =
  let s =
    match pb with
    | Some b1 ->
        let open Def in
        sign_text conf base (Adef.safe "&lt;&lt;") info b1 info.b2 (info.c1 - 1)
          info.c2
        ^>^ "\n"
    | _ -> Adef.safe ""
  in
  let s =
    match (pb, nb) with
    | None, None -> s
    | _ ->
        let open Def in
        s ^>^ "<span style=\"font-size:80%\">" ^ string_of_int info.c1
        ^ "</span>"
  in
  match nb with
  | Some b1 ->
      let open Def in
      s ^^^ "\n"
      ^<^ sign_text conf base (Adef.safe "&gt;&gt;") info b1 info.b2
            (info.c1 + 1) info.c2
  | _ -> s

let prev_next_2_text conf base info pb nb =
  let s =
    match pb with
    | Some b2 ->
        let open Def in
        sign_text conf base (Adef.safe "&lt;&lt;") info info.b1 b2 info.c1
          (info.c2 - 1)
        ^>^ "\n"
    | _ -> Adef.safe ""
  in
  let s =
    match (pb, nb) with
    | None, None -> s
    | _ ->
        let open Def in
        s ^>^ "<span style=\"font-size:80%\">" ^ string_of_int info.c2
        ^ "</span>"
  in
  match nb with
  | Some b2 ->
      let open Def in
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
        ( Gwdb.get_parents (Util.pget conf base sib1),
          Gwdb.get_parents (Util.pget conf base sib2) )
      with
      | Some ifam1, Some ifam2 -> (
          let cpl1 = Gwdb.foi base ifam1 in
          let cpl2 = Gwdb.foi base ifam2 in
          let other_parent =
            if Gwdb.get_father cpl1 = info.ip then
              if Gwdb.get_mother cpl1 = Gwdb.get_mother cpl2 then
                Some (Gwdb.get_mother cpl1)
              else None
            else if Gwdb.get_father cpl1 = Gwdb.get_father cpl2 then
              Some (Gwdb.get_father cpl1)
            else None
          in
          match other_parent with
          | Some ip ->
              let d =
                DateDisplay.short_marriage_date_text conf base
                  (Gwdb.foi base ifam1)
                  (Util.pget conf base (Gwdb.get_father cpl1))
                  (Util.pget conf base (Gwdb.get_mother cpl1))
              in
              let open Def in
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
    (DagDisplay.image_txt conf base (Util.pget conf base info.ip));
  (match other_parent_text_if_same conf base info with
  | Some (s, ip) ->
      Output.print_sstring conf "<br>";
      Output.print_string conf s;
      Output.print_string conf
        (DagDisplay.image_txt conf base (Util.pget conf base ip))
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
    conf.Config.border;
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
    conf.Config.border;
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
  Output.printf conf "<td align=\"%s\">" conf.Config.right;
  Output.printf conf "<hr class=\"%s\">\n" conf.Config.right;
  Output.print_sstring conf "</td>\n";
  Output.print_sstring conf "<td>";
  Output.print_sstring conf "<hr class=\"full\">\n";
  Output.print_sstring conf "</td>\n";
  Output.printf conf "<td align=\"%s\">" conf.Config.left;
  Output.printf conf "<hr class=\"%s\">\n" conf.Config.left;
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
    match Util.p_getenv conf.Config.env "tab" with
    | Some "on" -> true
    | Some "off" -> false
    | _ -> not (Util.browser_doesnt_have_tables conf)
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
    Util.transl_nth conf "relationship link/relationship links" 0
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
  Hutil.header_no_page_title conf title;
  Hutil.print_link_to_welcome conf true;
  Util.include_template conf conf.Config.env "buttons_rel" (fun () -> ());
  Output.print_sstring conf {|<p style="clear:both">|};
  print_relation_path conf base info;
  Hutil.trailer conf

let print_relation_no_dag conf base po ip1 ip2 =
  let params =
    match
      ( po,
        Util.p_getint conf.Config.env "l1",
        Util.p_getint conf.Config.env "l2" )
    with
    | Some p, Some l1, Some l2 ->
        let ip = Gwdb.get_iper p in
        let dist = make_dist_tab conf base ip (max l1 l2 + 1) in
        let b1 = find_first_branch conf base dist ip l1 ip1 Neuter in
        let b2 = find_first_branch conf base dist ip l2 ip2 Neuter in
        Some (ip, Gwdb.get_sex (Util.pget conf base ip), dist, b1, b2, 1, 1)
    | _ -> (
        match
          ( Util.p_getenv conf.Config.env "b1",
            Util.p_getenv conf.Config.env "b2" )
        with
        | Some b1str, Some b2str -> (
            let n1 = Sosa.of_string b1str in
            let n2 = Sosa.of_string b2str in
            match
              ( Option.bind n1 (Util.old_branch_of_sosa conf base ip1),
                Option.bind n2 (Util.old_branch_of_sosa conf base ip2) )
            with
            | Some ((ia1, sa1) :: b1), Some ((ia2, _) :: b2) ->
                if ia1 = ia2 then
                  let c1 =
                    match Util.p_getint conf.Config.env "c1" with
                    | Some n -> n
                    | None -> 0
                  in
                  let c2 =
                    match Util.p_getint conf.Config.env "c2" with
                    | Some n -> n
                    | None -> 0
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
      let sp1 = Util.find_person_in_env conf base "3" in
      let sp2 = Util.find_person_in_env conf base "4" in
      let bd =
        match Util.p_getint conf.Config.env "bd" with Some x -> x | None -> 0
      in
      let td_prop =
        match Util.p_getenv conf.Config.env "color" with
        | None | Some "" -> Adef.safe ""
        | Some x ->
            let open Def in
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
  | _ -> Hutil.incorrect_request conf

module RelData = struct
  type t = {
    descendants : Gwdb.IperSet.t;
    depths_1 : Ext_int.Set.t;
    depths_2 : Ext_int.Set.t;
  }

  let empty =
    {
      descendants = Gwdb.IperSet.empty;
      depths_1 = Ext_int.Set.empty;
      depths_2 = Ext_int.Set.empty;
    }

  let get_depths_1 { depths_1 } = depths_1
  let get_depths_2 { depths_2 } = depths_2

  let add_depth_1 d ({ depths_1 } as data) =
    { data with depths_1 = Ext_int.Set.add d depths_1 }

  let add_depth_2 d ({ depths_2 } as data) =
    { data with depths_2 = Ext_int.Set.add d depths_2 }
end

let all_ipers_between ~base ~store ~start_iper ~end_iper ~max_depths ~get_depths
    ~add_depth =
  let max_depths = Ext_int.Set.of_list max_depths in
  let max_depth =
    Option.value ~default:0 (Ext_int.Set.max_elt_opt max_depths)
  in
  let ancestors = Queue.create () in
  Queue.push (start_iper, 0) ancestors;
  let data = Gwdb.Marker.get store start_iper in
  Gwdb.Marker.set store start_iper (add_depth 0 data);
  let push_parent ancestor depth ancestor_parent =
    let ({ RelData.descendants; _ } as data) =
      Gwdb.Marker.get store ancestor_parent
    in
    let descendants = Gwdb.IperSet.add ancestor descendants in
    if not (Ext_int.Set.mem (depth + 1) (get_depths data)) then
      Queue.push (ancestor_parent, depth + 1) ancestors;
    let data = add_depth (depth + 1) { data with descendants } in
    Gwdb.Marker.set store ancestor_parent data
  in
  let rec loop () =
    if not (Queue.is_empty ancestors) then
      let ip, depth = Queue.pop ancestors in
      if depth < max_depth then (
        let p = Gwdb.poi base ip in
        let fam = Option.map (Gwdb.foi base) @@ Gwdb.get_parents p in
        let fath = Option.map Gwdb.get_father fam in
        let moth = Option.map Gwdb.get_mother fam in
        Option.iter (push_parent ip depth) fath;
        Option.iter (push_parent ip depth) moth;
        loop ())
  in
  loop ();
  let rec follow_descendants result remaining_descendants depths =
    if Gwdb.IperSet.is_empty remaining_descendants then result
    else
      let result, remaining_descendants =
        Gwdb.IperSet.fold
          (fun iper (res, des) ->
            let ({ RelData.descendants; _ } as data) =
              Gwdb.Marker.get store iper
            in
            let dep = Ext_int.Set.inter depths (get_depths data) in
            if not (Ext_int.Set.is_empty dep) then
              (Gwdb.IperSet.add iper res, Gwdb.IperSet.union descendants des)
            else (res, des))
          remaining_descendants
          (result, Gwdb.IperSet.empty)
      in
      let depths = Ext_int.Set.map (fun depth -> depth - 1) depths in
      follow_descendants result remaining_descendants depths
  in
  let set =
    follow_descendants Gwdb.IperSet.empty
      (Gwdb.IperSet.singleton end_iper)
      max_depths
  in
  if not (Gwdb.IperSet.is_empty set) then Gwdb.IperSet.add end_iper set else set

let print_relation_dag conf base a ip1 ip2 l1 l2 =
  let ia = Gwdb.get_iper a in
  let store = Gwdb.iper_marker (Gwdb.ipers base) RelData.empty in
  let s1 =
    all_ipers_between ~base ~store ~start_iper:ip1 ~end_iper:ia ~max_depths:l1
      ~get_depths:RelData.get_depths_1 ~add_depth:RelData.add_depth_1
  in
  let s2 =
    all_ipers_between ~base ~store ~start_iper:ip2 ~end_iper:ia ~max_depths:l2
      ~get_depths:RelData.get_depths_2 ~add_depth:RelData.add_depth_2
  in
  let set = Gwdb.IperSet.union s1 s2 |> Gwdb.IperSet.elements in
  let spl =
    List.fold_right
      (fun (ip, s) spl ->
        match Util.find_person_in_env conf base s with
        | Some sp -> (ip, (Gwdb.get_iper sp, None)) :: spl
        | None -> spl)
      [ (ip1, "3"); (ip2, "4") ]
      []
  in
  let elem_txt p = DagDisplay.Item (p, Adef.safe "") in
  let vbar_txt _ = Adef.escaped "" in
  let invert =
    match Util.p_getenv conf.Config.env "invert" with
    | Some "on" -> true
    | _ -> false
  in
  let page_title =
    Util.transl conf "tree" |> Utf8.capitalize_fst |> Adef.safe
  in
  DagDisplay.make_and_print_dag conf base elem_txt vbar_txt invert set spl
    page_title (Adef.escaped "")

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
  let l1 = Util.p_getenv conf.Config.env "l1" in
  let l2 = Util.p_getenv conf.Config.env "l2" in
  let po = Util.find_person_in_env conf base "" in
  match (Util.p_getenv conf.Config.env "dag", po, l1, l2) with
  | Some "on", Some p, Some l1, Some l2 ->
      print_relation_dag conf base p (Gwdb.get_iper p1) (Gwdb.get_iper p2)
        (int_list l1) (int_list l2)
  | _ ->
      print_relation_no_dag conf base po (Gwdb.get_iper p1) (Gwdb.get_iper p2)

let print conf base =
  match
    ( Util.find_person_in_env conf base "1",
      Util.find_person_in_env conf base "2" )
  with
  | Some p1, Some p2 -> print_relation conf base p1 p2
  | _ -> Hutil.incorrect_request conf
