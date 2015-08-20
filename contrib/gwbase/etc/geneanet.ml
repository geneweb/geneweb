(* $Id: geneanet.ml,v 4.16 2007-01-19 09:03:02 deraugla Exp $ *)

open Def;
open Gutil;
open Gwdb;

value limit_date = 1850;

value sou base s = Ansel.to_iso_8859_1 (sou base s);

value in_file = ref "";

value rec mark_ancestors base mark i =
  if mark.(i) = False then do {
    mark.(i) := True;
    match get_parents (poi base (Adef.iper_of_int i)) with
    [ Some ifam ->
        let cpl = foi base ifam in
        do {
          mark_ancestors base mark (Adef.int_of_iper (get_father cpl));
          mark_ancestors base mark (Adef.int_of_iper (get_mother cpl));
        }
    | None -> () ]
  }
  else ()
;

value main_title base =
  let val =
    fun
    [ "empereur" | "impératrice" -> 6
    | "roi" | "reine" -> 5
    | "prince" | "princesse" -> 4
    | "duc" | "duchesse" -> 3
    | "comte" | "comtesse" -> 2
    | "vicomte" | "vicomtesse" -> 1
    | _ -> 0 ]
  in
  let rec loop r =
    fun
    [ [] -> r
    | [x :: l] ->
        if x.t_name == Tmain then Some x
        else
          match r with
          [ Some t ->
              if val (sou base x.t_ident) > val (sou base t.t_ident) then
                loop (Some x) l
              else loop r l
          | None -> loop (Some x) l ] ]
  in
  loop None
;

value min_or_max_date f a base p =
  let a =
    match Adef.od_of_codate (get_birth p) with
    [ Some (Dgreg d _) -> f d.year a
    | _ -> a ]
  in
  let a =
    match CheckItem.date_of_death (get_death p) with
    [ Some (Dgreg d _) -> f d.year a
    | _ -> a ]
  in
  let a =
    List.fold_left
      (fun a ifam ->
         let fam = foi base ifam in
         let a =
           match Adef.od_of_codate (get_marriage fam) with
           [ Some (Dgreg d _) -> f d.year a
           | _ -> a ]
         in
         let a =
           match get_divorce fam with
           [ Divorced cod ->
               match Adef.od_of_codate cod with
               [ Some (Dgreg d _) -> f d.year a
               | _ -> a ]
           | _ -> a ]
         in
         a)
      a (Array.to_list (get_family p))
  in
  a
;

value region_of_person base p =
  match main_title base (get_titles p) with
  [ Some t ->
      match sou base t.t_place with
      [ "d'Alsace" -> "ALS"
      | "d'Angleterre" -> "ENG"
      | "d'Aquitaine" -> "AQU"
      | "d'Aragon" -> "ARA"
      | "d'Auvergne" -> "AUV"
      | "de Castille" -> "CYL"
      | "d'Ulster" -> "ULS"
      | "d'Écosse" -> "SCT"
      | _ -> "" ]
  | _ -> "" ]
;

value country_of_person base p =
  match main_title base (get_titles p) with
  [ Some t ->
      match sou base t.t_place with
      [ "d'Albanie" -> "ALB"
      | "d'Alsace" -> "FRA"
      | "d'Angleterre" -> "GBR"
      | "d'Aquitaine" -> "FRA"
      | "d'Aragon" -> "ESP"
      | "d'Athènes" -> "GRC"
      | "d'Autriche antérieure" -> "AUT"
      | "d'Autriche inférieure" -> "AUT"
      | "d'Autriche" -> "AUT"
      | "d'Autriche-Este" -> "AUT"
      | "d'Auvergne" -> "FRA"
      | "d'Espagne" -> "ESP"
      | "d'Irlande" -> "IRL"
      | "d'Italie" -> "ITA"
      | "d'Orléans" -> "FRA"
      | "d'Ulster" -> "GBR"
      | "d'Écosse" -> "GBR"
      | "de Belgique" | "des Belges" -> "BEL"
      | "de Brésil" -> "BRA"
      | "de Bulgarie" -> "BGR"
      | "de Castille" -> "ESP"
      | "de France" -> "FRA"
      | "de Grande-Bretagne" -> "GBR"
      | "de Grèce" -> "GRC"
      | "de Hongrie" -> "HUN"
      | "de Naples" -> "ITA"
      | "de Pologne" -> "POL"
      | "de Portugal" -> "PRT"
      | "de Roumanie" -> "ROM"
      | "de Suède" -> "SWE"
      | "" -> ""
      | x -> (* do Printf.eprintf "%s\n" x; flush stderr; return *)
          "" ]
  | _ -> "" ]
;

value place_of_person base p =
  let s = sou base (get_birth_place p) in
  if s <> "" then s
  else
    let s = sou base (get_death_place p) in
    if s <> "" then s
    else
      List.fold_left
        (fun s ifam ->
           if s <> "" then s
           else
             let fam = foi base ifam in
             sou base (get_marriage_place fam))
        "" (Array.to_list (get_family p))
;

value info_of_person base p =
  match main_title base (get_titles p) with
  [ Some t ->
      let title = sou base t.t_ident in
      let place = sou base t.t_place in
      if place <> "" && place <> sou base (get_surname p) then
        title ^ " " ^ place
      else title
  | None -> sou base (get_occupation p) ]
;

value infinity = 10000;

value begin_of_person base p = min_or_max_date min infinity base p;
value end_of_person base p = min_or_max_date max 0 base p;

value to_be_inserted base p =
  let b = end_of_person base p in
  if b = 0 then False else b < limit_date
;

type line =
  { name : string;
    info : mutable string;
    dbeg : mutable int;
    dend : mutable int;
    nbindi : mutable int;
    place : string;
    subregion : string;
    region : string;
    country : string }
;

value line_of_person base p =
  {name = sou base (get_surname p); info = info_of_person base p;
   dbeg = begin_of_person base p; dend = end_of_person base p; nbindi = 1;
   place = place_of_person base p; subregion = "";
   region = region_of_person base p; country = country_of_person base p}
;

value compare_lines l1 l2 =
  if l1.name < l2.name then -1
  else if l1.name > l2.name then 1
  else if l1.place < l2.place then -1
  else if l1.place > l2.place then 1
  else if l1.subregion < l2.subregion then -1
  else if l1.subregion > l2.subregion then 1
  else if l1.region < l2.region then -1
  else if l1.region > l2.region then 1
  else if l1.country < l2.country then -1
  else if l1.country > l2.country then 1
  else 0
;

module Line =
  Map.Make (struct type t = line; value compare = compare_lines; end)
;

value print_line line =
  (**)
  if line.info <> "" || line.dbeg <> infinity || line.dend <> 0 ||
     line.nbindi > 5 || line.place <> "" || line.subregion <> ""
  then do {
    (**)
    Printf.printf "%s;" line.name;
    Printf.printf "%s;" line.info;
    if line.dbeg = infinity then Printf.printf ";"
    else Printf.printf "%d;" line.dbeg;
    if line.dend = 0 then Printf.printf ";"
    else Printf.printf "%d;" line.dend;
    Printf.printf "%d;" line.nbindi;
    Printf.printf "%s;" line.place;
    Printf.printf "%s;" line.subregion;
    Printf.printf "%s;" line.region;
    Printf.printf "%s;" line.country;
    Printf.printf "M";
    Printf.printf "\n";
    ()
  }
  else
    (**)
    ()
;

value order_lines l1 l2 =
  if l1.name <> l2.name then Gutil.alphabetic l1.name l2.name < 0
  else if l1.info < l2.info then True
  else if l1.info > l2.info then False
  else if l1.dbeg < l2.dbeg then True
  else if l1.dbeg > l2.dbeg then False
  else if l1.dend < l2.dend then True
  else if l1.dend > l2.dend then False
  else if l1.place < l2.place then True
  else if l1.place > l2.place then False
  else if l1.subregion < l2.subregion then True
  else if l1.subregion > l2.subregion then False
  else if l1.region < l2.region then True
  else if l1.region > l2.region then False
  else if l1.country < l2.country then True
  else if l1.country > l2.country then False
  else True
;

value geneanet base =
  let mark = Array.make (nb_of_persons base) False in
  let lines = ref Line.empty in
  do {
    load_ascends_array base;
    load_unions_array base;
    load_couples_array base;
    for i = 0 to nb_of_persons base - 1 do {
      if mark.(i) then ()
      else if to_be_inserted base (poi base (Adef.iper_of_int i)) then
        mark_ancestors base mark i
      else ()
    };
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let surname = sou base (get_surname p) in
      if mark.(i) && surname <> "?" && surname <> "x" then
        let line = line_of_person base p in
        try
          let line1 = Line.find line lines.val in
          do {
            line1.dbeg := min line.dbeg line1.dbeg;
            line1.dend := max line.dend line1.dend;
            line1.nbindi := line1.nbindi + 1;
            if line1.info = "" || line.dbeg = line1.dbeg then
              line1.info := line.info
            else ();
            ()
          }
        with
        [ Not_found -> lines.val := Line.add line line lines.val ]
      else ()
    };
    (let list = Line.fold (fun _ line list -> [line :: list]) lines.val [] in
     let list = Sort.list order_lines list in
     List.iter print_line list);
    ()
  }
;

value main () =
  let speclist = [] in
  let errmsg = "Usage: geneanet <file>" in
  do {
    Argl.parse speclist (fun s -> in_file.val := s) errmsg;
    if in_file.val = "" then do {
      Printf.eprintf "Missing file\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 1
    }
    else ();
    let base = Gwdb.open_base in_file.val in
    geneanet base;
    flush stdout;
  }
;

Printexc.catch main ();
