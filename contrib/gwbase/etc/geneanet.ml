(* $Id: geneanet.ml,v 4.16 2007-01-19 09:03:02 deraugla Exp $ *)

open Def
open Gwdb

let limit_date = 1850

let sou base s = Ansel.to_iso_8859_1 (sou base s)

let in_file = ref ""

let rec mark_ancestors base mark i =
  if mark.(i) = false then
    begin
      mark.(i) <- true;
      match get_parents (poi base (Adef.iper_of_int i)) with
        Some ifam ->
          let cpl = foi base ifam in
          mark_ancestors base mark (Adef.int_of_iper (get_father cpl));
          mark_ancestors base mark (Adef.int_of_iper (get_mother cpl))
      | None -> ()
    end

let main_title base =
  let value =
    function
      "empereur" | "impératrice" -> 6
    | "roi" | "reine" -> 5
    | "prince" | "princesse" -> 4
    | "duc" | "duchesse" -> 3
    | "comte" | "comtesse" -> 2
    | "vicomte" | "vicomtesse" -> 1
    | _ -> 0
  in
  let rec loop r =
    function
      [] -> r
    | x :: l ->
        if x.t_name == Tmain then Some x
        else
          match r with
            Some t ->
              if value (sou base x.t_ident) > value (sou base t.t_ident) then
                loop (Some x) l
              else loop r l
          | None -> loop (Some x) l
  in
  loop None

let min_or_max_date f a base p =
  let a =
    match Adef.od_of_cdate (get_birth p) with
      Some (Dgreg (d, _)) -> f d.year a
    | _ -> a
  in
  let a =
    match CheckItem.date_of_death (get_death p) with
      Some (Dgreg (d, _)) -> f d.year a
    | _ -> a
  in
  List.fold_left
    (fun a ifam ->
       let fam = foi base ifam in
       let a =
         match Adef.od_of_cdate (get_marriage fam) with
           Some (Dgreg (d, _)) -> f d.year a
         | _ -> a
       in
       match get_divorce fam with
         Divorced cod ->
         begin match Adef.od_of_cdate cod with
             Some (Dgreg (d, _)) -> f d.year a
           | _ -> a
         end
       | _ -> a)
    a (Array.to_list (get_family p))

let region_of_person base p =
  match main_title base (get_titles p) with
    Some t ->
      begin match sou base t.t_place with
        "d'Alsace" -> "ALS"
      | "d'Angleterre" -> "ENG"
      | "d'Aquitaine" -> "AQU"
      | "d'Aragon" -> "ARA"
      | "d'Auvergne" -> "AUV"
      | "de Castille" -> "CYL"
      | "d'Ulster" -> "ULS"
      | "d'Écosse" -> "SCT"
      | _ -> ""
      end
  | _ -> ""

let country_of_person base p =
  match main_title base (get_titles p) with
    Some t ->
      begin match sou base t.t_place with
        "d'Albanie" -> "ALB"
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
      | x ->
          (* do Printf.eprintf "%s\n" x; flush stderr; return *)
          ""
      end
  | _ -> ""

let place_of_person base p =
  let s = sou base (get_birth_place p) in
  if s <> "" then s
  else
    let s = sou base (get_death_place p) in
    if s <> "" then s
    else
      List.fold_left
        (fun s ifam ->
           if s <> "" then s
           else let fam = foi base ifam in sou base (get_marriage_place fam))
        "" (Array.to_list (get_family p))

let info_of_person base p =
  match main_title base (get_titles p) with
    Some t ->
      let title = sou base t.t_ident in
      let place = sou base t.t_place in
      if place <> "" && place <> sou base (get_surname p) then
        title ^ " " ^ place
      else title
  | None -> sou base (get_occupation p)

let infinity = 10000

let begin_of_person base p = min_or_max_date min infinity base p
let end_of_person base p = min_or_max_date max 0 base p

let to_be_inserted base p =
  let b = end_of_person base p in if b = 0 then false else b < limit_date

type line =
  { name : string;
    mutable info : string;
    mutable dbeg : int;
    mutable dend : int;
    mutable nbindi : int;
    place : string;
    subregion : string;
    region : string;
    country : string }

let line_of_person base p =
  {name = sou base (get_surname p); info = info_of_person base p;
   dbeg = begin_of_person base p; dend = end_of_person base p; nbindi = 1;
   place = place_of_person base p; subregion = "";
   region = region_of_person base p; country = country_of_person base p}

let compare_lines l1 l2 =
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

module Line = Map.Make (struct type t = line let compare = compare_lines end)

let print_line line =
  if line.info <> "" || line.dbeg <> infinity || line.dend <> 0 ||
     line.nbindi > 5 || line.place <> "" || line.subregion <> ""
  then
    begin
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
    end

let cmp_lines l1 l2 =
  if l1.name <> l2.name then Gutil.alphabetic l1.name l2.name
  else if l1.info < l2.info then -1
  else if l1.info > l2.info then 1
  else if l1.dbeg < l2.dbeg then -1
  else if l1.dbeg > l2.dbeg then 1
  else if l1.dend < l2.dend then -1
  else if l1.dend > l2.dend then 1
  else if l1.place < l2.place then -1
  else if l1.place > l2.place then 1
  else if l1.subregion < l2.subregion then -1
  else if l1.subregion > l2.subregion then 1
  else if l1.region < l2.region then -1
  else if l1.region > l2.region then 1
  else if l1.country < l2.country then -1
  else if l1.country > l2.country then 1
  else 0

let geneanet base =
  let mark = Array.make (nb_of_persons base) false in
  let lines = ref Line.empty in
  load_ascends_array base;
  load_unions_array base;
  load_couples_array base;
  for i = 0 to nb_of_persons base - 1 do
    if mark.(i) then ()
    else if to_be_inserted base (poi base (Adef.iper_of_int i)) then
      mark_ancestors base mark i
  done;
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let surname = sou base (get_surname p) in
    if mark.(i) && surname <> "?" && surname <> "x" then
      let line = line_of_person base p in
      try
        let line1 = Line.find line !lines in
        line1.dbeg <- min line.dbeg line1.dbeg;
        line1.dend <- max line.dend line1.dend;
        line1.nbindi <- line1.nbindi + 1;
        if line1.info = "" || line.dbeg = line1.dbeg then
          line1.info <- line.info;
        ()
      with Not_found -> lines := Line.add line line !lines
  done;
  begin let list = Line.fold (fun _ line list -> line :: list) !lines [] in
    let list = List.sort cmp_lines list in List.iter print_line list
  end;
  ()

let main () =
  let speclist = [] in
  let errmsg = "Usage: geneanet <file>" in
  Argl.parse speclist (fun s -> in_file := s) errmsg;
  if !in_file = "" then
    begin
      Printf.eprintf "Missing file\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 1
    end;
  let base = Gwdb.open_base !in_file in geneanet base; flush stdout

let _ = Printexc.print main ()
