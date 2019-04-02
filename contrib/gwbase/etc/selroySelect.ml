(* Copyright (c) 2000-2006 INRIA *)
open Geneweb
open Def
open Gwdb

let select_ancestors base per_tab fam_tab flag =
  let rec loop iper =
    let i = Adef.int_of_iper iper in
    if per_tab.(i) land flag <> 0 then ()
    else
      begin
        per_tab.(i) <- per_tab.(i) lor flag;
        match get_parents (poi base iper) with
          Some ifam ->
            let i = Adef.int_of_ifam ifam in
            if fam_tab.(i) land flag <> 0 then ()
            else
              begin
                fam_tab.(i) <- fam_tab.(i) lor flag;
                let cpl = foi base ifam in
                loop (get_father cpl); loop (get_mother cpl)
              end
        | None -> ()
      end
  in
  loop

let ok_titles = ["roi"; "reine"; "empereur"]

let has_titles base p =
  List.exists (fun t -> List.mem (sou base t.t_ident) ok_titles)
    (get_titles p)

let parents_has_titles base p =
  match get_parents p with
    Some ifam ->
      let cpl = foi base ifam in
      has_titles base (poi base (get_father cpl)) ||
      has_titles base (poi base (get_mother cpl))
  | None -> false

let gen_good_dates p birth_lim death_lim =
  let death_ok =
    match get_death p with
      Death (_, cd) ->
        begin match Adef.date_of_cdate cd with
          Dgreg (d, _) -> d.year <= death_lim
        | _ -> false
        end
    | _ -> false
  in
  if death_ok then true
  else
    match Adef.od_of_cdate (get_birth p) with
      Some (Dgreg (d, _)) -> d.year <= birth_lim
    | _ -> false

let good_dates base p =
  if gen_good_dates p 1830 1900 then true
  else
    match get_parents p with
      Some ifam ->
        let cpl = foi base ifam in
        gen_good_dates (poi base (get_father cpl)) 1780 1800 ||
        gen_good_dates (poi base (get_mother cpl)) 1780 1800
    | None -> false

let rec select_closure base per_tab fam_tab flag ip =
  if per_tab.(Adef.int_of_iper ip) = 1 then
    let u = poi base ip in
    per_tab.(Adef.int_of_iper ip) <- flag;
    begin match get_parents (poi base ip) with
      Some ifam ->
        let cpl = foi base ifam in
        select_closure base per_tab fam_tab flag (get_father cpl);
        select_closure base per_tab fam_tab flag (get_mother cpl)
    | None -> ()
    end;
    for i = 0 to Array.length (get_family u) - 1 do
      let ifam = (get_family u).(i) in
      let desc = foi base ifam in
      if fam_tab.(Adef.int_of_ifam ifam) = 1 then
        begin
          fam_tab.(Adef.int_of_ifam ifam) <- flag;
          for i = 0 to Array.length (get_children desc) - 1 do
            select_closure base per_tab fam_tab flag (get_children desc).(i)
          done;
          ()
        end
    done;
    ()

let functions base _ _ _ _ _ _ _ _ =
  let per_tab = Array.make (nb_of_persons base) 0 in
  let fam_tab = Array.make (nb_of_families base) 0 in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let iaper = Adef.iper_of_int i in
    if has_titles base p || parents_has_titles base p || good_dates base p
    then
      select_ancestors base per_tab fam_tab 1 iaper
  done;
  for i = 0 to nb_of_persons base - 1 do
    if per_tab.(i) == 1 then
      let u = poi base (Adef.iper_of_int i) in
      for i = 0 to Array.length (get_family u) - 1 do
        let ifam = (get_family u).(i) in
        let cpl = foi base ifam in
        fam_tab.(Adef.int_of_ifam ifam) <- 1;
        per_tab.(Adef.int_of_iper (get_father cpl)) <- 1;
        per_tab.(Adef.int_of_iper (get_mother cpl)) <- 1;
        ()
      done
  done;
  match person_of_key base "juan carlos" "de borbon" 0 with
    Some ip ->
      select_closure base per_tab fam_tab 3 ip;
      (fun i -> per_tab.(Adef.int_of_iper i) == 3),
      (fun i -> fam_tab.(Adef.int_of_ifam i) == 3)
  | None -> failwith "not found juan carlos.0 de borbon"
