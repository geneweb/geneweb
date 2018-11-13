open Geneweb
open Def
open Gwdb

let year_of p =
  match
    Adef.od_of_cdate (get_birth p), Adef.od_of_cdate (get_baptism p),
    get_death p, CheckItem.date_of_death (get_death p)
  with
    _, _, NotDead, _ -> None
  | Some (Dgreg (d, _)), _, _, _ -> Some d.year
  | _, Some (Dgreg (d, _)), _, _ -> Some d.year
  | _, _, _, Some (Dgreg (d, _)) -> Some d.year
  | _ -> None

let most_recent_year_of p =
  match
    Adef.od_of_cdate (get_birth p), Adef.od_of_cdate (get_baptism p),
    get_death p, CheckItem.date_of_death (get_death p)
  with
    _, _, NotDead, _ -> None
  | _, _, _, Some (Dgreg (d, _)) -> Some d.year
  | _, Some (Dgreg (d, _)), _, _ -> Some d.year
  | Some (Dgreg (d, _)), _, _, _ -> Some d.year
  | _ -> None

let is_old lim_year p =
  match year_of p with
    Some y -> y < lim_year
  | None -> false

let nb_gen_by_century = 2

let nb_desc_gen lim_year p =
  match most_recent_year_of p with
    Some year -> (lim_year - year) * nb_gen_by_century / 100
  | None -> 0

let changes = ref false

let mark_descendants base scanned old lim_year =
  let rec loop p ndgen =
    if not scanned.(Adef.int_of_iper (get_key_index p)) then
      let dt = most_recent_year_of p in
      let ndgen =
        match dt with
          Some y ->
            scanned.(Adef.int_of_iper (get_key_index p)) <- true;
            if y < lim_year then nb_desc_gen lim_year p else 0
        | None -> ndgen
      in
      if ndgen > 0 then
        begin
          old.(Adef.int_of_iper (get_key_index p)) <- true;
          let ndgen = ndgen - 1 in
          for i = 0 to Array.length (get_family p) - 1 do
            let ifam = (get_family p).(i) in
            let fam = foi base ifam in
            let sp = Gutil.spouse (get_key_index p) fam in
            old.(Adef.int_of_iper sp) <- true;
            let children = get_children fam in
            for ip = 0 to Array.length children - 1 do
              let p = poi base children.(ip) in loop p ndgen
            done
          done
        end
  in
  loop

let mark_ancestors base scanned lim_year is_quest_string =
  let rec loop p =
    if not scanned.(Adef.int_of_iper (get_key_index p)) then
      begin
        scanned.(Adef.int_of_iper (get_key_index p)) <- true;
        if not (is_old lim_year p) && get_access p = IfTitles &&
           get_titles p = [] && not (is_quest_string (get_first_name p)) &&
           not (is_quest_string (get_surname p))
        then
          begin let _ =
            Printf.printf "%s\n" (Gutil.designation base p); flush stdout
          in
            begin match year_of p with
              Some y ->
                if y >= lim_year then
                  begin
                    Printf.eprintf "�a d�conne %s %d\n" (Gutil.designation base p) y;
                    flush stderr
                  end
            | None -> ()
            end;
            let p = {(gen_person_of_person p) with access = Public} in
            patch_person base p.key_index p; changes := true
          end;
        match get_parents p with
          Some ifam ->
            let cpl = foi base ifam in
            loop (poi base (get_father cpl)); loop (poi base (get_mother cpl))
        | None -> ()
      end
  in
  loop

let public_everybody bname =
  let base = Gwdb.open_base bname in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if get_access p <> Public then
      let p = {(gen_person_of_person p) with access = Public} in
      patch_person base p.key_index p
  done;
  commit_patches base

let public_all bname lim_year =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let old = Array.make (nb_of_persons base) false in
  let scanned = Array.make (nb_of_persons base) false in
  for i = 0 to nb_of_persons base - 1 do
    if not scanned.(i) then
      let p = poi base (Adef.iper_of_int i) in
      mark_descendants base scanned old lim_year p 0
  done;
  let scanned = Array.make (nb_of_persons base) false in
  for i = 0 to nb_of_persons base - 1 do
    if old.(i) && not scanned.(i) then
      let p = poi base (Adef.iper_of_int i) in
      mark_ancestors base scanned lim_year is_quest_string p
  done;
  if !changes then commit_patches base

let public_some bname lim_year key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      let scanned = Array.make (nb_of_persons base) false in
      let () = load_ascends_array base in
      let () = load_couples_array base in
      mark_ancestors base scanned lim_year is_quest_string p;
      if !changes then commit_patches base
  | _ -> Printf.eprintf "Bad key %s\n" key; flush stderr; exit 2

let lim_year = ref 1850
let ind = ref ""
let bname = ref ""
let everybody = ref false

let speclist =
  ["-y", Arg.Int (fun i -> lim_year := i),
   "limit year (default = " ^ string_of_int !lim_year ^ ")";
   "-everybody", Arg.Set everybody, "set flag public to everybody";
   "-ind", Arg.String (fun x -> ind := x), "individual key"]
let anonfun i = bname := i
let usage = "Usage: public [-y #] [-ind key] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  if !everybody then public_everybody !bname
  else if !ind = "" then public_all !bname !lim_year
  else public_some !bname !lim_year !ind

let _ = main ()
