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

let find_dated_ancestor base p =
  let rec loop nb_gen iplist =
    if iplist = [] then None
    else
      let anc_list =
        List.fold_left
          (fun anc_list ip ->
             match get_parents (poi base ip) with
               Some ifam ->
                 let fam = foi base ifam in
                 get_mother fam :: get_father fam :: anc_list
             | None -> anc_list)
          [] iplist
      in
      let rec loop_ind =
        function
          ip :: iplist ->
            let p = poi base ip in
            begin match year_of p with
              Some year -> Some (p, year, nb_gen)
            | None -> loop_ind iplist
            end
        | [] -> loop (nb_gen + 1) anc_list
      in
      loop_ind anc_list
  in
  loop 1 [get_key_index p]

let nb_years_by_gen = 30

let change_somebody_access base lim_year trace p year_of_p =
  if year_of_p = None && get_access p = IfTitles then
    match find_dated_ancestor base p with
      Some (a, year, nb_gen) ->
        let acc =
          if year + nb_gen * nb_years_by_gen > lim_year then Private
          else Public
        in
        let gp = {(gen_person_of_person p) with access = acc} in
        patch_person base gp.key_index gp;
        if trace then
          begin
            Printf.printf "%s -> " (Gutil.designation base p);
            if acc = Private then Printf.printf "private" else Printf.printf "public";
            Printf.printf " (anc %d gen %s year %d)" nb_gen
              (Gutil.designation base a) year;
            Printf.printf "\n";
            flush stdout;
            Some acc
          end
        else None
    | None -> None
  else None

let public_all bname lim_year trace =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let n = nb_of_persons base in
  let changes = ref false in
  ProgrBar.start ();
  for i = 0 to n - 1 do
    ProgrBar.run i n;
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    if year_of p = None && get_access p = IfTitles then
      match change_somebody_access base lim_year trace p (year_of p) with
        Some _ -> changes := true
      | None ->
          let fama = get_family p in
          let rec loop i =
            if i = Array.length fama then ()
            else
              let ifam = fama.(i) in
              let isp = Gutil.spouse ip (foi base ifam) in
              let sp = poi base isp in
              let year_of_sp = year_of sp in
              let acc_opt =
                match year_of_sp with
                  Some year ->
                    Some (if year > lim_year then Private else Public)
                | None ->
                    change_somebody_access base lim_year trace sp year_of_sp
              in
              match acc_opt with
                Some acc ->
                  let gp = {(gen_person_of_person p) with access = acc} in
                  patch_person base gp.key_index gp;
                  changes := true;
                  if trace then
                    begin
                      Printf.printf "%s -> " (Gutil.designation base p);
                      if acc = Private then Printf.printf "private"
                      else Printf.printf "public";
                      Printf.printf " (inherited from spouse %s)"
                        (Gutil.designation base sp);
                      Printf.printf "\n";
                      flush stdout
                    end
              | None -> loop (i + 1)
          in
          loop 0
  done;
  if !changes then commit_patches base;
  ProgrBar.finish ()

let lim_year = ref 1850
let trace = ref false
let bname = ref ""

let speclist =
  ["-y", Arg.Int (fun i -> lim_year := i),
   "limit year (default = " ^ string_of_int !lim_year ^ ")";
   "-t", Arg.Set trace, "trace changed persons"]
let anonfun i = bname := i
let usage = "Usage: public [-y #] [-t] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  public_all !bname !lim_year !trace

let _ = main ()
