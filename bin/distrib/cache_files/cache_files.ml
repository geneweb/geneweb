open Geneweb
open Gwdb

let bname = ref ""
let trace = ref false
let fnames = ref false
let places = ref false
let fname_alias = ref false
let snames = ref false
let alias = ref false
let qual = ref false
let all = ref false
let prog = ref false

let write_cache_file bname fname list =
  let bname =  if Filename.check_suffix bname ".gwb" then
    Filename.remove_extension bname
    else bname
  in
  let fname = Filename.concat
    (Util.base_path [] (bname ^ ".gwb"))
    (bname ^ "_" ^ fname ^ "_cache.txt") in
  Printf.printf "Write to : %s\n" fname;
  begin match try Some (Secure.open_out fname )
    with Sys_error _ -> None with
  | Some oc ->
      begin
        List.iter
          (fun (v, _) -> output_string oc ("<option>" ^ v ^ "\n"))
            list;
        close_out oc;
      end
  | None -> ()
  end

let places_all base bname fname =
  let start = Unix.gettimeofday () in
  let ht_size = 2048 in (* FIXME: find the good heuristic *)
  let ht : ('a, 'b) Hashtbl.t = Hashtbl.create ht_size in
  let ht_add istr _p =
    let key : 'a = sou base istr in
    match Hashtbl.find_opt ht key with
    | Some _ -> Hashtbl.replace ht key key
    | None -> Hashtbl.add ht key key
  in
  let len = nb_of_persons base in
  if !prog then
    begin
      Printf.printf "Places\n"; flush stdout;
      ProgrBar.full := '*'; ProgrBar.start ()
    end;
  let aux b fn p =
    if b then let x = fn p in if not (is_empty_string x) then ht_add x p
  in
  for i = 0 to len - 1 do
    let p = poi base (Adef.iper_of_int i) in
    aux true get_birth_place p ;
    aux true get_baptism_place p ;
    aux true get_death_place p ;
    aux true get_burial_place p ;
    if !prog then ProgrBar.run i len else ()
  done;
  if !prog then ProgrBar.finish ();
  let len = nb_of_families base in
  if !prog then
    begin
      ProgrBar.full := '*'; ProgrBar.start ()
    end;
  for i = 0 to len - 1 do
    if !prog then ProgrBar.run i len;
    let fam = foi base (Adef.ifam_of_int i) in
    if not @@ is_deleted_family fam then
    begin
      let pl_ma = get_marriage_place fam in
      if not (is_empty_string pl_ma) then
        let fath = poi base (get_father fam) in
        let moth = poi base (get_mother fam) in
        ht_add pl_ma fath ;
        ht_add pl_ma moth
    end ;
    if !prog then ProgrBar.run i len else ()
  done ;
  if !prog then ProgrBar.finish ();
  flush stderr;
  let places_list = Hashtbl.fold (fun _k v acc -> (v, 1) :: acc) ht [] in
  let places_list = List.sort (fun (v1, _) (v2, _) ->
    Gutil.alphabetic_utf_8 v1 v2) places_list in
  write_cache_file bname fname places_list;
  flush stderr;
  let stop = Unix.gettimeofday () in
  Printf.printf "Number of places: %d\n" (List.length places_list);
  Printf.printf "Execution time: %fs\n" (stop -. start);
  flush stderr

let names_all base bname fname =
  let start = Unix.gettimeofday () in
  let ht = Hashtbl.create 1 in
  let nb_ind = nb_of_persons base in
  flush stderr;
  if !prog then
    begin
      Printf.printf "%s\n" fname; flush stdout;
      ProgrBar.full := '*'; ProgrBar.start ()
    end;
  for ind = 0 to nb_ind - 1 do
    if !prog then ProgrBar.run ind nb_ind;
    let p = poi base (Adef.iper_of_int ind) in
    let nam = if !fnames then sou base (get_first_name p)
      else if !snames then sou base (get_surname p)
      else ""
    in
    let al = if !alias then get_aliases p else [] in
    let qual = if !qual then get_qualifiers p else [] in
    let fna = if !fname_alias && !fnames then
      get_first_names_aliases p else []
    in
    let key = nam in
    if nam <> "" then
        if not (Hashtbl.mem ht key) then Hashtbl.add ht key (nam, 1)
        else
          let (vv, i) = Hashtbl.find ht key in
          Hashtbl.replace ht key (vv, i + 1)
      else ();
    let nam2 = if al <> [] then al 
      else if fna <> [] then fna 
      else if qual <> [] then qual
      else []
    in
    if ind = 8862 then 
      Printf.eprintf "\nLists (%d) %s al: %d, qual: %d, fna: %d, nam2: %d\n" ind nam
        (List.length al)
        (List.length qual)
        (List.length fna)
        (List.length nam2);
    flush stderr;
    if nam2 <> [] then
      List.iter (fun nam ->
          let nam = sou base nam in
          let key = nam in
          if not (Hashtbl.mem ht key) then Hashtbl.add ht key (nam, 1)
          else
            let (vv, i) = Hashtbl.find ht key in
            Hashtbl.replace ht key (vv, i + 1)) nam2;
    if !prog then ProgrBar.run ind nb_ind else ()
  done;
  if !prog then ProgrBar.finish ();
  flush stderr;
  let name_list = Hashtbl.fold (fun _k v acc -> v :: acc) ht [] in
  let name_list = List.sort (fun v1 v2 -> compare v1 v2) name_list in
  write_cache_file bname fname name_list;
  flush stderr;
  let stop = Unix.gettimeofday () in
  Printf.printf "Number of %s : %d\n" fname (Hashtbl.length ht);
  Printf.printf "Execution time: %fs\n" (stop -. start);
  flush stderr
  
let speclist =
  ["-fn", Arg.Set fnames, "produce first names";
   "-sn", Arg.Set snames, "produce surnames";
   "-al", Arg.Set alias, "produce aliases";
   "-qu", Arg.Set qual, "produce qualifiers";
   "-pl", Arg.Set places, "produce places";
   "-all", Arg.Set all, "produce all";
   "-fna", Arg.Set fname_alias, "add first names aliases";
   "-prog", Arg.Set prog, "show progress bar"
   ]
   
let anonfun i = bname := i
let usage = "Usage: cache_files [-fn] [-sn] [-al] [-qu] \
  [-pl] [-all] [-fna] [-prog] base\n \
  cd bases; before running cache_files."

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" || !bname <> Filename.basename !bname
  then begin Arg.usage speclist usage; exit 2 end;
  let base = Gwdb.open_base !bname in
  bname := Filename.basename !bname;
  if !places then places_all base !bname "places";
  if !fnames then names_all base !bname "fnames";
  if !snames then names_all base !bname "snames";
  if !alias then names_all base !bname "aliases";
  if !qual then names_all base !bname "qualifiers";
  if !all then
    begin
      places_all base !bname "places";
      fnames := true; names_all base !bname "fnames"; fnames := false;
      snames := true; names_all base !bname "snames"; snames := false;
      alias := true; names_all base !bname "aliases"; alias := false;
      qual := true; names_all base !bname "qualifiers"; qual := false;
    end
    

let _ = main ()
