(* camlp4r *)
(* $Id: public.ml,v 4.26 2007-01-19 09:03:02 deraugla Exp $ *)

open Def;
open Gwdb;
open Printf;

value year_of p =
  match
    (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
     get_death p, CheckItem.date_of_death (get_death p))
  with
  [ (_, _, NotDead, _) -> None
  | (Some (Dgreg d _), _, _, _) -> Some d.year
  | (_, Some (Dgreg d _), _, _) -> Some d.year
  | (_, _, _, Some (Dgreg d _)) -> Some d.year
  | _ -> None ]
;

value most_recent_year_of p =
  match
    (Adef.od_of_codate (get_birth p), Adef.od_of_codate (get_baptism p),
     get_death p, CheckItem.date_of_death (get_death p))
  with
  [ (_, _, NotDead, _) -> None
  | (_, _, _, Some (Dgreg d _)) -> Some d.year
  | (_, Some (Dgreg d _), _, _) -> Some d.year
  | (Some (Dgreg d _), _, _, _) -> Some d.year
  | _ -> None ]
;

value is_old lim_year p =
  match year_of p with
  [ Some y -> y < lim_year
  | None -> False ]
;

value nb_gen_by_century = 2;

value nb_desc_gen lim_year p =
  match most_recent_year_of p with
  [ Some year -> (lim_year - year) * nb_gen_by_century / 100
  | None -> 0 ]
;

value changes = ref False;

value mark_descendants base scanned old lim_year =
  loop where rec loop p ndgen =
    if not scanned.(Adef.int_of_iper (get_key_index p)) then do {
      let dt = most_recent_year_of p in
      let ndgen =
        match dt with
        [ Some y ->
            do {
              scanned.(Adef.int_of_iper (get_key_index p)) := True;
              if y < lim_year then nb_desc_gen lim_year p else 0
            }
        | None -> ndgen ]
      in
      if ndgen > 0 then do {
        old.(Adef.int_of_iper (get_key_index p)) := True;
        let ndgen = ndgen - 1 in
        for i = 0 to Array.length (get_family p) - 1 do {
	  let ifam = (get_family p).(i) in
          let fam = foi base ifam in
          let sp = Gutil.spouse (get_key_index p) fam in
          old.(Adef.int_of_iper sp) := True;
          let children = get_children fam in
          for ip = 0 to Array.length children - 1 do {
            let p = poi base children.(ip) in
            loop p ndgen
          }
        }
      }
      else ()
  }
  else ()
;

value mark_ancestors base scanned lim_year is_quest_string =
  loop where rec loop p =
    if not scanned.(Adef.int_of_iper (get_key_index p)) then do {
      scanned.(Adef.int_of_iper (get_key_index p)) := True;
      if not (is_old lim_year p) && get_access p = IfTitles &&
         get_titles p = [] && not (is_quest_string (get_first_name p)) &&
         not (is_quest_string (get_surname p))
      then do {
(**)
  let _ = do { printf "%s\n" (Gutil.designation base p); flush stdout; } in
(**)
        match year_of p with
	[ Some y ->
	    if y >= lim_year then do {
              eprintf "�a d�conne %s %d\n" (Gutil.designation base p) y;
              flush stderr;
            }
	    else ()
	| None -> () ];
(**)
        let p = {(gen_person_of_person p) with access = Public} in
        patch_person base p.key_index p;
        changes.val := True;
      }
      else ();
      match get_parents p with
      [ Some ifam ->
          let cpl = foi base ifam in
          do {
            loop (poi base (get_father cpl));
            loop (poi base (get_mother cpl));
          }
      | None -> () ];
    }
    else ()
;

value public_everybody bname =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if get_access p <> Public then
        let p = {(gen_person_of_person p) with access = Public} in
        patch_person base p.key_index p
      else ();
    };
    commit_patches base;
  }
;

value public_all bname lim_year =
  let base = Gwdb.open_base bname in
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let old = Array.create (nb_of_persons base) False in
  do {
    let scanned = Array.create (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      if not scanned.(i) then do {
        let p = poi base (Adef.iper_of_int i) in
        mark_descendants base scanned old lim_year p 0
      }
      else ();
    };
    let scanned = Array.create (nb_of_persons base) False in
    for i = 0 to nb_of_persons base - 1 do {
      if old.(i) && not scanned.(i) then do {
        let p = poi base (Adef.iper_of_int i) in
        mark_ancestors base scanned lim_year is_quest_string p
      }
      else ();
    };
    if changes.val then commit_patches base else ();
  }
;

value public_some bname lim_year key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      let scanned = Array.create (nb_of_persons base) False in
      let () = load_ascends_array base in
      let () = load_couples_array base in
      do {
        mark_ancestors base scanned lim_year is_quest_string p;
        if changes.val then commit_patches base else ();
      }
  | _ ->
      do {
        Printf.eprintf "Bad key %s\n" key;
        flush stderr;
        exit 2
      } ]
;

value lim_year = ref 1850;
value ind = ref "";
value bname = ref "";
value everybody = ref False;

value speclist =
  [("-y", Arg.Int (fun i -> lim_year.val := i),
    "limit year (default = " ^ string_of_int lim_year.val ^ ")");
   ("-everybody", Arg.Set everybody, "set flag public to everybody");
   ("-ind", Arg.String (fun x -> ind.val := x),
    "individual key")]
;
value anonfun i = bname.val := i;
value usage = "Usage: public [-y #] [-ind key] base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    if everybody.val then public_everybody bname.val
    else if ind.val = "" then public_all bname.val lim_year.val
    else public_some bname.val lim_year.val ind.val
  }
;

main ();
