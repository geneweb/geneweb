(* camlp4r *)
(* $Id: lune.ml,v 4.7 2006-10-30 09:37:58 deraugla Exp $ *)

open Def;
open Gwdb;
open Printf;

value bname = ref "";

value lune bname =
  let base = Gwdb.open_base bname in
  let moon_age = Array.make 31 0 in
  let moon_phase = Array.make 5 0 in
  let nbb = ref 0 in
  do {
    for i = 0 to nb_of_persons base -1 do {
      let p = poi base (Adef.iper_of_int i) in
      match Adef.od_of_codate (get_birth p) with
      [ Some (Dgreg dt _) ->
          if dt.prec = Sure && dt.delta = 0 && dt.day > 0 then do {
            incr nbb;
            let jd = Calendar.sdn_of_gregorian dt in
            let (mp, ma) = Calendar.moon_phase_of_sdn jd in
            moon_age.(ma - 1) := moon_age.(ma - 1) + 1;
            let i =
              match mp with
              [ None -> 0
              | Some (Calendar.NewMoon, _, _) -> 1
              | Some (Calendar.FirstQuarter, _, _) -> 2
              | Some (Calendar.FullMoon, _, _) -> 3
              | Some (Calendar.LastQuarter, _, _) -> 4 ]
            in
            moon_phase.(i) := moon_phase.(i) + 1;
	  }
	  else ()
      | _ -> () ];
    };
    printf "Influence de la lune sur les naissances.\n\n";
    printf "Nombre de personnes = %d\n" nbb.val;
    printf "\n";
    printf "Naissances :\n\n";
    printf "- à la nouvelle lune  : %d (%.3f%%)\n" moon_phase.(1)
      (100.0 *. float moon_phase.(1) /. float nbb.val);
    printf "- au premier quartier : %d (%.3f%%)\n" moon_phase.(2)
      (100.0 *. float moon_phase.(2) /. float nbb.val);
    printf "- à la pleine lune    : %d (%.3f%%)\n" moon_phase.(3)
      (100.0 *. float moon_phase.(3) /. float nbb.val);
    printf "- au dernier quartier : %d (%.3f%%)\n" moon_phase.(4)
      (100.0 *. float moon_phase.(4) /. float nbb.val);
    printf "- entre deux phases   : %d (%.3f%%)\n" moon_phase.(0)
      (100.0 *. float moon_phase.(0) /. float nbb.val);
    printf "\n";
    printf "Naissances en fonction de l'âge de la lune:\n\n";
    printf "\tâge\tnombre\t%%/total\n";
    for i = 0 to Array.length moon_age - 1 do {
      printf "\t%2d\t%d\t%.3f%%\n" (i + 1) moon_age.(i)
        (float moon_age.(i) *. 100.0 /. float nbb.val);
    };
    flush stdout;
  }
;

value speclist =
  []
;
value anonfun i = bname.val := i;
value usage = "Usage: superstition base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    lune bname.val
  }
;

main ();
