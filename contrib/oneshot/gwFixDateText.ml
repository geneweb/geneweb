(* camlp4r *)

open Def;
open Gwdb;
open Printf;



value fix_date_text bname trace =
  let base = Gwdb.open_base bname in
  let space_to_unders = Mutil.tr ' ' '_' in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      let birth = get_birth p in
      let birth =
        match Adef.od_of_codate birth with
        [ Some d ->
            match d with
            [ Dgreg _ _ -> birth
            | Dtext t ->
                if String.length t < 10 then birth
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Adef.codate_of_od (Some (Dtext (sub_date ^ end_date))) ]
        | None -> birth ]
      in
      let baptism = get_baptism p in
      let baptism =
        match Adef.od_of_codate baptism with
        [ Some d ->
            match d with
            [ Dgreg _ _ -> baptism
            | Dtext t ->
                if String.length t < 10 then baptism
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Adef.codate_of_od (Some (Dtext (sub_date ^ end_date))) ]
        | None -> baptism ]
      in
      let death = get_death p in
      let death =
        match death with
        [ Death dr cd ->
            let d = Adef.date_of_cdate cd in
            match d with
            [ Dgreg _ _ -> death
            | Dtext t ->
                if String.length t < 10 then death
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Death dr (Adef.cdate_of_date (Dtext (sub_date ^ end_date)))
            ]
        | _ -> death ]
      in
      let burial = get_burial p in
      let burial =
        match burial with
        [ Buried d ->
            match Adef.od_of_codate d with
            [ Some d ->
                match d with
                [ Dgreg _ _ -> burial
                | Dtext t ->
                    if String.length t < 10 then burial
                    else
                      let sub_date = String.sub t 0 10 in
                      let end_date = String.sub t 10 (String.length t - 10) in
                      let sub_date = space_to_unders sub_date in
                      Buried (Adef.codate_of_od (Some (Dtext (sub_date ^ end_date)))) ]
            | None -> burial ]
        | Cremated d ->
            match Adef.od_of_codate d with
            [ Some d ->
                match d with
                [ Dgreg _ _ -> burial
                | Dtext t ->
                    if String.length t < 10 then burial
                    else
                      let sub_date = String.sub t 0 10 in
                      let end_date = String.sub t 10 (String.length t - 10) in
                      let sub_date = space_to_unders sub_date in
                      Cremated (Adef.codate_of_od (Some (Dtext (sub_date ^ end_date)))) ]
            | None -> burial ]
        | _ -> burial ]
      in
      let pevents = get_pevents p in
      let pevents =
        List.map
          (fun evt ->
             let date = evt.epers_date in
             let date =
               match Adef.od_of_codate date with
               [ Some d ->
                  match d with
                  [ Dgreg _ _ -> date
                  | Dtext t ->
                      if String.length t < 10 then date
                      else
                        let sub_date = String.sub t 0 10 in
                        let end_date = String.sub t 10 (String.length t - 10) in
                        let sub_date = space_to_unders sub_date in
                        Adef.codate_of_od (Some (Dtext (sub_date ^ end_date))) ]
               | None -> date ]
             in
             {(evt) with epers_date = date})
          pevents
      in
      let p =
        {(gen_person_of_person p) with birth = birth; baptism = baptism;
         death = death; burial = burial; pevents = pevents}
      in
      patch_person base p.key_index p
    };
    for i = 0 to nb_of_families base - 1 do {
      let fam = foi base (Adef.ifam_of_int i) in
      let marriage = get_marriage fam in
      let marriage =
        match Adef.od_of_codate marriage with
        [ Some d ->
            match d with
            [ Dgreg _ _ -> marriage
            | Dtext t ->
                if String.length t < 10 then marriage
                else
                  let sub_date = String.sub t 0 10 in
                  let end_date = String.sub t 10 (String.length t - 10) in
                  let sub_date = space_to_unders sub_date in
                  Adef.codate_of_od (Some (Dtext (sub_date ^ end_date))) ]
        | None -> marriage ]
      in
      let fevents = get_fevents fam in
      let fevents =
        List.map
          (fun evt ->
             let date = evt.efam_date in
             let date =
               match Adef.od_of_codate date with
               [ Some d ->
                  match d with
                  [ Dgreg _ _ -> date
                  | Dtext t ->
                      if String.length t < 10 then date
                      else
                        let sub_date = String.sub t 0 10 in
                        let end_date = String.sub t 10 (String.length t - 10) in
                        let sub_date = space_to_unders sub_date in
                        Adef.codate_of_od (Some (Dtext (sub_date ^ end_date))) ]
               | None -> date ]
             in
             {(evt) with efam_date = date})
          fevents
      in
      let fam = {(gen_family_of_family fam) with marriage = marriage; fevents = fevents} in
      patch_family base fam.fam_index fam
    };
    commit_patches base;
  }
;

value bname = ref "";
value trace = ref False;

value speclist =
   [ ("-t", Arg.Set trace, "trace changed persons") ]
;
value anonfun i = bname.val := i;
value usage = "Usage: " ^ Sys.argv.(0) ^ " base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    fix_date_text bname.val trace.val
  }
;

main ();







