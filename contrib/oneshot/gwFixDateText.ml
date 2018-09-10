(* camlp4r *)

open Def
open Gwdb



let fix_date_text bname trace =
  let base = Gwdb.open_base bname in
  let space_to_unders = Mutil.tr ' ' '_' in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let birth = get_birth p in
    let birth =
      match Adef.od_of_codate birth with
        Some d ->
          begin match d with
            Dgreg (_, _) -> birth
          | Dtext t ->
              if String.length t < 10 then birth
              else
                let sub_date = String.sub t 0 10 in
                let end_date = String.sub t 10 (String.length t - 10) in
                let sub_date = space_to_unders sub_date in
                Adef.codate_of_od (Some (Dtext (sub_date ^ end_date)))
          end
      | None -> birth
    in
    let baptism = get_baptism p in
    let baptism =
      match Adef.od_of_codate baptism with
        Some d ->
          begin match d with
            Dgreg (_, _) -> baptism
          | Dtext t ->
              if String.length t < 10 then baptism
              else
                let sub_date = String.sub t 0 10 in
                let end_date = String.sub t 10 (String.length t - 10) in
                let sub_date = space_to_unders sub_date in
                Adef.codate_of_od (Some (Dtext (sub_date ^ end_date)))
          end
      | None -> baptism
    in
    let death = get_death p in
    let death =
      match death with
        Death (dr, cd) ->
          let d = Adef.date_of_cdate cd in
          begin match d with
            Dgreg (_, _) -> death
          | Dtext t ->
              if String.length t < 10 then death
              else
                let sub_date = String.sub t 0 10 in
                let end_date = String.sub t 10 (String.length t - 10) in
                let sub_date = space_to_unders sub_date in
                Death (dr, Adef.cdate_of_date (Dtext (sub_date ^ end_date)))
          end
      | _ -> death
    in
    let burial = get_burial p in
    let burial =
      match burial with
        Buried d ->
          begin match Adef.od_of_codate d with
            Some d ->
              begin match d with
                Dgreg (_, _) -> burial
              | Dtext t ->
                  if String.length t < 10 then burial
                  else
                    let sub_date = String.sub t 0 10 in
                    let end_date = String.sub t 10 (String.length t - 10) in
                    let sub_date = space_to_unders sub_date in
                    Buried
                      (Adef.codate_of_od (Some (Dtext (sub_date ^ end_date))))
              end
          | None -> burial
          end
      | Cremated d ->
          begin match Adef.od_of_codate d with
            Some d ->
              begin match d with
                Dgreg (_, _) -> burial
              | Dtext t ->
                  if String.length t < 10 then burial
                  else
                    let sub_date = String.sub t 0 10 in
                    let end_date = String.sub t 10 (String.length t - 10) in
                    let sub_date = space_to_unders sub_date in
                    Cremated
                      (Adef.codate_of_od (Some (Dtext (sub_date ^ end_date))))
              end
          | None -> burial
          end
      | _ -> burial
    in
    let pevents = get_pevents p in
    let pevents =
      List.map
        (fun evt ->
           let date = evt.epers_date in
           let date =
             match Adef.od_of_codate date with
               Some d ->
                 begin match d with
                   Dgreg (_, _) -> date
                 | Dtext t ->
                     if String.length t < 10 then date
                     else
                       let sub_date = String.sub t 0 10 in
                       let end_date =
                         String.sub t 10 (String.length t - 10)
                       in
                       let sub_date = space_to_unders sub_date in
                       Adef.codate_of_od (Some (Dtext (sub_date ^ end_date)))
                 end
             | None -> date
           in
           {evt with epers_date = date})
        pevents
    in
    let p =
      {(gen_person_of_person p) with birth = birth; baptism = baptism;
       death = death; burial = burial; pevents = pevents}
    in
    patch_person base p.key_index p
  done;
  for i = 0 to nb_of_families base - 1 do
    let fam = foi base (Adef.ifam_of_int i) in
    let marriage = get_marriage fam in
    let marriage =
      match Adef.od_of_codate marriage with
        Some d ->
          begin match d with
            Dgreg (_, _) -> marriage
          | Dtext t ->
              if String.length t < 10 then marriage
              else
                let sub_date = String.sub t 0 10 in
                let end_date = String.sub t 10 (String.length t - 10) in
                let sub_date = space_to_unders sub_date in
                Adef.codate_of_od (Some (Dtext (sub_date ^ end_date)))
          end
      | None -> marriage
    in
    let fevents = get_fevents fam in
    let fevents =
      List.map
        (fun evt ->
           let date = evt.efam_date in
           let date =
             match Adef.od_of_codate date with
               Some d ->
                 begin match d with
                   Dgreg (_, _) -> date
                 | Dtext t ->
                     if String.length t < 10 then date
                     else
                       let sub_date = String.sub t 0 10 in
                       let end_date =
                         String.sub t 10 (String.length t - 10)
                       in
                       let sub_date = space_to_unders sub_date in
                       Adef.codate_of_od (Some (Dtext (sub_date ^ end_date)))
                 end
             | None -> date
           in
           {evt with efam_date = date})
        fevents
    in
    let fam =
      {(gen_family_of_family fam) with marriage = marriage; fevents = fevents}
    in
    patch_family base fam.fam_index fam
  done;
  commit_patches base

let bname = ref ""
let trace = ref false

let speclist = ["-t", Arg.Set trace, "trace changed persons"]
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  fix_date_text !bname !trace

let _ = main ()







