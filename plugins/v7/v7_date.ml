open Def

module Gwdb = Geneweb.Gwdb
module DateDisplay = Geneweb.DateDisplay

let short_family_dates_text conf _base fam =
  let marr_dates =
    match Adef.od_of_cdate (Gwdb.get_marriage fam) with
    | Some d ->
      begin match d with
        | Dgreg (dmy, _) -> Some (DateDisplay.prec_year_text conf dmy)
        | _ -> Some " "
      end
    | _ -> Some " "
  in
  let sep_dates =
    match List.find_opt (fun e ->
        e.efam_name = Efam_Divorce ||
        e.efam_name = Efam_Annulation ||
        e.efam_name = Efam_Separated)
        (Gwdb.get_fevents fam) with
    | Some e ->
      begin match Adef.od_of_cdate e.efam_date with
        | Some d ->
          begin match d with
            | Dgreg (dmy, _) -> Some (DateDisplay.prec_year_text conf dmy)
            | _ -> Some " "
          end
        | _ -> Some " "
      end
    | _ -> None
  in
  match marr_dates, sep_dates with
  | Some m, Some s -> m ^ "-" ^ s
  | Some m, None -> m
  | _, _ -> ""
