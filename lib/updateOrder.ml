open Config
open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let respond conf success message =
  Output.header conf "Content-type: application/json";
  Output.header conf "Connection: close";
  Output.print_sstring conf
    (Yojson.Basic.to_string
       (`Assoc [ ("success", `Bool success); ("message", `String message) ]));
  Output.flush conf

let rec take key l =
  match l with
  | [] -> None
  | ((k, x) as item) :: rest -> (
      if String.equal k key then Some (x, rest)
      else
        match take key rest with
        | Some (y, rest) -> Some (y, item :: rest)
        | None -> None)

let rec permute items keys =
  match (keys, items) with
  | [], [] -> Some []
  | [], _ :: _ -> None
  | key :: keys, _ -> (
      match take key items with
      | Some (x, items) -> Option.map (fun l -> x :: l) (permute items keys)
      | None -> None)

let reorder conf items ~cmp ~refusal ~commit =
  let keys = List.map fst items in
  let sent =
    String.split_on_char ','
      (Option.value ~default:"" (Util.p_getenv conf.env "o"))
  in
  if Util.p_getenv conf.env "prev" <> Some (String.concat "," keys) then
    respond conf false (Util.transl conf "NOTIF order changed")
  else
    match permute items sent with
    | None -> respond conf false (Util.transl conf "NOTIF order not saved")
    | Some _ when List.equal String.equal sent keys -> respond conf false ""
    | Some l -> (
        match CheckItem.misplaced cmp l with
        | Some pair -> respond conf false (refusal pair)
        | None ->
            commit l;
            respond conf true (Util.transl conf "NOTIF order saved"))

let print_families conf base =
  match Util.find_person_in_env conf base "" with
  | None -> Hutil.incorrect_request conf
  | Some p ->
      let ip = Driver.get_iper p in
      let fams = Array.to_list (Driver.get_family p) in
      let spouse ifam =
        let isp = Gutil.spouse ip (Driver.foi base ifam) in
        (Util.gen_person_text ~html:false conf base (Driver.poi base isp)
          :> string)
      in
      let date ifam =
        match CheckItem.union_date (Driver.foi base ifam) with
        | Some d ->
            (DateDisplay.string_of_date_aux ~link:false conf d :> string)
        | None -> ""
      in
      reorder conf
        (List.map (fun f -> (Driver.Ifam.to_string f, f)) fams)
        ~cmp:(CheckItem.compare_unions base)
        ~refusal:(fun (f1, f2) ->
          Util.cftransl conf "NOTIF order contradicts union dates"
            [ spouse f1; date f1; spouse f2; date f2 ])
        ~commit:(fun l ->
          Driver.patch_union base ip { family = Array.of_list l };
          Util.commit_patches conf base;
          let gen_p =
            Util.string_gen_person base (Driver.gen_person_of_person p)
          in
          List.combine l fams
          |> List.find_opt (fun (f, f') -> f <> f')
          |> Option.iter (fun (moved, _) ->
              History.record conf base (U_Invert_family (gen_p, moved)) "if"))
