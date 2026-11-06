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

let print_events conf events ~key ~name ~cmp ~commit =
  reorder conf
    (List.map (fun e -> (key e, e)) events)
    ~cmp
    ~refusal:(fun (e1, e2) ->
      Util.cftransl conf "NOTIF order contradicts events" [ name e1; name e2 ])
    ~commit

let print_pevents conf base =
  match Util.find_person_in_env conf base "" with
  | None -> Hutil.incorrect_request conf
  | Some p ->
      print_events conf (Driver.get_pevents p) ~key:Event.pevent_key
        ~name:(fun e ->
          (Util.string_of_pevent_name conf base e.epers_name :> string))
        ~cmp:CheckItem.compare_pevents
        ~commit:(fun pevents ->
          let o_p = Driver.gen_person_of_person p in
          let n_p = { o_p with pevents } in
          Driver.patch_person base n_p.key_index n_p;
          Util.commit_patches conf base;
          History.record conf base
            (U_Modify_person
               (Util.string_gen_person base o_p, Util.string_gen_person base n_p))
            "mp")

let print_fevents conf base =
  match Util.find_family_in_env conf base with
  | None -> Hutil.incorrect_request conf
  | Some (ifam, ctx) ->
      let fam = Driver.foi base ifam in
      print_events conf (Driver.get_fevents fam) ~key:Event.fevent_key
        ~name:(fun e ->
          (Util.string_of_fevent_name conf base e.efam_name :> string))
        ~cmp:CheckItem.compare_fevents
        ~commit:(fun fevents ->
          let o_f = Driver.gen_family_of_family fam in
          let nsck =
            match o_f.relation with
            | NoSexesCheckNotMarried | NoSexesCheckMarried -> true
            | Married | NotMarried | Engaged | NoMention | MarriageBann
            | MarriageContract | MarriageLicense | Pacs | Residence ->
                false
          in
          let n_f =
            UpdateFamOk.update_family_with_fevents base nsck
              { o_f with fevents }
          in
          Driver.patch_family base ifam n_f;
          Util.commit_patches conf base;
          let p =
            match ctx with
            | Some p -> p
            | None -> Driver.poi base (Driver.get_father fam)
          in
          History.record conf base
            (U_Modify_family
               ( Util.string_gen_person base (Driver.gen_person_of_person p),
                 Util.string_gen_family base o_f,
                 Util.string_gen_family base n_f ))
            "mf")
