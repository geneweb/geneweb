open Geneweb.Config
module Util = Geneweb.Util
module Some = Geneweb.Some
module AscendDisplay = Geneweb.AscendDisplay
module CousinsPrintOrCount = Geneweb.CousinsPrintOrCount
module DescendDisplay = Geneweb.DescendDisplay
module RelationDisplay = Geneweb.RelationDisplay
module RelationLink = Geneweb.RelationLink
module Perso = Geneweb.Perso
module SrcfileDisplay = Geneweb.SrcfileDisplay
module ImageDisplay = Geneweb.ImageDisplay
module Request = Gwd_lib.Request
module Templ_interp = Geneweb.Templ_interp

open Plugin_v7_lib

module SearchName = V7_searchName

let person_is_std_key conf base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (Gwdb.p_first_name base p ^ " " ^ Gwdb.p_surname base p) then
    true
  else if
    List.exists (fun n -> Name.strip n = k)
      (Gwdb.person_misc_names base p (Util.nobtit conf base))
  then
    true
  else false

let select_std_eq conf base pl k =
  List.fold_right
    (fun p pl -> if person_is_std_key conf base p k then p :: pl else pl) pl
    []

let find_all conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match sosa_ref, sosa_nb with
  | Some p, Some n ->
    if n <> Sosa.zero then
      match Util.branch_of_sosa conf base n p with
        Some (p :: _) -> [p], true
      | _ -> [], false
    else [], false
  | _ ->
    let acc = SearchName.search_by_key conf base an in
    if acc <> [] then acc, false
    else
      ( SearchName.search_key_aux begin fun conf base acc an ->
            let spl = select_std_eq conf base acc an in
            if spl = [] then
              if acc = [] then SearchName.search_by_name conf base an
              else acc
            else spl
          end conf base an
      , false )

let relation_print conf base p =
  let p1 =
    match Util.p_getenv conf.senv "ei" with
    | Some i ->
      conf.senv <- [] ;
      let i = Gwdb.iper_of_string i in
      if Gwdb.iper_exists base i
      then Some (Util.pget conf base i)
      else None
    | None ->
      match Util.find_person_in_env conf base "1" with
      | Some p1 ->
        conf.senv <- [];
        Some p1
      | None -> None
  in
  RelationDisplay.print conf base p p1

let w_base =
  Request.w_base
    ~none:(fun c -> Gwd_lib.Request.incorrect_request c ; true)
let w_person =
  Request.w_person
    ~none:(fun c b -> Gwd_lib.Request.very_unknown c b ; true)

let person_selected conf base p =
  match Util.p_getenv conf.senv "em" with
  | Some "R" -> relation_print conf base p
  | Some _ -> Request.incorrect_request conf
  | None -> Perso.print conf base p

let person_selected_with_redirect conf base p =
  match Util.p_getenv conf.senv "em" with
    Some "R" -> relation_print conf base p
  | Some _ -> Request.incorrect_request conf
  | None ->
    Wserver.http_redirect_temporarily (Util.commd conf ^ Util.acces conf base p)

let a = w_base @@ fun conf base ->
    match Util.find_person_in_env conf base "" with
    | Some p ->
      if Util.p_getenv conf.env "t" = Some  "FC"
      then Perso.interp_templ "fanchart" conf base p
      else AscendDisplay.print conf base p ;
      true
    | _ -> false

let c = w_base @@ fun conf base ->
    match Util.find_person_in_env conf base "" with
    | Some p -> CousinsPrintOrCount.print conf base p ; true
    | _ -> false

let d = w_base @@ w_person @@ fun conf base p ->
  DescendDisplay.print conf base p; true

let doc = w_base @@ fun conf base ->
    match Util.p_getenv conf.env "s" with
    | Some f ->
        begin
          if Filename.check_suffix f ".txt" then
            let f = Filename.chop_suffix f ".txt" in
            SrcfileDisplay.print_source conf base f
          else ImageDisplay.print_source_image conf f;
          true
        end
    | None -> false

let home conf base : bool =
  if base <> None
  then
    w_base begin fun conf base : bool ->
      if Request.only_special_env conf.env then false
      else w_person begin fun conf base p ->
          match Util.p_getenv conf.env "ptempl" with
          | Some t when Util.p_getenv conf.base_env "ptempl" = Some "yes" -> false
          | _ -> person_selected conf base p ; true
        end conf base
    end conf base
  else false

let l = w_base @@ fun conf base ->
    Gwdb.dummy_iper
    |> Gwdb.empty_person base
    |> !Templ_interp.templ "list" conf base
    |> fun () -> true
    
let md = w_base begin fun conf base ->
    V7_updateDataDisplay.print_mod conf base ;
    true
  end

let md_ok = w_base begin fun conf base ->
    V7_updateDataDisplay.print_mod_ok conf base ;
    true
  end

let ng = w_base @@ begin fun conf base ->
    (* Rétro-compatibilité <= 6.06 *)
    let env =
      match Util.p_getenv conf.env "n" with
        Some n ->
        begin match Util.p_getenv conf.env "t" with
            Some "P" -> ("fn", n) :: conf.env
          | Some "N" -> ("sn", n) :: conf.env
          | _ -> ("ri", n) :: conf.env
        end
      | None -> conf.env
    in
    let conf = {conf with env = env} in
    (* Nouveau mode de recherche. *)
    match Util.p_getenv conf.env "select" with
    | Some "input" | None ->
      (* Récupère le contenu non vide de la recherche. *)
      let real_input label =
        match Util.p_getenv conf.env label with
        | Some s -> if s = "" then None else Some s
        | None -> None
      in
      (* Recherche par clé, sosa, alias ... *)
      let search n =
        let (pl, sosa_acc) = find_all conf base n in
        match pl with
        | [] ->
          Some.surname_print conf base Request.unknown n
        | [p] ->
          if sosa_acc
          || Gutil.person_of_string_key base n <> None
          || person_is_std_key conf base p n
          then person_selected_with_redirect conf base p
          else Request.specify conf base n pl
        | pl -> Request.specify conf base n pl
      in
      begin match real_input "ri" with
        | Some n -> search n ; true
        | None ->
          match real_input "fn", real_input "sn" with
            Some fn, Some sn -> search (fn ^ " " ^ sn) ; true
          | Some fn, None ->
            Some.first_name_print conf base fn ; true
          | None, Some sn ->
            Some.surname_print conf base Request.unknown sn ; true
          | None, None -> Request.incorrect_request conf ; true
      end
    | Some i ->
      relation_print conf base
        (Util.pget conf base (Gwdb.iper_of_string i)) ; true
  end

let p = w_base begin fun conf base -> match Util.p_getenv conf.env "v" with
    | Some v -> Some.first_name_print conf base v ; true
    | None -> false
  end

let ps = w_base @@ fun conf base ->
    V7_place.print_all_places_surnames conf base ; true

let r = w_base @@ w_person @@ fun conf base p ->
    relation_print conf base p ; true

let rl = w_base @@ fun conf base -> RelationLink.print conf base ; true

let rlm = w_base @@ fun conf base -> RelationDisplay.print_multi conf base ; true

let s = w_base @@ fun conf base -> V7_searchName.print conf base
    Request.specify Request.unknown; true

let tp = w_base begin fun conf base ->
    match Util.p_getenv conf.env "v" with
    | Some f ->
      begin match Util.find_person_in_env conf base "" with
      | Some p -> !Templ_interp.templ ("tp_" ^ f) conf base p
      | _ -> !Templ_interp.templ ("tp0_" ^ f) conf base
               (Gwdb.empty_person base Gwdb.dummy_iper)
      end ;
      true
    | None -> false
  end

let ns = "v7"

let _ =
  Secure.add_assets !Gwd_lib.GwdPlugin.assets ;
  let aux fn assets conf base =
    fn conf base
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "", aux home
    ; "A", aux a
    ; "C", aux c
    ; "D", aux d
    ; "DOC", aux doc
    ; "L", aux l
    ; "MOD_DATA", aux md
    ; "MOD_DATA_OK", aux md_ok
    ; "NG", aux ng
    ; "P", aux p
    ; "PS", aux ps
    ; "R", aux r
    ; "RL", aux rl
    ; "RLM", aux rlm
    ; "S", aux s
    ; "TP", aux tp
    ]
