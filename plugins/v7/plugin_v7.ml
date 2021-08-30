open Geneweb.Config
module Gwdb = Gwdb
module Util = Geneweb.Util
module DescendDisplay = Geneweb.DescendDisplay
module Request = Gwd_lib.Request

open Plugin_v7_lib

let w_base =
  Request.w_base
    ~none:(fun c -> Gwd_lib.Request.incorrect_request c ; true)
let w_person =
  Request.w_person
    ~none:(fun c b -> Gwd_lib.Request.very_unknown c b ; true)

let person_selected conf base p =
  match Util.p_getenv conf.senv "em" with
  | Some "R" -> false
  | Some _ -> false
  | None -> V7_perso.print conf base p ; true

let home conf base : bool =
  if base <> None
  then
    w_base begin fun conf base : bool ->
      if Request.only_special_env conf.env then false
      else w_person begin fun conf base p ->
          match Util.p_getenv conf.env "ptempl" with
          | Some t when Util.p_getenv conf.base_env "ptempl" = Some "yes" -> false
          | _ -> person_selected conf base p
        end conf base
    end conf base
  else false

let l =
  w_base begin fun conf base ->
    Gwdb.dummy_iper
    |> Gwdb.empty_person base
    |> !V7_interp.templ "list" conf base
    |> fun () -> true
    end

let p =
  w_base begin fun conf base -> match Util.p_getenv conf.env "v" with
    | Some v -> V7_some.first_name_print conf base v ; true
    | None -> false
  end

let ps =
  w_base begin fun conf base ->
    V7_place.print_all_places_surnames conf base ;
    true
  end
 
let a =
  w_base begin fun conf base ->
   match Util.find_person_in_env conf base "" with
  | Some p ->
      if Util.p_getenv conf.env "t" = Some  "FC"
      then (!V7_interp.templ "fanchart" conf base p ; true)
      else false
  | _ -> false
  end

let c =
  w_base begin fun conf base ->
   match Util.find_person_in_env conf base "" with
  | Some p -> V7_cousins.print conf base p ; true
  | _ -> false
  end

let doc =
  w_base begin fun conf base ->
    match Util.p_getenv conf.env "s" with
    | Some f ->
        begin
          if Filename.check_suffix f ".txt" then
            let f = Filename.chop_suffix f ".txt" in
            V7_srcfile.new_print_source conf base f
          else V7_srcfile.new_print_source_image conf f;
          true
        end
    | None -> false
  end

let d =
  w_base @@ w_person @@ fun conf base p -> V7_descend.print conf base p; true

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
    ; "P", aux p
    ; "PS", aux ps
    ]
