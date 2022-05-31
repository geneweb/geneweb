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
module UpdateDataDisplay = Geneweb.UpdateDataDisplay
module SearchName = Geneweb.SearchName

open Plugin_v7_lib

let w_base =
  Request.w_base
    ~none:(fun c -> Gwd_lib.Request.incorrect_request c ; true)
let w_person =
  Request.w_person
    ~none:(fun c b -> Gwd_lib.Request.very_unknown c b ; true)

let person_selected conf base p =
  match Util.p_getenv conf.senv "em" with
  | Some "R" -> Request.relation_print conf base p
  | Some _ -> Request.incorrect_request conf
  | None -> Perso.print conf base p

let home conf base : bool =
  if base <> None
  then
    w_base begin fun conf base : bool ->
      if Request.only_special_env conf.env then false
      else w_person begin fun conf base p ->
          match Util.p_getenv conf.env "ptempl" with
          | Some t when Util.p_getenv conf.base_env "ptempl" = Some "yes" ->
            Perso.interp_templ t conf base p ; true
          | _ -> person_selected conf base p ; true
        end conf base
    end conf base
  else false
    
let ps = w_base @@ fun conf base ->
    V7_place.print_all_places_surnames conf base ; true

let ns = "v7"

let _ =
  Secure.add_assets !Gwd_lib.GwdPlugin.assets ;
  let aux fn assets conf base =
    fn conf base
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "", aux home
    ; "PS", aux ps
    ]
