open Geneweb
open Config

let () =
  Gwd_lib.GwdPlugin.register ~ns:"fanchart" [ "A", fun assets conf base ->
      let base = match base with Some b -> b | None -> assert false in
      match Util.find_person_in_env conf base "" with
      | None -> false
      | Some p ->
        if Util.p_getenv conf.env "t" = Some  "FC"
        then (Perso.interp_templ "fanchart" conf base p ; true)
        else false
    ]
