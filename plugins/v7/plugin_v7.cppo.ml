open Geneweb
open Config
open Util

module Cousins = struct
  #include "cousins.ml"
end
module Perso = struct
  #include "perso.ml"
end
module Some = struct
  #include "some.ml"
end
module Request = Gwd_lib.Request

let w_base =
  Request.w_base
    ~none:(fun c -> Gwd_lib.Request.incorrect_request c ; true)
let w_person =
  Request.w_person
    ~none:(fun c b -> Gwd_lib.Request.very_unknown c b ; true)

let person_selected conf base p =
  match p_getenv conf.senv "em" with
  | Some "R" -> false
  | Some _ -> false
  | None -> Perso.print conf base p ; true

let home conf base : bool =
  if base <> None
  then
    w_base begin fun conf base : bool ->
      if Request.only_special_env conf.env then false
      else w_person begin fun conf base p ->
          match p_getenv conf.env "ptempl" with
          | Some t when p_getenv conf.base_env "ptempl" = Some "yes" -> false
          | _ -> person_selected conf base p
        end conf base
    end conf base
  else false

let p =
  w_base begin fun conf base -> match p_getenv conf.env "v" with
    | Some v -> Some.first_name_print conf base v ; true
    | None -> false
  end

let ns = "v7"

let _ =
  let aux fn assets conf base =
    Util.add_lang_path assets ;
    fn conf base
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "", aux home
    ; "P", aux p
    ]
