open Geneweb.Config
module Request = Gwd_lib.Request
module Hutil = Geneweb.Hutil
module Util = Geneweb.Util
module Perso = Geneweb.Perso

open Plugin_v7_im_lib

let ns = "v7_im"

let w_base = Request.w_base ~none:(fun _ -> false)
let w_lock = Request.w_lock ~onerror:(fun _ _ -> false)
let w_person =
  Request.w_person
    ~none:(fun c b -> Gwd_lib.Request.very_unknown c b ; true)

let can_send_image conf =
  List.assoc_opt "can_send_image" conf.base_env <> Some "no"

let () =
  Secure.add_assets !Gwd_lib.GwdPlugin.assets ;
  let wrap' fn = fun conf base -> fn conf base ; true in
  let wrap fn =
    fun _ conf base ->
      if conf.wizard && can_send_image conf
      then w_base fn conf base
      else false
  in
  let w_person fn =
    fun conf base ->
      match Util.find_person_in_env conf base "" with
      | None -> Request.incorrect_request conf
      | Some p -> fn conf base p
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "DEL_IMAGE", V7_im_sendImage.print_del |> wrap' |> wrap
    ; "DEL_IMAGE_OK", V7_im_sendImage.print_del_ok |> wrap' |> w_lock |> wrap
    ; "SND_IMAGE", V7_im_sendImage.print |> wrap' |> wrap
    ; "SND_IMAGE_OK", V7_im_sendImage.print_send_ok |> wrap' |> w_lock |> wrap

    ; "IM_C", V7_im_carrousel.print ~saved:false |> wrap' |> wrap
    ; "IM_C_S", V7_im_carrousel.print ~saved:true |> wrap' |> wrap
    ; "SND_IMAGE_C", Perso.interp_templ "carrousel" |> w_person |> wrap' |> wrap
    ; "REFRESH", Perso.interp_templ "carrousel" |> w_person |> wrap' |> wrap
    ; "SND_IMAGE_C_OK", V7_im_carrousel.print_c |> wrap' |> w_lock |> wrap
    ; "DEL_IMAGE_C_OK", V7_im_carrousel.print_c |> wrap' |> w_lock |> wrap
    ; "IMAGE_C", V7_im_carrousel.print_c |> wrap' |> wrap
    ; "RESET_IMAGE_C_OK", V7_im_carrousel.print_c |> wrap' |> wrap
    ; "PERSO", Perso.interp_templ "perso"  |> w_person |> wrap' |> wrap
    ]
