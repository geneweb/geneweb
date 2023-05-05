open Geneweb.Config
module Request = Gwd_lib.Request
module Hutil = Geneweb.Hutil
open Plugin_v7_im_lib

let ns = "v7_im"
let w_base = Request.w_base ~none:(fun _ -> false)
let w_lock = Request.w_lock ~onerror:(fun _ _ -> false)

let w_person =
  Request.w_person ~none:(fun c b -> Gwd_lib.Request.very_unknown c b)

let can_send_image conf =
  List.assoc_opt "can_send_image" conf.base_env <> Some "no"

let () =
  Secure.add_assets !Gwd_lib.GwdPlugin.assets;
  let wrap' fn conf base_name =
    fn conf base_name;
    true
  in
  let wrap fn conf base_name =
    if can_send_image conf then w_base fn conf base_name else false
  in
  Gwd_lib.GwdPlugin.register ~ns
    [
      ("DEL_IMAGE", fun _assets -> ImageCarrousel.print_del |> wrap' |> wrap);
      ( "DEL_IMAGE_OK",
        fun _assets -> ImageCarrousel.print_del_ok |> wrap' |> wrap |> w_lock );
      ( "DEL_IMAGE_C_OK",
        fun _assets -> ImageCarrousel.print_main_c |> wrap' |> wrap |> w_lock );
      ("SND_IMAGE", fun _assets -> ImageCarrousel.print |> wrap' |> wrap);
      ( "SND_IMAGE_OK",
        fun _assets -> ImageCarrousel.print_send_ok |> wrap' |> wrap |> w_lock
      );
      ( "SND_IMAGE_C",
        fun _assets ->
          Geneweb.Perso.interp_templ "carrousel" |> w_person |> wrap' |> wrap );
      ( "SND_IMAGE_C_OK",
        fun _assets -> ImageCarrousel.print_main_c |> wrap' |> wrap |> w_lock );
      ( "IM_C",
        fun _assets -> ImageCarrousel.print_c ~saved:false |> wrap' |> wrap );
      ( "IM_C_S",
        fun __assets -> ImageCarrousel.print_c ~saved:true |> wrap' |> wrap );
      ( "PERSO",
        fun _assets ->
          Geneweb.Perso.interp_templ "perso" |> w_person |> wrap' |> wrap );
      ( "REFRESH",
        fun _assets ->
          Geneweb.Perso.interp_templ "carrousel" |> w_person |> wrap' |> wrap );
      ( "RESET_IMAGE_C_OK",
        fun _assets -> ImageCarrousel.print_main_c |> wrap' |> wrap );
    ]
