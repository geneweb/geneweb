open Geneweb.Config
module Request = Gwd_lib.Request
module Hutil = Geneweb.Hutil

open Plugin_v7_im_lib

let ns = "v7_im"

let w_base = Request.w_base ~none:(fun _ -> false)
let w_lock = Request.w_lock ~onerror:(fun _ _ -> false)

let can_send_image conf =
  List.assoc_opt "can_send_image" conf.base_env <> Some "no"

let () =
  let wrap' fn = fun conf base -> fn conf base ; true in
  let wrap fn =
    fun _ conf base ->
      if conf.wizard && can_send_image conf
      then w_base fn conf base
      else false
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "DEL_IMAGE", V7_im_sendImage.print_del |> wrap' |> wrap
    ; "DEL_IMAGE_OK", V7_im_sendImage.print_del_ok |> wrap' |> w_lock |> wrap
    ; "SND_IMAGE", V7_im_sendImage.print |> wrap' |> wrap
    ; "SND_IMAGE_OK", V7_im_sendImage.print_send_ok |> wrap' |> w_lock |> wrap
    ]
