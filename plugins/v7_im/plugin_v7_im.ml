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
  let wrap' fn conf base_name =
    fn conf base_name;
    true
  in
  let wrap fn conf base_name =
    if conf.wizard && can_send_image conf then w_base fn conf base_name
    else false
  in
  Gwd_lib.GwdPlugin.register ~ns
    [
      ("DEL_IMAGE", fun _assets -> V7_im_sendImage.print_del |> wrap' |> wrap);
      ( "DEL_IMAGE_OK",
        fun _assets -> V7_im_sendImage.print_del_ok |> wrap' |> wrap |> w_lock
      );
      ("SND_IMAGE", fun _assets -> V7_im_sendImage.print |> wrap' |> wrap);
      ( "SND_IMAGE_OK",
        fun _assets -> V7_im_sendImage.print_send_ok |> wrap' |> wrap |> w_lock
      );
    ]
