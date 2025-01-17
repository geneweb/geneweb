let status conf s = conf.Config.output_conf.Config.status s
let header conf fmt = Printf.ksprintf conf.Config.output_conf.Config.header fmt
let print_sstring conf s = conf.Config.output_conf.Config.body s

let print_string conf (s : [< `encoded | `escaped | `safe ] Adef.astring) =
  conf.Config.output_conf.Config.body ((s :> Adef.safe_string) :> string)

let printf conf fmt = Printf.ksprintf conf.Config.output_conf.Config.body fmt
let flush conf = conf.Config.output_conf.Config.flush ()
