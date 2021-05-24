open Config

let status conf s =
  conf.output_conf.status s

let header conf fmt =
  Printf.ksprintf conf.output_conf.header fmt

let print_string conf s =
  conf.output_conf.body s

let print_file conf fname ctype priv =
  conf.output_conf.file fname ctype priv
  
let printf conf fmt =
  Printf.ksprintf conf.output_conf.body fmt

let flush conf =
  conf.output_conf.flush ()
