include Geneweb_log.Driver

let oc : out_channel option ref = ref None

let log fn =
  match !oc with
  | Some oc -> fn oc
  | None -> ()
