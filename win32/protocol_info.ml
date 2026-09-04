type t

external init : unit -> unit = "geneweb_protocol_info_init"

external duplicate_socket : Unix.file_descr -> int -> t
  = "geneweb_protocol_info_duplicate_socket"

external to_socket : t -> Unix.file_descr = "geneweb_protocol_info_to_socket"

let () = if Sys.win32 then init ()
