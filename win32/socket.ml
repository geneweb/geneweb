external socket_to_addr : Unix.file_descr -> int64
  = "geneweb_win32_socket_to_addr"

external socket_of_addr : int64 -> Unix.file_descr
  = "geneweb_win32_socket_of_addr"

let input ic = socket_of_addr @@ input_value ic
let output oc a = output_value oc (socket_to_addr a)
