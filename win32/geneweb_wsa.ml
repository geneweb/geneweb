exception Error of int

let () = Callback.register_exception "geneweb_wsa.error" (Error 0)

module Protocol_info = Protocol_info
