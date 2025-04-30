let is_available = false

type t

let openlog ?logpath:_ ?facility:_ ?flags:_ _ : t = assert false
let syslog ?fac:_ _ _ _ : unit = assert false
let closelog _ : unit = assert false
