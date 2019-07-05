type istr = int
type iper = int
type ifam = int

external int_of_istr : istr -> int = "%identity"
external int_of_iper : iper -> int = "%identity"
external int_of_ifam : ifam -> int = "%identity"
external istr_of_int : int -> istr = "%identity"
external iper_of_int : int -> iper = "%identity"
external ifam_of_int : int -> ifam = "%identity"

let dummy_iper = -1
let dummy_ifam = -1
let dummy_istr = -1
