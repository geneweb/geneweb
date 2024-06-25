type t = GnWb20 | GnWb21 | GnWb22 | GnWb23 | GnWb24 | GnWb25

exception Not_a_geneweb_base
exception Unsupported_base

let gnwb20 = GnWb20
let gnwb21 = GnWb21
let gnwb22 = GnWb22
let gnwb23 = GnWb23
let gnwb24 = GnWb24
let gnwb25 = GnWb25

let check_version = function
  | "GnWb0020" -> Some gnwb20
  | "GnWb0021" -> Some gnwb21
  | "GnWb0022" -> Some gnwb22
  | "GnWb0023" -> Some gnwb23
  | "GnWb0024" -> Some gnwb24
  | "GnWb0025" -> Some gnwb25
  | s when String.sub s 0 4 = "GnWb" -> raise Unsupported_base
  | _ -> raise Not_a_geneweb_base

let eq_version = ( = )
