
type gnwb_version =
  | GnWb20
  | GnWb21
  | GnWb22
  | GnWb23
  | GnWb24
  | GnWb25

let gnwb20 = GnWb20
let gnwb21 = GnWb21
let gnwb22 = GnWb22
let gnwb23 = GnWb23
let gnwb24 = GnWb24
let gnwb25 = GnWb25

let magic_GnWb0020 = "GnWb0020"
let magic_GnWb0021 = "GnWb0021"
let magic_GnWb0022 = "GnWb0022"
let magic_GnWb0023 = "GnWb0023"
let magic_GnWb0024 = "GnWb0024"
let magic_GnWb0025 = "GnWb0025"
 
let format_version = function
  | GnWb20 -> Format.version_of_string magic_GnWb0020
  | GnWb21 -> Format.version_of_string magic_GnWb0021
  | GnWb22 -> Format.version_of_string magic_GnWb0022
  | GnWb23 -> Format.version_of_string magic_GnWb0023
  | GnWb24 -> Format.version_of_string magic_GnWb0024
  | GnWb25 -> Format.version_of_string magic_GnWb0025

let check_version = function
  | "GnWb0020" -> Some gnwb20
  | "GnWb0021" -> Some gnwb21
  | "GnWb0022" -> Some gnwb22
  | "GnWb0023" -> Some gnwb23
  | "GnWb0024" -> Some gnwb24
  | "GnWb0025" -> Some gnwb25
  | _  -> None
