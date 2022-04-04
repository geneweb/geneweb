
type gnwb_version =
  | GnWb20
  | GnWb21
  | GnWb22
  | GnWb23
  | GnWb24
  | GnWb25

val gnwb20 : gnwb_version
val gnwb21 : gnwb_version
val gnwb22 : gnwb_version
val gnwb23 : gnwb_version
val gnwb24 : gnwb_version
val gnwb25 : gnwb_version

val format_version : gnwb_version -> Format.version

val check_version : string -> gnwb_version option
