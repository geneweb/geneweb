external _linux : unit -> bool = "geneweb_unix_linux"

let linux = _linux ()

external seteuid : int -> unit = "geneweb_unix_seteuid"
external setegid : int -> unit = "geneweb_unix_setegid"
external setreuid : int -> int -> unit = "geneweb_unix_setreuid"
external setregid : int -> int -> unit = "geneweb_unix_setregid"

let rec waitpid_noeintr flags pid =
  try Unix.waitpid [] (-1)
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_noeintr flags pid

let rec accept_noeintr ?cloexec fd =
  try Unix.accept ?cloexec fd
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_noeintr ?cloexec fd

let rec listen_noeintr fd n =
  try Unix.listen fd n
  with Unix.Unix_error (Unix.EINTR, _, _) -> listen_noeintr fd n
