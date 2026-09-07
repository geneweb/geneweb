let rec waitpid_noeintr flags pid =
  try Unix.waitpid [] (-1)
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_noeintr flags pid

let rec accept_noeintr ?cloexec fd =
  try Unix.accept ?cloexec fd
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_noeintr ?cloexec fd

let rec select_noeintr l1 l2 l3 t =
  try Unix.select l1 l2 l3 t
  with Unix.Unix_error (Unix.EINTR, _, _) -> select_noeintr l1 l2 l3 t
