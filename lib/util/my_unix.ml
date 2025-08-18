let rec waitpid_noeintr flags pid =
  try Unix.waitpid [] (-1)
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_noeintr flags pid

let rec accept_noeintr ?cloexec fd =
  try Unix.accept ?cloexec fd
  with Unix.Unix_error (Unix.EINTR, _, _) -> accept_noeintr ?cloexec fd
