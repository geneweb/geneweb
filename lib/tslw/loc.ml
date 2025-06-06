type t = { start : int; stop : int }

let dummy = { start = -1; stop = -1 }

let[@inline always] mk start stop =
  assert (0 <= start && start <= stop);
  { start; stop }
