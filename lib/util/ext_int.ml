module Set = Set.Make (struct
  type t = int

  let compare = compare
end)
