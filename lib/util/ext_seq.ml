let next seq =
  match seq () with Seq.Cons (elt, seq) -> Some (elt, seq) | Seq.Nil -> None
