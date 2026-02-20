module type S = sig
  type t
  type wit

  val dummy : t
  val compare : t -> t -> int
end

type ('k, 'c) t = (module S with type t = 'k and type wit = 'c)
