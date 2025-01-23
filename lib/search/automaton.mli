(** A Levenshtein automaton for an alphabet A, a pattern p in A* and a maximal
    distance d is NFA that recognizes the language of all the words in A*
    that are at Levenshtein distance at most d of p.

    This module implements a lazy Levenshtein automaton. *)

module type X = sig
  type word

  val pattern : word
  val max_dist : int
end

module type S = sig
  type char_
  type word

  type state
  (** State of the automaton. *)

  val init : state
  (** The initial state of the automaton. *)

  val next : char_ -> state -> state
  (** [next c st] returns the next state of the automaton after
      reading the character [c]. *)

  val accept : state -> bool
  (** [accept st] determines if the state [st] is accepted by the
      automaton. *)

  val can_match : state -> bool

  val recognize : word -> bool
  (** [recognize s] determines if the string [s] is recognized by the automaton. *)
end

(** Create a Levenshtein automaton for the alphabet [W], the pattern
    [pattern] and the maximal Levenshtein distance [max_dist]. *)
module Make (W : Word.S) (_ : X with type word = W.t) :
  S with type char_ = W.char_ and type word = W.t
