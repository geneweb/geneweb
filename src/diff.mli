(* $Id: diff.mli,v 5.1 2006-11-03 10:31:18 ddr Exp $ *)

(** Differences between two arrays. *)

value f : array 'a -> array 'a -> (array bool * array bool);
(** [Diff.f a1 a2] returns a couple of two arrays of booleans [(d1, d2)].
      [d1] has the same size as [a1].
      [d2] has the same size as [a2].
      [d1.(i)] is [True] if [a1.(i)] has no corresponding value in [a2].
      [d2.(i)] is [True] if [a2.(i)] has no corresponding value in [a1].
      [d1] and [s2] have the same number of values equal to [False].

    Can be used to write the [diff] program (comparison of two files),
    the input arrays being the array of lines of each file.

    Can be used also to compare two strings (they must have been exploded
    into arrays of chars), or two DNA strings, and so on.
*)
