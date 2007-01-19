(* $Id: pqueue.mli,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* Module [Pqueue]: priority queues. *)

(* This module implements priority queues, given a total ordering function
   over the elements inserted. All operations are purely applicative
   (no side effects).
   The implementation uses binomial queues from Chris Okasak.
   "add", "take" and "union" are in o(log n) in the worst case. *)

module type OrderedType = sig type t = 'a; value leq : t -> t -> bool; end;
          (* The input signature of the functor [Pqueue.Make].
             [t] is the type of the inserted elements.
             [leq] is a total ordering function over the elements.
             This is a two-argument function [f] returning [True] if the
             first argument is less or equal to the second one. *)

module type S =
  sig
    type elt = 'a;
    type t = 'a;
    value empty : t;
    value is_empty : t -> bool;
    value add : elt -> t -> t;
    value take : t -> (elt * t);
    value union : t -> t -> t;
  end
;

module Make (Ord : OrderedType) :
  S with type elt = Ord.t
;
