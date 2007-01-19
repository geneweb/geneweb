(* $Id: pqueue.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

module type OrderedType = sig type t = 'a; value leq : t -> t -> bool; end;

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

module Make (Ord : OrderedType) =
  struct
    type elt = Ord.t;
    type t = list tree and tree = { node : elt; rank : int; list : t };
    value link t1 t2 =
      if Ord.leq t1.node t2.node then
        {node = t1.node; rank = t1.rank + 1; list = [t2 :: t1.list]}
      else {node = t2.node; rank = t2.rank + 1; list = [t1 :: t2.list]}
    ;
    value rec ins t =
      fun
      [ [] -> [t]
      | [t' :: ts] ->
          if t.rank < t'.rank then [t; t' :: ts] else ins (link t t') ts ]
    ;
    value rec union fts1 fts2 =
      match (fts1, fts2) with
      [ ([], ts) -> ts
      | (ts, []) -> ts
      | ([t1 :: ts1], [t2 :: ts2]) ->
          if t1.rank < t2.rank then [t1 :: union ts1 fts2]
          else if t2.rank < t1.rank then [t2 :: union fts1 ts2]
          else ins (link t1 t2) (union ts1 ts2) ]
    ;
    value empty : t = [];
    value is_empty (q : t) = q = [];
    value add x q = ins {node = x; rank = 0; list = []} q;
    value rec getMin =
      fun
      [ [] -> raise Not_found
      | [t] -> (t, [])
      | [t :: ts] ->
          let (t', ts') = getMin ts in
          if Ord.leq t.node t'.node then (t, ts) else (t', [t :: ts']) ]
    ;
    value take ts =
      let (t, ts) = getMin ts in (t.node, union (List.rev t.list) ts)
    ;
  end
;
