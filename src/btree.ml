(* $Id: btree.ml,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

module type OrderedType = sig type t = 'a; value compare : t -> t -> int; end;

module Make (Ord : OrderedType) =
  struct
    type key = Ord.t;
    type t 'a = [ Empty | Node of t 'a and key and 'a and t 'a and int ];
    value empty = Empty;
    value height =
      fun
      [ Empty -> 0
      | Node _ _ _ _ h -> h ]
    ;
    value create l x d r =
      let hl = height l
      and hr = height r in
      Node l x d r (if hl >= hr then hl + 1 else hr + 1)
    ;
    value bal l x d r =
      let hl =
        match l with
        [ Empty -> 0
        | Node _ _ _ _ h -> h ]
      in
      let hr =
        match r with
        [ Empty -> 0
        | Node _ _ _ _ h -> h ]
      in
      if hl > hr + 2 then
        match l with
        [ Empty -> invalid_arg "Map.bal"
        | Node ll lv ld lr _ ->
            if height ll >= height lr then create ll lv ld (create lr x d r)
            else
              match lr with
              [ Empty -> invalid_arg "Map.bal"
              | Node lrl lrv lrd lrr _ ->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r) ] ]
      else if hr > hl + 2 then
        match r with
        [ Empty -> invalid_arg "Map.bal"
        | Node rl rv rd rr _ ->
            if height rr >= height rl then create (create l x d rl) rv rd rr
            else
              match rl with
              [ Empty -> invalid_arg "Map.bal"
              | Node rll rlv rld rlr _ ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr) ] ]
      else Node l x d r (if hl >= hr then hl + 1 else hr + 1)
    ;
    value rec add x data =
      fun
      [ Empty -> Node Empty x data Empty 1
      | Node l v d r h ->
          let c = Ord.compare x v in
          if c = 0 then Node l x data r h
          else if c < 0 then bal (add x data l) v d r
          else bal l v d (add x data r) ]
    ;
    value rec find x =
      fun
      [ Empty -> raise Not_found
      | Node l v d r _ ->
          let c = Ord.compare x v in
          if c = 0 then d else find x (if c < 0 then l else r) ]
    ;
    value rec key_after f_compare =
      fun
      [ Empty -> raise Not_found
      | Node l v d r _ ->
          let c = f_compare v in
          if c < 0 then try key_after f_compare l with [ Not_found -> v ]
          else if c > 0 then key_after f_compare r
          else v ]
    ;
    value rec next x =
      fun
      [ Empty -> raise Not_found
      | Node l v d r _ ->
          let c = Ord.compare x v in
          if c < 0 then try next x l with [ Not_found -> v ]
          else next x r ]
    ;
    value rec merge t1 t2 =
      match (t1, t2) with
      [ (Empty, t) -> t
      | (t, Empty) -> t
      | (Node l1 v1 d1 r1 h1, Node l2 v2 d2 r2 h2) ->
          bal l1 v1 d1 (bal (merge r1 l2) v2 d2 r2) ]
    ;
    value rec remove x =
      fun
      [ Empty -> Empty
      | Node l v d r h ->
          let c = Ord.compare x v in
          if c = 0 then merge l r
          else if c < 0 then bal (remove x l) v d r
          else bal l v d (remove x r) ]
    ;
    value rec iter f =
      fun
      [ Empty -> ()
      | Node l v d r _ -> do { iter f l; f v d; iter f r } ]
    ;
    value rec map f =
      fun
      [ Empty -> Empty
      | Node l v d r h -> Node (map f l) v (f d) (map f r) h ]
    ;
    value rec fold f m accu =
      match m with
      [ Empty -> accu
      | Node l v d r _ -> fold f l (f v d (fold f r accu)) ]
    ;
  end
;
