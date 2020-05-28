(* This code is directly copied from OCaml stdlib (Map module)
   with t*)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val mem:  key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val map: ('a -> 'b) -> 'a t -> 'b t
    (* Added for GeneWeb *)
    val key_after : (key -> int) -> 'a t -> key
    val next : key -> 'a t -> key
  end

module Make(Ord: OrderedType) : S with type key = Ord.t = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of {l:'a t; v:key; d:'a; r:'a t; h:int}

    let height = function
        Empty -> 0
      | Node {h} -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node {h} -> h in
      let hr = match r with Empty -> 0 | Node {h} -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node{l=ll; v=lv; d=ld; r=lr} ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node{l=lrl; v=lrv; d=lrd; r=lrr}->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node{l=rl; v=rv; d=rd; r=rr} ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node{l=rll; v=rlv; d=rld; r=rlr} ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let empty = Empty

    let rec add x data = function
        Empty ->
          Node{l=Empty; v=x; d=data; r=Empty; h=1}
      | Node {l; v; d; r; h} as m ->
          let c = Ord.compare x v in
          if c = 0 then
            if d == data then m else Node{l; v=x; d=data; r; h}
          else if c < 0 then
            let ll = add x data l in
            if l == ll then m else bal ll v d r
          else
            let rr = add x data r in
            if r == rr then m else bal l v d rr

    let rec find x = function
        Empty ->
          raise Not_found
      | Node {l; v; d; r} ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node {l; v; r} ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node {l=Empty; v; d} -> (v, d)
      | Node {l} -> min_binding l

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node {l=Empty; r} -> r
      | Node {l; v; d; r} -> bal (remove_min_binding l) v d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec update x f = function
        Empty ->
          begin match f None with
          | None -> Empty
          | Some data -> Node{l=Empty; v=x; d=data; r=Empty; h=1}
          end
      | Node {l; v; d; r; h} as m ->
          let c = Ord.compare x v in
          if c = 0 then begin
            match f (Some d) with
            | None -> merge l r
            | Some data ->
                if d == data then m else Node{l; v=x; d=data; r; h}
          end else if c < 0 then
            let ll = update x f l in
            if l == ll then m else bal ll v d r
          else
            let rr = update x f r in
            if r == rr then m else bal l v d rr

    let rec map f = function
        Empty ->
          Empty
      | Node {l; v; d; r; h} ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          Node{l=l'; v; d=d'; r=r'; h}

    (* Added for GeneWeb *)

    let rec key_after f_compare = function
      | Empty -> raise Not_found
      | Node {l; v; r; _} ->
        let c = f_compare v in
        if c < 0 then try key_after f_compare l with Not_found -> v
        else if c > 0 then key_after f_compare r
        else v

    let rec next x = function
      | Empty -> raise Not_found
      | Node {l; v; r; _} ->
        let c = Ord.compare x v in
        if c < 0 then try next x l with Not_found -> v else next x r

  end
