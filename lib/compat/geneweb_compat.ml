(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "-32-33"]

module In_channel = struct
  type t = in_channel

  let with_open openfun s f =
    let ic = openfun s in
    Fun.protect ~finally:(fun () -> Stdlib.close_in_noerr ic) (fun () -> f ic)

  let with_open_bin s f = with_open Stdlib.open_in_bin s f
  let with_open_text s f = with_open Stdlib.open_in s f

  let with_open_gen flags perm s f =
    with_open (Stdlib.open_in_gen flags perm) s f

  let input = Stdlib.input

  let input_line ic =
    match Stdlib.input_line ic with
    | s -> Some s
    | exception End_of_file -> None
end

module Out_channel = struct
  type t = out_channel

  let with_open openfun s f =
    let oc = openfun s in
    Fun.protect ~finally:(fun () -> Stdlib.close_out_noerr oc) (fun () -> f oc)

  let with_open_bin s f = with_open Stdlib.open_out_bin s f
  let with_open_text s f = with_open Stdlib.open_out s f

  let with_open_gen flags perm s f =
    with_open (Stdlib.open_out_gen flags perm) s f

  let flush = Stdlib.flush
  let output = Stdlib.output
end

module List = struct
  open List

  let rec equal eq l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | [], _ :: _ | _ :: _, [] -> false
    | a1 :: l1, a2 :: l2 -> eq a1 a2 && equal eq l1 l2

  include List
end

module Seq = struct
  open Seq

  let empty () = Nil

  (* [take] is defined in such a way that [take 0 xs] returns [empty]
     immediately, without allocating any memory. *)

  let rec take_aux n xs =
    if n = 0 then empty
    else fun () ->
      match xs () with
      | Nil -> Nil
      | Cons (x, xs) -> Cons (x, take_aux (n - 1) xs)

  let take n xs =
    if n < 0 then invalid_arg "Seq.take";
    take_aux n xs

  let rec append seq1 seq2 () =
    match seq1 () with
    | Nil -> seq2 ()
    | Cons (x, next) -> Cons (x, append next seq2)

  let rec concat seq () =
    match seq () with Nil -> Nil | Cons (x, next) -> append x (concat next) ()

  let rec equal eq xs ys =
    match (xs (), ys ()) with
    | Nil, Nil -> true
    | Cons (x, xs), Cons (y, ys) -> eq x y && equal eq xs ys
    | Nil, Cons (_, _) | Cons (_, _), Nil -> false

  include Seq
end
