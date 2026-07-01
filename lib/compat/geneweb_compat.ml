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

module Out_channel = struct
  type t = out_channel

  external isatty : t -> bool = "caml_sys_isatty"
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

module String = struct
  open String

  let starts_with ~prefix s =
    let len_s = length s and len_pre = length prefix in
    let rec aux i =
      if i = len_pre then true
      else if unsafe_get s i <> unsafe_get prefix i then false
      else aux (i + 1)
    in
    len_s >= len_pre && aux 0

  include String
end
