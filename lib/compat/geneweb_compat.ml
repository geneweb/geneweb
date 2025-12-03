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

  (* Read up to [len] bytes into [buf], starting at [ofs]. Return total bytes
     read. *)
  let read_upto ic buf ofs len =
    let rec loop ofs len =
      if len = 0 then ofs
      else
        let r = Stdlib.input ic buf ofs len in
        if r = 0 then ofs else loop (ofs + r) (len - r)
    in
    loop ofs len - ofs

  (* Best effort attempt to return a buffer with >= (ofs + n) bytes of storage,
     and such that it coincides with [buf] at indices < [ofs].

     The returned buffer is equal to [buf] itself if it already has sufficient
     free space.

     The returned buffer may have *fewer* than [ofs + n] bytes of storage if this
     number is > [Sys.max_string_length]. However the returned buffer will
     *always* have > [ofs] bytes of storage. In the limiting case when [ofs = len
     = Sys.max_string_length] (so that it is not possible to resize the buffer at
     all), an exception is raised. *)

  let ensure buf ofs n =
    let len = Bytes.length buf in
    if len >= ofs + n then buf
    else
      let new_len = ref len in
      while !new_len < ofs + n do
        new_len := (2 * !new_len) + 1
      done;
      let new_len = !new_len in
      let new_len =
        if new_len <= Sys.max_string_length then new_len
        else if ofs < Sys.max_string_length then Sys.max_string_length
        else
          failwith
            "In_channel.input_all: channel content is larger than maximum \
             string length"
      in
      let new_buf = Bytes.create new_len in
      Bytes.blit buf 0 new_buf 0 ofs;
      new_buf

  let input_all ic =
    let chunk_size = 65536 in
    (* IO_BUFFER_SIZE *)
    let initial_size =
      try Stdlib.in_channel_length ic - Stdlib.pos_in ic
      with Sys_error _ -> -1
    in
    let initial_size = if initial_size < 0 then chunk_size else initial_size in
    let initial_size =
      if initial_size <= Sys.max_string_length then initial_size
      else Sys.max_string_length
    in
    let buf = Bytes.create initial_size in
    let nread = read_upto ic buf 0 initial_size in
    if nread < initial_size then
      (* EOF reached, buffer partially filled *)
      Bytes.sub_string buf 0 nread
    else
      (* nread = initial_size, maybe EOF reached *)
      match Stdlib.input_char ic with
      | exception End_of_file ->
          (* EOF reached, buffer is completely filled *)
          Bytes.unsafe_to_string buf
      | c ->
          (* EOF not reached *)
          let rec loop buf ofs =
            let buf = ensure buf ofs chunk_size in
            let rem = Bytes.length buf - ofs in
            (* [rem] can be < [chunk_size] if buffer size close to
               [Sys.max_string_length] *)
            let r = read_upto ic buf ofs rem in
            if r < rem then (* EOF reached *)
              Bytes.sub_string buf 0 (ofs + r)
            else (* r = rem *)
              loop buf (ofs + rem)
          in
          let buf = ensure buf nread (chunk_size + 1) in
          Bytes.set buf nread c;
          loop buf (nread + 1)
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
