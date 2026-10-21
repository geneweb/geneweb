(** Functor building a local implementation of the buffer. *)
module Make () : sig
  val buff : bytes ref
  (** Internal representation of the buffer *)

  val store : int -> char -> int
  (** [store i c] stores a character [c] at the position [i] inside the buffer.
      Automatically extends buffer if needed. Returns the position that follows
      inserted character ([i+1]) in buffer. Should be used either with position
      0 or with position returned by previous calls of store functions. *)

  val mstore : int -> string -> int
  (** [mstore i s] stores a string [s] starting from the postion [i] inside the
      buffer. Automatically extends buffer if needed. Returns the position that
      follows inserted string in buffer. Should be used either with position 0
      or with position returned by previous calls of store functions.*)

  val gstore : int -> string -> int -> int -> int
  (** [gstore i s si len] stores substring of [s] from [si] position with length
      [len] inside the buffer starting from the postion [i]. Automatically
      extends buffer if needed. Returns the position that follows inserted
      substring in buffer. Should be used either with position 0 or with
      position returned by previous calls of store functions.*)

  val get : int -> string
  (** [get len] returns buffer's content until position [len] *)
end

val buff : bytes ref
(** Variable [buff] for the global buffer *)

val get : int -> string
(** Function [get] for the global buffer. *)

val store : int -> char -> int
(** Function [store] for the global buffer. *)

val mstore : int -> string -> int
(** Function [mstore] for the global buffer. *)

val gstore : int -> string -> int -> int -> int
(** Function [gstore] for the global buffer. *)
