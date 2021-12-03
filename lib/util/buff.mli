
(** Functor building a local implementation of the buffer. *)
module Make :
  functor () ->
    sig
      (** Internal representation of the buffer *)
      val buff : bytes ref

      (** [store i c] stores a character [c] at the position [i] inside the buffer.
          Automatically extends buffer if needed. Returns the position that follows 
          inserted character ([i+1]) in buffer. Should be used either with position 
          0 or with position returned by previous calls of store functions. *)
      val store : int -> char -> int
    
      (** [mstore i s] stores a string [s] starting from the postion [i] inside the 
          buffer. Automatically extends buffer if needed. Returns the position that 
          follows inserted string in buffer. Should be used either with position 0 
          or with position returned by previous calls of store functions.*)
      val mstore : int -> string -> int

      (** [gstore i s si len] stores substring of [s] from [si] position with length 
          [len] inside the buffer starting from the postion [i]. Automatically extends 
          buffer if needed. Returns the position that follows inserted substring in 
          buffer. Should be used either with position 0 or with position returned 
          by previous calls of store functions.*)
      val gstore : int -> string -> int -> int -> int
      
      (** [get len] returns buffer's content until position [len] *)
      val get : int -> string

    end

(** Variable [buff] for the global buffer *)
val buff : bytes ref

(** Function [get] for the global buffer. *)
val get : int -> string

(** Function [store] for the global buffer. *)
val store : int -> char -> int

(** Function [mstore] for the global buffer. *)
val mstore : int -> string -> int

(** Function [gstore] for the global buffer. *)
val gstore : int -> string -> int -> int -> int
