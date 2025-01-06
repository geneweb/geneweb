module type S = sig
  type 'a t
  type char_
  type word

  val cardinal : 'a t -> int
  (** Return the cardinal of the trie. *)

  val mem : word -> 'a t -> bool
  (** [mem w t] checks if the word [w] is present in the trie [t]. *)

  (** {1 Modifying} *)

  val empty : 'a t
  (** An empty trie. *)

  val is_empty : 'a t -> bool
  (** Determine if the trie is empty. *)

  val add : word -> 'a -> 'a t -> 'a t
  (** [add w t v] inserts the word [w] with the value [v] into the trie [t].
      If [w] alreadyt exists in [t], its value is updated to [v].

      This function is not tail recursive but the stack usage is bounded by
      the length of the word [w]. *)

  val remove : word -> 'a t -> 'a t
  (** [remove w t] removes the word [w] in the trie [t] if it exists.

      This function is not tail recursive but the stack usage is bounded by
      the length of the word [w]. *)

  val update : word -> ('a option -> 'a option) -> 'a t -> 'a t
  (** [update w f t] updates the data of the word [w] in the trie [t].

      This function is not tail recursive but the stack usage is bounded by
      the length of the word [w]. *)

  (** {1 Iterators} *)

  val fold : (word -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f t acc] invokes [f] successively for every word in the trie [t]
      in lexicographic order. *)

  val fold_subtries : (char_ -> 'a t -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (word -> 'a -> unit) -> 'a t -> unit
  (** [iter f t] invokes [f] successively for every word in the trie [t] in
      lexicographic order. *)

  val step : char_ -> 'a t -> 'a t
  (** [step c t] returns the subtrie at the character [c] in [t]. *)

  val data : 'a t -> 'a option
  (** [data t] returns the data stored at the root of trie of [t]. *)

  (** {1 Converting} *)

  val of_seq : (word * 'a) Seq.t -> 'a t
  (** Create a trie from an associative sequence. *)

  val to_seq : 'a t -> (word * 'a) Seq.t
  (** Convert a trie into an associative sequence in lexicographic order. *)

  (** {1 Printing} *)

  val pp : 'a Fmt.t -> 'a t Fmt.t
  (** Prints all the binding in the trie in lexicographic order. *)

  val pp_statistics : 'a t Fmt.t
  (** Prints statistics information for debugging. *)
end

module Make (W : Word.S) : S with type char_ = W.char_ and type word = W.t
module Default : S with type char_ = char and type word = string
