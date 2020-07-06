(* Copyright (c) 2006-2007 INRIA *)

val int_size : int
val verbose : bool ref

val list_iter_first : (bool -> 'a -> unit) -> 'a list -> unit
val strip_all_trailing_spaces : string -> string

val decline : char -> string -> string
val nominative : string -> string

val mkdir_p : string -> unit
val remove_dir : string -> unit
val lock_file : string -> string

val name_key : string -> string
val initial : string -> int
val input_particles : string -> string list
val surnames_pieces : string -> string list

val utf_8_of_iso_8859_1 : string -> string
val iso_8859_1_of_utf_8 : string -> string

val roman_of_arabian : int -> string
val arabian_of_roman : string -> int

val compare_after_particle : string list -> string -> string -> int

val input_lexicon :
  string -> (string, string) Hashtbl.t -> (unit -> in_channel) -> unit

module StrSet : Set.S with type elt = string

(** [tr c1 c2 str]
    Return a new string which is the same as [str] with all occurences of [c1]
    replaced by [c2].
    If [str] does not contain [c1]. [str] is returned intouched.

 *)
val tr : char -> char -> string -> string

(** [unsafe_tr c1 c2 str]
    Update [str] in place. Replace all occurences of [c1] replaced by [c2].
 *)
val unsafe_tr : char -> char -> string -> string

(** [array_to_list_map fn a] is almost like [Array.to_list a |> List.map fn]
    but is more efficient.

    The list is constructed backward,
    so if [fn] have side effects it may not behave as excepted.
 *)
val array_to_list_map : ('a -> 'b) -> 'a array -> 'b list

(** [array_to_list_revmap fn a] is almost like [Array.to_list a |> List.rev_map fn]
    but is more efficient.
 *)
val array_to_list_rev_map : ('a -> 'b) -> 'a array -> 'b list

(** [array_assoc k arr]
    returns the value associated with key [k] in the array of pairs [arr].
    That is, [array_assoc k [| ... ; (k,v) ; ... |] = v]
    if [(k,v)] is the leftmost binding of a in array [arr].
    Raise [Not_found] if there is no value associated with [k] in [arr]. *)
val array_assoc : 'k -> ('k * 'v) array -> 'v

(** [start_with ?wildcard prefix off str]
    Test if [str] starts with [prefix] (at offset [off]).
    If [wildcard] is set to [true], occurences of ['_'] in [prefix]
    will match both ['_'] and [' '] in [str] and trailing ['_'] of [prefix]
    is treated as an optional ['_'] [' '].

    Raise [Invalid_argument] if [off] is not a valid index in [str].
*)
val start_with : ?wildcard:bool -> string -> int -> string -> bool

(** [contains ?wildcard str sub] Test [sub] is contained in [str].
    See {!val:start_with} for details about [wildcard]
    (with [sub] in the role of [prefix]).
*)
val contains : ?wildcard:bool -> string -> string -> bool

(** [get_particle particles name]
    Return [p] where [p] is in [particles] and is prefix of [name].
    If no such [p] exists, empty string [""] is returned. *)
val get_particle : string list -> string -> string

(** [rm fname]
    Remove [fname]. If [fname] does not exists, do nothing.
*)
val rm : string -> unit

(** [mv src dst]
    Move [src] to [dst]. If [src] does not exists, do nothing.
*)
val mv : string -> string -> unit

(** [string_of_int_sep "," 1000000] is ["1,000,000"]
*)
val string_of_int_sep : string -> int -> string

(** [list_compare cmp l1 l2]
    Comparison function for lists, using [cmp] to compare each elements
*)
val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

(** [check_magic magic ic]
    Read (and consume) the [magic] string at the beggining of [ic]
    and return [true].
    If [ic] does not start with [magic], reset the reading position
    of [ic] to where is was before you call [check_magic] and return [false].
*)
val check_magic : string -> in_channel -> bool

(** Magic string generated from the md5sum of the running executable.
    It can be used for volatile files which can be easily corrupted
    by any change in program or data representation.
*)
val executable_magic : string

(** [array_except value array]
    Return a new array containing all the elements
    from [array] except the first occurence of [value]
 *)
val array_except : 'a -> 'a array -> 'a array

(** List of default particles used in GeneWeb *)
val default_particles : string list

(** [array_forall2 p a b]
    Checks if all elements of the arrays satisfy the predicate [p].
    That is, it returns [(p a1 b1) && (p a2 b2) && ... && (p an bn)].
    Raise Invalid_argument if the two lists are determined to have different lengths.
*)
val array_forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool

(** [list_replace old_v new_v list]
    Return the same list as [list] were the first occurence of [old_v]
    has been replaced by [new_v]. If [old_v] is unbound, the list is
    returned unchanged.
*)
val list_replace : 'a -> 'a -> 'a list -> 'a list

(** [list_except old_v new_v list] *)
val list_except : 'a -> 'a list -> 'a list

(** Read the content of a file.
    Starts from the position where it is when calling [input_file_ic],
    and read until the end of the file.

    This function avoid crashes with text files on Windows platform.

    If the channel is opened on a file that is not a regular file,
    the result is meaningless.
*)
val input_file_ic : in_channel -> string

(** [normalize_utf_8 s]
    Return [s] normalized using
    {{:http://www.unicode.org/glossary/#normalization_form_c}NFC}
    with all malformed UTF-8 character replaced by
    {{:http://unicode.org/glossary/#replacement_character}the replacement character}
*)
val normalize_utf_8 : string -> string

val list_map_sort_uniq : ('a -> 'b) -> 'a list -> 'b list

val list_rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list
