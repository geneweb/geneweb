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

(** [start_with prefix off str]
    Test if [str] starts with [prefix] (at offset [off]).

    Raise [Invalid_argument] if [off] is not a valid index in [str].
*)
val start_with : string -> int -> string -> bool

(** [start_with_wildcard prefix off str]
    Test if [str] starts with [prefix] (at offset [off]).
    Occurences of ['_'] in [prefix] will match both ['_']
    and [' '] in [str] and trailing ['_'] of [prefix]
    is treated as an optional ['_'] [' '].

    Raise [Invalid_argument] if [off] is not a valid index in [str].
*)
val start_with_wildcard : string -> int -> string -> bool

(** [contains str sub] Test [sub] is contained in [str].
*)
val contains : string -> string -> bool

(** [compile_particles list]
    Compile [list] so it can be used with [get_particle]
    or [compare_after_particle] function. *)
val compile_particles : string list -> Re.re

(** [get_particle particles name]
    Return [p] where [p] is in [particles] and is prefix of [name].
    If no such [p] exists, empty string [""] is returned. *)
val get_particle : Re.re -> string -> string

(** [compare_after_particle particles s1 s2] *)
val compare_after_particle : Re.re -> string -> string -> int

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

(** [list_find_map fn list]
    OCaml Stdlib's [List.find_map] (introduced in 4.10.0)
    backported into GeneWeb
  *)
val list_find_map : ('a -> 'b option) -> 'a list -> 'b option

(** [list_last list]
    Return the last element of the list.
    Raises [Failure] if the list is empty.
  *)
val list_last : 'a list -> 'a

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

(** Magic string generated from 30 random bits.
    It should be different each time you launch the program.
*)
val random_magic : string

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

(** [list_ref_append tl hd]
    Add [hd] at the beginning of [tl] ref.
 *)
val list_ref_append : 'a list ref -> 'a -> unit

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

(** [read_or_create_channel ?magic fname read write]

    If [fname] exists (and starts with [magic] if this one is provided),
    [read] function is used on the file.
    If it does not, or does not start with [magic], or if [read] raise an exception,
    [write] function is used on the file.

    This function takes care of locking and closing files so you must not take care of
    that in [read]/[write].
    It also takes care of writing [magic] at the beginning of the file before calling
    [write]

    On Windows, file is not locked.
*)
val read_or_create_channel
  :  ?magic:string
  -> ?wait:bool
  -> string
  -> (in_channel -> 'a)
  -> (out_channel -> 'a)
  -> 'a

(** [read_or_create_value ?magic fname create]

    If [fname] exists (and starts and ends with [magic] if this one is provided),
    return the unmarshalled value.
    If it does not, or does not start with [magic], or if unmarshalling raise an exception,
    [create] function is used to produce the value to be marshalled.

    On Windows, file is not locked.
*)
val read_or_create_value
  :  ?magic:string
  -> ?wait:bool
  -> string
  -> (unit -> 'a)
  -> 'a

(** [bench name fn]
    Execute [fn], print stats about time and memory allocation, return [fn] result.
 *)
val bench : string -> (unit -> 'a) -> 'a

val print_callstack : ?max:int -> unit -> unit

(** [encode s]
    Encodes the string [s] in another string
    where spaces and special characters are coded. This allows
    to put such strings in html links <a href=...>. This is
    the same encoding done by Web browsers in forms.
*)
val encode : string -> string

(** [decode s]
    Does the inverse job than [code],
    restoring the initial string. The heading and trailing spaces
    are stripped.
*)
val decode : string -> string

(** Like above but heading and trailing spaces are stripped
    only if bool parameter is [true]. [decode] = [gen_decode true].
*)
val gen_decode : bool -> string -> string

(** [extract_param name stopc request] can be used to extract some
    parameter from a browser [request] (list of strings); [name]
    is a string which should match the beginning of a request line,
    [stopc] is a character ending the request line. For example, the
    string request has been obtained by: [extract_param "GET /" ' '].
    Answers the empty string if the parameter is not found. *)
val extract_param : string -> char -> string list -> string

(** Print a date using "%04d-%02d-%02d %02d:%02d:%02d" format. *)
val sprintf_date : Unix.tm -> string

(** [rev_input_line ic pos (rbytes, rpos)]
    Read characters in reverse order from the given input channel,
    until a newline character is encountered.
    Return the string of all characters read, without the newline
    character at the end, and the position of the first character
    of the returned line (to be used with next [rev_input_line] call).

    [rpos] and [rbytes] must be the same in each subsequents calls

    Raises [End_of_file] if the beginning of the file is reached
    at the beginning of line.
*)
val rev_input_line : in_channel -> int -> (bytes ref * int ref) -> string * int

(** [search_file directories file]
    Search for a [file] in different [directories] and return
    then first result or [None] if not found
  *)
val search_file_opt : string list -> string -> string option

(** [search_asset fname]
    Searches for a file in assets directories.
    i.e. directories previously registered with [Secure.add_assets] *)
val search_asset_opt : string -> string option
