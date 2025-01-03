(* Copyright (c) 2006-2007 INRIA *)

val verbose : bool ref
(** Global variable that indicates either servers should be in verbose mode. *)

val list_iter_first : (bool -> 'a -> unit) -> 'a list -> unit
(** [list_iter_first f l] iter over first element with [f true] and over others
    with [f false]. *)

val strip_all_trailing_spaces : string -> string
(** Remove all trailing spaces in string *)

(* [decline dform dformat] encode name that could be declined (like in the czech language)
   and its declination form in more comprehensible for computer format.
   Declination form [dform] is one of the follows:
   - 'n' for nominative
   - 'a' for accusative
   - 'g' for genitif
   Declination format [dformat] describes how does a name changes throughout different
   declination forms comparing to the nominative form.
   See {{: https://geneweb.tuxfamily.org/wiki/declension }Declination in Geneweb} for more details.
   Example 1: [decline 'a' "Vladana:a:Vladanu:g:Vladany"] returns encoding "@(@(a)@(a?Vladanu:g?Vladany:Vladana))."
   Example 2: [decline 'a' "Vladana:a:-u:g:-y"] returns encoding "@(@(a)Vladan@(a?u:g?y:a))"
   @deprecated *)
val decline : char -> string -> string

val nominative : string -> string
(** Encodes name for nominative declination format.
    @deprecated *)

val mkdir_p : ?perm:int -> string -> unit
(** [mkdir_p ?perm dir] Create the directory [dir]. No error if existing, make
    parent directories as needed. *)

val lock_file : string -> string
(** Returns the name of a lock file (with extension .lck). Result is generally
    used as an argument for [Lock.control] function. *)

val initial : string -> int
(** Returns position of first capital letter in the name (0 if no capitals). *)

val input_particles : string -> string list
(** [input_particles fname] read file and returns list of lines. Empty lines are
    skipped. *)

val surnames_pieces : string -> string list
(** Divide surnames on pieces. Every separated word that contains at least 4
    character forms one piece. Words that contains less than 4 characters or
    words "saint" and "sainte" are considered as the particles and are attached
    to the another word to form a piece. If string contains less than two
    pieces, returns an empty list. *)

val utf_8_of_iso_8859_1 : string -> string
(** Convert encoded string with ISO 8859-1 to UTF 8 *)

val iso_8859_1_of_utf_8 : string -> string
(** Convert encoded string with UTF 8 to ISO 8859-1 *)

val roman_of_arabian : int -> string
(** Convert arabic number (int) to roman (string). Number should be < 4000. *)

val arabian_of_roman : string -> int
(** Convert roman number (string) to arabic (int). Number should be less or
    equal to MMMCMXCIX (3999). *)

val fallback : (string * string) list ref

val read_fallback : string -> string -> unit
(** reads a file lexicon.gwf which defines a possible fallback language for each
    of the available languages. Most of the lines of this file are commented and
    can be uncommented by the user who can place the new file in bases/etc/lang
*)

val input_lexicon :
  string -> (string, string) Hashtbl.t -> (unit -> in_channel) -> unit
(** [input_lexicon lang ht open_file] open {i lexicon.txt} file with
    [open_file ()], parse it and fill [ht] where key is the keyword and value is
    a coresponding traduction associated to a [lang] language code. The second
    parameter defines a set of fallback languages if a translation is not
    available in a given language. If traduction line has a form [->: kw] it
    associates to the current section name the value associated to [kw] section
    name inside [ht] (keyword alias). *)

module StrSet : Set.S with type elt = string
(** Set of strings *)

val tr : char -> char -> string -> string
(** [tr c1 c2 str] Return a new string which is the same as [str] with all
    occurences of [c1] replaced by [c2]. If [str] does not contain [c1] [str] is
    returned untouched. *)

val unsafe_tr : char -> char -> string -> string
(** [unsafe_tr c1 c2 str] Update [str] in place. Replace all occurences of [c1]
    by [c2]. *)

val array_to_list_map : ('a -> 'b) -> 'a array -> 'b list
(** [array_to_list_map fn a] is almost like [Array.to_list a |> List.map fn] but
    is more efficient.

    The list is constructed backward, so if [fn] have side effects it may not
    behave as excepted. *)

val array_to_list_rev_map : ('a -> 'b) -> 'a array -> 'b list
(** [array_to_list_revmap fn a] is almost like
    [Array.to_list a |> List.rev_map fn] but is more efficient. *)

val array_assoc : 'k -> ('k * 'v) array -> 'v
(** [array_assoc k arr] returns the value associated with key [k] in the array
    of pairs [arr]. That is, [array_assoc k [| ... ; (k,v) ; ... |] = v] if
    [(k,v)] is the leftmost binding of a in array [arr]. Raise [Not_found] if
    there is no value associated with [k] in [arr]. *)

val start_with : string -> int -> string -> bool
(** [start_with prefix off str] Test if [str] starts with [prefix] (at offset
    [off]).

    Raise [Invalid_argument] if [off] is not a valid index in [str]. *)

val start_with_wildcard : string -> int -> string -> bool
(** [start_with_wildcard prefix off str] Test if [str] starts with [prefix] (at
    offset [off]). Occurences of ['_'] in [prefix] will match both ['_'] and
    [' '] in [str] and trailing ['_'] of [prefix] is treated as an optional
    ['_'] [' '].

    Raise [Invalid_argument] if [off] is not a valid index in [str]. *)

val contains : string -> string -> bool
(** [contains str sub] Test [sub] is contained in [str]. *)

val compile_particles : string list -> Re.re
(** [compile_particles list] Compile [list] so it can be used with
    [get_particle] or [compare_after_particle] function. *)

val get_particle : Re.re -> string -> string
(** [get_particle particles name] Return [p] where [p] is in [particles] and is
    prefix of [name]. If no such [p] exists, empty string [""] is returned. *)

val compare_after_particle : Re.re -> string -> string -> int
(** [compare_after_particle particles s1 s2] compare strings [s1] [s2] starting
    from the first character after particle's match. If they are equal, compare
    particles. *)

val rm : string -> unit
(** [rm fname] Remove [fname]. If [fname] does not exists, do nothing. *)

val mv : string -> string -> unit
(** [mv src dst] Move [src] to [dst]. If [src] does not exists, do nothing. *)

val string_of_int_sep : string -> int -> string
(** [string_of_int_sep "," 1000000] is ["1,000,000"] *)

val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
(** [list_compare cmp l1 l2] Comparison function for lists, using [cmp] to
    compare each elements *)

val list_find_map : ('a -> 'b option) -> 'a list -> 'b option
(** [list_find_map fn list] OCaml Stdlib's [List.find_map] (introduced in
    4.10.0) backported into GeneWeb *)

val array_find_map : ('a -> 'b option) -> 'a array -> 'b option
(** [array_find_map f a] applies [f] to the elements of [a] in order, and
    returns the first result of the form [Some v], or [None] if none exist. TODO
    OCaml 4.13; use Stdlib *)

val list_rev_iter : ('a -> unit) -> 'a list -> unit
(** [list_rev_iter f l] gives the same result as [List.rev l |> List.iter fn],
    but without creating intermediate list (not tail-recursive). *)

val list_last : 'a list -> 'a
(** [list_last list] Return the last element of the list. Raises [Failure] if
    the list is empty. *)

val list_slice : int -> int -> 'a list -> 'a list
(** [list_slice from_ to_ list] Extracts elements from [a]-nth (starts with
    zero, inclusive) to [b]-nth (exclusive). If [list] is not long enough,
    result will be shorter than requested, but the function will not fail. *)

val check_magic : string -> in_channel -> bool
(** [check_magic magic ic] Read (and consume) the [magic] string at the
    beggining of [ic] and return [true]. If [ic] does not start with [magic],
    reset the reading position of [ic] to where is was before you call
    [check_magic] and return [false]. *)

val executable_magic : string
(** Magic string are either get from {i GW_EXECUTABLE_MAGIC} environement
    variable either generated from the md5sum of the running executable. It can
    be used for volatile files which can be easily corrupted by any change in
    program or data representation. *)

val random_magic : string
(** Magic string generated from 30 random bits. It should be different each time
    you launch the program. *)

val array_except : 'a -> 'a array -> 'a array
(** [array_except value array] Return a new array containing all the elements
    from [array] except the first occurence of [value] *)

val default_particles : string list
(** List of default particles used in GeneWeb *)

val array_forall2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
(** [array_forall2 p a b] Checks if all elements of the arrays satisfy the
    predicate [p]. That is, it returns
    [(p a1 b1) && (p a2 b2) && ... && (p an bn)]. Raise Invalid_argument if the
    two lists are determined to have different lengths. *)

val list_replace : 'a -> 'a -> 'a list -> 'a list
(** [list_replace old_v new_v list] Return the same list as [list] were the
    first occurence of [old_v] has been replaced by [new_v]. If [old_v] is
    unbound, the list is returned unchanged. *)

val list_except : 'a -> 'a list -> 'a list
(** [list_except x list] Return a list containing all the elements from [list]
    except the first occurence of [x]. *)

val list_index : 'a -> 'a list -> int
(** [list_index element list] Finds the index of [element] in list. Raises
    [Not_found] if it does not exists. *)

val list_ref_append : 'a list ref -> 'a -> unit
(** [list_ref_append tl hd] Add [hd] at the beginning of [tl] ref. *)

val input_file_ic : in_channel -> string
(** Read the content of a file. Starts from the position where it is when
    calling [input_file_ic], and read until the end of the file.

    This function avoid crashes with text files on Windows platform.

    If the channel is opened on a file that is not a regular file, the result is
    meaningless. *)

val normalize_utf_8 : string -> string
(** [normalize_utf_8 s] Return [s] normalized using
    {{:http://www.unicode.org/glossary/#normalization_form_c}NFC} with all
    malformed UTF-8 character replaced by
    {{:http://unicode.org/glossary/#replacement_character}the replacement
     character} *)

val list_map_sort_uniq : ('a -> 'b) -> 'a list -> 'b list
(** [list_map_sort_uniq f l] apply [f] to every element and return sorted with
    Merge Sort algorithm list where every element is unique. *)

val list_rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list
(** [list_rev_map_append f l1 l2] apply [f] to every element in [l1], reverse it
    and concat with [l2]. *)

val read_or_create_channel :
  ?magic:string ->
  ?wait:bool ->
  string ->
  (in_channel -> 'a) ->
  (out_channel -> 'a) ->
  'a
(** [read_or_create_channel ?magic fname read write]

    If [fname] exists (and starts with [magic] if this one is provided), [read]
    function is used on the file. If it does not, or does not start with
    [magic], or if [read] raise an exception, [write] function is used on the
    file.

    This function takes care of locking and closing files so you must not take
    care of that in [read]/[write]. It also takes care of writing [magic] at the
    beginning of the file before calling [write]

    On Windows, file is not locked. *)

val read_or_create_value :
  ?magic:string -> ?wait:bool -> string -> (unit -> 'a) -> 'a
(** [read_or_create_value ?magic fname create]

    If [fname] exists (and starts and ends with [magic] if this one is
    provided), return the unmarshalled value. If it does not, or does not start
    with [magic], or if unmarshalling raise an exception, [create] function is
    used to produce the value to be marshalled.

    On Windows, file is not locked. *)

val bench : string -> (unit -> 'a) -> 'a
(** [bench name fn] Execute [fn], print stats about time and memory allocation,
    return [fn] result. *)

val print_callstack : ?max:int -> unit -> unit
(** Prints call stack on stderr with at most [max] entries. *)

val encode : string -> Adef.encoded_string
(** [encode s] Encodes the string [s] in another string where spaces and special
    characters are coded. This allows to put such strings in html links <a
    href=...>. This is the same encoding done by Web browsers in forms. *)

val decode : Adef.encoded_string -> string
(** [decode s] Does the inverse job than [code], restoring the initial string.
    The heading and trailing spaces are stripped. *)

val gen_decode : bool -> Adef.encoded_string -> string
(** Like above but heading and trailing spaces are stripped only if bool
    parameter is [true]. [decode] = [gen_decode true]. *)

val extract_param : string -> char -> string list -> string
(** [extract_param name stopc request] can be used to extract some parameter
    from a browser [request] (list of strings); [name] is a string which should
    match the beginning of a request line, [stopc] is a character ending the
    request line. For example, the string request has been obtained by:
    [extract_param "GET /" ' ']. Answers the empty string if the parameter is
    not found. *)

val sprintf_date : Unix.tm -> Adef.safe_string
(** Print a date using "%04d-%02d-%02d %02d:%02d:%02d" format Example :
    2021-12-13 22:35:08. *)

val rev_input_line : in_channel -> int -> bytes ref * int ref -> string * int
(** [rev_input_line ic pos (rbytes, rpos)] Read characters in reverse order from
    the given input channel, until a newline character is encountered. Return
    the string of all characters read, without the newline character at the end,
    and the position of the first character of the returned line (to be used
    with next [rev_input_line] call).

    [rpos] and [rbytes] are intermediate between [ic] and reading functions. At
    the beginig when [!rpos = 0] and [rbytes] is empty, initialise buffer with
    the size = 1024, then reads last 1024 characters from [ci]. When [rpos]
    comes down to 0, resize buffer *2 and reads 2048 characters before 1024 last
    characters. [rpos] and [rbytes] must be the same in each subsequents calls

    Raises [End_of_file] if the beginning of the file is reached at the
    beginning of line. *)

val search_file_opt : string list -> string -> string option
(** [search_file directories file] Search for a [file] in different
    [directories] and return then first result or [None] if not found *)

val search_asset_opt : string -> string option
(** [search_asset fname] Searches for a file in assets directories. i.e.
    directories previously registered with [Secure.add_assets] *)

val eq_key : string * string * int -> string * string * int -> bool
(** [eq_key (fn1, sn1, oc1) (fn2, sn2, oc2)] Tests if two persons would have the
    same key *)

val ls_r : string list -> string list
(** [ls_r dirs] List directories (and subdirectories) contents of [dirs],
    including [dirs] themselves. *)

val rm_rf : string -> unit
(** [rm_rf dir] Remove directory [dir] and everything inside [dir]. *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** [filter_map fn list] is a combination of map and filter. Not tail-recursive.
*)

val rev_iter : ('a -> unit) -> 'a list -> unit
(** [rev_iter fn list] is like [List.iter fn (List.rev list)]. Not
    tail-recursive. *)

val groupby :
  key:('a -> 'k) -> value:('a -> 'v) -> 'a list -> ('k * 'v list) list
(** [groupby ~key ~value list] Group the elements returning the same key
    together. Ordering of elements is unspecified. *)

val digest : string -> string
(** [digest s] Returns the (128 bits long, using MD5 algorithm) digest of [s].
*)

val empty_person : 'string -> 'string -> (unit, _, 'string) Def.gen_person
(** [empty_person empty quest] returns a Def.gen_person with [first_name] and
    [surname] initialized to [quest], other 'string field initialized to
    [empty], and only empty arrays/lists. *)

val empty_family : 'string -> (_, unit, 'string) Def.gen_family
(** [empty_family empty] returns a Def.gen_person with string field initialized
    initialized with [empty] and only empty arrays/lists. *)

val good_name : string -> bool
(** test the base name fir accepted characters: a..z, A..Z, - *)
