(* Copyright (c) 2006-2007 INRIA *)

val verbose : bool ref
(** Global variable that indicates either servers should be in verbose mode. *)

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

val input_particles : string -> string list
(** [input_particles fname] read file and returns list of lines. Empty lines are
    skipped. *)

val surnames_pieces : string -> string list
(** Divide surnames on pieces. Every separated word that contains at least 4
    character forms one piece. Words that contains less than 4 characters or
    words "saint" and "sainte" are considered as the particles and are attached
    to the another word to form a piece. If string contains less than two
    pieces, returns an empty list. *)

val roman_of_arabian : int -> string
(** Convert arabic number (int) to roman (string). Number should be < 4000. *)

val arabian_of_roman : string -> int option
(** Convert roman number (string) to arabic (int). Number should be less or
    equal to MMMCMXCIX (3999). *)

val input_lexicon :
  string -> (string, string) Hashtbl.t -> (unit -> in_channel) -> unit
(** [input_lexicon lang ht open_file] open {i lexicon.txt} file with
    [open_file ()], parse it and fill [ht] where key is the keyword and value is
    a coresponding traduction associated to a [lang] language code. If
    traduction line has a form [->: kw] it associates to the current section
    name the value associated to [kw] section name inside [ht] (keyword alias).
*)

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

val string_of_int_sep : string -> int -> string
(** [string_of_int_sep "," 1000000] is ["1,000,000"] *)

val executable_magic : string
(** Magic string are either get from {i GW_EXECUTABLE_MAGIC} environement
    variable either generated from the md5sum of the running executable. It can
    be used for volatile files which can be easily corrupted by any change in
    program or data representation. *)

val random_magic : string
(** Magic string generated from 30 random bits. It should be different each time
    you launch the program. *)

val default_particles : string list
(** List of default particles used in GeneWeb *)

val input_file_ic : in_channel -> string
(** Read the content of a file. Starts from the position where it is when
    calling [input_file_ic], and read until the end of the file.

    If the channel is opened on a file that is not a regular file, the result is
    meaningless. *)

val read_file_content : string -> string
(** [read_file_content filename] Reads the content of the file with full path
    [filename]. *)

val bench : string -> (unit -> 'a) -> 'a
(** [bench name fn] Execute [fn], print stats about time and memory allocation,
    return [fn] result. *)

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

val extract_param : string -> char -> string list -> string option
(** [extract_param name stopc request] can be used to extract some parameter
    from a browser [request] (list of strings); [name] is a string which should
    match the beginning of a request line, [stopc] is a character ending the
    request line. For example, the string request has been obtained by:
    [extract_param "GET /" ' ']. *)

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

val eq_key : string * string * int -> string * string * int -> bool
(** [eq_key (fn1, sn1, oc1) (fn2, sn2, oc2)] Tests if two persons would have the
    same key *)

val empty_person : 'string -> 'string -> (unit, _, 'string) Def.gen_person
(** [empty_person empty quest] returns a Def.gen_person with [first_name] and
    [surname] initialized to [quest], other 'string field initialized to
    [empty], and only empty arrays/lists. *)

val empty_family : 'string -> (_, unit, 'string) Def.gen_family
(** [empty_family empty] returns a Def.gen_person with string field initialized
    initialized with [empty] and only empty arrays/lists. *)
