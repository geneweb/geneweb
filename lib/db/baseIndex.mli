(** Index of the dictionaries of a base ({!Dict}), written with the files of the
    base.

    {i base.inx} starts with a magic number and a directory of sections, one per
    dictionary: its code, its number of entries [n], its block size, the width
    of its positions (4 or 8 bytes) and the offsets of its arrays. A section
    holds, in the order of the keys of its entries ({!Dict.key}):

    - the string ids of the entries (4 bytes each);
    - the positions of their lists in {i base.dat};
    - the landmarks: the key of one entry out of [block], truncated to 64 bytes,
      searched in memory before reading the strings of a block.

    {i base.dat} holds for each entry the number of records using its string,
    then these records: [2 * iper] for a person, [2 * ifam + 1] for a family.
    Only the sections of first names and surnames have an entry for the empty
    string; deleted families are left out. *)

val default_budget : int
(** Default size in bytes of the landmarks of a dictionary: 1 MiB. *)

val output : budget:int -> Dbdisk.base_data -> inx:string -> dat:string -> unit
(** [output ~budget data ~inx ~dat] writes the index of the persons, families
    and strings of [data], committed patches included and pending ones left
    aside, to the files [inx] and [dat]. The block size of a section is the
    smallest power of 2 whose landmarks fit in [budget] bytes. *)

val with_reader :
  string ->
  particles:Re.re Lazy.t ->
  strings_len:int ->
  file_string:(int -> string) ->
  (Dbdisk.dict_index -> 'a) ->
  'a
(** [with_reader bdir ~particles ~strings_len ~file_string k] calls [k] with the
    queries over the index of the base directory [bdir]. [file_string] is the
    string of an id in the files of the base, patches left aside, and
    [strings_len] their number. The files of the index are opened at the first
    query and closed when [k] returns.

    @raise Dutil.Incompatible_format
      at the first query if {i base.inx} is not in the format of this version.
*)
