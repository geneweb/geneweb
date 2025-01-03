val save_mem : bool ref
(** Flag that enables memory saving by calling gc sometimes *)

val output : Dbdisk.dsk_base -> unit
(** [output base] uses data section of the [base] to store database on the disk
    in the files:

    - {i base} main file that stores all the arrays of the database
    - {i base.acc} direct accesses to arrays inside {i base} (list of offsets
      for every array)
    - {i names.inx} 3 different name indexes :

    - For all kind of mix between person's names. Associate hash value of the
      name to the array of persons (index of its entry) containing the given
      name
    - sub-strings of surname. Associate hash value of the sub-string to the
      array of string indexes of the names that contains mentionned sub string.
    - sub-strings of first name. Same storage principe as for surname
      sub-strings.
    - {i names.acc} direct accesses to arrays inside {i names.inx}.
    - {i strings.inx} strings index. Associate hash of the string to the index
      of its entry in the base's string array. Save also previus value in the
      case of collision of hash.
    - {i snames.inx} ordered index for surnames. Associate index of surname to
      its offset in {i snames.dat}.
    - {i snames.dat} For a giving surname give associated list of perosons
      (index of its entry)
    - {i fnames.inx} and {i fnames.dat} same as for {i snames.inx} and
      {i snames.dat} but deals with first names
    - {i notes} text file containing data base notes.
    - {i notes_d} directory containing .txt for each extended page.
    - {i particles.txt} text file with autorised name's particles. *)
