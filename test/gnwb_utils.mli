
module Gwdb_utils : sig
  type iper = Gwdb.iper
  type ifam = Gwdb.ifam
  type istr = Gwdb.istr
  val ifam : int -> ifam
  val iper : int -> iper
  val istr : int -> istr
end

type iper
type ifam
type istr

val ifam : int -> ifam
val iper : int -> iper
val istr : int -> istr


type person = (int, int, int) Def.gen_person
type family = (int, int, int) Def.gen_family
type descend = int Def.gen_descend
type ascend = int Def.gen_ascend
type union = int Def.gen_union
type couple = int Def.gen_couple

val empty_string : int
val quest_string : int

val default_strings : unit -> string array

val union : families:(ifam array) -> union
val couple : father:iper -> mother:iper -> couple
val ascend : parents:(ifam option) -> ascend
val descend : children:iper array -> descend

val person : iper -> person
val family : ifam -> family
