(** Places to reorganize, in a French base: a town followed by its other parts
    in parentheses, the department among them, written as a code or not, such as

    {[
    Nantes (2ème canton) (Loire-Atlantique)
    Nantes (44) (Rue du Bocage)
    Nantes (44)  Saint Nicolas
    Nantes (2ème canton), Loire-Atlantique
    ]}

    are rewritten "the rest, town, department":

    {[
    2ème canton, Nantes, Loire-Atlantique
    Rue du Bocage, Nantes, Loire-Atlantique
    Saint Nicolas, Nantes, Loire-Atlantique
    ]}

    or "the rest, town (department)" ({!Parens}). The town is the text before
    the first parenthesis; the department is a part in parentheses or the next
    term, its code written with its name ({!FrDepartments}); the rest is the
    other parts, in their order; the terms after the department are kept. A
    place with a suburb, without department, with two different departments, or
    whose town is a department (["Paris (16)"]) is left out. *)

type format =
  | Commas  (** ["rest, town, department"] *)
  | Parens  (** ["rest, town (department)"] *)

val reorganize : format -> string -> string option
(** The place rewritten, [None] when it is left out or unchanged. *)

val town_and_department : string -> (string * string) option
(** The town and the department of a place that {!reorganize} rewrites. *)

type proposal = {
  istr : Geneweb_db.Driver.istr;
  raw : string;
  new_s : string;
  uses : int;
  known : bool;  (** the town is a term of the base found under the department *)
}

val proposals :
  format ->
  PlaceTerms.entry list ->
  (Geneweb_db.Driver.istr * string * int) list ->
  proposal list
(** The proposals for the [(istr, raw, uses)] places, [entries] being their
    terms (to know which towns are known under their department); sorted as the
    places. *)

val plan : Geneweb_db.Driver.base -> proposal list -> PlaceRename.item list
