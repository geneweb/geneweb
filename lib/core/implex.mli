(** Pedigree collapse (implex) and inbreeding structure of an ancestry.

    Generations are numbered from the parents (1). Unknown persons, whose first
    name and surname are both ["?"], are neither counted nor traversed. Results
    ignore access rights: callers mask persons at display time. *)

type generation = {
  gen : int;  (** Generation number. *)
  paths : int;  (** Known positions: paths from the root person. *)
  present : int;  (** Distinct ancestors present at this generation. *)
  fresh : int;  (** Ancestors first met at this generation. *)
  loops : int;
      (** Loops closed at this generation: parent links of the ancestors first
          met at the previous generation that reach an already met ancestor. *)
  period : (int * int) option;  (** Earliest and latest known years. *)
}

type ancestor = {
  ip : Geneweb_db.Driver.iper;
  first_gen : int;  (** Generation of the first occurrence. *)
  multiplicity : int;  (** Number of paths from the root person. *)
  weight : float;
      (** Expected genetic contribution: sum of 2{^ -g} over the paths. *)
}

type profile = { generations : generation list; ancestors : ancestor list }

val profile : Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> profile
(** [profile base ip] aggregates path counts per distinct ancestor, generation
    by generation, without enumerating paths. The sum of [paths] minus the sum
    of [fresh] is the number of repeated positions; the sum of [loops] is the
    cyclomatic number of the ancestry graph. *)

val loop_count :
  ?limit:int -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int
(** [loop_count base ip] is the sum of [loops] of [profile base ip], computed by
    a breadth-first walk expanding each ancestor once. With [~limit], the walk
    stops after the first generation reaching [limit] loops, so
    [loop_count ~limit:1 base ip > 0] cheaply tells whether the ancestry of [ip]
    has any loop. *)

type link = {
  ancestor : Geneweb_db.Driver.iper;  (** Repeated ancestor. *)
  partner : Geneweb_db.Driver.iper option;
      (** Other parent when both branches start from full siblings. *)
  sosa1 : Geneweb_sosa.t;
  sosa2 : Geneweb_sosa.t;  (** Two occurrences of [ancestor], [sosa1] first. *)
  len1 : int;  (** Generations from the father of the junction to [ancestor]. *)
  len2 : int;  (** Generations from the mother of the junction to [ancestor]. *)
  contribution : float;
      (** Wright term 2{^ -(len1+len2+1)}(1+F), summed over [ancestor] and
          [partner]. *)
}

type junction = {
  sosa : Geneweb_sosa.t;  (** Sosa number of the junction person. *)
  person : Geneweb_db.Driver.iper;
      (** Person whose parents are related by loops, or having a stored
          inbreeding coefficient. *)
  links : link list;
  direct : float;
      (** Sum of the contributions: part of the inbreeding coefficient of
          [person] due to loops between walked positions. The remainder comes
          from loops nested in repeated ancestries. *)
  nested : Geneweb_db.Driver.iper list;
      (** Minimal common ancestors of the parents of [person] missing from
          [links], when the stored inbreeding coefficient exceeds [direct]: the
          ancestors explaining the nested remainder. *)
  enclosing : Geneweb_sosa.t option;
      (** Nearest junction below on the same line. *)
  cluster : int;  (** Identifier of the cluster. *)
}

type cluster = {
  id : int;
  members : Geneweb_sosa.t list;  (** Junctions, by increasing Sosa number. *)
  common : Geneweb_db.Driver.Iper.Set.t;  (** Repeated ancestors. *)
  size : int;  (** Number of links. *)
}

type implex = {
  persons : Geneweb_db.Driver.iper list;
      (** Repeated ancestor, or father and mother repeated through the same
          children. *)
  occurrences : Geneweb_sosa.t list;
      (** Sosa numbers of the first person through its distinct children, by
          increasing value: the first occurrence, then one per loop closed. *)
  closing : Geneweb_sosa.t list;
      (** Junctions where these occurrences close loops, by increasing value. *)
  above : (int * int) Lazy.t;
      (** Known ancestors of [persons], and loops of the ancestry of a child of
          [persons]: for a single person, the loops of its own ancestry. *)
}

type structure = {
  implexes : implex list;  (** By increasing first occurrence. *)
  junctions : junction list;  (** By increasing Sosa number. *)
  clusters : cluster list;  (** By decreasing size, [id] counting from 1. *)
}

val structure : Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> structure
(** [structure base ip] pairs the occurrences of each repeated ancestor met by a
    breadth-first walk expanding each ancestor once. Each pair closes a loop at
    its junction, the common Sosa prefix of both occurrences. Both paths share
    no other person, so each pair gives an exact term of Wright's formula. The
    [nested] ancestors are searched in the parent links kept by the walk,
    indexed in arrays, without reading the database again. Clusters are the
    connected components of the graph linking each junction to the ancestors of
    its links and to its [nested] ancestors. *)
