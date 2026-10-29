(** Layered drawing of directed acyclic graphs (Sugiyama framework). *)

type layout = {
  x : int array;
      (** Centre abscissa of every vertex: the input vertices first, then the
          dummy vertices. *)
  rank : int array;  (** Layer of every vertex, 0 at the top. *)
  paths : int array list;
      (** Vertices crossed by each input edge, from its source to its target, in
          the order of the input edges. *)
  crossings : int;  (** Edge crossings of the chosen order. *)
  breadth : int;  (** Most vertices on one layer, dummies included. *)
  width : int;  (** Overall width, vertex widths included. *)
}

val layout :
  ?iterations:int ->
  ?separation:int ->
  ?slots:(int -> bool) ->
  widths:int array ->
  ranks:int array ->
  keys:int array ->
  (int * int) list ->
  layout
(** [layout ~widths ~ranks ~keys edges] lays out the vertices [0] to [n - 1] on
    the layers given by [ranks], every edge [(u, v)] satisfying
    [ranks.(u) < ranks.(v)]. Edges spanning several layers are split by dummy
    vertices of zero width, numbered from [n]. Each layer is first ordered by
    [keys], then by alternate median sweeps followed by adjacent transpositions,
    at most [iterations] times (24 by default), keeping the order with the
    fewest crossings.

    Abscissas, trial 1 (without [slots]): every vertex is pulled towards the
    mean of its neighbours, alternately above and below, keeping [separation]
    (16 by default) between vertices. On an ancestry it crowds the ancestors of
    the root person towards the centre and bends even the branches without
    implex.

    Abscissas, trial 2 (with [slots]): the layers whose rank satisfies [slots]
    are spread over the width of the broadest of them, every vertex, dummies
    included, at the centre of an equal slot; a complete binary ancestry is thus
    drawn with straight lines and sparse layers use the whole width. The
    vertices of the other layers are put at the mean of their neighbours above,
    or below when they have none. *)
