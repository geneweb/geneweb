(** Results computed from the index of the dictionaries, kept by the process.

    A page reading a whole dictionary (the spellings to group read every place
    of the base) pays the same seconds at each request, whereas its result only
    depends on the index and on the patches correcting it. A value of type
    ['a t] keeps the last result computed for each base, and gives it back as
    long as {!Geneweb_db.Driver.dict_version} does not change: until a wizard
    modifies the base or the index is rewritten. Each process of gwd has its own
    copy. *)

type 'a t
(** Results of one computation, one per base (and per [key]). *)

val create : unit -> 'a t

val get : 'a t -> ?key:string -> Geneweb_db.Driver.base -> (unit -> 'a) -> 'a
(** [get t ?key base compute] is [compute ()], or the result kept for [base] and
    [key] when it was computed from the same version of the index and of the
    patches. [key] tells apart the results of [t] that depend on something else
    than the base (a parameter of its .gwf, a part of the strings). Nothing is
    kept when the version is unknown. A result replaces the one of an earlier
    version: the memory used is the one of a single result per base and key. *)
