val split_suburb : string -> string * string
val only_suburb : string -> string
val without_suburb : string -> string
val normalize : string -> string
val compare_places : string -> string -> int
val fold_place_long : bool -> string -> string list
val fold_place_short : bool -> string -> string

exception List_too_long

val get_all :
  Config.config ->
  Gwdb.base ->
  add_birth:bool ->
  add_baptism:bool ->
  add_death:bool ->
  add_burial:bool ->
  add_marriage:bool ->
  'a ->
  'c ->
  (string -> 'a) ->
  ('a -> bool) ->
  ('b option -> Gwdb.person -> 'b) ->
  ('b -> 'c) ->
  int ->
  ('a * 'c) array
