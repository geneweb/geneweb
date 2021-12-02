(** [iter f o] if [o=Some x] then executes [f x]. *)
val iter : ('a -> unit) -> 'a option -> unit

(** [map f o] if [o=Some x] then returns [Some (f x)] otherwise returns None. *)
val map : ('a -> 'b) -> 'a option -> 'b option

(** [map_default d f o] if [o=Some x] then returns [(f x)] otherwise returns [d]. *)
val map_default : 'a -> ('b -> 'a) -> 'b option -> 'a

(** [default d o] if [o=Some x] then returns [x] otherwise returns [d]. *)
val default : 'a -> 'a option -> 'a

(** [to_string so] if [so=Some s] then returns [s] otherwise returns empty string.  *)
val to_string : string option -> string