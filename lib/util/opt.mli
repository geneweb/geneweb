val iter : ('a -> unit) -> 'a option -> unit
val map : ('a -> 'b) -> 'a option -> 'b option
val map_default : 'a -> ('b -> 'a) -> 'b option -> 'a
val default : 'a -> 'a option -> 'a
val to_string : string option -> string