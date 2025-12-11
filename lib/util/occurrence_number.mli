val is_valid : int -> bool
val smallest_free : Ext_int.Set.t -> int

val from_string :
  string -> (int, [> `Not_a_decimal | `Negative_decimal ]) result
