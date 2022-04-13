

type t
   
type 'a inchan
type 'a outchan

val open_in :
  base_path:string ->
  format:Format.t ->
  partition_name:Format.partition_name ->
  conv:(bytes -> 'a) ->
  'a inchan

val open_out :
  base_path:string ->
  format:Format.t ->
  partition_name:Format.partition_name ->
  conv:('a -> bytes) ->
  'a outchan


(*val read : inchan -> int -> (bytes -> 'a) -> 'a
val write : outchan -> 'a -> ('a -> bytes) -> unit*)
  
(*val open_in : string -> int -> inchan*)
(*val open_out : string -> int -> outchan*)
  
val close_in : 'a inchan -> unit
val close_out : 'a outchan -> unit

(*val seek_in : inchan -> int -> unit
val seek_out : outchan -> int -> unit*)

(*val input_binary_int : inchan -> int
val output_binary_int : outchan -> int -> unit


val read_acc : inchan -> int -> int
 *)                                

val read : ic:'a inchan -> index:int -> 'a
val write : oc:'a outchan -> index:int -> value:'a -> unit
