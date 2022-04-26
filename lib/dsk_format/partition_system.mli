
type ipart
val solo_partition_ipart : ipart
val is_solo_partition : ipart -> bool

(*module Seq : sig
  type 'a t
  val cons : 'a t -> ('a -> 'b t) -> 'b t
  val ret : 'a -> 'a t
  val step : 'a t -> 'a t
  val from_fun : (int -> 'a) -> 'a t
    
end*)
  
module type PartitionSystem = sig
  type data
  type inchan
  type outchan
  val open_in : base_path:string -> inchan
  val open_out : base_path:string -> outchan
  val close_in : inchan -> unit
  val close_out : outchan -> unit
  val read : ic:inchan -> index:int -> data
  val writes : oc:outchan -> (int * data) list -> unit
end

module type AccessPartitionSystem

module type AccessPartition_S = sig
  val filename : string
  val ipart : ipart
end
          
module type Partition_S = sig
  type data
  val to_bytes : data -> bytes
  val of_bytes : bytes -> data
  val filename : string
  val ipart : ipart
end
                            
module type FixedSizePartition_S = sig
  include Partition_S
  val data_bytes_size : int 
end
                                 
module MakeAccessPartition (P : AccessPartition_S) : AccessPartitionSystem

module MakeFixedSize (P : FixedSizePartition_S) : PartitionSystem

module MakeVariableSize (P : Partition_S) (AP : AccessPartitionSystem) : PartitionSystem


module type Patch_S = sig
  type t
  val read_patches : base_path:string -> t
  val commit_patches : base_path:string -> patch:t -> unit
end

module type Patch_I = sig
  type t
  val filename : string
end

module MakePatch (P : Patch_I) : Patch_S with type t = P.t
