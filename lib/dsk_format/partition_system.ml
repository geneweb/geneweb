
                 
type ipart = int

let binary_int_size = 4

let solo_partition_ipart = -1
external ipart_of_int : int -> ipart = "%identity"

let is_solo_partition ipart = ipart = solo_partition_ipart

(* assumes ic corresponding file  has a master partition header *)
let partition_offset_of_ipart ic ipart : int =
  seek_in ic (ipart * binary_int_size);
  let offset = input_binary_int ic in
  offset



type channel_infos = {
    start_offset : int;
    base_path : string
  }

type ic = {
    infos : channel_infos;
    ic : in_channel
  }
type oc = {
    infos : channel_infos;
    oc : out_channel
  }


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

module type Partition_S = sig
  type data
  val to_bytes : data -> bytes
  val of_bytes : bytes -> data
  val filename : string
  val ipart : ipart
end

module type AccessPartitionSystem = PartitionSystem with type data = int
                        
module type FixedSizePartition_S = sig
  include Partition_S
  val data_bytes_size : int
end




let open_in' base_path ipart fname =
  let ic = Secure.open_in_bin (Filename.concat base_path fname) in
  let infos =
    if is_solo_partition ipart then
      { start_offset = 0; base_path }
    else
      { start_offset = partition_offset_of_ipart ic ipart; base_path }
  in { ic; infos }

let close_in' ic = close_in ic.ic
let close_out' oc = close_out oc.oc

let seek_in' ic pos = seek_in ic.ic (ic.infos.start_offset + pos)

module MakeFixedSize (P : FixedSizePartition_S) : PartitionSystem with type data = P.data = struct

  type data = P.data
  type inchan = ic
  type outchan = oc
               
  let open_in ~base_path = open_in' base_path P.ipart P.filename
    
  let open_out  ~base_path = assert false

  let close_in = close_in'
  let close_out = close_out'

  let read ~ic ~index =
    seek_in' ic (index * P.data_bytes_size);
    let b = Bytes.create (P.data_bytes_size) in
    really_input ic.ic b 0 P.data_bytes_size;
    P.of_bytes b

  let writes ~oc data = ()
                      
                     
end

module type AccessPartition_S = sig
  val filename : string
  val ipart : ipart
end
                              
module MakeAccessPartition (P : AccessPartition_S) : AccessPartitionSystem =
  MakeFixedSize (struct
      type data = int
      let to_bytes _ = assert false
      let of_bytes _ = assert false
      let filename = P.filename
      let ipart = P.ipart
      let data_bytes_size = binary_int_size
    end)
                                                                                               
module MakeVariableSize (P : Partition_S) (AP : AccessPartitionSystem) : PartitionSystem = struct

  type data = P.data
            
  type inchan = ic
              
  type outchan = oc
     
  let open_in ~base_path = open_in' base_path P.ipart P.filename
                         
  let open_out  ~base_path = assert false

  let close_in ic = close_in' ic
    
  let close_out oc = close_out' oc
                  
  let read ~(ic : inchan) ~index =
    let access_ic = AP.open_in ~base_path:ic.infos.base_path in
    let offset = AP.read ~ic:access_ic ~index in
    AP.close_in access_ic;
    seek_in' ic offset;
    let len = input_binary_int ic.ic in
    let b = Bytes.create len in
    really_input ic.ic b 0 len;
    P.of_bytes b
    
  let writes ~oc data = ()

                     
end
