

type t
   
type size = int

type ipart = int

type filename = string
type partition_name = string
                    
type version
val version_of_string : string -> version
val string_of_version : version -> string
   
type partition

type partition_kind =
| Acc of partition_name (* direct access via offsets arrays to {partition_name} *)
| Dat (* actual data *)

type size_type =
  Fixed of size
| Variable of partition_name

type file
   
val partition_table : file -> partition list
val filename : file -> filename
val files : t -> file list
val version : t -> version

val make_format : version -> file list -> t
val make_file : filename -> partition list -> file
val make_partition : partition_name -> partition_kind -> size_type -> partition    

type informations

val infos : partition_name -> t -> informations
val file_of_infos : informations -> file
val partition_of_infos : informations -> partition
val ipart_of_infos : informations -> ipart

val file_has_multiple_partitions : file -> bool

val content_size_of_partition : partition -> size_type

val partition_kind : partition -> partition_kind
