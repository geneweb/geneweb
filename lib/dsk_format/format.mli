
(* Format type *)
type t
   
(* size of data representation in bytes *)
type size = int

(* index of a partition when several partitions are located in the same file *)          
type ipart = int

(* *)
type filename = string
(* *)
type partition_name = string
(* *)               
type version
val version_of_string : string -> version
val string_of_version : version -> string

(* the type of partitions *)
type partition

(* Paritions can either be used to store data, or to store access offsets to said data *)   
type partition_kind =
| Acc of partition_name (* direct access via offsets arrays to {partition_name} *)
| Dat (* actual data *)

(* Partition content can be either of fixed size or have a variable size,
   when content is of variable size we use another partition to store relations
   between index and offsets, ie. data corresponding to {index} is stored in the file
   at {offset} byte.
*)
type size_type =
  Fixed of size
| Variable of partition_name

(* The type of files *)            
type file

(* Returns the partitions stored in the given file, *)   
val partition_table : file -> partition list
(* *)
val filename : file -> filename
(* Returns all files from format *)
val files : t -> file list
(* Version of format *)
val version : t -> version

(* Builds format *)  
val make_format : version -> file list -> t
(* Builds file *)
val make_file : filename -> partition list -> file
(* Builds partition *)
val make_partition : partition_name -> partition_kind -> size_type -> partition    

(* the type of informations related to some partition *)  
type informations

(** [infos partition_name format]
    Retrieve informations related to [partition_name] from {format} *)
val infos : partition_name -> t -> informations
val file_of_infos : informations -> file
val partition_of_infos : informations -> partition
val ipart_of_infos : informations -> ipart

val file_has_multiple_partitions : file -> bool

val content_size_of_partition : partition -> size_type

val partition_kind : partition -> partition_kind
