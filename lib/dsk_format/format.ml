
type size = int
type ipart = int
type filename = string
type partition_name = string

type version = string

external version_of_string : string -> version = "%identity"
external string_of_version : version -> string = "%identity"
           
type size_type =
  Fixed of size
| Variable of partition_name

type partition_kind =
| Acc of partition_name (* direct access via offsets arrays *)
| Dat (* actual data *)

type partition = {
    name : partition_name;
    size_type : size_type;
    kind : partition_kind;
  }
               
type file = {
    master_table : partition option;
    filename : filename;
    partitions : partition list;
  }

module FormatEnv : sig

  type t

  type partition_infos = {
      file : file;
      partition : partition;
      ipart : ipart
    }
     
  val empty : t
  val bind_partition : partition_name -> file -> partition -> ipart -> t -> t
  val lookup_partition : partition_name -> t -> partition_infos

  val bind_all_partitions : file -> t -> t
    
end = struct
  
  module StrMap = Map.Make(String)


  type partition_infos = {
      file : file;
      partition : partition;
      ipart : ipart;
    }

  type t = {
      (* keys are partition names, values are partition_infos *)
      partitions_env : partition_infos StrMap.t;
    }


  let empty = {partitions_env = StrMap.empty}

  let bind_partition pn file partition ipart e =
    {partitions_env = StrMap.add pn {file; partition; ipart} e.partitions_env}

  let lookup_partition pn e = StrMap.find pn e.partitions_env

  let fold_left_i f acc l =
    let rec aux n acc l = match l with
      | x :: xs ->
         let acc = f n acc x in
         aux (n + 1) acc xs
      | [] -> acc
    in
    aux 0 acc l
                            
  let bind_all_partitions f e =
    fold_left_i (fun index env partition ->
        bind_partition partition.name f partition index env
      ) e f.partitions
                            
end
              
type format = {
    env : FormatEnv.t;
    version : version;
    files : file list
  }

type t = format

let partition_table f = f.partitions
let filename f = f.filename
let files ft = ft.files
let version ft = ft.version

let make_partition ~name ~kind ~size_type = {name; kind; size_type}

let make_file ~name ~partitions =
  let filename = name in
  let master_table = match partitions with
    | [] | [_] -> None
    | _ ->
       Some {name = filename ^ " master partition";
             kind = Acc "";
             size_type = Fixed 4}
  in
  {master_table; filename; partitions}

let make_format_environment files =
  let env = FormatEnv.empty in
  let env = List.fold_left (fun e f -> FormatEnv.bind_all_partitions f e) env files in
  env
  
  
let make_format ~version ~files =
  let env = make_format_environment files in
  {env; version; files}


type informations = FormatEnv.partition_infos

let infos partition_name format =
  FormatEnv.lookup_partition partition_name format.env

let file_of_infos informations = informations.FormatEnv.file
let partition_of_infos informations = informations.FormatEnv.partition
let ipart_of_infos informations = informations.FormatEnv.ipart

let file_has_multiple_partitions file = file.master_table <> None
let content_size_of_partition partition = partition.size_type

let partition_kind partition = partition.kind

