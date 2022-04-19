
module GnWb25 : sig
  val gnwb25_format : Geneweb_dsk_format.Format.t
end = struct

  open Geneweb_dsk_format


  let make_variable_dat_part ~name ~acc_name =
    Format.make_partition
      ~name
      ~kind:Format.Dat
      ~size_type:(Format.Variable acc_name)

  let make_access_part ~name ~accessed = 
    Format.make_partition
      ~name
      ~kind:(Format.Acc accessed)
      ~size_type:(Format.Fixed 4)
    
  let persons_partition = make_variable_dat_part ~name:"persons" ~acc_name:"persons access"
  let persons_access_partition = make_access_part ~name:"persons access" ~accessed:"persons"
    
  let base_file =
    Format.make_file
      ~name:"base"
      ~partitions:[persons_partition]

  let persons_acc_file =
    Format.make_file
      ~name:"persons.acc"
      ~partitions:[persons_access_partition]
    
  let gnwb25_format =
    Format.make_format
      ~version:(Version.format_version Version.gnwb25)
      ~files:[base_file; persons_acc_file]

end
