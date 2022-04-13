


module GnWb25F : sig
  val gnwb25_format : Format.t
end = struct

  let names_acc_file =
    Format.make_file
      "names.acc"
      [Format.make_partition "names access" (Format.Acc "names data") (Format.Fixed 4)]
    
  let gnwb25_format =
    Format.make_format
      (Version.format_version Version.gnwb25)
      [names_acc_file]

end

    
let names_inx_file =
  Format.make_file
    "names.inx2"
    [Format.make_partition "names index" (Format.Dat) (Format.Variable "names access")]
  
let names_acc_file =
  Format.make_file
    "names.acc2"
    [Format.make_partition "names access" (Format.Acc "names index") (Format.Fixed 4)]
    
let gnwb_legacy_ft =
  Format.make_format 
    (Version.format_version Version.gnwb24)
    [names_acc_file;
     names_inx_file;]
    
module GnWb25 = (struct
                  include GnWb25F
                            (*include Gwdb_legacy.Gwdb_driver*)
                end (*: Gwdb_legacy.Gwdb_driver.Gwdb_driver*))
              
let test base_name =

  

  
  (*  let ic = Partition.open_in ~base_name GnWb25F.gnwb25_format "names access" in*)
  (*  let value = Partition.read_acc ic 0 in*)
  let names_ic = Partition.open_in
             ~base_path:base_name
             ~format:gnwb_legacy_ft
             ~partition_name:"names index"
             ~conv:(fun x -> String.of_bytes x)
  in

  print_endline "open1";
  
  let names_oc = Partition.open_out
             ~base_path:base_name
             ~format:gnwb_legacy_ft
             ~partition_name:"names index"
             ~conv:(fun x -> String.to_bytes x)
  in

  print_endline "open2";
  
  let names_access_oc = Partition.open_out
             ~base_path:base_name
             ~format:gnwb_legacy_ft
             ~partition_name:"names access"
             ~conv:(fun x ->
               let b = Bytes.create 4 in
               Bytes.set_int32_be b 0 (Int32.of_int x);
               b
             )
  in

  print_endline "open3";
  
  Partition.write ~oc:names_access_oc ~index:0 ~value:0;

  print_endline "write1";
  
  Partition.write ~oc:names_access_oc ~index:4 ~value:10;

  print_endline "write2";

  Partition.close_out names_access_oc;
  
  Partition.write ~oc:names_oc ~index:0 ~value:"bidule";

  print_endline "write3";
  
  Partition.write ~oc:names_oc ~index:10 ~value:"machin";

  print_endline "write4";
  
  Partition.close_out names_oc;

  
  
  let s1 = Partition.read ~ic:names_ic ~index:0 in

  print_endline "read1";


  let s2 = Partition.read ~ic:names_ic ~index:1 in

  print_endline "read2";
  
  print_endline s1;
  print_endline s2;
  
  Partition.close_in names_ic;
  print_endline ("========================================" ^ (string_of_int 42 (*value*)))
