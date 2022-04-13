
(**
   Partitions store either :
   - data
   then Partition kind is Format.Dat
   - relation table between index and byte offset to read indexed data
   then Partition kind is Format.Acc

   Access partitions only store the offset from the relation table index -- offset,
   it is stored as a binary big endian 32 bits integer as per ouptut_binary_int/input_binary_int
   semantic from Stdlib. Offset to read data indexed as number n is then readable
   at offset (4 * n) in the partition.

   A given file may hold several partitions at once, when it is the case the first partition of
   the file is the master table storing offsets to the other partitions. It behaves as an access
   partitions where indexes correspond to partition indexes.
 *)


(*type t*)
   
type 'a inchan = {
    start_offset : int;
    inchan : in_channel;
    infos : Format.informations;
    conv : (bytes -> 'a);
    base_path : string;
    format : Format.t;
  }

type 'a outchan = {
    start_offset : int;
    outchan : out_channel;
    infos : Format.informations;
    conv : ('a -> bytes);
    base_path : string;
    format : Format.t;
  }


(* bytes size of binary int *)
let binary_int_size = 4

(* opening a partition input channel *)
let open_in' ~format ~infos ~base_path ~fname ~start_offset ~conv =
  let inchan = Secure.open_in_bin (Filename.concat base_path fname) in
  seek_in inchan start_offset;
  { inchan; start_offset; infos; conv; base_path; format }

(* opening a partition output channel *)  
let open_out' ~format ~infos ~base_path ~fname ~start_offset ~conv =
  let outchan = Secure.open_out_bin (Filename.concat base_path fname) in
  seek_out outchan start_offset;
  { outchan; start_offset; infos; conv; base_path; format }

(* seek_in and seek_out for partition channels *)
let seek_in' p i  = seek_in p.inchan (p.start_offset + i)
let seek_out' p i = seek_out p.outchan (p.start_offset + i)
(* close_in and close_out for partition channels *)
let close_in' p = close_in p.inchan
let close_out' p = close_out p.outchan

(* input_binary_int output_binary_int for partition channels *)
let input_binary_int' p = input_binary_int p.inchan
let output_binary_int' p n = output_binary_int p.outchan n



let loop max f x =
  assert (max >= 0);
  let rec loop n x =
    if n = max then x
    else loop (n + 1) (f n x)
  in
  loop 0 x


(*let read_acc_partition ic : int array =
  let len = input_binary_int' ic in
  let ar = Array.make len 0 in
  let _ = loop len (fun n _ -> ar.(n) <- (input_binary_int' ic)) () in
  ar
 *)

(* [read_acc_partition_i inchan index]
   Returns the [offset] for [index] read from inchan.
   assumes partition channel [inchan] to have been opened on an access partition and
   index to be valid (in the partition)
*)
let read_acc_partition_i ic i : int =
  seek_in' ic (binary_int_size * i);
  input_binary_int' ic

let read_acc = read_acc_partition_i

(* All multi partition file start with a master table of partitions storing
   their start offset in the file.
   We return the in_channel and the found offset.
 *)
let partition_offset format base_path infos conv =
  let file = Format.file_of_infos infos in
  let fname = Format.filename file in
  let ic = open_in' ~format ~infos ~base_path ~fname ~start_offset:0 ~conv in
  (* If it is a multiparted file we read the start offset corresponding to the
     index of the partition in the master table. *)
  let mult = Format.file_has_multiple_partitions file in  
  if mult then
    let ipart = Format.ipart_of_infos infos in
    let offset = read_acc ic ipart in ic, offset
  else ic, 0

(* Opens an input channel to a partition *)
let open_in ~base_path ~format ~partition_name ~conv =
  let infos = Format.infos partition_name format in
  let ic, offset = partition_offset format base_path infos conv in
  seek_in' ic offset; ic

(* Opens an output channel to a partition *)
let open_out ~base_path ~format ~partition_name ~conv =
  let infos = Format.infos partition_name format in
  let fname = Format.filename (Format.file_of_infos infos) in
  let ic, start_offset = partition_offset format base_path infos (fun _ -> assert false) in
  close_in' ic;
  open_out' ~format ~infos ~base_path ~fname ~start_offset ~conv


(*
(* oc should be correctly placed *)
let write_acc_partition part_oc acc : unit =
  let len = Array.length acc in
  output_binary_int part_oc len;
  let _ = loop len (fun n _ -> output_binary_int part_oc acc.(n)) () in
  ()
 *)
  
let close_in = close_in'
let close_out = close_out'

(* Reads len bytes from Stdlib.in_channel ic and convert
   those using the given conv function. *)
let read ic len conv =
  let b = Bytes.create len in
  really_input ic b 0 len;
  conv b

(* Convert value to bytes using conv function and writes those bytes
   in the given Stdlib.out_channel oc *)
let write ?(variable = false) oc v conv =
  let b = conv v in
  if variable then begin
    let len = Bytes.length b in
    output_binary_int oc len end;
  output_bytes oc b

(* Determines the offset of given data index by searching it in the access partition *)
let offset_of_index_in_partition (ic : 'a inchan) index access_partition_name =
  let ic_access =
    open_in ~base_path:ic.base_path ~format:ic.format
      ~partition_name:access_partition_name ~conv:(fun _ -> assert false)
  in
  let offset = read_acc ic_access index in
  close_in ic_access;
  offset
  
let read ~(ic : 'a inchan) ~index =
  let infos = ic.infos in
  let part = Format.partition_of_infos infos in
  let content_size = Format.content_size_of_partition part in
  match content_size with
  (* Content is of fixed size len, direct access based on the index *)
  | Format.Fixed len ->
     seek_in' ic (index * len);
     read ic.inchan len ic.conv

  (* Content is of variable size: we need to find the offset for the index. *)
  | Format.Variable access_partition ->
     let offset = offset_of_index_in_partition ic index access_partition in

     print_endline @@ "VARIABLE OFFSET FOUND TO BE :" ^ (string_of_int offset);
     
     seek_in' ic offset;

     print_endline @@ "POS IS : " ^ (string_of_int @@ pos_in ic.inchan);
     
     let len = input_binary_int' ic in

     print_endline @@ "VARIABLE READ LEN FOUND TO BE :" ^ (string_of_int len);
     
     read ic.inchan len ic.conv

(* WRONG *)
let write_fixed_size ~oc ~index ~value =
  seek_out' oc index;
  write oc.outchan value oc.conv

let write_var_size ~oc ~index ~value =
  seek_out' oc index;
  write oc.outchan value oc.conv ~variable:true

let write ~oc ~index ~value =
  let size = Format.partition_of_infos oc.infos |> Format.content_size_of_partition in
  match size with
  | Format.Fixed _ ->
     write_fixed_size ~oc ~index ~value
  | Format.Variable _ ->
     write_var_size ~oc ~index ~value
