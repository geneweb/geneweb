
let rm fname =
  if Sys.file_exists fname then Sys.remove fname

let ls_r dirs =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
      Sys.readdir f
      |> Array.to_list
      |> List.rev_map (Filename.concat f)
      |> List.rev_append fs
      |> loop (f :: result)
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] dirs
  
let rm_rf f =
  if Sys.file_exists f then
    let (directories, files) = ls_r [f] |> List.partition Sys.is_directory in
    List.iter Unix.unlink files ;
    List.iter Unix.rmdir directories

let mv src dst =
  if Sys.file_exists src then Sys.rename src dst

let mkdir_p ?(perm = 0o755) x =
  let rec loop x =
    let y = Filename.dirname x in
    if y <> x && String.length y < String.length x then loop y;
    try Unix.mkdir x perm with Unix.Unix_error (_, _, _) -> ()
  in
  loop x

let rec remove_dir d =
  begin try
    let files = Sys.readdir d in
    for i = 0 to Array.length files - 1 do
      remove_dir (Filename.concat d files.(i));
      rm (Filename.concat d files.(i))
    done
  with Sys_error _ -> ()
  end;
  try Unix.rmdir d with Unix.Unix_error (_, _, _) -> ()

let search_file_opt directories fname =
  let rec loop = function
    | hd :: tl ->
      let f = Filename.concat hd fname in
      if Sys.file_exists f then Some f else loop tl
    | [] -> None
  in loop directories

let search_asset_opt fname =
  search_file_opt (Secure.assets ()) fname

let lock_file bname =
  let bname =
    if Filename.check_suffix bname ".gwb" then
      Filename.chop_suffix bname ".gwb"
    else bname
  in
  bname ^ ".lck"

let check_magic magic ic =
  let len = String.length magic in
  let pos = pos_in ic in
  if in_channel_length ic - pos < len then false
  else if magic = really_input_string ic len then true
  else begin seek_in ic pos ; false end

let check_magics magics ic =
  List.exists (fun magic -> check_magic magic ic) magics

(* POSIX lockf(3), and fcntl(2), releases its locks when the process
   that holds the locks closes ANY file descriptor that was open on that file.
*)
let read_or_create_channel ?magic ?(wait = false) fname read write =
#ifdef WINDOWS
  let _ = wait in
#endif
  assert (Secure.check fname) ;
  let fd = Unix.openfile fname [ Unix.O_RDWR ; Unix.O_CREAT ] 0o666 in
#ifndef WINDOWS
  begin try
    Unix.lockf fd (if wait then Unix.F_LOCK else Unix.F_TLOCK) 0
    with e -> Unix.close fd; raise e
  end;
#endif
  let ic = Unix.in_channel_of_descr fd in
  let read () =
    seek_in ic 0;
    try
      match magic with
      | Some m when check_magic m ic ->
         let r = Some (read ic) in
         let _ = seek_in ic (in_channel_length ic - (String.length m)) in
         assert (check_magic m ic);
         r
      | Some _ -> None
      | None -> Some (read ic)
    with _ -> None
  in
  match read () with
  | Some v ->
#ifndef WINDOWS
     Unix.lockf fd Unix.F_ULOCK 0;
#endif
     close_in ic;
     v
  | None ->
     Unix.ftruncate fd 0 ;
     let oc = Unix.out_channel_of_descr fd in
     seek_out oc 0;
     begin match magic with Some m -> seek_out oc (String.length m) | None -> () end;
     let v = write oc in
     flush oc;
     let _ = seek_out oc (out_channel_length oc) in
     begin match magic with Some m -> output_string oc m | None -> () end;
     begin match magic with Some m -> seek_out oc 0 ; output_string oc m | None -> () end ;
     flush oc;
#ifndef WINDOWS
     Unix.lockf fd Unix.F_ULOCK 0;
#endif
     close_out oc;
     v

let read_or_create_value ?magic ?wait fname create =
  let read ic = Marshal.from_channel ic in
  let write oc =
    let v = create () in
    Marshal.to_channel oc v [ Marshal.No_sharing ; Marshal.Closures ];
    v
  in
  try read_or_create_channel ?magic ?wait fname read write
  with _ -> create ()
