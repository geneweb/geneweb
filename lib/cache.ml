module Driver = Geneweb_db.Driver

type entry = Driver.istr * string

let file bname name =
  let bname = Filename.remove_extension bname in
  Filename.concat (GWPARAM.cache_d bname) (bname ^ "_" ^ name ^ ".bin")

let exists bname name = Sys.file_exists (file bname name)

let read bname name =
  match Secure.open_in_bin (file bname name) with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          try Some (Marshal.from_channel ic : entry list)
          with End_of_file | Failure _ -> None)

let write bname name entries =
  let f = file bname name in
  let tmp = f ^ ".tmp" in
  Filesystem.create_dir ~parent:true (Filename.dirname f);
  Secure.with_open_out_bin tmp (fun oc ->
      Marshal.to_channel oc entries [ Marshal.No_sharing ]);
  (try Sys.remove f with Sys_error _ -> ());
  Sys.rename tmp f

let update bname name ~old_istr ~new_istr value =
  match read bname name with
  | Some entries
    when List.exists (fun (i, _) -> Driver.Istr.equal i old_istr) entries -> (
      let updated =
        List.filter_map
          (fun (i, v) ->
            if Driver.Istr.equal i old_istr then Some (new_istr, value)
            else if Driver.Istr.equal i new_istr then None
            else Some (i, v))
          entries
      in
      try
        write bname name updated;
        true
      with Sys_error _ -> false)
  | _ -> false
