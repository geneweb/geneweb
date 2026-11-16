let src = Logs.Src.create ~doc:"Reload" "RELOAD"

module Log = (val Logs.src_log src : Logs.LOG)

let stamp_file () = Filename.concat (Secure.base_dir ()) "gwd.reload"
let mtime f = try (Unix.stat f).Unix.st_mtime with Unix.Unix_error _ -> 0.
let current = ref 0.
let hooks : (unit -> unit) list ref = ref []
let on_reload f = hooks := f :: !hooks
let init () = current := mtime (stamp_file ())
let generation () = Printf.sprintf "%.6f" !current

let check () =
  let g = mtime (stamp_file ()) in
  if Float.equal g !current then false
  else (
    current := g;
    Log.info (fun k ->
        k "reload: flushing lexicon and template caches (pid %d)"
          (Unix.getpid ()));
    List.iter
      (fun f ->
        try f ()
        with e ->
          Log.warn (fun k -> k "reload hook failed: %s" (Printexc.to_string e)))
      (List.rev !hooks);
    true)

let request () =
  let f = stamp_file () in
  let oc =
    Secure.open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o644 f
  in
  Printf.fprintf oc "%.6f %d\n" (Unix.gettimeofday ()) (Unix.getpid ());
  close_out oc;
  (* Force a fresh mtime even on filesystems with 1 s resolution. *)
  (try
     let now = Unix.gettimeofday () in
     Unix.utimes f now now
   with Unix.Unix_error _ -> ());
  ignore (check ())
