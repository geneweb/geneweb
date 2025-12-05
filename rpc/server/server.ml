module Json_rpc = Geneweb_rpc.Json_rpc
module Service = Geneweb_rpc.Service
module Analyze = Geneweb_search.Analyze
module Index = Geneweb_search.Index.Default

let set_levels dflags =
  let flag_to_src (d : Cmd.dflag) =
    match d with TLS -> Tls.Core.src | RPC -> Logs.default
  in
  List.iter
    (fun flag -> Logs.Src.set_level (flag_to_src flag) (Some Debug))
    dflags

let index_from_gzip path =
  let content = My_gzip.gunzip_file path in
  let len = Bigstringaf.length content in
  let rec split acc pos =
    if pos >= len then acc
    else
      let eol =
        let rec find i =
          if i >= len then len
          else if Bigstringaf.get content i = '\n' then i
          else find (i + 1)
        in
        find pos
      in
      if eol = pos then split acc (eol + 1)
      else
        let line = Bigstringaf.substring content ~off:pos ~len:(eol - pos) in
        split (line :: acc) (eol + 1)
  in
  split [] 0
  |> List.fold_left
       (fun acc line ->
         let words = Analyze.preprocess line in
         List.fold_left
           (fun acc w -> (w.Analyze.content, line) :: acc)
           acc words)
       []
  |> List.to_seq |> Index.of_seq

let load_dictionaries path =
  Filesystem.walk_folder ~recursive:true
    (fun file acc ->
      match file with
      | File f when String.equal (Filename.extension f) ".gz" ->
          let basename = Filename.(basename f |> chop_extension) in
          Logs.app (fun k -> k "Found dictionary %s" basename);
          (basename, index_from_gzip f) :: acc
      | Dir _ | File _ -> acc
      | Exn { path; exn; bt = _ } ->
          Logs.err (fun k ->
              k "Uncaught exception while opening %s:@ %a" path Util.pp_exn exn);
          acc)
    path []

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  match Cmdliner.Cmd.eval_value' Cmd.cfg with
  | `Ok cfg ->
      set_levels cfg.dflags;
      if Option.is_none cfg.tls then
        Logs.warn (fun k ->
            k
              "The server is starting without TLS support. WebSocket \
               connections@ will be insecure and may be rejected by browsers. \
               To ensure secure@ connections, please provide valid TLS \
               certificate and private key@ files.");
      Logs.app (fun k -> k "Loading dictionaries in %s..." cfg.index_dir);
      let dicts = load_dictionaries cfg.index_dir in
      Logs.app (fun k -> k "Found %d dictionaries" (List.length dicts));
      Rpc.start ~interface:cfg.interface ~port:cfg.port
        ?max_connection:cfg.max_connection ?idle_timeout:cfg.idle_timeout
        ?task_timeout:cfg.task_timeout
      @@ Route.route
           [
             Route.path "/pingpong" Service.PingPong.srv;
             Route.path "/search"
               (Service.Search.make ~fuel:cfg.index_fuel dicts);
           ];
      let forever, _ = Lwt.wait () in
      Lwt_main.run forever;
      exit (if Logs.err_count () > 0 then 1 else 0)
  | `Exit code -> exit code
