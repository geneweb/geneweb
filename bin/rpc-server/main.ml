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

let fold_lines f acc ic =
  let rec loop acc =
    match My_gzip.input_line ic with
    | exception End_of_file -> acc
    | s -> loop (f acc s)
  in
  loop acc

let index_from_gzip path =
  My_gzip.with_open path @@ fun ic ->
  fold_lines
    (fun acc line ->
      let words = Analyze.preprocess line in
      List.fold_left (fun acc w -> (w.Analyze.content, line) :: acc) acc words)
    [] ic
  |> List.to_seq |> Index.of_seq

let load_dictionaries path =
  File.walk_folder ~recursive:true
    (fun file acc ->
      match file with
      | `File f when String.equal (Filename.extension f) ".gz" ->
          let basename = Filename.(basename f |> chop_extension) in
          Logs.info (fun k -> k "found %s" basename);
          (basename, index_from_gzip f) :: acc
      | `Dir _ | `File _ -> acc)
    path []

let () =
  Logs.set_reporter @@ Util.lwt_reporter ();
  let cfg = Cmd.parse () in
  set_levels cfg.dflags;
  if Option.is_none cfg.tls then
    Logs.warn (fun k ->
        k
          "The server is starting without TLS support. WebSocket connections@ \
           will be insecure and may be rejected by browsers. To ensure secure@ \
           connections, please provide valid TLS certificate and private key@ \
           files.");
  Logs.info (fun k -> k "Loading dictionaries in %s..." cfg.base_dir);
  let dicts = load_dictionaries cfg.base_dir in
  Logs.info (fun k -> k "%d indexes generated." (List.length dicts));
  Server.start ~interface:cfg.interface ~port:cfg.port
    ?max_connection:cfg.max_connection ?idle_timeout:cfg.idle_timeout
    ?task_timeout:cfg.task_timeout
  @@ Route.route
       [
         Route.path "/pingpong" Service.PingPong.srv;
         Route.path "/search" (Service.Search.make dicts);
       ];
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever;
  exit (if Logs.err_count () > 0 then 1 else 0)
