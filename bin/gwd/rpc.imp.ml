module Json_rpc = Geneweb_rpc.Json_rpc
module Service = Geneweb_rpc.Service
module Route = Geneweb_rpc.Route
module Analyze = Geneweb_search.Analyze
module Index = Geneweb_search.Index.Default
module Reporter = Gwd_lib.Reporter
module Compat = Geneweb_compat

let is_available = true
let src = Logs.Src.create ~doc:"Rpc" "RPC "

module Log = (val Logs.src_log src : Logs.LOG)

let flush_buffer buf =
  let s = Buffer.contents buf in
  Buffer.reset buf;
  s

let make_backend fd =
  let buf = Buffer.create 512 in
  let oc = Lwt_io.of_unix_fd ~mode:Lwt_io.Output fd in
  let ppf = Fmt.with_buffer buf in
  let style = Reporter.infer_renderer fd in
  Fmt.set_style_renderer ppf style;
  let close () =
    Lwt.async @@ fun () ->
    Lwt.bind (Lwt_io.write oc (flush_buffer buf)) @@ fun () -> Lwt_io.close oc
  in
  let flush over =
    Lwt.async @@ fun () ->
    Lwt.finalize
      (fun () -> Lwt_io.write oc (flush_buffer buf))
      (fun () ->
        over ();
        Lwt.return_unit)
  in
  Reporter.{ ppf; close; flush }

let pp_exception ppf (exn, bt) =
  let pp_header ppf pid = Fmt.pf ppf "Exception uncaught in process %d:" pid in
  let pp_header = Fmt.(styled (`Fg `Red) pp_header) in
  let exn = Printexc.to_string exn in
  let pid = Unix.getpid () in
  if Printexc.backtrace_status () then
    let bt = Printexc.raw_backtrace_to_string bt in
    Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header pid exn Fmt.lines bt
  else Fmt.pf ppf "@[%a@ %s@]" pp_header pid exn

module Search : sig
  val make :
    fuel:int -> (string * Geneweb_search.Index.Default.t) list -> Service.t
  (** [make ~fuel l] prepares a search service with a list [l] of inverted
      indexes, each with its name.

      The service exposes two methods:
      - [lookup name s size] searches the string [s] in the index [name] and
        stops after at most [size] results.
      - [info] returns the list of loaded indexes with their cardinals.

      The [fuel] argument sets an upper bound on the number of results returned
      by the lookup operation. If the requested size exceeds this limit, the
      request is not fully honored and at most [fuel] results are returned. *)
end = struct
  module Index = Geneweb_search.Index.Default
  module Analyze = Geneweb_search.Analyze

  let info indexes =
    Service.decl "info"
      Service.Desc.Syntax.(ret (list (tup2 string int)))
      (Lwt_result.return
      @@ List.map (fun (name, idx) -> (name, Index.cardinal idx)) indexes)

  let lookup ~fuel indexes =
    Service.decl "lookup"
      Service.Desc.Syntax.(string @-> string @-> int @-> ret (list string))
      (fun name s size ->
        match List.assoc name indexes with
        | exception Not_found ->
            (* TODO: Methods could fail and emit errors. We can implement
               this by changing the return type of methods to Lwt_result.t *)
            Lwt_result.return []
        | idx ->
            let words =
              List.map
                (fun Analyze.{ content; _ } -> content)
                (Analyze.preprocess s)
            in
            (* We look up for entries in the following order of priority:
                1. Entries that exactly match all the [words].
                2. Entries that match prefixes of all the [words].
                3. Entries that match prefixes of all the [words] up to
                   Levenshtein distance of 1. *)
            let size = min size fuel in
            let r =
              (* TODO: There is no guarantee that bounding the size of forced
                 elements in the sequence will limit the running time. We
                 should find a solution to limit the running time itself. *)
              List.of_seq @@ Compat.Seq.take size @@ Compat.Seq.concat
              (* BUG: We can introduce duplicate in the output here. *)
              @@ List.to_seq
                   [
                     Index.search words idx;
                     Index.search_prefix words idx;
                     Index.fuzzy_search ~max_dist:1 words idx;
                   ]
            in
            Lwt_result.return r)

  let make ~fuel idx =
    Service.(empty |> add (info idx) |> add (lookup ~fuel idx))
end

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
          Log.info (fun k -> k "Found dictionary %s" basename);
          (basename, index_from_gzip f) :: acc
      | Dir _ | File _ -> acc
      | Exn { path; exn; bt } ->
          Log.err (fun k ->
              k "Uncaught exception while opening %s:@ %a" path pp_exception
                (exn, bt));
          acc)
    path []

let start ?interface ~port ~max_requests ~timeout ~task_timeout ?tls ?certfile
    ?keyfile ~index_fuel ~index_dir =
  if Option.is_none tls then
    Log.warn (fun k ->
        k
          "The server is starting without TLS support. WebSocket connections \
           will be insecure and may be rejected by browsers. To ensure secure \
           connections, please provide valid TLS certificate and private key \
           files.");
  Log.info (fun k -> k "Loading dictionaries in %s..." index_dir);
  let dicts = load_dictionaries index_dir in
  Log.info (fun k -> k "Found %d dictionaries" (List.length dicts));
  Geneweb_rpc.Server.start ?interface ~port ~max_requests ~timeout ~task_timeout
    ?tls ?certfile ?keyfile
  @@ Route.route
       [
         Route.path "/pingpong" Service.PingPong.srv;
         Route.path "/search" (Search.make ~fuel:index_fuel dicts);
       ];
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever;
  exit (if Logs.err_count () > 0 then 1 else 0)
