open Config
open Def

type 'a value = Vint of int | Vother of 'a | Vnone

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

module BufferPool = struct
  let small_pool = Queue.create ()
  let small_pool_max = 20
  let small_size = 2048
  let page_pool = Queue.create ()
  let page_pool_max = 5
  let page_size = 65536

  let acquire_small () =
    try
      let buf = Queue.pop small_pool in
      Buffer.clear buf;
      buf
    with Queue.Empty -> Buffer.create small_size

  let release_small buf =
    if Queue.length small_pool < small_pool_max && Buffer.length buf < 8192 then
      Queue.push buf small_pool

  let acquire_page () =
    try
      let buf = Queue.pop page_pool in
      Buffer.clear buf;
      buf
    with Queue.Empty -> Buffer.create page_size

  let release_page buf =
    if Queue.length page_pool < page_pool_max then (
      if Buffer.length buf > 524288 then Buffer.reset buf else Buffer.clear buf;
      Queue.push buf page_pool)

  let with_buffer f =
    let buf = acquire_small () in
    try
      let result = f buf in
      release_small buf;
      result
    with e ->
      release_small buf;
      raise e
end

module TemplateCache = struct
  type entry = {
    content : string;
    size : int;
    mutable last_access : float;
    mutable access_count : int;
  }

  let cache = Hashtbl.create 64
  let max_cache_size = 10_485_760
  let current_size = ref 0
  let enabled = ref true
  let hits = ref 0
  let misses = ref 0

  let evict_lru () =
    let entries =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) cache []
      |> List.sort (fun (_, a) (_, b) -> compare a.last_access b.last_access)
    in
    match entries with
    | (oldest_key, oldest_entry) :: _ ->
        Hashtbl.remove cache oldest_key;
        current_size := !current_size - oldest_entry.size
    | [] -> ()

  let get_cached name =
    try
      let entry = Hashtbl.find cache name in
      incr hits;
      entry.last_access <- Unix.time ();
      entry.access_count <- entry.access_count + 1;
      Some entry.content
    with Not_found ->
      incr misses;
      None

  let add_to_cache name content =
    let size = String.length content in
    if size > max_cache_size / 4 then None
    else (
      while !current_size + size > max_cache_size && Hashtbl.length cache > 0 do
        evict_lru ()
      done;
      let entry =
        { content; size; last_access = Unix.time (); access_count = 0 }
      in
      Hashtbl.replace cache name entry;
      current_size := !current_size + size;
      Some content)

  let get_or_load conf name =
    if not !enabled then None
    else
      match get_cached name with
      | Some content -> Some content
      | None -> (
          try
            BufferPool.with_buffer (fun buf ->
                let output_conf_buf =
                  { conf.output_conf with body = Buffer.add_string buf }
                in
                let conf_buf = { conf with output_conf = output_conf_buf } in
                Templ.output_simple conf_buf Templ.Env.empty name;
                let content = Buffer.contents buf in
                add_to_cache name content)
          with _ -> None)
end

module StaticParts = struct
  let doctype_html = lazy "<!DOCTYPE html>\n"
  let head_open = lazy "<head>\n  <title>"
  let title_close = lazy "</title>\n"
  let head_close = lazy "</head>\n"
  let body_close = lazy "</body>\n</html>\n"
  let container_close = lazy "</div><!-- container end -->\n"
  let h1_close = lazy "</h1>\n"
  let script_open = lazy "<script>\n"
  let script_close = lazy "\n</script>\n"

  let meta_viewport =
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, \
     shrink-to-fit=no\">\n"

  let meta_robots_index = "  <meta name=\"robots\" content=\"index,follow\">\n"
  let meta_robots_none = "  <meta name=\"robots\" content=\"none\">\n"
  let meta_robots robot = if robot then meta_robots_index else meta_robots_none
  let container_normal = "<div class=\"container\">\n"
  let container_fluid = "<div class=\"container-fluid\">\n"
  let container fluid = if fluid then container_fluid else container_normal
  let h1_open_normal = "<h1>"
  let h1_open_error = "<h1 class=\"error\">"
  let h1_open error = if error then h1_open_error else h1_open_normal
  let html_cache = Hashtbl.create 8
  let charset_cache = Hashtbl.create 4
  let favicon_cache = Hashtbl.create 8
  let body_cache = Hashtbl.create 16

  let html_open lang =
    try Hashtbl.find html_cache lang
    with Not_found ->
      let result = Printf.sprintf "  <html lang=\"%s\">\n" lang in
      Hashtbl.add html_cache lang result;
      result

  let meta_charset charset =
    try Hashtbl.find charset_cache charset
    with Not_found ->
      let result = Printf.sprintf "  <meta charset=\"%s\">\n" charset in
      Hashtbl.add charset_cache charset result;
      result

  let favicon prefix =
    try Hashtbl.find favicon_cache prefix
    with Not_found ->
      let result =
        Printf.sprintf
          "  <link rel=\"shortcut icon\" href=\"%s/favicon_gwd.png\">\n\
           <link rel=\"apple-touch-icon\" href=\"%s/favicon_gwd.png\">\n"
          prefix prefix
      in
      Hashtbl.add favicon_cache prefix result;
      result

  let body_open dir_attr body_prop =
    let key = dir_attr ^ "|" ^ body_prop in
    try Hashtbl.find body_cache key
    with Not_found ->
      let result = Printf.sprintf "<body%s%s>\n" dir_attr body_prop in
      Hashtbl.add body_cache key result;
      result
end

module HtmlBuffer = struct
  let create_buffered_conf ?(initial_size = 65536) conf =
    let buffer = Buffer.create initial_size in
    let original_output = conf.output_conf in
    let buffered_output =
      {
        original_output with
        body = Buffer.add_string buffer;
        flush =
          (fun () ->
            let content = Buffer.contents buffer in
            if String.length content > 0 then (
              original_output.body content;
              original_output.flush ();
              Buffer.clear buffer));
      }
    in
    { conf with output_conf = buffered_output }

  let wrap conf f =
    let buffer = BufferPool.acquire_page () in
    let original_output = conf.output_conf in
    let buffered_output =
      {
        original_output with
        body = Buffer.add_string buffer;
        flush =
          (fun () ->
            let content = Buffer.contents buffer in
            if String.length content > 0 then (
              original_output.body content;
              original_output.flush ();
              Buffer.clear buffer));
      }
    in
    let buffered_conf = { conf with output_conf = buffered_output } in
    try
      let result = f buffered_conf in
      buffered_conf.output_conf.flush ();
      BufferPool.release_page buffer;
      result
    with e ->
      buffered_conf.output_conf.flush ();
      BufferPool.release_page buffer;
      raise e

  let wrap_measured conf f =
    let start_time = Unix.gettimeofday () in
    let bytes_before = Gc.allocated_bytes () in
    let buffer_size = ref 0 in
    let write_count = ref 0 in

    let buffer = BufferPool.acquire_page () in
    let original_output = conf.output_conf in
    let measuring_output =
      {
        original_output with
        body =
          (fun s ->
            incr write_count;
            buffer_size := !buffer_size + String.length s;
            Buffer.add_string buffer s);
        flush =
          (fun () ->
            let content = Buffer.contents buffer in
            if String.length content > 0 then (
              original_output.body content;
              original_output.flush ();
              Buffer.clear buffer));
      }
    in
    let measuring_conf = { conf with output_conf = measuring_output } in

    try
      let result = f measuring_conf in
      measuring_conf.output_conf.flush ();
      BufferPool.release_page buffer;

      (if List.mem_assoc "debug_buffer" conf.env then
         let bytes_after = Gc.allocated_bytes () in
         let time_elapsed = Unix.gettimeofday () -. start_time in
         let bytes_allocated = bytes_after -. bytes_before in
         Printf.eprintf
           "Buffer stats: %d bytes, %d writes, %.3fs, %.0f bytes allocated\n"
           !buffer_size !write_count time_elapsed bytes_allocated);
      result
    with e ->
      measuring_conf.output_conf.flush ();
      BufferPool.release_page buffer;
      raise e
end

let incorrect_request ?(comment = "") conf =
  GWPARAM.output_error conf Def.Bad_Request ~content:(Adef.safe comment)

let error_cannot_access conf fname =
  GWPARAM.output_error conf Def.Not_Found
    ~content:
      ("Cannot access file \""
      ^<^ (Util.escape_html fname : Adef.escaped_string :> Adef.safe_string)
      ^>^ ".txt\".")

let include_home_template conf =
  let ifun =
    Templ.
      {
        eval_var = (fun _ -> raise Not_found);
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = (fun _ -> raise Not_found);
      }
  in
  try Templ.output conf ifun Templ.Env.empty () "home"
  with _ -> error_cannot_access conf "home"

let link_to_referer conf =
  let referer = Util.get_referer conf in
  let back = Utf8.capitalize_fst (Util.transl conf "back") in
  if (referer :> string) <> "" then
    ({|<a href="|} ^<^ referer
     ^>^ {|" class="btn btn-sm btn-link p-0 border-0" title="|} ^ back
     ^ {|"><i class="fa fa-arrow-left-long fa-fw fa-sm"></i></a>|}
      :> Adef.safe_string)
  else Adef.safe ""

let is_fluid conf =
  (try List.assoc "wide" conf.env = Adef.encoded "on" with Not_found -> false)
  || try List.assoc "wide" conf.base_env = "on" with Not_found -> false

let rec header_without_http_nor_home conf title =
  let robot = List.assoc_opt "robot_index" conf.base_env = Some "yes" in
  let prefix = (Util.images_prefix conf :> string) in

  Output.print_sstring conf (Lazy.force StaticParts.doctype_html);
  Output.print_sstring conf (StaticParts.html_open conf.lang);
  Output.print_sstring conf (Lazy.force StaticParts.head_open);
  title true;
  Output.print_sstring conf (Lazy.force StaticParts.title_close);

  BufferPool.with_buffer (fun buf ->
      Buffer.add_string buf (StaticParts.meta_robots robot);
      Buffer.add_string buf (StaticParts.meta_charset conf.charset);
      Buffer.add_string buf StaticParts.meta_viewport;
      Buffer.add_string buf (StaticParts.favicon prefix);
      Output.print_sstring conf (Buffer.contents buf));

  Templ.output_simple conf Templ.Env.empty "css";
  Output.print_sstring conf (Lazy.force StaticParts.head_close);

  let dir_attr =
    try " dir=\"" ^ Hashtbl.find conf.lexicon "!dir" ^ "\""
    with Not_found -> ""
  in
  Output.print_sstring conf
    (StaticParts.body_open dir_attr (Util.body_prop conf));
  Templ.output_simple conf Templ.Env.empty "hed";
  Util.message_to_wizard conf

and header_without_title conf =
  let fluid = is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf (fun _ -> ());
  include_home_template conf;
  Output.print_sstring conf (StaticParts.container fluid)

and header_with_title ?(error = false) ?(fluid = false) conf title =
  let fluid = fluid || is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf title;
  include_home_template conf;
  Output.print_sstring conf (StaticParts.container fluid);
  Output.print_sstring conf (StaticParts.h1_open error);
  title false;
  Output.print_sstring conf (Lazy.force StaticParts.h1_close)

and header_fluid conf title = header_with_title ~fluid:true conf title

and header_without_home conf title =
  let fluid = is_fluid conf in
  Util.html conf;
  header_without_http_nor_home conf title;
  Output.print_sstring conf (StaticParts.container fluid);
  Output.print_sstring conf (StaticParts.h1_open false);
  title false;
  Output.print_sstring conf (Lazy.force StaticParts.h1_close)

and header_with_conf_title conf _title =
  let title _ =
    match Util.p_getenv conf.env "p_title" with
    | None | Some "" -> ()
    | Some s -> Output.printf conf "<h1>%s</h1>\n" s
  in
  header_with_title conf title

and header ?(error = false) ?(fluid = false) conf title =
  header_with_title ~error ~fluid conf title

and rheader conf title = header_with_title ~error:true conf title

and trailer conf =
  let conf = { conf with is_printed_by_template = false } in
  Templ.output_simple conf Templ.Env.empty "trl";
  Templ.output_simple conf Templ.Env.empty "copyr";
  Output.print_sstring conf (Lazy.force StaticParts.container_close);
  Templ.output_simple conf Templ.Env.empty "js";
  let query_time = Unix.gettimeofday () -. conf.query_start in
  Util.time_debug conf query_time !GWPARAM.nb_errors !GWPARAM.errors_undef
    !GWPARAM.errors_other !GWPARAM.set_vars;
  Output.print_sstring conf (Lazy.force StaticParts.body_close)

and trailer_with_extra_js conf extra_js_sources =
  let conf = { conf with is_printed_by_template = false } in
  Templ.output_simple conf Templ.Env.empty "trl";
  Templ.output_simple conf Templ.Env.empty "copyr";
  Output.print_sstring conf (Lazy.force StaticParts.container_close);
  if extra_js_sources <> "" then (
    let sources = String.split_on_char '|' extra_js_sources in
    List.iter
      (fun source ->
        let source = String.trim source in
        if source <> "" then
          if Filename.check_suffix source ".js" then
            let js_path =
              Filename.concat conf.etc_prefix (Filename.concat "js" source)
            in
            match Util.open_etc_file conf js_path with
            | Some (ic, fname) -> (
                close_in ic;
                match Util.hash_file_cached fname with
                | Some hash ->
                    Output.printf conf {|<script src="js/%s?hash=%s"></script>|}
                      (* TODO manage etc prefix *)
                      source hash
                | None ->
                    Output.printf conf {|<script src="%sjs/%s"></script>|}
                      conf.etc_prefix source)
            | None -> ()
          else
            try
              Output.print_sstring conf (Lazy.force StaticParts.script_open);
              Templ.output_simple conf Templ.Env.empty source;
              Output.print_sstring conf (Lazy.force StaticParts.script_close)
            with _ -> ())
      sources;
    Templ.output_simple conf Templ.Env.empty "js";
    let query_time = Unix.gettimeofday () -. conf.query_start in
    Util.time_debug conf query_time !GWPARAM.nb_errors !GWPARAM.errors_undef
      !GWPARAM.errors_other !GWPARAM.set_vars;
    Output.print_sstring conf (Lazy.force StaticParts.body_close))
