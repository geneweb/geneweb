let add_extension path ext = Format.sprintf "%s%s" path ext

let pp_rule path =
  let target_gzip = add_extension path ".gz" in
  let target_brotli = add_extension path ".br" in
  Format.printf
    {|
(rule
  (target %s)
  (deps (:input %s))
  (action
    (run gzip -9 -k -f %%{input})))
(rule
  (target %s)
  (deps (:input %s))
  (action
    (run brotli -f -q 11 %%{input})))@?|}
    target_gzip path target_brotli path

let () =
  let handle = Unix.opendir Sys.argv.(1) in
  Fun.protect ~finally:(fun () -> Unix.closedir handle) @@ fun () ->
  let rec loop () =
    match Unix.readdir handle with
    | exception End_of_file -> ()
    | s -> (
        match Filename.extension s with
        | ".css" | ".js" ->
            pp_rule s;
            loop ()
        | _ -> loop ())
  in
  loop ()
