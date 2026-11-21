open Geneweb
open Config
module Driver = Geneweb_db.Driver

let src = Logs.Src.create ~doc:"Stats log" "STATS"

module Log = (val Logs.src_log src : Logs.LOG)

(* accumulated since [since], the last snapshot: durations of the
   [count] timed requests, bytes received and sent by all requests *)
type acc = {
  count : int;
  total : float;
  max : float;
  since : float;
  bytes_in : int;
  bytes_out : int;
}

let empty tm =
  { count = 0; total = 0.; max = 0.; since = tm; bytes_in = 0; bytes_out = 0 }

(* period in seconds, from [stats_period] (minutes) of the .gwf file *)
let period conf =
  match List.assoc_opt "stats_period" conf.base_env with
  | Some s -> (
      match int_of_string_opt (String.trim s) with
      | Some n when n > 0 -> Some (float (n * 60))
      | _ -> None)
  | None -> None

let bfile conf = Filename.concat (Secure.base_dir ()) (conf.bname ^ ".gwb")

let cnt_file conf suffix =
  Filename.concat (Util.test_cnt_d conf) (conf.bname ^ suffix)

let stats_file conf = cnt_file conf "_stats.txt"

(* accumulator file: "count total max since bytes_in bytes_out" on one line;
   a line without the byte counts (previous format) reads them as 0 *)
let read_acc fname tm =
  match Secure.open_in fname with
  | exception Sys_error _ -> empty tm
  | ic -> (
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      try
        let line = input_line ic in
        try
          Scanf.sscanf line "%d %f %f %f %d %d"
            (fun count total max since bytes_in bytes_out ->
              { count; total; max; since; bytes_in; bytes_out })
        with Scanf.Scan_failure _ | Failure _ | End_of_file ->
          Scanf.sscanf line "%d %f %f %f" (fun count total max since ->
              { (empty since) with count; total; max })
      with End_of_file | Scanf.Scan_failure _ | Failure _ -> empty tm)

let write_acc fname r =
  match Secure.open_out fname with
  | exception Sys_error _ -> ()
  | oc ->
      Fun.protect ~finally:(fun () -> close_out_noerr oc) @@ fun () ->
      Printf.fprintf oc "%d %.6f %.6f %.0f %d %d\n" r.count r.total r.max
        r.since r.bytes_in r.bytes_out

let header =
  "date\ttimestamp\tnb_persons\tnb_families\tconnections\twizards\tfriends\twindow\trequests\tmean\tmax\tbytes_in\tbytes_out\n"

let iso8601 tm =
  let t = Unix.gmtime tm in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
    t.Unix.tm_sec

let countries_file conf = cnt_file conf "_countries.txt"
let countries_header = "date\ttimestamp\tcountries\n"

(* Appends the countries of the users connected at [tm]: "CC:n" separated by
   spaces, empty when nobody is connected. *)
let write_countries conf tm l =
  let fname = countries_file conf in
  let is_new = not (Sys.file_exists fname) in
  Secure.with_open_out_gen
    [ Open_wronly; Open_creat; Open_append; Open_text ]
    0o644 fname
  @@ fun oc ->
  if is_new then output_string oc countries_header;
  List.map (fun (cc, n) -> Printf.sprintf "%s:%d" cc n) l
  |> String.concat " "
  |> Printf.fprintf oc "%s\t%.0f\t%s\n" (iso8601 tm) tm

(* Appends the snapshot taken at [tm], with the durations [r] accumulated
   since the previous one. Called without the accumulator lock held, since
   it opens the base. *)
let snapshot conf ~window tm r =
  let geoip = Geoip.file () in
  let with_countries = Sys.file_exists geoip in
  let cn, addrs =
    let on_exn _exn _bt = (None, []) in
    Lock.control ~on_exn ~wait:true ~lock_file:(Robot.lock_file ()) @@ fun () ->
    ( Some (Robot.snapshot conf window),
      if with_countries then Robot.addresses conf window else [] )
  in
  let nb_persons, nb_families =
    Driver.with_database (bfile conf) @@ fun base ->
    (Driver.nb_of_real_persons base, Driver.nb_of_families base)
  in
  let total, wizards, friends =
    match cn with
    | Some cn -> (cn.cn_total, cn.cn_wizards, cn.cn_friends)
    | None -> (0, 0, 0)
  in
  if with_countries then
    Option.iter (write_countries conf tm) (Geoip.countries geoip addrs);
  let fname = stats_file conf in
  let is_new = not (Sys.file_exists fname) in
  Secure.with_open_out_gen
    [ Open_wronly; Open_creat; Open_append; Open_text ]
    0o644 fname
  @@ fun oc ->
  if is_new then output_string oc header;
  Printf.fprintf oc "%s\t%.0f\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%.3f\t%.3f\t%d\t%d\n"
    (iso8601 tm) tm nb_persons nb_families total wizards friends window r.count
    (if r.count = 0 then 0. else r.total /. float r.count)
    r.max r.bytes_in r.bytes_out

let record conf ~window ~bytes_in ~bytes_out duration =
  match period conf with
  | None -> ()
  | Some _ when conf.bname = "" || not (Sys.file_exists (bfile conf)) -> ()
  | Some period -> (
      let tm = Unix.time () in
      let fname = cnt_file conf "_qtime.txt" in
      let due =
        let on_exn _exn _bt = None in
        Lock.control ~on_exn ~wait:true ~lock_file:(cnt_file conf "_qtime.lck")
        @@ fun () ->
        let r = read_acc fname tm in
        let r =
          {
            r with
            bytes_in = r.bytes_in + bytes_in;
            bytes_out = r.bytes_out + bytes_out;
          }
        in
        let r =
          match duration with
          | Some d ->
              {
                r with
                count = r.count + 1;
                total = r.total +. d;
                max = Float.max r.max d;
              }
          | None -> r
        in
        if tm -. r.since >= period then (
          write_acc fname (empty tm);
          Some r)
        else (
          write_acc fname r;
          None)
      in
      match due with
      | None -> ()
      | Some r -> (
          try snapshot conf ~window tm r
          with e ->
            Log.warn (fun k ->
                k "snapshot of %s failed: %s" conf.bname (Printexc.to_string e))
          ))
