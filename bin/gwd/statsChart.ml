open Geneweb
open Config

(* One line of [<base>_stats.txt], see {!StatsLog}. *)
type snap = {
  ts : float;
  conn : int;
  wiz : int;
  fri : int;
  requests : int;
  mean : float;
  max : float;
  bytes_in : int;
  bytes_out : int;
}

(* Lines written before the byte counts existed have 11 fields. The header
   line fails on its timestamp and is skipped. *)
let parse_line l =
  match String.split_on_char '\t' l with
  | _date :: ts :: _np :: _nf :: conn :: wiz :: fri :: _window :: requests
    :: mean :: max :: rest -> (
      try
        let bytes_in, bytes_out =
          match rest with
          | bi :: bo :: _ -> (int_of_string bi, int_of_string bo)
          | _ -> (0, 0)
        in
        Some
          {
            ts = float_of_string ts;
            conn = int_of_string conn;
            wiz = int_of_string wiz;
            fri = int_of_string fri;
            requests = int_of_string requests;
            mean = float_of_string mean;
            max = float_of_string max;
            bytes_in;
            bytes_out;
          }
      with Failure _ -> None)
  | _ -> None

(* snapshots in chronological order (the file is appended to) *)
let read_snaps fname =
  match Secure.open_in fname with
  | exception Sys_error _ -> []
  | ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      let rec loop acc =
        match input_line ic with
        | l -> (
            match parse_line (String.trim l) with
            | Some s -> loop (s :: acc)
            | None -> loop acc)
        | exception End_of_file -> List.rev acc
      in
      loop []

(* One line of [<base>_countries.txt]: timestamp and users by country. The
   header line fails on its timestamp and is skipped. *)
let parse_countries l =
  match String.split_on_char '\t' l with
  | _date :: ts :: rest -> (
      match float_of_string_opt ts with
      | None -> None
      | Some ts ->
          let field = match rest with f :: _ -> f | [] -> "" in
          let counts =
            String.split_on_char ' ' field
            |> List.filter_map (fun x ->
                match String.index_opt x ':' with
                | Some i -> (
                    match
                      int_of_string_opt
                        (String.sub x (i + 1) (String.length x - i - 1))
                    with
                    | Some n -> Some (String.sub x 0 i, n)
                    | None -> None)
                | None -> None)
          in
          Some (ts, counts))
  | _ -> None

(* Users by country over the snapshots taken in [start, stop): for each
   country, the sum of its users over the snapshots and its maximum, most
   present first; with the number of snapshots. *)
let read_countries fname ~start ~stop =
  match Secure.open_in fname with
  | exception Sys_error _ -> (0, [])
  | ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      let tbl = Hashtbl.create 64 in
      let rec loop n =
        match input_line ic with
        | l -> (
            match parse_countries (String.trim l) with
            | Some (ts, counts) when ts >= start && ts < stop ->
                List.iter
                  (fun (cc, k) ->
                    let sum, mx =
                      Option.value ~default:(0, 0) (Hashtbl.find_opt tbl cc)
                    in
                    Hashtbl.replace tbl cc (sum + k, Stdlib.max mx k))
                  counts;
                loop (n + 1)
            | Some _ | None -> loop n)
        | exception End_of_file -> n
      in
      let n = loop 0 in
      let l =
        Hashtbl.fold (fun cc (sum, mx) l -> (cc, sum, mx) :: l) tbl []
        |> List.sort (fun (c1, s1, _) (c2, s2, _) ->
            match Int.compare s2 s1 with 0 -> String.compare c1 c2 | c -> c)
      in
      (n, l)

type period = Day | Week | Month

let period_of_string = function
  | Some "week" -> Week
  | Some "month" -> Month
  | _ -> Day

let string_of_period = function
  | Day -> "day"
  | Week -> "week"
  | Month -> "month"

(* Local time (time zone of the server): [mktime] normalises the fields, e.g.
   day 32 of a month or hour 24. *)
let mk ?(hour = 0) y m d =
  Unix.mktime
    {
      Unix.tm_sec = 0;
      tm_min = 0;
      tm_hour = hour;
      tm_mday = d;
      tm_mon = m - 1;
      tm_year = y - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }

let ymd tm = (tm.Unix.tm_year + 1900, tm.Unix.tm_mon + 1, tm.Unix.tm_mday)
let string_of_ymd (y, m, d) = Printf.sprintf "%04d-%02d-%02d" y m d

let ymd_of_string s =
  try Scanf.sscanf s "%4d-%2d-%2d%!" (fun y m d -> Some (y, m, d))
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None

(* first day of the period containing the day [y, m, d] *)
let period_start period (y, m, d) =
  match period with
  | Day -> mk y m d
  | Week ->
      let _, tm = mk y m d in
      (* weeks start on Monday *)
      mk y m (d - ((tm.Unix.tm_wday + 6) mod 7))
  | Month -> mk y m 1

(* first day of the period [n] periods after the one starting on [tm] *)
let shift period tm n =
  let y, m, d = ymd tm in
  match period with
  | Day -> mk y m (d + n)
  | Week -> mk y m (d + (7 * n))
  | Month -> mk y (m + n) 1

(* Bucket of the aggregated views: the hour (week) or the day (month)
   containing [ts]. The day view has one point per snapshot. *)
let bucket_start period ts =
  let tm = Unix.localtime ts in
  let y, m, d = ymd tm in
  match period with
  | Day -> ts
  | Week -> fst (mk ~hour:tm.Unix.tm_hour y m d)
  | Month -> fst (mk y m d)

let next_bucket period t =
  let tm = Unix.localtime t in
  let y, m, d = ymd tm in
  match period with
  | Day -> t
  | Week -> fst (mk ~hour:(tm.Unix.tm_hour + 1) y m d)
  | Month -> fst (mk y m (d + 1))

let label period t =
  let tm = Unix.localtime t in
  match period with
  | Day -> Printf.sprintf "%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min
  | Week ->
      Printf.sprintf "%02d-%02d %02dh" (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        tm.Unix.tm_hour
  | Month -> Printf.sprintf "%02d-%02d" (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

(* Snapshots aggregated in a bucket. Each snapshot counts the requests and
   bytes of the interval since the previous one, [dt] seconds: rates are
   computed over the intervals of known length only. *)
type acc = {
  n : int;
  conn_sum : int;
  conn_max : int;
  wiz_sum : int;
  fri_sum : int;
  req : int;
  req_dt : int;  (** requests of the intervals of known length *)
  time_sum : float;  (** sum of the durations: mean * requests *)
  tmax : float;
  bin : int;
  bout : int;
  dt : float;
}

let acc0 =
  {
    n = 0;
    conn_sum = 0;
    conn_max = 0;
    wiz_sum = 0;
    fri_sum = 0;
    req = 0;
    req_dt = 0;
    time_sum = 0.;
    tmax = 0.;
    bin = 0;
    bout = 0;
    dt = 0.;
  }

let add a s dt =
  let a =
    {
      a with
      n = a.n + 1;
      conn_sum = a.conn_sum + s.conn;
      conn_max = Stdlib.max a.conn_max s.conn;
      wiz_sum = a.wiz_sum + s.wiz;
      fri_sum = a.fri_sum + s.fri;
      req = a.req + s.requests;
      time_sum = a.time_sum +. (s.mean *. float s.requests);
      tmax = Float.max a.tmax s.max;
    }
  in
  match dt with
  | Some dt ->
      {
        a with
        req_dt = a.req_dt + s.requests;
        bin = a.bin + s.bytes_in;
        bout = a.bout + s.bytes_out;
        dt = a.dt +. dt;
      }
  | None -> a

let median l =
  match List.sort compare l with
  | [] -> None
  | sl -> Some (List.nth sl (List.length sl / 2))

(* Points of the chart, [None] for a gap. *)
let points period ~start ~stop snaps =
  (* pair each snapshot with the length of its interval, keep the period *)
  let rec with_dt prev acc = function
    | [] -> List.rev acc
    | s :: l ->
        let dt = Option.map (fun p -> s.ts -. p.ts) prev in
        let acc =
          if s.ts >= start && s.ts < stop then (s, dt) :: acc else acc
        in
        with_dt (Some s) acc l
  in
  let snaps = with_dt None [] snaps in
  match period with
  | Day ->
      (* a gap of more than 2.5 typical intervals breaks the lines *)
      let gap =
        match median (List.filter_map snd snaps) with
        | Some m -> 2.5 *. m
        | None -> infinity
      in
      List.concat_map
        (fun (s, dt) ->
          let p = (s.ts, Some (add acc0 s dt)) in
          match dt with
          | Some dt when dt > gap -> [ (s.ts -. (dt /. 2.), None); p ]
          | _ -> [ p ])
        snaps
  | Week | Month ->
      (* every bucket of the period, empty ones as gaps *)
      let tbl = Hashtbl.create 256 in
      List.iter
        (fun (s, dt) ->
          let b = bucket_start period s.ts in
          let a = Option.value ~default:acc0 (Hashtbl.find_opt tbl b) in
          Hashtbl.replace tbl b (add a s dt))
        snaps;
      let rec grid t acc =
        if t >= stop then List.rev acc
        else grid (next_bucket period t) ((t, Hashtbl.find_opt tbl t) :: acc)
      in
      grid start []

let round3 x = `Float (Float.round (x *. 1000.) /. 1000.)

(* all-time maximum of the connections, with the first snapshot reaching it *)
let record snaps =
  List.fold_left
    (fun r s ->
      match r with Some r when r.conn >= s.conn -> Some r | _ -> Some s)
    None snaps

let json period ~day ~prev ~next ~first ~record ~countries pts =
  let col f =
    `List
      (List.map
         (fun (_, a) -> Option.value ~default:`Null (Option.bind a f))
         pts)
  in
  let per_snap f a =
    if a.n = 0 then None else Some (round3 (f a /. float a.n))
  in
  let rate f a = if a.dt <= 0. then None else Some (round3 (f a /. a.dt)) in
  let total =
    List.fold_left
      (fun t (_, a) ->
        match a with
        | None -> t
        | Some a ->
            {
              t with
              n = t.n + a.n;
              conn_max = Stdlib.max t.conn_max a.conn_max;
              req = t.req + a.req;
              time_sum = t.time_sum +. a.time_sum;
              tmax = Float.max t.tmax a.tmax;
              bin = t.bin + a.bin;
              bout = t.bout + a.bout;
            })
      acc0 pts
  in
  let opt_string = function Some s -> `String s | None -> `Null in
  `Assoc
    [
      ("period", `String (string_of_period period));
      ("day", `String day);
      ("prev", opt_string prev);
      ("next", opt_string next);
      ("first", opt_string first);
      ( "record",
        match record with
        | None -> `Null
        | Some s ->
            `Assoc
              [
                ("conn", `Int s.conn);
                ("day", `String (string_of_ymd (ymd (Unix.localtime s.ts))));
                ("time", `String (label Day s.ts));
              ] );
      ( "countries",
        let n, l = countries in
        `List
          (List.map
             (fun (cc, sum, mx) ->
               `Assoc
                 [
                   ("cc", `String cc);
                   ("mean", round3 (float sum /. float n));
                   ("max", `Int mx);
                 ])
             l) );
      ("labels", `List (List.map (fun (t, _) -> `String (label period t)) pts));
      ("conn", col (per_snap (fun a -> float a.conn_sum)));
      ( "conn_max",
        col (fun a -> if a.n = 0 then None else Some (`Int a.conn_max)) );
      ("wiz", col (per_snap (fun a -> float a.wiz_sum)));
      ("fri", col (per_snap (fun a -> float a.fri_sum)));
      ("in_kbs", col (rate (fun a -> float a.bin /. 1024.)));
      ("out_kbs", col (rate (fun a -> float a.bout /. 1024.)));
      ("req_min", col (rate (fun a -> float a.req_dt *. 60.)));
      ( "mean",
        col (fun a ->
            if a.req = 0 then None
            else Some (round3 (a.time_sum /. float a.req))) );
      ("max", col (fun a -> if a.req = 0 then None else Some (round3 a.tmax)));
      ( "totals",
        `Assoc
          [
            ("snapshots", `Int total.n);
            ("conn_max", `Int total.conn_max);
            ("requests", `Int total.req);
            ( "mean",
              if total.req = 0 then `Null
              else round3 (total.time_sum /. float total.req) );
            ("max", round3 total.tmax);
            ("bytes_in", `Int total.bin);
            ("bytes_out", `Int total.bout);
          ] );
    ]

let print_json conf =
  let period = period_of_string (Util.p_getenv conf.env "p") in
  let now = Unix.time () in
  let d =
    match Option.bind (Util.p_getenv conf.env "d") ymd_of_string with
    | Some d -> d
    | None -> ymd (Unix.localtime now)
  in
  let start, start_tm = period_start period d in
  let stop, _ = shift period start_tm 1 in
  let snaps = read_snaps (StatsLog.stats_file conf) in
  let first = match snaps with s :: _ -> Some s.ts | [] -> None in
  (* no link to a period before the first snapshot or after today *)
  let prev =
    match first with
    | Some f when f < start ->
        Some (string_of_ymd (ymd (snd (shift period start_tm (-1)))))
    | _ -> None
  in
  let next =
    let t, tm = shift period start_tm 1 in
    if t <= now then Some (string_of_ymd (ymd tm)) else None
  in
  let pts = points period ~start ~stop snaps in
  let j =
    json period
      ~day:(string_of_ymd (ymd start_tm))
      ~prev ~next
      ~first:
        (Option.map (fun f -> string_of_ymd (ymd (Unix.localtime f))) first)
      ~record:(record snaps)
      ~countries:(read_countries (StatsLog.countries_file conf) ~start ~stop)
      pts
  in
  Output.header conf "Content-type: application/json; charset=utf-8";
  Output.header conf "Cache-control: no-store";
  Output.print_sstring conf (Yojson.Safe.to_string j);
  Output.flush conf

type 'a env = Vother of 'a

let get_vother = function Vother x -> Some x
let set_vother x = Vother x

let print_page conf =
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
  Templ.output conf ifun Templ.Env.empty () "gwdstats"

let print conf =
  match Util.p_getenv conf.env "json" with
  | Some _ -> print_json conf
  | None -> print_page conf
