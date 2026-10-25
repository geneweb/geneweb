open Anomaly

let ( // ) = Filename.concat

let can_verify k =
  match k with
  | K_YoungForMarriage | K_OldForMarriage | K_ParentTooOld | K_ParentTooYoung
  | K_DeadOld | K_BigAgeBetweenSpouses | K_CloseChildren | K_DistantChildren
  | K_ChildrenNotInOrder | K_ChangedOrderOfChildren | K_PossibleDuplicateFam
  | K_PossibleDuplicateFamHomonymous | K_PEventOrder | K_FEventOrder
  | K_ChangedOrderOfFamilyEvents | K_ChangedOrderOfPersonEvents | K_UndefinedSex
    ->
      true
  | K_BirthAfterDeath | K_ChangedOrderOfMarriages | K_DeadTooEarlyToBeFather
  | K_FWitnessEventAfterDeath | K_FWitnessEventBeforeBirth
  | K_IncoherentAncestorDate | K_IncoherentSex | K_MarriageDateAfterDeath
  | K_MarriageDateBeforeBirth | K_MotherDeadBeforeChildBirth
  | K_ParentBornAfterChild | K_PWitnessEventAfterDeath
  | K_PWitnessEventBeforeBirth | K_TitleDatesError ->
      false

type entry = { anomaly : Anomaly.t; wizard : string; date : string }

(* ---- records -------------------------------------------------------------- *)

type record = { entry : entry; verified : bool }

let record_to_json r =
  `Assoc
    [
      ("a", Anomaly.to_json r.entry.anomaly);
      ("by", `String r.entry.wizard);
      ("at", `String r.entry.date);
      ("ok", `Bool r.verified);
    ]

let record_of_json (j : Yojson.Safe.t) =
  match j with
  | `Assoc fields -> (
      match
        ( Option.bind (List.assoc_opt "a" fields) Anomaly.of_json,
          List.assoc_opt "by" fields,
          List.assoc_opt "at" fields,
          List.assoc_opt "ok" fields )
      with
      | ( Some anomaly,
          Some (`String wizard),
          Some (`String date),
          Some (`Bool ok) ) ->
          Some { entry = { anomaly; wizard; date }; verified = ok }
      | _ -> None)
  | _ -> None

(* records of a file, in file order; damaged lines are skipped *)
let read_records path =
  match open_in_bin path with
  | exception Sys_error _ -> []
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop acc =
            match input_line ic with
            | exception End_of_file -> List.rev acc
            | line -> (
                match Yojson.Safe.from_string line with
                | exception Yojson.Json_error _ -> loop acc
                | j -> (
                    match record_of_json j with
                    | Some r -> loop (r :: acc)
                    | None -> loop acc))
          in
          loop [])

(* ---- files ---------------------------------------------------------------- *)

let file config_dir = config_dir // "verified.jsonl"
let fragments_dir config_dir = config_dir // "verified_d"
let is_directory d = try Sys.is_directory d with Sys_error _ -> false

let pending config_dir =
  let dir = fragments_dir config_dir in
  if not (is_directory dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun n ->
        n <> ""
        && n.[0] <> '.'
        && Filename.check_suffix n ".jsonl"
        && not (is_directory (dir // n)))
    |> List.sort compare
    |> List.map (fun n -> (Filename.chop_suffix n ".jsonl", dir // n))

(* ---- state ---------------------------------------------------------------- *)

type t = (string, entry) Hashtbl.t

(* The merged file first, then the pending records by date: a cancellation
   made after a verification wins, whichever wizard made them. The sort is
   stable, so records of the same second keep their file order. *)
let apply t records =
  List.iter
    (fun r ->
      let id = Anomaly.identity r.entry.anomaly in
      if r.verified then (
        if not (Hashtbl.mem t id) then Hashtbl.replace t id r.entry)
      else Hashtbl.remove t id)
    records

let pending_records paths =
  List.concat_map read_records paths
  |> List.stable_sort (fun r1 r2 -> compare r1.entry.date r2.entry.date)

let load config_dir =
  let t = Hashtbl.create 1024 in
  apply t (read_records (file config_dir));
  apply t (pending_records (List.map snd (pending config_dir)));
  t

let find t a =
  if Hashtbl.length t = 0 then None else Hashtbl.find_opt t (identity a)

let mem t a = Option.is_some (find t a)
let size t = Hashtbl.length t

let entries t =
  Hashtbl.fold (fun _ e acc -> e :: acc) t []
  |> List.sort (fun e1 e2 -> compare e2.date e1.date)

(* ---- recording ------------------------------------------------------------ *)

let now () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

(* The fragment name comes from the login: keep it to a safe set so that it
   stays inside verified_d/. *)
let fragment_name wizard =
  let wizard = if wizard = "" then "anonymous" else wizard in
  String.map
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> c
      | _ -> '_')
    wizard
  ^ ".jsonl"

let record config_dir ~wizard ~verified a =
  if not (can_verify (kind a)) then
    Error "this kind of anomaly cannot be verified"
  else if List.exists is_unnamed (persons a) then
    Error "an anomaly about an unnamed person cannot be verified"
  else
    let line =
      Yojson.Safe.to_string
        (record_to_json
           { entry = { anomaly = a; wizard; date = now () }; verified })
      ^ "\n"
    in
    let dir = fragments_dir config_dir in
    match
      Filesystem.create_dir ~parent:true dir;
      let fd =
        Unix.openfile
          (dir // fragment_name wizard)
          [ O_WRONLY; O_CREAT; O_APPEND ]
          0o644
      in
      Fun.protect
        ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
        (fun () ->
          (* a single write of one line: appends are not interleaved *)
          ignore (Unix.write_substring fd line 0 (String.length line)))
    with
    | () -> Ok ()
    | exception Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
    | exception Sys_error msg -> Error msg

(* ---- merging -------------------------------------------------------------- *)

type fold_result = { entries : int; fragments : int; rewritten : bool }

let timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d-%02d%02d%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let claim config_dir =
  let suffix =
    Printf.sprintf ".claimed.%d.%.0f" (Unix.getpid ())
      (Unix.gettimeofday () *. 1000.)
  in
  List.filter_map
    (fun (wizard, path) ->
      match Sys.rename path (path ^ suffix) with
      | () -> Some (wizard, path ^ suffix)
      | exception Sys_error _ -> None)
    (pending config_dir)

let archive config_dir claimed =
  if claimed <> [] then (
    let dir = fragments_dir config_dir // "archive" // timestamp () in
    Filesystem.create_dir ~parent:true dir;
    List.iter
      (fun (wizard, path) ->
        let rec free n =
          let p =
            if n = 0 then dir // (wizard ^ ".jsonl")
            else dir // Printf.sprintf "%s.%d.jsonl" wizard n
          in
          if Sys.file_exists p then free (n + 1) else p
        in
        try Sys.rename path (free 0) with Sys_error _ -> ())
      claimed)

let read_whole path =
  match open_in_bin path with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> Some (really_input_string ic (in_channel_length ic)))

let fold_locked config_dir =
  let path = file config_dir in
  let claimed = claim config_dir in
  let t = Hashtbl.create 1024 in
  apply t (read_records path);
  apply t (pending_records (List.map snd claimed));
  (* oldest first, so that the file reads as a log of the verifications *)
  let buf = Buffer.create 65536 in
  List.iter
    (fun entry ->
      Buffer.add_string buf
        (Yojson.Safe.to_string (record_to_json { entry; verified = true }));
      Buffer.add_char buf '\n')
    (List.rev (entries t));
  let folded = Buffer.contents buf in
  let original = read_whole path in
  let rewritten =
    match original with
    | Some s when s = folded -> false
    | None when folded = "" -> false
    | Some _ | None ->
        if original <> None then
          Filesystem.copy_file ~perm:0o644 path (path ^ ".bak");
        let tmp = path ^ ".tmp" in
        let oc = open_out_bin tmp in
        Fun.protect
          ~finally:(fun () -> close_out_noerr oc)
          (fun () -> output_string oc folded);
        Sys.rename tmp path;
        true
  in
  archive config_dir claimed;
  { entries = Hashtbl.length t; fragments = List.length claimed; rewritten }

let fold config_dir =
  Filesystem.create_dir ~parent:true config_dir;
  Lock.control
    ~on_exn:(fun exn bt -> Printexc.raise_with_backtrace exn bt)
    ~wait:true
    ~lock_file:(config_dir // "verified.lock")
    (fun () -> fold_locked config_dir)
