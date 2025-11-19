(* Robot blacklist manager for Geneweb *)
(* Usage: robot <command> [options] *)

open Printf

let magic_robot = "GWRB0008"

let get_robot_file () =
  String.concat Filename.dir_sep [ Secure.base_dir (); "cnt"; "robot" ]

module W = Map.Make (struct
  type t = string

  let compare = compare
end)

type norfriwiz = Normal | Friend of string | Wizard of string

type who = {
  acc_times : float list;
  oldest_time : float;
  nb_connect : int;
  nbase : string;
  utype : norfriwiz;
}

type excl = {
  mutable excl : (string * int ref) list;
  mutable who : who W.t;
  mutable max_conn : int * string;
  mutable last_summary : float;
}

let input_excl ic =
  let b = really_input_string ic (String.length magic_robot) in
  if b <> magic_robot then raise Not_found else (input_value ic : excl)

let output_excl oc xcl =
  output_string oc magic_robot;
  output_value oc (xcl : excl)

let read_robot_file fname =
  try
    let ic = Secure.open_in_bin fname in
    let b = really_input_string ic (String.length magic_robot) in
    if b <> magic_robot then (
      close_in ic;
      if b = "GWRB0007" then (
        printf "Error: Old format detected.\n";
        printf "Please start gwd once to migrate.\n";
        exit 1)
      else (
        printf "Error: Invalid robot file format.\n";
        exit 1));
    let v = (input_value ic : excl) in
    close_in ic;
    Some v
  with _ -> None

let write_robot_file fname xcl =
  let oc = Secure.open_out_bin fname in
  output_excl oc xcl;
  close_out oc

let string_of_norfriwiz = function
  | Normal -> "visitor"
  | Friend s -> sprintf "friend (%s)" s
  | Wizard s -> sprintf "wizard (%s)" s

let is_valid_ip ip =
  if String.contains ip '*' then
    let parts = String.split_on_char '.' ip in
    let rec check = function
      | [] -> false
      | [ "*" ] -> true
      | h :: t when h = "*" -> t = []
      | h :: t -> (
          try
            let n = int_of_string h in
            n >= 0 && n <= 255 && check t
          with _ -> false)
    in
    List.length parts <= 4 && check parts
  else
    match String.split_on_char '.' ip with
    | [ a; b; c; d ] ->
        List.for_all
          (fun s ->
            try
              let n = int_of_string s in
              n >= 0 && n <= 255
            with _ -> false)
          [ a; b; c; d ]
    | _ -> false

let ip_matches_pattern ip pattern =
  if String.contains pattern '*' then
    let pattern_parts = String.split_on_char '.' pattern in
    let ip_parts = String.split_on_char '.' ip in
    if List.length ip_parts <> 4 then false
    else
      let rec match_parts pp ip_p =
        match (pp, ip_p) with
        | [], [] -> true
        | "*" :: _, _ -> true
        | p :: pt, i :: it when p = i -> match_parts pt it
        | _ -> false
      in
      match_parts pattern_parts ip_parts
  else ip = pattern

let print_status () =
  let fname = get_robot_file () in
  if not (Sys.file_exists fname) then printf "No robot file found at %s\n" fname
  else
    match read_robot_file fname with
    | None -> printf "Cannot read robot file\n"
    | Some xcl ->
        printf "Robot file: %s\n" fname;
        let individual_ips, patterns =
          List.partition (fun (ip, _) -> not (String.contains ip '*')) xcl.excl
        in
        printf "Blocked individual IPs: %d\n" (List.length individual_ips);
        printf "Blocked IP patterns: %d\n" (List.length patterns);
        printf "Monitored IPs: %d\n" (W.cardinal xcl.who);
        printf "Max connections: %d by %s\n" (fst xcl.max_conn)
          (snd xcl.max_conn);
        printf "\nBlocked IPs/Patterns:\n";
        let total_attempts = ref 0 in
        List.iter
          (fun (ip, cnt) ->
            printf "  %s (%d attempts)\n" ip !cnt;
            total_attempts := !total_attempts + !cnt)
          xcl.excl;
        printf "\nTotal attempts: %d\n" !total_attempts;
        printf "\nTop monitored IPs:\n";
        let monitored =
          W.fold (fun ip who acc -> (ip, who) :: acc) xcl.who []
        in
        let sorted =
          List.sort
            (fun (_, w1) (_, w2) -> compare w2.nb_connect w1.nb_connect)
            monitored
        in
        let top_10 =
          let rec take n = function
            | [] -> []
            | h :: t -> if n = 0 then [] else h :: take (n - 1) t
          in
          take 10 sorted
        in
        if top_10 = [] then printf "  (No active monitoring)\n"
        else
          List.iter
            (fun (ip, who) ->
              if who.acc_times <> [] then
                printf "  %s: %d req, last: %.0f s ago, type: %s\n" ip
                  who.nb_connect
                  (Unix.time () -. List.hd who.acc_times)
                  (string_of_norfriwiz who.utype))
            top_10

let export_blacklist output_file =
  let fname = get_robot_file () in
  match read_robot_file fname with
  | None -> printf "No robot file found\n"
  | Some xcl ->
      let oc = Secure.open_out output_file in
      fprintf oc "# Robot blacklist export\n";
      fprintf oc "# Format: IP<TAB>attempts\n";
      let tm = Unix.localtime (Unix.time ()) in
      fprintf oc "# Generated: %04d-%02d-%02d %02d:%02d:%02d\n\n"
        (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
      List.iter (fun (ip, cnt) -> fprintf oc "%s\t%d\n" ip !cnt) xcl.excl;
      close_out oc;
      printf "Exported %d entries to %s\n" (List.length xcl.excl) output_file

let import_blacklist input_file =
  if not (Sys.file_exists input_file) then (
    printf "Error: File %s not found\n" input_file;
    exit 1);

  let fname = get_robot_file () in
  let xcl =
    match read_robot_file fname with
    | Some x -> x
    | None ->
        { excl = []; who = W.empty; max_conn = (0, ""); last_summary = 0.0 }
  in

  let ic = Secure.open_in input_file in
  let imported = ref [] in
  let line_num = ref 0 in
  (try
     while true do
       incr line_num;
       let line = input_line ic in
       let line = String.trim line in
       if line <> "" && line.[0] <> '#' then
         try
           let ip, cnt =
             match String.split_on_char '\t' line with
             | [ ip; cnt_str ] ->
                 (String.trim ip, int_of_string (String.trim cnt_str))
             | [ ip ] -> (String.trim ip, 1)
             | _ -> raise (Failure "format")
           in
           if not (is_valid_ip ip) then
             printf "Warning line %d: Invalid IP %s\n" !line_num ip
           else if not (List.mem_assoc ip !imported) then
             imported := (ip, ref cnt) :: !imported
           else
             let existing = List.assoc ip !imported in
             existing := !existing + cnt
         with _ -> printf "Warning line %d: Invalid format\n" !line_num
     done
   with End_of_file -> ());
  close_in ic;

  List.iter
    (fun (ip, cnt) ->
      if List.mem_assoc ip xcl.excl then
        let existing = List.assoc ip xcl.excl in
        existing := !existing + !cnt
      else xcl.excl <- (ip, cnt) :: xcl.excl)
    !imported;

  xcl.who <- W.empty;
  write_robot_file fname xcl;
  printf "Imported %d unique IPs\n" (List.length !imported)

let add_ip patterns_str =
  let patterns =
    if String.contains patterns_str ',' then
      String.split_on_char ',' patterns_str
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    else [ patterns_str ]
  in

  List.iter
    (fun p ->
      if not (is_valid_ip p) then (
        printf "Error: Invalid IP/pattern: %s\n" p;
        exit 1))
    patterns;

  let fname = get_robot_file () in
  let xcl =
    match read_robot_file fname with
    | Some x -> x
    | None ->
        { excl = []; who = W.empty; max_conn = (0, ""); last_summary = 0.0 }
  in

  List.iter
    (fun pattern ->
      if String.contains pattern '*' then (
        let matching_ips, remaining_ips =
          List.partition
            (fun (ip, _) ->
              (not (String.contains ip '*')) && ip_matches_pattern ip pattern)
            xcl.excl
        in
        if List.mem_assoc pattern remaining_ips then (
          let existing = List.assoc pattern remaining_ips in
          let total =
            List.fold_left
              (fun acc (_, cnt) -> acc + !cnt)
              !existing matching_ips
          in
          existing := total;
          xcl.excl <- remaining_ips;
          printf "Updated pattern %s (%d attempts)\n" pattern total)
        else
          let total =
            List.fold_left (fun acc (_, cnt) -> acc + !cnt) 0 matching_ips
          in
          let final_count = max 1 total in
          xcl.excl <- (pattern, ref final_count) :: remaining_ips;
          if List.length matching_ips > 0 then
            printf "Added pattern %s (consolidated %d IPs, %d attempts)\n"
              pattern (List.length matching_ips) final_count
          else printf "Added pattern %s (%d attempts)\n" pattern final_count)
      else if not (List.mem_assoc pattern xcl.excl) then (
        xcl.excl <- (pattern, ref 1) :: xcl.excl;
        xcl.who <- W.remove pattern xcl.who;
        printf "Added %s to blacklist\n" pattern)
      else printf "IP %s already in blacklist\n" pattern)
    patterns;

  write_robot_file fname xcl

let remove_ip ip =
  let fname = get_robot_file () in
  let xcl =
    match read_robot_file fname with
    | Some x -> x
    | None ->
        { excl = []; who = W.empty; max_conn = (0, ""); last_summary = 0.0 }
  in
  if List.mem_assoc ip xcl.excl then (
    xcl.excl <- List.remove_assoc ip xcl.excl;
    write_robot_file fname xcl;
    printf "Removed %s from blacklist\n" ip)
  else printf "IP %s not found in blacklist\n" ip

let clear_all () =
  let fname = get_robot_file () in
  let xcl =
    { excl = []; who = W.empty; max_conn = (0, ""); last_summary = 0.0 }
  in
  write_robot_file fname xcl;
  printf "Cleared all robot data\n"

let clear_monitoring () =
  let fname = get_robot_file () in
  let xcl =
    match read_robot_file fname with
    | Some x -> { x with who = W.empty; max_conn = (0, "") }
    | None ->
        { excl = []; who = W.empty; max_conn = (0, ""); last_summary = 0.0 }
  in
  write_robot_file fname xcl;
  printf "Cleared monitoring data (kept blacklist)\n"

let suggest_ranges threshold =
  let fname = get_robot_file () in
  match read_robot_file fname with
  | None -> printf "No robot file found\n"
  | Some xcl ->
      let ranges = Hashtbl.create 256 in
      List.iter
        (fun (ip, cnt) ->
          if not (String.contains ip '*') then
            let parts = String.split_on_char '.' ip in
            match parts with
            | [ a; b; c; _ ] ->
                let range = Printf.sprintf "%s.%s.%s.*" a b c in
                let current =
                  try Hashtbl.find ranges range with Not_found -> []
                in
                Hashtbl.replace ranges range ((ip, !cnt) :: current)
            | _ -> ())
        xcl.excl;

      let suggestions = ref [] in
      Hashtbl.iter
        (fun range ips ->
          let count = List.length ips in
          if count >= threshold then
            let total =
              List.fold_left (fun acc (_, attempts) -> acc + attempts) 0 ips
            in
            suggestions := (range, count, total, ips) :: !suggestions)
        ranges;

      let sorted =
        List.sort
          (fun (r1, _, _, _) (r2, _, _, _) -> String.compare r1 r2)
          !suggestions
      in

      printf "=== RANGE SUGGESTIONS ===\n";
      printf "Threshold: %d IPs per /24 range\n\n" threshold;

      if sorted = [] then printf "No ranges found with %d+ IPs\n" threshold
      else (
        printf "Suggested ranges to consolidate:\n\n";
        let all_ranges = ref [] in
        List.iter
          (fun (range, count, attempts, ips) ->
            all_ranges := range :: !all_ranges;
            printf "%s: %d IPs, %d total attempts\n" range count attempts;
            let sorted_ips = List.sort (fun (_, a) (_, b) -> compare b a) ips in
            let samples =
              let rec take n = function
                | [] -> []
                | h :: t -> if n = 0 then [] else h :: take (n - 1) t
              in
              take 5 sorted_ips
            in
            printf "  Sample IPs: ";
            List.iter
              (fun (ip, attempts) -> printf "%s (%d) " ip attempts)
              samples;
            if count > 5 then printf "... and %d more" (count - 5);
            printf "\n\n")
          sorted;

        printf "Summary:\n";
        printf "  Total suggested ranges: %d\n" (List.length sorted);
        let total_ips =
          List.fold_left (fun acc (_, count, _, _) -> acc + count) 0 sorted
        in
        printf "  Total IPs that would be consolidated: %d\n\n" total_ips;

        printf "Single command to add all ranges:\n";
        let ranges_str = String.concat "," (List.rev !all_ranges) in
        printf "  robot -add \"%s\"\n" ranges_str)

let usage () =
  printf "Usage: robot [options] <command> [args]\n";
  printf "\nOptions:\n";
  printf
    "  -bd <DIR>                  Set bases directory (default: current \
     directory)\n";
  printf "\nCommands:\n";
  printf "  -status                    Show current status\n";
  printf
    "  -suggest [threshold]       Suggest ranges to consolidate (default: 10)\n";
  printf "  -export <output_file>      Export blacklist to text\n";
  printf "  -import <input_file>       Import blacklist from text\n";
  printf
    "  -add <ip_or_pattern>       Add IP/pattern to blacklist (comma-separated \
     for multiple)\n";
  printf "  -rm <ip_or_pattern>        Remove IP/pattern from blacklist\n";
  printf "  -clear                     Clear all data\n";
  printf "  -clear-monitoring          Clear only monitoring data\n";
  printf "\nText file format for import/export:\n";
  printf "  Format: ip_address<TAB>attempt_count\n";
  printf "  Simple: ip_address (defaults to 1 attempt)\n";
  printf "  Comments: Lines starting with # are ignored\n";
  printf "\nExamples:\n";
  printf "  robot -status\n";
  printf "  robot -suggest 25\n";
  printf "  robot -bd ../bases -status\n";
  printf "  robot -add 192.168.1.100\n";
  printf "  robot -add \"146.174.*\"\n";
  printf "  robot -add \"146.174.*,202.76.*,1.2.3.*\"\n";
  printf "  robot -export blacklist.txt\n";
  printf "\nRobot file location: %s\n" (get_robot_file ())

let rec parse_args = function
  | "-bd" :: basedir :: rest ->
      Secure.set_base_dir basedir;
      parse_args rest
  | [ "-status" ] -> print_status ()
  | [ "-suggest" ] -> suggest_ranges 10
  | [ "-suggest"; threshold_str ] ->
      let threshold = try int_of_string threshold_str with _ -> 10 in
      suggest_ranges threshold
  | [ "-export"; output ] -> export_blacklist output
  | [ "-import"; input ] -> import_blacklist input
  | [ "-add"; ip ] -> add_ip ip
  | [ "-rm"; ip ] -> remove_ip ip
  | [ "-clear" ] -> clear_all ()
  | [ "-clear-monitoring" ] -> clear_monitoring ()
  | _ -> usage ()

let () =
  let default_base_dir =
    if Sys.file_exists "bases" && Sys.is_directory "bases" then "bases"
    else "../bases"
  in
  Secure.set_base_dir default_base_dir;
  match Array.to_list Sys.argv with
  | _ :: args -> parse_args args
  | [] -> usage ()
