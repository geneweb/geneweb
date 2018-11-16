open Geneweb

let () =
  let xcl = Robot.input_excl (open_in_bin "cnt/robot") in
  let tm_now = Unix.time () in
  if xcl.Robot.excl <> [] then Printf.printf "excl\n";
  List.iter (fun (s, ri) -> Printf.printf "  \"%s\" %d\n" s !ri) xcl.Robot.excl;
  Printf.printf "who\n";
  let sorted_who =
    List.sort
      (fun (_k1, w1) (_k2, w2) ->
         compare (List.hd w1.Robot.acc_times) (List.hd w2.Robot.acc_times))
      (Robot.W.fold (fun k x l -> (k, x) :: l) xcl.Robot.who [])
  in
  List.iter
    (fun (k, w) ->
       let tml = w.Robot.acc_times in
       let _tm0 = w.Robot.oldest_time in
       let cnt = w.Robot.nb_connect in
       let bn = w.Robot.nbase in
       let nfw = w.Robot.utype in
       Printf.printf "  %s (%d)" k cnt;
       begin match nfw with
       | Robot.Wizard n ->
           Printf.printf " (\027[31mwiz %s%s\027[30m)" bn
             (if n = "" then "" else " " ^ n)
       | Robot.Friend n ->
           Printf.printf " (\027[34mfri %s%s\027[30m)" bn
             (if n = "" then "" else " " ^ n)
       | Robot.Normal -> ()
       end;
       Printf.printf "\n";
       Printf.printf "    ";
       let cnt = ref 0 in
       let siz = List.length tml in
       List.iter
         (fun tm ->
            incr cnt;
            if true || !cnt <= 8 || !cnt > siz - 8 then
              Printf.printf "-%g" (tm_now -. tm)
            else if !cnt = 9 then Printf.printf "-...")
         tml;
       Printf.printf "\n")
    sorted_who;
  Printf.printf "max_conn\n";
  Printf.printf "  %d \"%s\"\n" (fst xcl.Robot.max_conn) (snd xcl.Robot.max_conn);
  flush stdout
