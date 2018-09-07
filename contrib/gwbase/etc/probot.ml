(* $Id: probot.ml,v 1.7 2006-10-05 13:37:58 deraugla Exp $ *)

open Printf

let main () =
  let xcl = Robot.input_excl (open_in_bin "cnt/robot") in
  let tm_now = Unix.time () in
  if xcl.excl <> [] then printf "excl\n";
  List.iter (fun (s, ri) -> printf "  \"%s\" %d\n" s !ri) xcl.Robot.excl;
  printf "who\n";
  let sorted_who =
    List.sort
      (fun (k1, w1) (k2, w2) ->
         compare (List.hd w1.Robot.acc_times) (List.hd w2.Robot.acc_times))
      (Robot.W.fold (fun k x l -> (k, x) :: l) xcl.who [])
  in
  List.iter
    (fun (k, w) ->
       let tml = w.Robot.acc_times in
       let _tm0 = w.Robot.oldest_time in
       let cnt = w.Robot.nb_connect in
       let bn = w.Robot.nbase in
       let nfw = w.Robot.utype in
       printf "  %s (%d)" k cnt;
       begin match nfw with
       | Robot.Wizard n ->
           printf " (\027[31mwiz %s%s\027[30m)" bn
             (if n = "" then "" else " " ^ n)
       | Robot.Friend n ->
           printf " (\027[34mfri %s%s\027[30m)" bn
             (if n = "" then "" else " " ^ n)
       | Robot.Normal -> ()
       end;
       printf "\n";
       printf "    ";
       let cnt = ref 0 in
       let siz = List.length tml in
       List.iter
         (fun tm ->
            incr cnt;
            if true || !cnt <= 8 || !cnt > siz - 8 then
              printf "-%g" (tm_now -. tm)
            else if !cnt = 9 then printf "-...")
         tml;
       printf "\n")
    sorted_who;
  printf "max_conn\n";
  printf "  %d \"%s\"\n" (fst xcl.Robot.max_conn) (snd xcl.Robot.max_conn);
  flush stdout

let _ = main ()
