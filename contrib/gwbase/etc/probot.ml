(* camlp5r *)
(* $Id: probot.ml,v 1.7 2006-10-05 13:37:58 deraugla Exp $ *)


value main () =
  let xcl = Robot.input_excl (open_in_bin "cnt/robot") in
  let tm_now = Unix.time () in
  do {
    if xcl.Robot.excl <> [] then Printf.printf "excl\n" else ();
    List.iter
      (fun (s, ri) -> Printf.printf "  \"%s\" %d\n" s ri.val)
      xcl.Robot.excl;
    Printf.printf "who\n";
    let sorted_who =
      List.sort
        (fun (k1, w1) (k2, w2) ->
          compare (List.hd w1.Robot.acc_times) (List.hd w2.Robot.acc_times))
        (Robot.W.fold (fun k x l -> [(k, x) :: l]) xcl.Robot.who [])
    in
    List.iter
      (fun (k, w) ->
         let tml = w.Robot.acc_times in
	 let _tm0 = w.Robot.oldest_time in
	 let cnt = w.Robot.nb_connect in
	 let bn = w.Robot.nbase in
	 let nfw = w.Robot.utype in
	 do {
           Printf.printf "  %s (%d)" k cnt;
	   match nfw with
	   [ Robot.Wizard n ->
	       Printf.printf " (\027[31mwiz %s%s\027[30m)" bn
	         (if n = "" then "" else " " ^ n)
	   | Robot.Friend n ->
	       Printf.printf " (\027[34mfri %s%s\027[30m)" bn
                 (if n = "" then "" else " " ^ n)
           | Robot.Normal -> () ];
           Printf.printf "\n";
           Printf.printf "    ";
	   let cnt = ref 0 in
	   let siz = List.length tml in
	   List.iter
	     (fun tm -> do {
		incr cnt;
		if True || cnt.val <= 8 || cnt.val > siz - 8 then
		  Printf.printf "-%g" (tm_now -. tm)
		else if cnt.val = 9 then Printf.printf "-..."
		else ();
	      })
	   tml;
	   Printf.printf "\n";
	 })
      sorted_who;
    Printf.printf "max_conn\n";
    Printf.printf
      "  %d \"%s\"\n"
      (fst xcl.Robot.max_conn)
      (snd xcl.Robot.max_conn);
    flush stdout;
  }
;

main ();
