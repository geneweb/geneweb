(* camlp4r *)
(* $Id: probot.ml,v 1.7 2006-10-05 13:37:58 deraugla Exp $ *)

open Printf;

value magic_robot = "GWRB0007";

module W = Map.Make (struct type t = string; value compare = compare; end);

type norfriwiz = [ Normal | Friend of string | Wizard of string ];

type who =
  { acc_times : list float;
    oldest_time : float;
    nb_connect : int;
    nbase : string;
    utype : norfriwiz }
;

type excl =
  { excl : mutable list (string * ref int);
    who : mutable W.t who;
    max_conn : mutable (int * string) }
;

value input_excl =
  let b = Bytes.create (String.length magic_robot) in
  fun ic ->
    do {
      really_input ic b 0 (String.length b);
      if b <> magic_robot then raise Not_found else (input_value ic : excl)
    }
;

value main () =
  let xcl = input_excl (open_in_bin "cnt/robot") in
  let tm_now = Unix.time () in
  do {
    if xcl.excl <> [] then printf "excl\n" else ();
    List.iter (fun (s, ri) -> printf "  \"%s\" %d\n" s ri.val) xcl.excl;
    printf "who\n";
    let sorted_who =
      List.sort
        (fun (k1, w1) (k2, w2) ->
            compare (List.hd w1.acc_times) (List.hd w2.acc_times))
        (W.fold (fun k x l -> [(k, x) :: l]) xcl.who [])
    in
    List.iter
      (fun (k, w) ->
         let tml = w.acc_times in
	 let tm0 = w.oldest_time in
	 let cnt = w.nb_connect in
	 let bn = w.nbase in
	 let nfw = w.utype in
	 do {
           printf "  %s (%d)" k cnt;
	   match nfw with
	   [ Wizard n ->
	       printf " (\027[31mwiz %s%s\027[30m)" bn
	         (if n = "" then "" else " " ^ n)
	   | Friend n ->
	       printf " (\027[34mfri %s%s\027[30m)" bn
                 (if n = "" then "" else " " ^ n)
           | Normal -> () ];
           printf "\n";
           printf "    ";
	   let cnt = ref 0 in
	   let siz = List.length tml in
	   List.iter
	     (fun tm -> do {
		incr cnt;
		if True || cnt.val <= 8 || cnt.val > siz - 8 then
		  printf "-%g" (tm_now -. tm)
		else if cnt.val = 9 then printf "-..."
		else ();
	      })
	   tml;
	   printf "\n";
	 })
      sorted_who;
    printf "max_conn\n";
    printf "  %d \"%s\"\n" (fst xcl.max_conn) (snd xcl.max_conn);
    flush stdout;
  }
;

main ();
