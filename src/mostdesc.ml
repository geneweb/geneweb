(* $Id: mostdesc.ml,v 1.1 1998-11-15 22:55:04 ddr Exp $ *)

open Gutil;
open Def;

(*
value qsort len leq exch =
  loop 0 len where rec loop imin imax =
    if imin + 1 <= imax then
      let i =
        loop (imin + 1) imax where rec loop i j =
          if i == j then i
          else if leq i imin then loop (i + 1) j
          else
            do if i < j - 1 then exch i (j - 1) else (); return loop i (j - 1)
      in
      let i = if i < imax && leq i imin then i else i - 1 in
      do if imin < i then exch imin i else ();
         if imin < i then loop imin i else ();
         if i + 1 < imax then loop (i + 1) imax else ();
      return ()
    else ()
;

value sort_tab tab =
  let ind = Array.create (Array.length tab) 0 in
  do for i = 0 to Array.length ind - 1 do ind.(i) := i; done;
     qsort (Array.length tab) (fun i j -> Num.gt tab.(i) tab.(j))
       (fun i j ->
          let a = tab.(i) and b = ind.(i) in
          do tab.(i) := tab.(j); tab.(j) := a;
             ind.(i) := ind.(j); ind.(j) := b;
          return ());
  return ind
;

value print_result base tab =
  let ind = sort_tab tab in
do Printf.eprintf "*** sort ok\n"; flush stderr; return
  loop 0 where rec loop i =
    if i < Array.length tab && not (Num.eq tab.(i) Num.zero) then
      let m_val = tab.(i) in
      let (m_list, i) =
        loop (i + 1) [ind.(i)] where rec loop i list =
          if i < Array.length tab && Num.eq tab.(i) m_val then
            loop (i + 1) [ind.(i) :: list]
          else (list, i)
      in
      do Num.print print_string "." m_val;
         print_newline ();
         List.iter
           (fun i ->
              let p = base.persons.get i in
              do Printf.printf "- %s.%d %s\n"
                   (Ansel.to_iso_8859_1 (sou base p.first_name)) p.occ
                   (Ansel.to_iso_8859_1 (sou base p.surname));
                 flush stdout;
              return ())
           m_list;
      return loop i
    else ()
;
*)

value print_result base tab =
  let m_val = ref Num.zero in
  let m_list = ref [] in
  loop () where rec loop () =
    do m_val.val := Num.zero;
       m_list.val := [];
       for i = 0 to Array.length tab - 1 do
         if Num.eq tab.(i) Num.zero then ()
         else if Num.gt tab.(i) m_val.val then
           do m_val.val := tab.(i); m_list.val := [i]; return ()
         else if Num.eq tab.(i) m_val.val then m_list.val := [i :: m_list.val]
         else ();
       done;
    return
    if m_list.val <> [] then
      do Num.print print_string "." m_val.val;
         print_newline ();
         List.iter
           (fun i ->
              let p = base.persons.get i in
              do Printf.printf "- %s.%d %s\n"
                   (Ansel.to_iso_8859_1 (sou base p.first_name)) p.occ
                   (Ansel.to_iso_8859_1 (sou base p.surname));
                 flush stdout;
              return ())
           m_list.val;
         List.iter (fun i -> tab.(i) := Num.zero) m_list.val;
      return loop ()
    else ()
;

value most_desc base p =
  let _ = base.ascends.array () in
  let id = Consang.topological_sort base in
  let module Pq =
    Pqueue.Make
      (struct
         type t = iper;
         value leq x y = id.(Adef.int_of_iper x) > id.(Adef.int_of_iper y);
       end)
  in
  let _ = base.persons.array () in
  let _ = base.families.array () in
  let tab = Array.create base.persons.len Num.zero in
  let entered = Array.create base.persons.len False in
  let q = ref Pq.empty in
  do q.val := Pq.add p.cle_index q.val;
     tab.(Adef.int_of_iper p.cle_index) := Num.one;
     while not (Pq.is_empty q.val) do
       let (ip, nq) = Pq.take q.val in
       do q.val := nq; return
       let p = poi base ip in
       let n = tab.(Adef.int_of_iper ip) in
       for i = 0 to Array.length p.family - 1 do
         let fam = foi base p.family.(i) in
         for j = 0 to Array.length fam.children - 1 do
           let ip = fam.children.(j) in
           do tab.(Adef.int_of_iper ip) := Num.add tab.(Adef.int_of_iper ip) n;
              if not entered.(Adef.int_of_iper ip) then
                do q.val := Pq.add ip q.val;
                   entered.(Adef.int_of_iper ip) := True;
                return ()
              else ();
           return ();
         done;
       done;
     done;
     print_result base tab;
  return ()
;

value bname = ref "";
value p_fname = ref "";
value p_num = ref 0;
value p_sname = ref "";
value usage =
  "usage: " ^ Sys.argv.(0) ^ " <base> <first_name> <num> <surname>"
;
value speclist = [];

value main () =
  let cnt = ref 0 in
  do Argl.parse speclist
       (fun s ->
          do match cnt.val with
             [ 0 -> bname.val := s
             | 1 -> p_fname.val := s
             | 2 -> p_num.val := int_of_string s
             | 3 -> p_sname.val := s
             | _ -> raise (Arg.Bad "too many parameters") ];
             incr cnt;
          return ())
       usage;
     if cnt.val <> 4 then 
       do Printf.eprintf "Missing parameter\n";
          Printf.eprintf "Use option -help for usage\n";
          flush stderr;
       return exit 2
     else ();
  return
  let base = Iobase.input bname.val in
  let ip =
    Gutil.person_ht_find_unique base p_fname.val p_sname.val p_num.val
  in
  most_desc base (poi base ip)
;

Printexc.catch main ();
