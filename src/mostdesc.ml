(* $Id: mostdesc.ml,v 3.1 1999-11-10 08:44:28 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Gutil;
open Def;

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
      do m_list.val :=
           let f i1 i2 =
             let p1 = base.data.persons.get i1 in
             let p2 = base.data.persons.get i2 in
             let s1 = Name.abbrev (Name.lower (p_surname base p1)) in
             let s2 = Name.abbrev (Name.lower (p_surname base p2)) in
             if s1 < s2 then True
             else if s1 > s2 then False
             else 
               let f1 = Name.abbrev (Name.lower (p_first_name base p1)) in
               let f2 = Name.abbrev (Name.lower (p_first_name base p2)) in
               f1 <= f2
           in
           Sort.list f m_list.val;
         Num.print print_string "." m_val.val;
         print_newline ();
         List.iter
           (fun i ->
              let p = base.data.persons.get i in
              do Printf.printf "- %s.%d %s\n"
                   (p_first_name base p) p.occ
                   (p_surname base p);
                 flush stdout;
              return ())
           m_list.val;
         List.iter (fun i -> tab.(i) := Num.zero) m_list.val;
      return loop ()
    else ()
;

value most_desc base p =
  let _ = base.data.ascends.array () in
  let id = Consang.topological_sort base in
  let module Pq =
    Pqueue.Make
      (struct
         type t = iper;
         value leq x y = id.(Adef.int_of_iper x) > id.(Adef.int_of_iper y);
       end)
  in
  let _ = base.data.persons.array () in
  let _ = base.data.families.array () in
  let tab = Array.create base.data.persons.len Num.zero in
  let entered = Array.create base.data.persons.len False in
  let q = ref Pq.empty in
  do q.val := Pq.add p.cle_index q.val;
     tab.(Adef.int_of_iper p.cle_index) := Num.one;
     while not (Pq.is_empty q.val) do
       let (ip, nq) = Pq.take q.val in
       do q.val := nq; return
       let u = uoi base ip in
       let n = tab.(Adef.int_of_iper ip) in
       for i = 0 to Array.length u.family - 1 do
         let des = doi base u.family.(i) in
         for j = 0 to Array.length des.children - 1 do
           let ip = des.children.(j) in
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
