(* $Id: mostdesc.ml,v 5.9 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gwdb;

value print_result base tab =
  let m_val = ref Num.zero in
  let m_list = ref [] in
  loop () where rec loop () =
    do {
      m_val.val := Num.zero;
      m_list.val := [];
      for i = 0 to Array.length tab - 1 do {
        if Num.eq tab.(i) Num.zero then ()
        else if Num.gt tab.(i) m_val.val then do {
          m_val.val := tab.(i); m_list.val := [i];
        }
        else if Num.eq tab.(i) m_val.val then m_list.val := [i :: m_list.val]
        else ()
      };
      if m_list.val <> [] then do {
        m_list.val :=
          let f i1 i2 =
            let p1 = poi base (Adef.iper_of_int i1) in
            let p2 = poi base (Adef.iper_of_int i2) in
            let s1 = Name.abbrev (Name.lower (p_surname base p1)) in
            let s2 = Name.abbrev (Name.lower (p_surname base p2)) in
            if s1 < s2 then -1
            else if s1 > s2 then 1
            else
              let f1 = Name.abbrev (Name.lower (p_first_name base p1)) in
              let f2 = Name.abbrev (Name.lower (p_first_name base p2)) in
              compare f1 f2
          in
          List.sort f m_list.val;
        Num.print print_string "." m_val.val;
        print_newline ();
        List.iter
          (fun i ->
             let p = poi base (Adef.iper_of_int i) in
             do {
               Printf.printf "- %s.%d %s\n" (p_first_name base p) (get_occ p)
                 (p_surname base p);
               flush stdout;
             })
          m_list.val;
        List.iter (fun i -> tab.(i) := Num.zero) m_list.val;
        loop ()
      }
      else ()
    }
;

value most_desc base p =
  let _ = load_ascends_array base in
  let _ = load_couples_array base in
  let id = Consang.topological_sort base poi in
  let module Pq =
    Pqueue.Make
      (struct
         type t = iper;
         value leq x y = id.(Adef.int_of_iper x) > id.(Adef.int_of_iper y);
       end)
  in
(*
  let _ = base.data.persons.array () in
*)
  let _ = load_descends_array base in
  let _ = load_unions_array base in
  let tab = Array.create (nb_of_persons base) Num.zero in
  let entered = Array.create (nb_of_persons base) False in
  let q = ref Pq.empty in
  do {
    q.val := Pq.add (get_key_index p) q.val;
    tab.(Adef.int_of_iper (get_key_index p)) := Num.one;
    while not (Pq.is_empty q.val) do {
      let (ip, nq) = Pq.take q.val in
      q.val := nq;
      let u = uoi base ip in
      let n = tab.(Adef.int_of_iper ip) in
      for i = 0 to Array.length (get_family u) - 1 do {
        let des = doi base (get_family u).(i) in
        for j = 0 to Array.length (get_children des) - 1 do {
          let ip = (get_children des).(j) in
          tab.(Adef.int_of_iper ip) := Num.add tab.(Adef.int_of_iper ip) n;
          if not entered.(Adef.int_of_iper ip) then do {
            q.val := Pq.add ip q.val;
            entered.(Adef.int_of_iper ip) := True;
          }
          else ();
        }
      }
    };
    print_result base tab;
  }
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
  do {
    Argl.parse speclist
      (fun s ->
         do {
           match cnt.val with
           [ 0 -> bname.val := s
           | 1 -> p_fname.val := s
           | 2 -> p_num.val := int_of_string s
           | 3 -> p_sname.val := s
           | _ -> raise (Arg.Bad "too many parameters") ];
           incr cnt;
         })
      usage;
    if cnt.val <> 4 then do {
      Printf.eprintf "Missing parameter\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2
    }
    else ();
    Secure.set_base_dir (Filename.dirname bname.val);
    let base = Gwdb.open_base bname.val in
    match Gwdb.person_of_key base p_fname.val p_sname.val p_num.val with
    [ Some ip -> most_desc base (poi base ip)
    | None -> raise Not_found ]
  }
;

Printexc.catch main ();
