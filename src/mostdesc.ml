(* $Id: mostdesc.ml,v 5.9 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Def
open Gwdb

let print_result base tab =
  let m_val = ref Sosa.zero in
  let m_list = ref [] in
  let rec loop () =
    m_val := Sosa.zero;
    m_list := [];
    for i = 0 to Array.length tab - 1 do
      if Sosa.eq tab.(i) Sosa.zero then ()
      else if Sosa.gt tab.(i) !m_val then
        begin m_val := tab.(i); m_list := [i] end
      else if Sosa.eq tab.(i) !m_val then m_list := i :: !m_list
    done;
    if !m_list <> [] then
      begin
        m_list :=
          begin let f i1 i2 =
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
            List.sort f !m_list
          end;
        Sosa.print print_string "." !m_val;
        print_newline ();
        List.iter
          (fun i ->
             let p = poi base (Adef.iper_of_int i) in
             Printf.printf "- %s.%d %s\n" (p_first_name base p) (get_occ p)
               (p_surname base p);
             flush stdout)
          !m_list;
        List.iter (fun i -> tab.(i) <- Sosa.zero) !m_list;
        loop ()
      end
  in
  loop ()

let most_desc base p =
  let _ = load_ascends_array base in
  let _ = load_couples_array base in
  let id = Consang.topological_sort base poi in
  let module Pq =
    Pqueue.Make
      (struct
         type t = iper
         let leq x y = id.(Adef.int_of_iper x) > id.(Adef.int_of_iper y)
       end)
  in
  let _ = load_descends_array base in
  let _ = load_unions_array base in
  let tab = Array.make (nb_of_persons base) Sosa.zero in
  let entered = Array.make (nb_of_persons base) false in
  let q = ref Pq.empty in
  q := Pq.add (get_key_index p) !q;
  tab.(Adef.int_of_iper (get_key_index p)) <- Sosa.one;
  while not (Pq.is_empty !q) do
    begin let (ip, nq) = Pq.take !q in
      q := nq;
      let u = uoi base ip in
      let n = tab.(Adef.int_of_iper ip) in
      for i = 0 to Array.length (get_family u) - 1 do
        let des = doi base (get_family u).(i) in
        for j = 0 to Array.length (get_children des) - 1 do
          let ip = (get_children des).(j) in
          tab.(Adef.int_of_iper ip) <- Sosa.add tab.(Adef.int_of_iper ip) n;
          if not entered.(Adef.int_of_iper ip) then
            begin q := Pq.add ip !q; entered.(Adef.int_of_iper ip) <- true end
        done
      done
    end
  done;
  print_result base tab

let bname = ref ""
let p_fname = ref ""
let p_num = ref 0
let p_sname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base> <first_name> <num> <surname>"
let speclist = []

let main () =
  let cnt = ref 0 in
  Argl.parse speclist
    (fun s ->
       begin match !cnt with
         0 -> bname := s
       | 1 -> p_fname := s
       | 2 -> p_num := int_of_string s
       | 3 -> p_sname := s
       | _ -> raise (Arg.Bad "too many parameters")
       end;
       incr cnt)
    usage;
  if !cnt <> 4 then
    begin
      Printf.eprintf "Missing parameter\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2
    end;
  Secure.set_base_dir (Filename.dirname !bname);
  let base = Gwdb.open_base !bname in
  match Gwdb.person_of_key base !p_fname !p_sname !p_num with
    Some ip -> most_desc base (poi base ip)
  | None -> raise Not_found

let _ = Printexc.print main ()
