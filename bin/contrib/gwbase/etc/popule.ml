(* $Id: popule.ml,v 4.31 2007-02-16 10:35:39 deraugla Exp $ *)

open Def
open Gwdb

let _ = Mutil.verbose := false

let h_first_names =
  [| "Albert"; "Bernard"; "Cyrille"; "Daniel"; "Éric"; "François"; "Gérard";
     "Hervé"; "Isidore"; "Jacques"; "Kevin"; "Louis"; "Michel"; "Nicolas";
     "Octave"; "Philippe"; "Quentin"; "René"; "Sylvain"; "Thierry"; "Urbain";
     "Vincent"; "Wolfgang"; "Xavier"; "Yann"; "Zébulon" |]
let f_first_names =
  [| "Anne"; "Brigitte"; "Cécile"; "Denise"; "Emmanuelle"; "Fanny";
     "Geneviève"; "Hélène"; "Isabelle"; "Joëlle"; "Karine"; "Lise"; "Marie";
     "Noëlle"; "Odile"; "Patricia"; "Quitterie"; "Rosine"; "Sidonie";
     "Thérèse"; "Ursule"; "Vanessa"; "Wilfried"; "Xavière"; "Yvonne"; "Zoé" |]

let char1 =
  let a =
    [| ""; "b"; "c"; "ch"; "d"; "f"; "g"; "gr"; "h"; "j"; "l"; "m"; "p"; "r";
       "s"; "t"; "v" |]
  in
  fun () -> a.(Random.int (Array.length a))

let char2 =
  let a =
    [| "a"; "ail"; "al"; "ar"; "as"; "au"; "e"; "é"; "el"; "er"; "i"; "o";
       "ou"; "u" |]
  in
  fun () -> a.(Random.int (Array.length a))

let char3 =
  let a =
    [| "b"; "c"; "ch"; "d"; "f"; "g"; "gl"; "l"; "m"; "n"; "p"; "r"; "s"; "t";
       "th"; "v"; "z" |]
  in
  fun () -> a.(Random.int (Array.length a))

let char4 =
  let a =
    [| "a"; "ai"; "ail"; "ar"; "au"; "e"; "eil"; "er"; "i"; "il"; "o"; "u" |]
  in
  fun () -> a.(Random.int (Array.length a))

let char5 =
  let a =
    [| "b"; "ch"; "d"; "g"; "gn"; "gu"; "l"; "n"; "p"; "r"; "t"; "th"; "tr";
       "v"; "vr" |]
  in
  fun () -> a.(Random.int (Array.length a))

let char6 =
  let a =
    [| "al"; "an"; "at"; "ay"; "e"; "é"; "eau"; "ert"; "es"; "et"; "ie";
       "ier"; "in"; "our"; "y" |]
  in
  fun () -> a.(Random.int (Array.length a))

let nameize _ =
  let two = Random.int 4 <> 0 in
  let len = 0 in
  let len =
    if two then len
    else
      let len = Buff.mstore len (char1 ()) in
      let len = Buff.mstore len (char2 ()) in len
  in
  let len = Buff.mstore len (char3 ()) in
  let len =
    let c4 = char4 () in
    let c5 = char5 () in
    let len = Buff.mstore len c4 in
    match c4.[String.length c4 - 1] with
      'l' | 'r' when String.length c5 > 1 ->
        let rec loop c5 =
          match c5.[String.length c5 - 1] with
            'l' | 'r' -> loop (char5 ())
          | _ -> Buff.mstore len c5
        in
        loop c5
    | _ -> Buff.mstore len c5
  in
  let len = Buff.mstore len (char6 ()) in
  String.uppercase_ascii (Buff.get len)

let h_fn n =
  let n1 = h_first_names.(Random.int (Array.length h_first_names)) in
  let n2 = h_first_names.(Random.int (Array.length h_first_names)) in
  if n1 = n2 then n1 else n1 ^ "-" ^ n2
let f_fn n =
  let n1 = f_first_names.(Random.int (Array.length f_first_names)) in
  let n2 = f_first_names.(Random.int (Array.length f_first_names)) in
  if n1 = n2 then n1 else n1 ^ "-" ^ n2

let add_indi (base, cnt, bname) (fn, sn1, sn2, oc) sex =
  if !cnt >= 300 then
    begin
      cnt := 0;
      commit_patches !base;
      Gwdb.apply_base1 !base
        (fun base ->
           let bname = base.Dbdisk.data.Dbdisk.bdir in
           Outbase.output bname base);
      base := Gwdb.open_base bname
    end;
  incr cnt;
  let sn = if sn2 = "" then sn1 else sn1 ^ " " ^ sn2 in
  GwBaseLib.add_indi !base (fn, sn, oc) sex

let mkcelib size =
  let rec loop list n =
    if n = 0 then list
    else
      let i = Random.int size in
      if List.mem i list then loop list n else loop (i :: list) (n - 1)
  in
  loop [] (max 1 (size / 3))

let popule bname size ngen gyear =
  let h = Array.make size (Adef.iper_of_int 0, "", "") in
  let f = Array.make size (Adef.iper_of_int 0, "", "") in
  let base = ref (Gwdb.open_base bname) in
  let base_info = base, ref 0, bname in
  let d =
    let d = Unix.localtime (Unix.time ()) in
    let day = d.Unix.tm_mday in
    let month = d.Unix.tm_mon + 1 in
    let year = d.Unix.tm_year + 1900 - 1 in
    {day = day; month = month; year = year - gyear * (ngen - 1) - 2;
     prec = Sure; delta = 0}
  in
  Random.self_init ();
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  let jd = Calendar.sdn_of_gregorian d in
  let (persons_get, persons_set) = persons_array !base in
  for i = 0 to size - 1 do
    let surn = nameize i in
    let ip = add_indi base_info (h_fn 0, surn, "", 1) Male in
    h.(i) <- ip, surn, "";
    let x = persons_get (Adef.int_of_iper ip) in
    let d = Calendar.gregorian_of_sdn Sure (jd + Random.int 365) in
    let x =
      {x with birth = Adef.cdate_of_od (Some (Dgreg (d, Dgregorian)))}
    in
    persons_set (Adef.int_of_iper ip) x;
    let surn = nameize i in
    let ip = add_indi base_info (f_fn 0, surn, "", 1) Female in
    f.(i) <- ip, surn, "";
    let x = persons_get (Adef.int_of_iper ip) in
    let d = Calendar.gregorian_of_sdn Sure (jd + Random.int 365) in
    let x =
      {x with birth = Adef.cdate_of_od (Some (Dgreg (d, Dgregorian)))}
    in
    persons_set (Adef.int_of_iper ip) x
  done;
  begin let rec loop celib d n =
    if n > ngen then ()
    else
      let jd = Calendar.sdn_of_gregorian d in
      Printf.eprintf "%d." n;
      flush stderr;
      for i = 0 to size - 1 do
        if List.mem i celib then ()
        else
          let (ifath, hsn1, hsn2) = h.(i) in
          let (imoth, fsn1, fsn2) = f.(i) in
          let (sn1, sn2) =
            match Random.int 15 with
              0 -> nameize (), ""
            | 1 | 2 | 3 | 4 -> fsn1, ""
            | _ -> hsn1, ""
          in
          let h_before_f = Random.int 2 = 0 in
          let list =
            if false then []
            else
              let ip = add_indi base_info (h_fn n, sn1, sn2, n) Male in
              h.(i) <- ip, sn1, sn2;
              let x = persons_get (Adef.int_of_iper ip) in
              let sh_h = if h_before_f then 0 else 2 * 365 in
              let d =
                Calendar.gregorian_of_sdn Sure (jd + sh_h + Random.int 365)
              in
              let x =
                {x with birth =
                  Adef.cdate_of_od (Some (Dgreg (d, Dgregorian)))}
              in
              persons_set (Adef.int_of_iper ip) x; [ip]
          in
          let list =
            if false then list
            else
              let ip = add_indi base_info (f_fn n, sn1, sn2, n) Female in
              f.(i) <- ip, sn1, sn2;
              let x = persons_get (Adef.int_of_iper ip) in
              let sh_f = if h_before_f then 2 * 365 else 0 in
              let d =
                Calendar.gregorian_of_sdn Sure (jd + sh_f + Random.int 365)
              in
              let x =
                {x with birth =
                  Adef.cdate_of_od (Some (Dgreg (d, Dgregorian)))}
              in
              persons_set (Adef.int_of_iper ip) x;
              if h_before_f then list @ [ip] else ip :: list
          in
          let _ = (GwBaseLib.add_fam !base ifath imoth list : ifam) in ()
      done;
      for i = 0 to size - 2 do
        let rf = i + Random.int (size - i - 1) + 1 in
        let tmp = f.(i) in f.(i) <- f.(rf); f.(rf) <- tmp
      done;
      loop (mkcelib size) {d with year = d.year + gyear} (n + 1)
  in
    loop (mkcelib size) {d with year = d.year + gyear} 2
  end;
  Printf.eprintf "\n";
  commit_patches !base;
  Gwdb.apply_base1 !base
    (fun base ->
       let bname = base.Dbdisk.data.Dbdisk.bdir in Outbase.output bname base)

let size = ref 100
let ngen = ref 200
let gyear = ref 20
let bname = ref ""

let speclist =
  ["-s", Arg.Int (fun i -> size := i),
   "size = nb of men = nb of women (" ^ string_of_int !size ^ ")";
   "-n", Arg.Int (fun i -> ngen := i),
   "nb of generations (" ^ string_of_int !ngen ^ ")";
   "-g", Arg.Int (fun i -> gyear := i),
   "age at marriage (" ^ string_of_int !gyear ^ ")"]
let anonfun i = bname := i
let usage = "Usage: popule base [args]"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  popule !bname !size !ngen !gyear

let _ = main ()
