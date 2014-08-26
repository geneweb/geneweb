(* camlp5r *)
(* $Id: gw_most_asc.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Def;
open Gwdb;
open Printf;


value trace = ref False;

value ht = Hashtbl.create 7001;

value rec compute_nb_asc ifam =
  let fam = foi base ifam in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let father_asc =
    try Hashtbl.find ht ifath with
    [ Not_found ->
        let fahter = poi base ifath in
        let nb_asc =
          match get_parents father with
          [ None -> 0
          | Some ifam -> compute_nb_asc ifam ]
        in
        let () = Hashtbl.add ht ifath nb_asc in
        nb_asc ]
  in
  let mother_asc =
    try Hashtbl.find ht imoth with
    [ Not_found ->
        let mother = poi base imoth in
        let nb_asc =
          match get_parents mother with
          [ None -> 0
          | Some ifam -> compute_nb_asc ifam ]
        in
        let () = Hashtbl.add ht imoth nb_asc in
        nb_asc ]
  in
  father_asc + mother_asc
;

value compute_most_asc base = do {
  (* on récupère tous les enfants *)
  let l = ref [] in
  for i = 0 to nb_base_persons - 1 do {
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let has_children =
      loop (Array.to_list (get_family p)) where rec loop faml =
        match faml with
        [ [] -> False
        | [ifam :: faml] ->
            let fam = foi base ifam in
            if get_children fam <> [| |] then True
            else loop faml ]
    in
    if has_children then ()
    else l.val := [ ip :: l.val ]
  }
  List.iter
    (fun ip ->
      let p = poi base ip in
      match get_parents p with
      [ Some ifam -> compute_nb_asc ifam
      | None -> 0 ])
    l.val
};


(**/**)


value bname = ref "";
value fname = ref "";

value speclist = [];
value anonfun i = bname.val := i;
value usage = "Usage: " ^ Sys.argv.(0) ^ " base";

value main () = do {
  Arg.parse speclist anonfun usage;
  if bname.val = "" || fname.val = "" then
    do { Arg.usage speclist usage; exit 2; }
  else ();
  let base = Gwdb.open_base bname.val in
  compute_most_asc base
};

main ();
