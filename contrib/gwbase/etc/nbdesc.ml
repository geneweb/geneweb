(* camlp4r *)
(* $Id: nbdesc.ml,v 4.7 2007-10-28 06:57:31 deraugla Exp $ *)

open Def;
open Gwdb;
open Printf;

value designation base ip p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then
    "i=" ^ string_of_int (Adef.int_of_iper ip)
  else
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)
;

value before_date d d1 =
  if d1.year < d.year then True
  else if d1.year > d.year then False
  else if d1.month < d.month then True
  else if d1.month > d.month then False
  else if d1.prec > d.prec then True
  else if d1.prec < d.prec then False
  else if d1.day < d.day then True
  else if d1.day > d.day then False
  else True
;

value string_of_date d = string_of_int d.year;

value apply base date nb_ind f = do {
  let cnt = ref 0 in
  for i = 0 to nb_ind - 1 do {
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    match Adef.od_of_codate (get_birth p) with
    [ Some (Dgreg b_dmy _) ->
        let alive_at_that_date =
          if before_date date b_dmy then
            match get_death p with
            [ Death _ cd ->
                match Adef.date_of_cdate cd with
                [ Dgreg d_dmy _ -> before_date d_dmy date
                | _ -> False ]
            | NotDead -> True
            | _ -> False ]
          else False
        in
        if alive_at_that_date then do {
          f cnt.val ip p;
          incr cnt
        }
        else ()
    | Some (Dtext _) | None -> () ];
  };
  cnt.val
};

(**)
value glop = ref 0;
(**)

value mark_cnt = ref 0;
value number_of_desc base mark ip p = do {
  incr mark_cnt;
  let curr_mark = mark_cnt.val in
  loop [] 0 [] (Array.to_list (get_family p))
  where rec loop nb_list nb new_gen =
    fun
    [ [ifam :: ifaml] -> do {
        let (nb, new_gen) =
          if mark.(Adef.int_of_ifam ifam) = curr_mark then (nb, new_gen)
          else do {
            mark.(Adef.int_of_ifam ifam) := curr_mark;
            let fam = foi base ifam in
            let ipa = get_children fam in
            let nb = nb + Array.length ipa in
            let new_gen =
              Array.fold_left
                (fun ifaml ip ->
                   let p = poi base ip in
                   Array.to_list (get_family p) @ ifaml)
                new_gen ipa
            in
            (nb, new_gen)
          }
        in
        loop nb_list nb new_gen ifaml;
      }
    | [] ->
         match new_gen with
         [ [] -> List.rev nb_list
         | _ ->
(*
let _ = do { if List.length nb_list > glop.val then do { glop.val := List.length nb_list; printf "\n%s gen %d" (designation base ip p) glop.val; flush stdout; } else () } in
*)
             loop [nb :: nb_list] 0 [] new_gen ] ]
};

value nb_desc bname date = do {
  let base = Gwdb.open_base bname in
  let () = Gwdb.load_descends_array base in
  let () = Gwdb.load_unions_array base in
  let nb_ind = nb_of_persons base in
  let nb_fam = nb_of_families base in
  let nb_liv = apply base date nb_ind (fun _ _ _ -> ()) in
  printf "nombre de personnes vivantes en %s : %d\n" (string_of_date date)
    nb_liv;
  flush stdout;
  let mark = Array.create nb_fam 0 in
  let nb_desc = ref [] in
  ProgrBar.start ();
  ignore
     (apply base date nb_ind
        (fun cnt ip p -> do {
           ProgrBar.run cnt nb_liv;
           let nb_list = number_of_desc base mark ip p in
           nb_desc.val :=
             loop nb_desc.val nb_list where rec loop l1 l2 =
               match (l1, l2) with
               [ ([x1 :: l1], [x2 :: l2]) -> [x1+x2 :: loop l1 l2]
               | (_, []) -> l1
               | ([], _) -> l2 ];
         }) :
     int);
  ProgrBar.finish ();
  let (nb_gen, nb_tot) =
    List.fold_left
      (fun (nb_gen, nb_tot) nb_at_gen -> do {
         printf "nombre moyen de descendants à la génération %2d :" nb_gen;
         printf " %7.2f\n" (float nb_at_gen /. float nb_liv);
         (nb_gen + 1, nb_tot + nb_at_gen)
       })
      (1, 0) nb_desc.val
  in
  printf "nombre de descendants moyen = %.2f\n"
    (float nb_tot /. float nb_liv);
  flush stdout;
};

value date year = {day = 0; month = 0; year = year; prec = Sure; delta = 0};

value main () =
  let bname = Sys.argv.(1) in
  let year = int_of_string Sys.argv.(2) in
  nb_desc bname (date year)
;

main ();
