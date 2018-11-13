open Geneweb
open Def
open Gwdb

let before_date d d1 =
  if d1.year < d.year then true
  else if d1.year > d.year then false
  else if d1.month < d.month then true
  else if d1.month > d.month then false
  else if d1.prec > d.prec then true
  else if d1.prec < d.prec then false
  else if d1.day < d.day then true
  else if d1.day > d.day then false
  else true

let string_of_date d = string_of_int d.year

let apply base date nb_ind f =
  let cnt = ref 0 in
  for i = 0 to nb_ind - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    match Adef.od_of_cdate (get_birth p) with
      Some (Dgreg (b_dmy, _)) ->
        let alive_at_that_date =
          if before_date date b_dmy then
            match get_death p with
              Death (_, cd) ->
                begin match Adef.date_of_cdate cd with
                  Dgreg (d_dmy, _) -> before_date d_dmy date
                | _ -> false
                end
            | NotDead -> true
            | _ -> false
          else false
        in
        if alive_at_that_date then begin f !cnt ip p; incr cnt end
    | Some (Dtext _) | None -> ()
  done;
  !cnt

(**)
let glop = ref 0
(**)

let mark_cnt = ref 0
let number_of_desc base mark ip p =
  incr mark_cnt;
  let curr_mark = !mark_cnt in
  let rec loop nb_list nb new_gen =
    function
      ifam :: ifaml ->
        let (nb, new_gen) =
          if mark.(Adef.int_of_ifam ifam) = curr_mark then nb, new_gen
          else
            begin
              mark.(Adef.int_of_ifam ifam) <- curr_mark;
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
              nb, new_gen
            end
        in
        loop nb_list nb new_gen ifaml
    | [] ->
        match new_gen with
          [] -> List.rev nb_list
        | _ ->
            loop (nb :: nb_list) 0 [] new_gen
  in
  loop [] 0 [] (Array.to_list (get_family p))

let nb_desc bname date =
  let base = Gwdb.open_base bname in
  let () = Gwdb.load_descends_array base in
  let () = Gwdb.load_unions_array base in
  let nb_ind = nb_of_persons base in
  let nb_fam = nb_of_families base in
  let nb_liv = apply base date nb_ind (fun _ _ _ -> ()) in
  Printf.printf "nombre de personnes vivantes en %s : %d\n" (string_of_date date)
    nb_liv;
  flush stdout;
  let mark = Array.make nb_fam 0 in
  let nb_desc = ref [] in
  ProgrBar.start ();
  ignore
    (apply base date nb_ind
       (fun cnt ip p ->
          ProgrBar.run cnt nb_liv;
          let nb_list = number_of_desc base mark ip p in
          nb_desc :=
            let rec loop l1 l2 =
              match l1, l2 with
                x1 :: l1, x2 :: l2 -> x1 + x2 :: loop l1 l2
              | _, [] -> l1
              | [], _ -> l2
            in
            loop !nb_desc nb_list) :
     int);
  ProgrBar.finish ();
  let (nb_gen, nb_tot) =
    List.fold_left
      (fun (nb_gen, nb_tot) nb_at_gen ->
         Printf.printf "nombre moyen de descendants à la génération %2d :" nb_gen;
         Printf.printf " %7.2f\n" (float nb_at_gen /. float nb_liv);
         nb_gen + 1, nb_tot + nb_at_gen)
      (1, 0) !nb_desc
  in
  Printf.printf "nombre de descendants moyen = %.2f\n"
    (float nb_tot /. float nb_liv);
  flush stdout

let date year = {day = 0; month = 0; year = year; prec = Sure; delta = 0}

let main () =
  let bname = Sys.argv.(1) in
  let year = int_of_string Sys.argv.(2) in nb_desc bname (date year)

let _ = main ()
