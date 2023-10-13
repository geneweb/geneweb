open Def
open Gwdb

type stats = {
  mutable men : int;
  mutable women : int;
  mutable neutre : int;
  mutable noname : int;
  mutable oldest_father : int * person;
  mutable oldest_mother : int * person;
  mutable youngest_father : int * person;
  mutable youngest_mother : int * person;
  mutable oldest_dead : int * person;
  mutable oldest_still_alive : int * person;
}

let birth_year p =
  match Date.cdate_to_dmy_opt (get_birth p) with
  | Some { year; prec = Sure } -> Some year
  | Some _ | None -> None

let death_year current_year p =
  match get_death p with
  | Death (_, d) -> (
      match Date.cdate_to_dmy_opt d with
      | Some { year = y; prec = Sure } -> Some y
      | _ -> None)
  | NotDead -> Some current_year
  | _ -> None

let update_stats base current_year s p =
  (match get_sex p with
  | Male -> s.men <- s.men + 1
  | Female -> s.women <- s.women + 1
  | Neuter -> s.neutre <- s.neutre + 1);
  if is_quest_string (get_first_name p) && is_quest_string (get_surname p) then
    s.noname <- s.noname + 1;
  (match (birth_year p, death_year current_year p) with
  | Some y1, Some y2 ->
      let age = y2 - y1 in
      if age > fst s.oldest_dead && get_death p <> NotDead then
        s.oldest_dead <- (age, p);
      if age > fst s.oldest_still_alive && get_death p = NotDead then
        s.oldest_still_alive <- (age, p)
  | _ -> ());
  match (birth_year p, get_parents p) with
  | Some y2, Some ifam -> (
      let cpl = foi base ifam in
      (match birth_year (poi base (get_father cpl)) with
      | Some y1 ->
          let age = y2 - y1 in
          if age > fst s.oldest_father then
            s.oldest_father <- (age, poi base (get_father cpl));
          if age < fst s.youngest_father then
            s.youngest_father <- (age, poi base (get_father cpl))
      | _ -> ());
      match birth_year (poi base (get_mother cpl)) with
      | Some y1 ->
          let age = y2 - y1 in
          if age > fst s.oldest_mother then
            s.oldest_mother <- (age, poi base (get_mother cpl));
          if age < fst s.youngest_mother then
            s.youngest_mother <- (age, poi base (get_mother cpl))
      | _ -> ())
  | _ -> ()

let stat_base : base -> stats =
 fun base ->
  let s =
    let y = (1000, poi base Gwdb.dummy_iper) in
    let o = (0, poi base Gwdb.dummy_iper) in
    {
      men = 0;
      women = 0;
      neutre = 0;
      noname = 0;
      oldest_father = o;
      oldest_mother = o;
      youngest_father = y;
      youngest_mother = y;
      oldest_dead = o;
      oldest_still_alive = o;
    }
  in
  let current_year = (Unix.localtime (Unix.time ())).Unix.tm_year + 1900 in
  Gwdb.Collection.iter
    (fun p ->
      update_stats base current_year s p;
      flush stdout)
    (Gwdb.persons base);
  s

let print_stats : base -> stats -> unit =
 fun base s ->
  Printf.printf "\n";
  Printf.printf "%d men\n" s.men;
  Printf.printf "%d women\n" s.women;
  Printf.printf "%d unknown sex\n" s.neutre;
  Printf.printf "%d unnamed\n" s.noname;
  Printf.printf "Oldest: %s, %d\n"
    (Gutil.designation base (snd s.oldest_dead))
    (fst s.oldest_dead);
  Printf.printf "Oldest still alive: %s, %d\n"
    (Gutil.designation base (snd s.oldest_still_alive))
    (fst s.oldest_still_alive);
  Printf.printf "Youngest father: %s, %d\n"
    (Gutil.designation base (snd s.youngest_father))
    (fst s.youngest_father);
  Printf.printf "Youngest mother: %s, %d\n"
    (Gutil.designation base (snd s.youngest_mother))
    (fst s.youngest_mother);
  Printf.printf "Oldest father: %s, %d\n"
    (Gutil.designation base (snd s.oldest_father))
    (fst s.oldest_father);
  Printf.printf "Oldest mother: %s, %d\n"
    (Gutil.designation base (snd s.oldest_mother))
    (fst s.oldest_mother);
  Printf.printf "\n";
  flush stdout
