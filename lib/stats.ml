open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

type stats = {
  mutable men : int;
  mutable women : int;
  mutable neutre : int;
  mutable noname : int;
  mutable oldest_father : int * Driver.person;
  mutable oldest_mother : int * Driver.person;
  mutable youngest_father : int * Driver.person;
  mutable youngest_mother : int * Driver.person;
  mutable oldest_dead : int * Driver.person;
  mutable oldest_still_alive : int * Driver.person;
}

let birth_year p =
  match Date.cdate_to_dmy_opt (Driver.get_birth p) with
  | Some { year; prec = Sure; _ } -> Some year
  | Some _ | None -> None

let death_year current_year p =
  match Driver.get_death p with
  | Death (_, d) -> (
      match Date.cdate_to_dmy_opt d with
      | Some { year = y; prec = Sure; _ } -> Some y
      | _ -> None)
  | NotDead -> Some current_year
  | _ -> None

let update_stats base current_year s p =
  (match Driver.get_sex p with
  | Male -> s.men <- s.men + 1
  | Female -> s.women <- s.women + 1
  | Neuter -> s.neutre <- s.neutre + 1);
  if
    Driver.Istr.is_quest (Driver.get_first_name p)
    && Driver.Istr.is_quest (Driver.get_surname p)
  then s.noname <- s.noname + 1;
  (match (birth_year p, death_year current_year p) with
  | Some y1, Some y2 ->
      let age = y2 - y1 in
      if age > fst s.oldest_dead && Driver.get_death p <> NotDead then
        s.oldest_dead <- (age, p);
      if age > fst s.oldest_still_alive && Driver.get_death p = NotDead then
        s.oldest_still_alive <- (age, p)
  | _ -> ());
  match (birth_year p, Driver.get_parents p) with
  | Some y2, Some ifam -> (
      let cpl = Driver.foi base ifam in
      (match birth_year (Driver.poi base (Driver.get_father cpl)) with
      | Some y1 ->
          let age = y2 - y1 in
          if age > fst s.oldest_father then
            s.oldest_father <- (age, Driver.poi base (Driver.get_father cpl));
          if age < fst s.youngest_father then
            s.youngest_father <- (age, Driver.poi base (Driver.get_father cpl))
      | _ -> ());
      match birth_year (Driver.poi base (Driver.get_mother cpl)) with
      | Some y1 ->
          let age = y2 - y1 in
          if age > fst s.oldest_mother then
            s.oldest_mother <- (age, Driver.poi base (Driver.get_mother cpl));
          if age < fst s.youngest_mother then
            s.youngest_mother <- (age, Driver.poi base (Driver.get_mother cpl))
      | _ -> ())
  | _ -> ()

let stat_base : Geneweb_db.Driver.base -> stats =
 fun base ->
  let s =
    let y = (1000, Driver.poi base Driver.Iper.dummy) in
    let o = (0, Driver.poi base Driver.Iper.dummy) in
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
  Collection.iter
    (fun p ->
      update_stats base current_year s p;
      flush stdout)
    (Geneweb_db.Driver.persons base);
  s

let print_stats : Geneweb_db.Driver.base -> stats -> unit =
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
