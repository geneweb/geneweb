(* $Id: check.ml,v 1.6 1998-12-16 17:36:24 ddr Exp $ *)

open Def;
open Gutil;

type gen =
  { g_strings : mutable Mhashtbl.t string istr;
    g_names : mutable Mhashtbl.t int iper;
    g_pcnt : mutable int;
    g_fcnt : mutable int;
    g_scnt : mutable int;
    g_base : base;
    g_def : mutable array bool;
    g_shift : mutable int;
    g_errored : mutable bool }
;

value error gen = gen.g_errored := True;

value feminin =
  fun
  [ Masculine -> ""
  | Feminine -> "e"
  | Neuter -> "(e)" ]
;

value glop x =
  let y = String.create (String.length x) in
  do for i = 0 to String.length x - 1 do
       y.[i] := if x.[i] == ' ' then '_' else x.[i];
     done;
  return y
;

value print_base_error base =
  fun
  [ AlreadyDefined p ->
      Printf.printf "%s\nis defined several times\n" (denomination base p)
  | OwnAncestor p ->
      Printf.printf "%s\nis his/her own ancestor\n" (denomination base p)
  | BadSexOfMarriedPerson p ->
      Printf.printf "%s\n  bad sex\n" (denomination base p) ]
;

value print_base_warning base =
  fun
  [ BirthAfterDeath p ->
      Printf.printf "%s\n  born after his/her death\n" (denomination base p)
  | ChangedOrderOfChildren fam _ ->
      let cpl = coi base fam.fam_index in
      Printf.printf "changed order of children of %s and %s\n"
        (denomination base (poi base cpl.father))
        (denomination base (poi base cpl.mother))
  | ChildrenNotInOrder fam elder x ->
      let cpl = coi base fam.fam_index in
      do Printf.printf
           "the following children of\n  %s\nand\n  %s\nare not in order:\n"
           (denomination base (poi base cpl.father))
           (denomination base (poi base cpl.mother));
         Printf.printf "- %s\n" (denomination base elder);
         Printf.printf "- %s\n" (denomination base x);
      return ()
  | DeadTooEarlyToBeFather father child ->
      do Printf.printf "%s\n" (denomination base child);
         Printf.printf
           "  is born more than 2 years after the death of his/her father\n";
         Printf.printf "%s\n" (denomination base father);
      return ()
  | MarriageDateAfterDeath p ->
      do Printf.printf "%s\n" (denomination base p);
         Printf.printf "married after his/her death\n";
      return ()
  | MarriageDateBeforeBirth p ->
      do Printf.printf "%s\n" (denomination base p);
         Printf.printf "married before his/her birth\n";
      return ()
  | MotherDeadAfterChildBirth mother child ->
      Printf.printf "%s\n  is born after the death of his/her mother\n%s\n"
        (denomination base child) (denomination base mother)
  | ParentBornAfterChild parent child ->
      Printf.printf "%s born after his/her child %s\n"
        (denomination base parent) (denomination base child)
  | ParentTooYoung p a ->
      Printf.printf "%s was parent at age of %d\n" (denomination base p)
        (annee a)
  | TitleDatesError p t ->
      do Printf.printf "%s\n" (denomination base p);
         Printf.printf "has incorrect title dates as:\n";
         Printf.printf "  %s %s\n" (sou base t.t_title) (sou base t.t_place);
      return ()
  | YoungForMarriage p a ->
      Printf.printf "%s married at age %d\n" (denomination base p) (annee a) ]
;      

value set_error base gen x =
  do Printf.printf "\nError: ";
     print_base_error base x;
     error gen;
  return ()
;

value set_warning base x =
  do Printf.printf "\nWarning: ";
     print_base_warning base x;
  return ()
;

type stats =
  {men : mutable int;
   women : mutable int;
   neutre : mutable int;
   noname : mutable int;
   oldest_father : mutable (int * base_person);
   oldest_mother : mutable (int * base_person);
   youngest_father : mutable (int * base_person);
   youngest_mother : mutable (int * base_person);
   oldest_dead : mutable (int * base_person);
   oldest_still_alive : mutable (int * base_person)}
;

value birth_year p =
  match Adef.od_of_codate p.birth with
  [ Some d ->
      match d with
      [ {year = y; prec = Sure} -> Some y
      | _ -> None ]
  | _ -> None ]
;

value death_year current_year p =
  match p.death with
  [ Death _ d ->
      match Adef.date_of_cdate d with
      [ {year = y; prec = Sure} -> Some y
      | _ -> None ]
  | NotDead -> Some current_year
  | _ -> None ]
;

value update_stats base current_year s p =
  do match p.sex with
     [ Masculine -> s.men := s.men + 1
     | Feminine -> s.women := s.women + 1
     | Neuter -> s.neutre := s.neutre + 1 ];
     if sou base p.first_name = "?" && sou base p.surname = "?" then
       s.noname := s.noname + 1
     else ();
     match (birth_year p, death_year current_year p) with
     [ (Some y1, Some y2) ->
         let age = y2 - y1 in
         do if age > fst s.oldest_dead && p.death <> NotDead then
              s.oldest_dead := (age, p)
            else ();
            if age > fst s.oldest_still_alive && p.death = NotDead then
              s.oldest_still_alive := (age, p)
            else ();
         return ()
     | _ -> () ];
     match (birth_year p, (aoi base p.cle_index).parents) with
     [ (Some y2, Some ifam) ->
         let cpl = coi base ifam in
         do match birth_year (poi base cpl.father) with
            [ Some y1 ->
                let age = y2 - y1 in
                do if age > fst s.oldest_father then
                     s.oldest_father := (age, poi base cpl.father)
                   else ();
                   if age < fst s.youngest_father then
                     s.youngest_father := (age, poi base cpl.father)
                   else ();
                return ()
            | _ -> () ];
            match birth_year (poi base cpl.mother) with
            [ Some y1 ->
                let age = y2 - y1 in
                do if age > fst s.oldest_mother then
                     s.oldest_mother := (age, poi base cpl.mother)
                   else ();
                   if age < fst s.youngest_father then
                     s.youngest_mother := (age, poi base cpl.mother)
                   else ();
                return ()
            | _ -> () ];
         return ()
     | _ -> () ];
  return ()
;

(* the parameter "gen" should disapear while changing along the functions
   called by check_base which should use now "base" instead of "gen" *)

value check_base base gen pr_stats =
  let s =
    let y = (1000, base.data.persons.get 0) in
    let o = (0, base.data.persons.get 0) in
    {men = 0; women = 0; neutre = 0; noname = 0;
     oldest_father = o; oldest_mother = o; youngest_father = y;
     youngest_mother = y; oldest_dead = o; oldest_still_alive = o}
  in
  let current_year = (Unix.localtime (Unix.time ())).Unix.tm_year + 1900 in
  do Gutil.check_base base (set_error base gen) (set_warning base);
     for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       do if not gen.g_def.(i) then
            Printf.printf "Undefined: %s%s %s\n"
              (sou base p.first_name)
              (if p.occ == 0 then "" else "." ^ string_of_int p.occ)
              (glop (sou base p.surname))
          else ();
          if pr_stats then update_stats base current_year s p else ();
       return ();
       flush stdout;
     done;
     if pr_stats then
       do Printf.printf "\n";
          Printf.printf "%d men\n" s.men;
          Printf.printf "%d women\n" s.women;
          Printf.printf "%d unknown sex\n" s.neutre;
          Printf.printf "%d unnamed\n" s.noname;
          Printf.printf "Oldest: %s, %d\n"
            (denomination base (snd s.oldest_dead)) (fst s.oldest_dead);
          Printf.printf "Oldest still alive: %s, %d\n"
            (denomination base (snd s.oldest_still_alive))
            (fst s.oldest_still_alive);
          Printf.printf "Youngest father: %s, %d\n"
            (denomination base (snd s.youngest_father))
            (fst s.youngest_father);
          Printf.printf "Youngest mother: %s, %d\n"
            (denomination base (snd s.youngest_mother))
            (fst s.youngest_mother);
          Printf.printf "Oldest father: %s, %d\n"
            (denomination base (snd s.oldest_father))
            (fst s.oldest_father);
          Printf.printf "Oldest mother: %s, %d\n"
            (denomination base (snd s.oldest_mother))
            (fst s.oldest_mother);
          Printf.printf "\n";
          flush stdout;
       return ()
     else ();
  return ()
;
