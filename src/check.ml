(* $Id: check.ml,v 4.12 2004-08-05 19:41:04 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Gutil;
open Printf;

type gen_min_person 'person 'string =
  { m_first_name : mutable 'string;
    m_surname : mutable 'string;
    m_occ : mutable int;
    m_rparents : mutable list (gen_relation 'person 'string);
    m_related : mutable list iper;
    m_sex : mutable sex;
    m_notes : mutable 'string }
;

type min_person = gen_min_person iper istr;

type cbase =
  { c_persons : mutable array min_person;
    c_ascends : mutable array ascend;
    c_unions : mutable array union;
    c_couples : mutable array couple;
    c_descends : mutable array descend;
    c_strings : mutable array string;
    c_bnotes : notes }
;

type gen =
  { g_strings : mutable Hashtbl.t string istr;
    g_names : mutable Hashtbl.t int iper;
    g_local_names : mutable Hashtbl.t (int * int) iper;
    g_pcnt : mutable int;
    g_fcnt : mutable int;
    g_scnt : mutable int;
    g_base : cbase;
    g_def : mutable array bool;
    g_separate : mutable bool;
    g_shift : mutable int;
    g_errored : mutable bool;
    g_per_index : out_channel;
    g_per : out_channel;
    g_fam_index : out_channel;
    g_fam : out_channel }
;

value error gen = gen.g_errored := True;

value feminin =
  fun
  [ Male -> ""
  | Female -> "e"
  | Neuter -> "(e)" ]
;

value print_base_error base =
  fun
  [ AlreadyDefined p ->
      printf "%s\nis defined several times\n" (designation base p)
  | OwnAncestor p ->
      printf "%s\nis his/her own ancestor\n" (designation base p)
  | BadSexOfMarriedPerson p ->
      printf "%s\n  bad sex\n" (designation base p) ]
;

value print_base_warning base =
  fun
  [ BirthAfterDeath p ->
      printf "%s\n  born after his/her death\n" (designation base p)
  | IncoherentSex p fixed not_fixed ->
      do {
        printf "%s\n  sex not coherent with relations"
          (designation base p);
        if fixed > 0 then
          if not_fixed > 0 then
            printf " (fixed in %d of the %d cases)" fixed (fixed + not_fixed)
          else
            printf " (fixed)"
        else ();
        printf "\n";
      }
  | ChangedOrderOfChildren ifam des _ ->
      let cpl = coi base ifam in
      printf "changed order of children of %s and %s\n"
        (designation base (poi base (father cpl)))
        (designation base (poi base (mother cpl)))
  | ChildrenNotInOrder ifam des elder x ->
      let cpl = coi base ifam in
      do {
        printf
          "the following children of\n  %s\nand\n  %s\nare not in order:\n"
          (designation base (poi base (father cpl)))
          (designation base (poi base (mother cpl)));
        printf "- %s\n" (designation base elder);
        printf "- %s\n" (designation base x);
      }
  | DeadTooEarlyToBeFather father child ->
      do {
        printf "%s\n" (designation base child);
        printf
          "  is born more than 2 years after the death of his/her father\n";
        printf "%s\n" (designation base father);
      }
  | MarriageDateAfterDeath p ->
      do {
        printf "%s\n" (designation base p);
        printf "married after his/her death\n";
      }
  | MarriageDateBeforeBirth p ->
      do {
        printf "%s\n" (designation base p);
        printf "married before his/her birth\n";
      }
  | MotherDeadAfterChildBirth mother child ->
      printf "%s\n  is born after the death of his/her mother\n%s\n"
        (designation base child) (designation base mother)
  | ParentBornAfterChild parent child ->
      printf "%s born after his/her child %s\n"
        (designation base parent) (designation base child)
  | ParentTooYoung p a ->
      printf "%s was parent at age of %d\n" (designation base p)
        (year_of a)
  | TitleDatesError p t ->
      do {
        printf "%s\n" (designation base p);
        printf "has incorrect title dates as:\n";
        printf "  %s %s\n" (sou base t.t_ident) (sou base t.t_place);
      }
  | UndefinedSex p ->
      ()
  | YoungForMarriage p a ->
      printf "%s married at age %d\n" (designation base p) (year_of a) ]
;      

value set_error base gen x =
  do { printf "\nError: "; print_base_error base x; error gen; }
;

value set_warning base =
  fun
  [ UndefinedSex _ -> ()
  | x -> do { printf "\nWarning: "; print_base_warning base x; } ]
;

type stats =
  { men : mutable int;
    women : mutable int;
    neutre : mutable int;
    noname : mutable int;
    oldest_father : mutable (int * person);
    oldest_mother : mutable (int * person);
    youngest_father : mutable (int * person);
    youngest_mother : mutable (int * person);
    oldest_dead : mutable (int * person);
    oldest_still_alive : mutable (int * person) }
;

value birth_year p =
  match Adef.od_of_codate p.birth with
  [ Some d ->
      match d with
      [ Dgreg {year = y; prec = Sure} _ -> Some y
      | _ -> None ]
  | _ -> None ]
;

value death_year current_year p =
  match p.death with
  [ Death _ d ->
      match Adef.date_of_cdate d with
      [ Dgreg {year = y; prec = Sure} _ -> Some y
      | _ -> None ]
  | NotDead -> Some current_year
  | _ -> None ]
;

value update_stats base current_year s p =
  do {
    match p.sex with
    [ Male -> s.men := s.men + 1
    | Female -> s.women := s.women + 1
    | Neuter -> s.neutre := s.neutre + 1 ];
    if p_first_name base p = "?" && p_surname base p = "?" then
      s.noname := s.noname + 1
    else ();
    match (birth_year p, death_year current_year p) with
    [ (Some y1, Some y2) ->
        let age = y2 - y1 in
        do {
          if age > fst s.oldest_dead && p.death <> NotDead then
            s.oldest_dead := (age, p)
          else ();
          if age > fst s.oldest_still_alive && p.death = NotDead then
            s.oldest_still_alive := (age, p)
          else ();
        }
    | _ -> () ];
    match (birth_year p, parents (aoi base p.cle_index)) with
    [ (Some y2, Some ifam) ->
        let cpl = coi base ifam in
        do {
          match birth_year (poi base (father cpl)) with
          [ Some y1 ->
              let age = y2 - y1 in
              do {
                if age > fst s.oldest_father then
                  s.oldest_father := (age, poi base (father cpl))
                else ();
                if age < fst s.youngest_father then
                  s.youngest_father := (age, poi base (father cpl))
                else ();
              }
          | _ -> () ];
          match birth_year (poi base (mother cpl)) with
          [ Some y1 ->
              let age = y2 - y1 in
              do {
                if age > fst s.oldest_mother then
                  s.oldest_mother := (age, poi base (mother cpl))
                else ();
                if age < fst s.youngest_mother then
                  s.youngest_mother := (age, poi base (mother cpl))
                else ();
              }
          | _ -> () ];
          ()
        }
    | _ -> () ];
  }
;

(* the parameter "gen" should disapear while changing along the functions
   called by check_base which should use now "base" instead of "gen" *)

value check_base base gen pr_stats =
  let s =
    let y = (1000, base.data.persons.get 0) in
    let o = (0, base.data.persons.get 0) in
    {men = 0; women = 0; neutre = 0; noname = 0; oldest_father = o;
     oldest_mother = o; youngest_father = y; youngest_mother = y;
     oldest_dead = o; oldest_still_alive = o}
  in
  let current_year = (Unix.localtime (Unix.time ())).Unix.tm_year + 1900 in
  do {
    Gutil.check_base base (set_error base gen) (set_warning base);
    for i = 0 to base.data.persons.len - 1 do {
      let p = base.data.persons.get i in
      if not gen.g_def.(i) then
        printf "Undefined: %s\n" (designation base p)
      else ();
      if pr_stats then update_stats base current_year s p else ();
      flush stdout;
    };
    if pr_stats then do {
      printf "\n";
      printf "%d men\n" s.men;
      printf "%d women\n" s.women;
      printf "%d unknown sex\n" s.neutre;
      printf "%d unnamed\n" s.noname;
      printf "Oldest: %s, %d\n" (designation base (snd s.oldest_dead))
        (fst s.oldest_dead);
      printf "Oldest still alive: %s, %d\n"
        (designation base (snd s.oldest_still_alive))
        (fst s.oldest_still_alive);
      printf "Youngest father: %s, %d\n"
        (designation base (snd s.youngest_father)) (fst s.youngest_father);
      printf "Youngest mother: %s, %d\n"
        (designation base (snd s.youngest_mother)) (fst s.youngest_mother);
      printf "Oldest father: %s, %d\n"
        (designation base (snd s.oldest_father)) (fst s.oldest_father);
      printf "Oldest mother: %s, %d\n"
        (designation base (snd s.oldest_mother)) (fst s.oldest_mother);
      printf "\n";
      flush stdout;
    }
    else ();
  }
;
