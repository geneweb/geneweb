(* $Id: alln.ml,v 1.3 1998-11-21 10:54:08 ddr Exp $ *)

open Def;
open Config;
open Util;
open Gutil;

value print_menu mode conf base is_fam liste len par_frequence =
  let titre _ =
    do if is_fam then
         Wserver.wprint (fcapitale (ftransl conf "the %d surnames")) len
       else
         Wserver.wprint (fcapitale (ftransl conf "the %d first names")) len;
       Wserver.wprint " (%d %s)" base.persons.len
         (transl_nth conf "person/persons" 1);
    return ()
  in
  do header conf titre;
     let _ =
       List.fold_left
         (fun last (x, c, _) ->
            let i = x.[initiale x] in
            let same_than_last =
              match last with
              [ Some (i1, c1) -> if par_frequence then c = c1 else i = i1
              | _ -> False ]
            in
            do if not same_than_last then
                 let t =
                   if par_frequence then string_of_int c else String.make 1 i
                 in
                 Wserver.wprint "<a href=\"%sm=%s;tri=%s;k=%s\">%s</a>\n"
                   (commd conf) mode (if par_frequence then "F" else "A")
                   t t
               else ();
            return Some (i, c))
         None liste
     in
     ();
     if p_getenv conf.env "k" <> Some "" then
       Wserver.wprint "<p>\n<a href=\"%sm=%s;tri=%s;k=\">%s</a>\n"
         (commd conf) mode (if par_frequence then "F" else "A")
         (capitale (transl conf "the whole list"))
     else ();
     trailer conf;
  return ()
;

value print_all mode conf base is_fam liste len par_frequence =
  let title _ =
    do if is_fam then
         Wserver.wprint (fcapitale (ftransl conf "the %d surnames")) len
       else
         Wserver.wprint (fcapitale (ftransl conf "the %d first names")) len;
       Wserver.wprint " (%d %s)" base.persons.len
         (transl_nth conf "person/persons" 1);
    return ()
  in
  do header conf title;
     print_alphab_list
       (fun (x, c, _) ->
          let x = coa conf x in
          if par_frequence then string_of_int c
          else String.sub x (initiale x) 1)
       (fun (x, c, istr) ->
          let x = coa conf x in
          do Wserver.wprint "<a href=\"%sm=%s;v=%s\">" (commd conf) mode
               (code_varenv (sou base istr));
             Wserver.wprint "%s</a>%s\n"
               (if is_fam then surname_end x ^ surname_begin x else x)
               (if par_frequence then "" else " (" ^ string_of_int c ^ ")");
          return ())
       liste;
     trailer conf;
  return ()
;

value print_elem mode conf base is_fam par_frequence (x, c, istr) =
  do Wserver.wprint "<a href=\"%sm=%s;" (commd conf) mode;
     Wserver.wprint "v=%s" (code_varenv (sou base istr));
     Wserver.wprint "\">";
     if is_fam then
       Wserver.wprint "%s%s" (coa conf (surname_end x))
         (coa conf (surname_begin x))
     else Wserver.wprint "%s" (coa conf x);
     Wserver.wprint "</a>";
     if not par_frequence then Wserver.wprint " (%d)" c else ();
     Wserver.wprint "\n";
  return ()
;

value print_frequence mode conf base is_fam liste len f =
  let liste =
    List.fold_right
      (fun (x, c, ip) liste ->
         if c == f then [(x, c, ip) :: liste] else liste)
      liste []
  in
  let len = List.length liste in
  let title _ =
    let lab =
      if is_fam then transl_nth conf "surname/surnames" 1
      else transl_nth conf "first name/first names" 1
    in
    Wserver.wprint "%s %s %d %s"
      (capitale lab) (transl conf "shared by") f
      (transl_nth conf "person/persons" (if f == 1 then 0 else 1))
  in
  do header conf title;
     print_alphab_list (fun (x, _, _) -> String.sub x (initiale x) 1)
       (print_elem mode conf base is_fam True) liste;
     trailer conf;
  return ()
;

value rec same_initial s1 i1 s2 i2 =
  if i1 >= String.length s1 || i2 >= String.length s2 then True
  else if s1.[i1] == s2.[i2] then same_initial s1 (succ i1) s2 (succ i2)
  else False
;

value print_alphab mode conf base is_fam liste len l =
  let liste =
    List.fold_right
      (fun (x, c, ip) liste ->
         if same_initial l 0 x (initiale x) then [(x, c, ip) :: liste]
         else liste)
      liste []
  in
  let len = List.length liste in
  let title _ =
    let lab =
      if is_fam then transl_nth conf "surname/surnames" 1
      else transl_nth conf "first name/first names" 1
    in
    Wserver.wprint "%s %s %s" (capitale lab) (transl conf "starting with") l
  in
  let crit_len = String.length l + 1 in
  do header conf title;
     print_alphab_list
       (fun (e, _, _) ->
          let i = initiale e in
          String.sub e i (min crit_len (String.length e - i)))
       (print_elem mode conf base is_fam False) liste;
     trailer conf;
  return ()
;

value afficher_tous_x proj mode is_fam conf base =
  let par_frequence =
    match p_getenv conf.env "tri" with
    [ Some "F" -> True
    | _ -> False ]
  in
  let liste =
    let table_x = Mhashtbl.create 801 in
    let liste = ref [] in
    do for i = 0 to base.persons.len - 1 do
         let p = base.persons.get i in
         let istr = proj p in
         let pr = sou base istr in
         if pr = "?" then ()
         else
           let compte =
             try fst (Mhashtbl.find table_x pr) with
             [ Not_found ->
                 let c = ref 0 in
                 do Mhashtbl.add table_x pr (c, istr); return c ]
           in
           incr compte;
       done;
       Mhashtbl.iter
         (fun x (compte, i) -> liste.val := [(x, compte.val, i) :: liste.val])
         table_x;
    return
    let tri =
      if par_frequence then
        fun (x, cx, _) (y, cy, _) ->
          if cx > cy then True
          else if cx < cy then False
          else alphabetique x y <= 0
      else fun (x, cx, _) (y, cy, _) -> alphabetique x y <= 0
    in
    Sort.list tri liste.val
  in
  let len = List.length liste in
  if len >= 50 && p_getenv conf.env "k" <> Some "" then
    if par_frequence then
      match p_getint conf.env "k" with
      [ Some f -> print_frequence mode conf base is_fam liste len f
      | _ -> print_menu mode conf base is_fam liste len par_frequence ]
    else
      match p_getenv conf.env "k" with
      [ Some x -> print_alphab mode conf base is_fam liste len x
      | _ -> print_menu mode conf base is_fam liste len par_frequence ]
  else print_all mode conf base is_fam liste len par_frequence
;

value first_alphabetique =
  let iv_min = ref 500000 in
  let i_min = ref 0 in
  do for i = 0 to 255 do
       let iv = valeur_alphabetique (Char.chr i) in
       if iv < iv_min.val then do iv_min.val := iv; i_min.val := i; return ()
       else ();
     done;
  return Char.chr (i_min.val)
;

value next_alphabetique c =
  let v = valeur_alphabetique c in
  let iv_min = ref 500000 in
  let i_min = ref (-1) in
  do for i = 0 to 255 do
       let iv = valeur_alphabetique (Char.chr i) in
       if iv > v && iv < iv_min.val then
         do iv_min.val := iv; i_min.val := i; return ()
       else ();
     done;
  return
  if i_min.val = -1 then raise Not_found else Char.chr (i_min.val)
;

value person_has_surname base key ip =
  (poi base ip).surname = key
;

value family_names_print conf base =
  afficher_tous_x (fun p -> p.surname) "N" True conf base
;

value first_names_print conf base =
  afficher_tous_x (fun p -> p.first_name) "P" False conf base
;
