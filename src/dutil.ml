(* $Id: dutil.ml,v 5.1 2006-10-03 03:42:33 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Config;
open Dbdisk;
open Def;
open Mutil;

value poi base i = base.data.persons.get (Adef.int_of_iper i);
value aoi base i = base.data.ascends.get (Adef.int_of_iper i);
value uoi base i = base.data.unions.get (Adef.int_of_iper i);
value coi base i = base.data.couples.get (Adef.int_of_ifam i);
value sou base i = base.data.strings.get (Adef.int_of_istr i);

value p_first_name base p = nominative (sou base p.first_name);
value p_surname base p = nominative (sou base p.surname);

value dsk_person_misc_names base p nobtit =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then []
  else
    let public_names =
      let titles_names =
        let tnl = ref [] in
        do {
          List.iter
            (fun t ->
               match t.t_name with
               [ Tmain | Tnone -> ()
               | Tname x -> tnl.val := [x :: tnl.val] ])
            (nobtit p);
          tnl.val
        }
      in
      if sou base p.public_name = "" || nobtit p = [] then titles_names
      else [p.public_name :: titles_names]
    in
    let first_names =
      let pn =
        if sou base p.public_name <> "" && nobtit p = [] then
          [p.public_name :: public_names]
        else public_names
      in
      [first_name :: List.map (sou base) (p.first_names_aliases @ pn)]
    in
    let surnames =
      [surname ::
       surnames_pieces surname @
         List.map (sou base) (p.surnames_aliases @ p.qualifiers)]
    in
    let surnames =
      if p.sex == Female then
        let u = uoi base p.key_index in
        List.fold_left
          (fun list ifam ->
             let cpl = coi base ifam in
             let husband = poi base (Adef.father cpl) in
             let husband_surname = p_surname base husband in
             let husband_surnames_aliases =
               List.map (sou base) husband.surnames_aliases
             in
             if p_surname base husband = "?" then
               husband_surnames_aliases @ list
             else
               [husband_surname ::
                surnames_pieces husband_surname @ husband_surnames_aliases @
                  list])
          surnames (Array.to_list u.family)
      else surnames
    in
    let list = [] in
    let list =
      List.fold_left (fun list s -> [sou base s :: list]) list public_names
    in
    let list =
      List.fold_left
        (fun list f ->
           List.fold_left (fun list s -> [f ^ " " ^ s :: list]) list surnames)
        list first_names
    in
    let list =
      let first_names =
        [first_name :: List.map (sou base) (p.first_names_aliases)]
      in
      List.fold_left
        (fun list t ->
           let s = sou base t.t_place in
           if s = "" then list
           else
             let first_names =
               match t.t_name with
               [ Tname f -> [sou base f :: first_names]
               | Tmain | Tnone ->
                   let f = sou base (p.public_name) in
                   if f = "" then first_names else [f :: first_names] ]
             in
             List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
               first_names)
        list (nobtit p)
    in
    let list =
      match (aoi base p.key_index).parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let fath = poi base (Adef.father cpl) in
          let first_names =
            [first_name :: List.map (sou base) (p.first_names_aliases)]
          in
          List.fold_left
            (fun list t ->
               let s = sou base t.t_place in
               if s = "" then list
               else
                 List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
                   first_names)
            list (nobtit fath)
      | _ -> list ]
    in
    let list =
      List.fold_left (fun list s -> [sou base s :: list]) list p.aliases
    in
    let fn = Name.lower (first_name ^ " " ^ surname) in
    List.fold_left
      (fun list s ->
         let s = Name.lower s in
         if s = fn || List.mem s list then list else [s :: list])
      [] list
;

value dsk_nobtit conf base p =
  match Lazy.force conf.allowed_titles with
  [ [] -> p.titles
  | allowed_titles ->
      List.fold_right
        (fun t l ->
           let id = sou base t.t_ident in
           let pl = sou base t.t_place in
           if List.mem (id ^ "/" ^ pl) allowed_titles then [t :: l] else l)
        p.titles [] ]
;
