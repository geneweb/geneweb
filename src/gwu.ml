(* $Id: gwu.ml,v 1.1.1.1 1998-09-01 14:32:11 ddr Exp $ *)

open Def;
open Gutil;

value soy y = if y == 0 then "-0" else string_of_int y;

value print_date oc =
  fun
  [ Djma d m y -> Printf.fprintf oc "%d/%d/%s" d m (soy y)
  | Dma m y -> Printf.fprintf oc "%d/%s" m (soy y)
  | Da prec y ->
      do match prec with
         [ About -> Printf.fprintf oc "~"
         | Maybe -> Printf.fprintf oc "?"
         | Before -> Printf.fprintf oc "<"
         | After -> Printf.fprintf oc ">"
         | _ -> () ];
         Printf.fprintf oc "%s" (soy y);
         match prec with
         [ OrYear y -> Printf.fprintf oc "|%s" (soy y)
         | _ -> () ];
      return () ]
;

value print_date_option oc =
  fun
  [ Some d -> print_date oc d
  | None -> () ]
;

value buff = ref (String.create 80);
value store len x =
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;

value get_buff len = String.sub buff.val 0 len;

value starting_char =
  fun
  [ 'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' | '0'..'9' | '?' | ' ' -> True
  | _ -> False ]
;

value correct_string base is =
  let s = sou base is in
  loop 0 0 where rec loop i len =
    if i == String.length s then get_buff len
    else
      if i == 0 && not (starting_char s.[0]) then
        loop (i + 1) (store (store len '_') s.[0])
      else if s.[i] == ' ' then loop (i + 1) (store len '_')
      else loop (i + 1) (store len s.[i])
;

value has_infos_not_dates base p =
  p.first_names_aliases <> [] || p.surnames_aliases <> [] ||
  sou base p.public_name <> "" || p.nick_names <> [] || p.aliases <> [] ||
  p.titles <> [] || sou base p.occupation <> "" ||
  sou base p.birth_place <> "" || sou base p.baptism_place <> "" ||
  sou base p.death_place <> "" || sou base p.psources <> ""
;

value has_infos base p =
  has_infos_not_dates base p || p.birth <> Adef.codate_None ||
  p.baptism <> Adef.codate_None ||  p.death <> NotDead
;

value print_first_name_alias oc base is =
  Printf.fprintf oc " {%s}" (correct_string base is)
;

value print_surname_alias oc base is =
  Printf.fprintf oc " #salias %s" (correct_string base is)
;

value print_nick_name oc base is =
  Printf.fprintf oc " #nick %s" (correct_string base is)
;

value print_alias oc base is =
  Printf.fprintf oc " #alias %s" (correct_string base is)
;

value print_photo oc base is =
  if sou base is = "" then ()
  else Printf.fprintf oc " #photo %s" (correct_string base is)
;

value print_burial oc base b =
  match b with
  [ Buried cod ->
      do Printf.fprintf oc " #buri";
         match Adef.od_of_codate cod with
         [ Some d ->
             do Printf.fprintf oc " ";
                print_date oc d;
             return ()
         | _ -> () ];
      return ()
  | Cremated cod ->
      do Printf.fprintf oc " #crem";
         match Adef.od_of_codate cod with
         [ Some d ->
             do Printf.fprintf oc " ";
                print_date oc d;
             return ()
         | _ -> () ];
      return ()
  | UnknownBurial -> () ]
;

value print_burial_place oc base is =
  Printf.fprintf oc " #rp %s" (correct_string base is)
;

value print_title oc base t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  do Printf.fprintf oc " [";
     match t.t_name with
     [ Tmain -> Printf.fprintf oc "*"
     | Tname s -> Printf.fprintf oc "%s" (correct_string base s)
     | Tnone -> () ];
     Printf.fprintf oc ":";
     Printf.fprintf oc "%s" (correct_string base t.t_title);
     Printf.fprintf oc ":";
     Printf.fprintf oc "%s" (correct_string base t.t_place);
     if t.t_nth <> 0 then Printf.fprintf oc ":"
     else
       match (t_date_start, t_date_end) with
       [ (Some _, _) | (_, Some _) -> Printf.fprintf oc ":"
       | _ -> () ];
     print_date_option oc t_date_start;
     if t.t_nth <> 0 then Printf.fprintf oc ":"
     else
       match t_date_end with
       [ Some _ -> Printf.fprintf oc ":"
       | _ -> () ];
     print_date_option oc t_date_end;
     if t.t_nth <> 0 then Printf.fprintf oc ":%d" t.t_nth else ();
     Printf.fprintf oc "]";
  return ()
;

value print_infos oc base is_child print_sources p =
  do List.iter (print_first_name_alias oc base) p.first_names_aliases;
     List.iter (print_surname_alias oc base) p.surnames_aliases;
     match p.public_name with
     [ s when sou base s <> "" ->
         Printf.fprintf oc " (%s)" (correct_string base s)
     | _ -> () ];
     print_photo oc base p.photo;
     List.iter (print_nick_name oc base) p.nick_names;
     List.iter (print_alias oc base) p.aliases;
     List.iter (print_title oc base) p.titles;
     match p.access with
     [ IfTitles -> ()
     | Public -> Printf.fprintf oc " #apubl"
     | Private -> Printf.fprintf oc " #apriv" ];
     match p.occupation with
     [ s when sou base s <> "" ->
         Printf.fprintf oc " #occu %s" (correct_string base s)
     | _ -> () ];
     if print_sources then
       match p.psources with
       [ s when sou base s <> "" ->
           Printf.fprintf oc " #src %s" (correct_string base s)
       | _ -> () ]
     else ();
     match Adef.od_of_codate p.birth with
     [ Some d ->
         do Printf.fprintf oc " ";
            print_date oc d;
         return ()
     | _ ->
         if p.baptism <> Adef.codate_None then ()
         else
           match p.death with
           [ Death _ _ | DeadYoung | DeadDontKnowWhen -> Printf.fprintf oc " 0"
           | DontKnowIfDead
             when not is_child && not (has_infos_not_dates base p) &&
             sou base p.first_name <> "?" && sou base p.surname <> "?" ->
               Printf.fprintf oc " 0"
           | _ -> () ] ];
     if sou base p.birth_place <> "" then
       Printf.fprintf oc " #bp %s" (correct_string base p.birth_place)
     else ();
     match Adef.od_of_codate p.baptism with
     [ Some d ->
         do Printf.fprintf oc " !";
            print_date oc d;
         return ()
     | _ -> () ];
     if sou base p.baptism_place <> "" then
       Printf.fprintf oc " #pp %s" (correct_string base p.baptism_place)
     else ();
     match p.death with
     [ Death dr d ->
         do Printf.fprintf oc " ";
            match dr with
            [ Killed -> Printf.fprintf oc "k"
            | Murdered -> Printf.fprintf oc "m"
            | Executed -> Printf.fprintf oc "e"
            | Disappeared -> Printf.fprintf oc "s"
            | _ -> () ];
            print_date oc (Adef.date_of_cdate d);
         return ()
     | DeadYoung -> Printf.fprintf oc " mj"
     | DeadDontKnowWhen -> Printf.fprintf oc " 0"
     | DontKnowIfDead ->
         match (Adef.od_of_codate p.birth, Adef.od_of_codate p.baptism) with
         [ (Some _, _) | (_, Some _) -> Printf.fprintf oc " ?"
         | _ -> () ]
     | NotDead -> () ];
     if sou base p.death_place <> "" then
       Printf.fprintf oc " #dp %s" (correct_string base p.death_place)
     else ();
     print_burial oc base p.burial;
     if sou base p.burial_place <> "" then
       print_burial_place oc base p.burial_place
     else ();
  return ()
;

value print_parent oc base ifaml fam_sel fam ip =
  let p = poi base ip in
  let a = aoi base ip in
  do Printf.fprintf oc "%s %s%s" (correct_string base p.surname)
       (correct_string base p.first_name)
       (if p.occ == 0 || sou base p.first_name = "?"
        || sou base p.surname = "?" then ""
        else "." ^ string_of_int p.occ);
  return
  let has_printed_parents =
    match a.parents with
    [ Some ifam -> fam_sel ifam
    | None -> False ]
  in
  let first_parent_definition =
    loop ifaml where rec loop =
      fun
      [ [ifam1 :: ifaml1] ->
          let cpl = coi base ifam1 in
          if cpl.father == ip || cpl.mother == ip then fam.fam_index == ifam1
          else loop ifaml1
      | [] -> assert False ]
  in
  let pr = not has_printed_parents && first_parent_definition in
  if pr (* && sou base p.first_name <> "?" *) then
    if has_infos base p then print_infos oc base False True p
    else if sou base p.first_name <> "?" && sou base p.surname <> "?" then
      Printf.fprintf oc " 0"
    else ()
  else ()
;

value print_child oc base fam_surname print_sources ip =
  let p = poi base ip in
  do Printf.fprintf oc "-";
     match p.sexe with
     [ Masculin -> Printf.fprintf oc " h"
     | Feminin -> Printf.fprintf oc " f"
     | _ -> () ];
     Printf.fprintf oc " %s" (correct_string base p.first_name);
     if p.occ == 0 || sou base p.first_name = "?" || sou base p.surname = "?"
     then ()
     else Printf.fprintf oc ".%d" p.occ;
     if p.surname <> fam_surname then
       Printf.fprintf oc " %s" (correct_string base p.surname)
     else ();
     print_infos oc base True print_sources p;
     Printf.fprintf oc "\n";
  return ()
;

value bogus_person base ip =
  let p = poi base ip in
  sou base p.first_name = "?" && sou base p.surname = "?"
;

value common_children_sources base children =
  if Array.length children <= 1 then None
  else
    loop 1 (poi base children.(0)).psources where rec loop i src =
      if i == Array.length children then
        let s = sou base src  in
        if s = "" then None else Some src
      else
        let p = poi base children.(i) in
        if p.psources == src then loop (i + 1) src else None
;

value array_forall f a =
  loop 0 where rec loop i =
    if i == Array.length a then True
    else if f a.(i) then loop (i + 1)
    else False
;

value empty_family base fam =
  let cpl = coi base fam.fam_index in
  bogus_person base cpl.father && bogus_person base cpl.mother &&
  array_forall (bogus_person base) fam.children
;

value print_family oc base ifaml (per_sel, fam_sel) fam_done ifam =
  let fam = foi base ifam in
  let cpl = coi base ifam in
  do Printf.fprintf oc "fam ";
     print_parent oc base ifaml fam_sel fam cpl.father;
     Printf.fprintf oc " +";
     print_date_option oc (Adef.od_of_codate fam.marriage);
     match sou base fam.marriage_place with
     [ "" -> ()
     | s ->
         Printf.fprintf oc " #mp %s"
           (correct_string base fam.marriage_place) ];
     match fam.divorce with
     [ NotDivorced -> ()
     | Divorced d ->
         let d = Adef.od_of_codate d in
         do Printf.fprintf oc " -"; print_date_option oc d; return () ];
     Printf.fprintf oc " ";
     print_parent oc base ifaml fam_sel fam cpl.mother;
     Printf.fprintf oc "\n";
     match sou base fam.fsources with
     [ "" -> ()
     | s -> Printf.fprintf oc "src %s\n" (correct_string base fam.fsources) ];
     match fam.comment with
     [ txt when sou base txt <> "" ->
         Printf.fprintf oc "comm %s\n" (sou base txt)
     | _ -> () ];
     let print_sources =
       match common_children_sources base fam.children with
       [ Some s ->
          do Printf.fprintf oc "csrc %s\n" (correct_string base s); return
          False
       | _ -> True ]
     in
     match Array.length fam.children with
     [ 0 -> ()
     | _ ->
         let fam_surname = (poi base cpl.father).surname in
         do Printf.fprintf oc "beg\n";
            Array.iter
              (fun ip ->
                 if per_sel ip then
                   print_child oc base fam_surname print_sources ip
                 else ())
              fam.children;
            Printf.fprintf oc "end\n";
         return () ];
     fam_done.(Adef.int_of_ifam fam.fam_index) := True;
  return ()
;

value get_persons_with_notes base ifam list =
  let fam = foi base ifam in
  let cpl = coi base ifam in
  let father = poi base cpl.father in
  let mother = poi base cpl.mother in
  let list =
    match (sou base father.notes, (aoi base cpl.father).parents) with
    [ ("", _) | (_, Some _) -> list
    | _ -> [cpl.father :: list] ]
  in
  let list =
    match (sou base mother.notes, (aoi base cpl.mother).parents) with
    [ ("", _) | (_, Some _) -> list
    | _ -> [cpl.mother :: list] ]
  in
  List.fold_right
    (fun ip list ->
       let p = poi base ip in
       match sou base p.notes with
       [ "" -> list
       | _ -> [ip :: list] ])
    (Array.to_list fam.children) list
;

value print_notes_for_person oc base ip =
  let p = poi base ip in
  do Printf.fprintf oc "\n";
     Printf.fprintf oc "notes %s %s%s\n"
       (correct_string base p.surname)
       (correct_string base p.first_name)
       (if p.occ == 0 then "" else "." ^ string_of_int p.occ);
     Printf.fprintf oc "beg\n";
     Printf.fprintf oc "%s\n" (sou base p.notes);
     Printf.fprintf oc "end notes\n";
  return ()
;

value print_notes oc base ifaml per_sel =
  let ipl = List.fold_right (get_persons_with_notes base) ifaml [] in
  let ipl =
    List.fold_right
      (fun ip ipl -> if List.memq ip ipl then ipl else [ip :: ipl])
      ipl []
  in
  List.iter
    (fun ip -> if per_sel ip then print_notes_for_person oc base ip else ())
    ipl
;

value rec merge_families ifaml1f ifaml2f =
  match (ifaml1f, ifaml2f) with
  [ ([ifam1 :: ifaml1], [ifam2 :: ifaml2]) ->
      let m1 = List.memq ifam1 ifaml2 in
      let m2 = List.memq ifam2 ifaml1 in
      if m1 && m2 then merge_families ifaml1 ifaml2
      else if m1 then
        [ifam2 :: merge_families ifaml1f ifaml2]
      else if m2 then
        [ifam1 :: merge_families ifaml1 ifaml2f]
      else if ifam1 == ifam2 then [ifam1 :: merge_families ifaml1 ifaml2]
      else [ifam1; ifam2 :: merge_families ifaml1 ifaml2]
  | (ifaml1, []) -> ifaml1
  | ([], ifaml2) -> ifaml2 ]
;

value rec filter f =
  fun
  [ [x :: l] -> if f x then [x :: filter f l] else filter f l
  | [] -> [] ]
;

value connected_families base fam_sel fam =
  loop [fam.fam_index] [] [(coi base fam.fam_index).father]
  where rec loop ifaml ipl_scanned =
    fun
    [ [ip :: ipl] ->
        if List.memq ip ipl_scanned then loop ifaml ipl_scanned ipl
        else
          let p = poi base ip in
          let ifaml1 = Array.to_list p.family in
          let ifaml1 = filter fam_sel ifaml1 in
          let ifaml = merge_families ifaml ifaml1 in
          let ipl =
            List.fold_right
              (fun ifam ipl ->
                 let cpl = coi base ifam in
                 [cpl.father; cpl.mother :: ipl])
              ifaml1 ipl
          in
          loop ifaml [ip :: ipl_scanned] ipl
    | [] -> ifaml ]
;

value find_person base p1 po p2 =
  try Gutil.person_ht_find_unique base p1 p2 po with
  [ Not_found ->
      do Printf.eprintf "Not found: %s%s %s\n"
           p1 (if po == 0 then "" else " " ^ string_of_int po) p2;
         flush stderr;
      return exit 2 ]
;

value gwu base out_dir src_oc_list anc =
  let anc =
    match anc with
    [ Some (p1, po, p2) -> Some (find_person base p1 po p2)
    | None -> None ]
  in
  let ((per_sel, fam_sel) as sel) = Select.functions base anc None in
  let fam_done = Array.create (base.families.len) False in
  for i = 0 to base.families.len - 1 do
    let fam = base.families.get i in
    if is_deleted_family fam then ()
    else
      do if fam_done.(i) then ()
         else if fam_sel fam.fam_index then
           let ifaml = connected_families base fam_sel fam in
           let (oc, first) =
             try List.assoc fam.origin_file src_oc_list.val with
             [ Not_found ->
                 let fname = sou base fam.origin_file in
                 let oc =
                   if out_dir = "" then stdout
                   else if fname = "" then stdout
                   else open_out (Filename.concat out_dir fname)
                 in
                 let x = (oc, ref True) in
                 do src_oc_list.val :=
                      [(fam.origin_file, x) :: src_oc_list.val];
                 return x ]
           in
           let ifaml =
             List.fold_right
               (fun ifam ifaml ->
                  if empty_family base (foi base ifam) then ifaml
                  else [ifam :: ifaml])
               ifaml []
           in
           if ifaml <> [] then
             do if not first.val then Printf.fprintf oc "\n" else ();
                first.val := False;
                List.iter (print_family oc base ifaml sel fam_done) ifaml;
                print_notes oc base ifaml per_sel;
             return ()
           else ()
         else ();
      return ();
  done
;

value in_file = ref "";
value out_dir = ref "";
value anc_1st = ref "";
value anc_occ = ref 0;
value anc_2nd = ref "";

type arg_state = [ ASnone | ASwaitAncOcc | ASwaitAncSurn ];
value arg_state = ref ASnone;

value speclist =
  [("-odir", Arg.String (fun s -> out_dir.val := s),
   "<dir>   create files in this directories (else all on stdout)");
   ("-a",
    Arg.String
      (fun s -> do anc_1st.val := s; return arg_state.val := ASwaitAncOcc),
    "\"<1st_name>\" [num] \"<surname>\": select ancestors of...")]
;

value anon_fun s =
  match arg_state.val with
  [ ASnone -> in_file.val := s
  | ASwaitAncOcc ->
      try
        do anc_occ.val := int_of_string s; return
        arg_state.val := ASwaitAncSurn
      with
      [ Failure _ ->
          do anc_occ.val := 0; anc_2nd.val := s; return
          arg_state.val := ASnone ]
  | ASwaitAncSurn ->
      do anc_2nd.val := s; return arg_state.val := ASnone ]
;

value errmsg = "Usage: " ^ Sys.argv.(0) ^ " [options] <base_file>
Options are:";

value main () =
  do Argl.parse speclist anon_fun errmsg;
     if in_file.val = "" then
       do Printf.eprintf "Missing base\n";
          Printf.eprintf "Use option -help for usage\n";
          flush stderr;
       return exit 2
     else ();
  return
  let anc =
    if anc_1st.val <> "" then
      if anc_2nd.val = "" then
        do Printf.eprintf "Misused option -a\n";
           Printf.eprintf "Use option -help for usage\n";
           flush stderr;
        return exit 2
      else Some (anc_1st.val, anc_occ.val, anc_2nd.val)
    else None
  in
  let base = Iobase.input in_file.val in
  let src_oc_list = ref [] in
  let _ = base.persons.array () in
  let _ = base.ascends.array () in
  let _ = base.families.array () in
  let _ = base.couples.array () in
  let _ = base.strings.array () in
  let oc_list = ref [] in
(*
     for i = 0 to base.families.len - 1 do
       let fam = base.families.get i in
       if is_deleted_family fam then ()
       else
         let first = ref True in
         try let _ = List.assoc fam.origin_file src_oc_list.val in () with
         [ Not_found ->
             let oc_f =
               if out_dir.val = "" then (stdout, first)
               else if sou base fam.origin_file = "" then
                 (stdout, first)
               else
                 (open_out
                    (Filename.concat out_dir.val (sou base fam.origin_file)),
                  ref True)
             in
             src_oc_list.val :=
               [(fam.origin_file, oc_f) :: src_oc_list.val] ];
     done;
*)
  do gwu base out_dir.val src_oc_list anc;
     List.iter
       (fun (src, (oc, _)) ->
          do flush oc; return
          if oc != stdout then close_out oc else ())
       src_oc_list.val;
  return ()
;

Printexc.catch main ();
