(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: birthDeath.ml,v 1.1 1999-01-20 09:26:55 ddr Exp $ *)

open Def;
open Gutil;
open Util;
open Config;

value insert_at tab len i p d =
  let len = min len (Array.length tab - 1) in
  do Array.blit tab i tab (i + 1) (len - i); return
  tab.(i) := Some (p, d)
;

value before d =
  fun
  [ Some (_, d1) -> d1 strictement_avant d
  | _ -> assert False ]
;

value after d x = not (before d x);

value insert conf tab len p d =
  do assert (len <= Array.length tab); return
  if len == 0 then if Array.length tab > 0 then tab.(0) := Some (p, d) else ()
  else if before d tab.(0) then insert_at tab len 0 p d
  else if after d tab.(len - 1) then
    if len == Array.length tab then () else tab.(len) := Some (p, d)
  else
    loop 0 (len - 1) where rec loop imin imax =
      do assert (imin < imax);
         assert (after d tab.(imin));
         assert (before d tab.(imax));
      return
      if imin == imax - 1 then insert_at tab len imax p d
      else
        let imid = (imin + imax) / 2 in
        if before d tab.(imid) then loop imin imid
        else loop imid imax
;

value select conf base get_date =
  let n =
    match p_getint conf.env "k" with
    [ Some x -> x
    | _ -> 3 ]
  in
  let n = min (max 0 n) base.data.persons.len in
  let tab = Array.create n None in
  let len = ref 0 in
  do for i = 0 to base.data.persons.len - 1 do
       let p = base.data.persons.get i in
       if age_autorise conf base p then
         match get_date p with
         [ Some d ->
             if d.day != 0 && d.month != 0 then
               do insert conf tab len.val p d;
                  if len.val == Array.length tab then () else incr len;
               return ()
             else ()
         | _ -> () ]
       else ();
     done;
  return (tab, len.val)
;

value print_birth conf base =
  let (tab, len) = select conf base (fun p -> Adef.od_of_codate p.birth) in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %d births")) len
  in
  do header conf title;
     for i = 0 to Array.length tab - 1 do
       match tab.(i) with
       [ Some (p, d) ->
           tag "ul" begin
             Wserver.wprint "<li><strong>\n";
             afficher_personne_referencee conf base p;
             Wserver.wprint "</strong>,\n";
             Wserver.wprint "%s <em>%s</em>.\n"
               (transl_nth conf "born" (index_of_sex p.sex))
               (Date.string_of_ondate conf d);
           end
       | None -> () ];
     done;
     trailer conf;
  return ()
;

value get_death p =
  match p.death with
  [ Death _ cd -> Some (Adef.date_of_cdate cd)
  | _ -> None ]
;

value print_death conf base =
  let (tab, len) = select conf base get_death in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %d deaths")) len
  in
  do header conf title;
     for i = 0 to Array.length tab - 1 do
       match tab.(i) with
       [ Some (p, d) ->
           tag "ul" begin
             Wserver.wprint "<li><strong>\n";
             afficher_personne_referencee conf base p;
             Wserver.wprint "</strong>,\n";
             Wserver.wprint "%s <em>%s</em>.\n"
               (transl_nth conf "died" (index_of_sex p.sex))
               (Date.string_of_ondate conf d);
           end
       | None -> () ];
     done;
     trailer conf;
  return ()
;
