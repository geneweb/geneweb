open Config
open Def
open Gwdb
open Util
open Api_def



(**/**) (* Fonctions d'auto-complétion, config, recherche ... *)


(* ************************************************************************ *)
(*  [Fonc] create_autocomplete_cache : config -> base -> unit               *)
(** [Description] : Créé les fichiers de cache qui vont servir pour l'auto-
                    complétion.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : unit
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let print_cache base mode cache_file =
  let string_cache get_field =
    let cache = ref StringSetAutoComplete.empty in
    for i = 0 to nb_of_persons base - 1 do
      let p = poi base (Adef.iper_of_int i) in
      let field = sou base (get_field p) in
      if field = "" then ()
      else cache := StringSetAutoComplete.add (sou base (get_field p)) !cache
    done;
    let list = StringSetAutoComplete.elements !cache in
    let list = List.sort Gutil.alphabetic_order list in Cache_string list
  in
  let cache =
    match mode with
      `lastname -> string_cache get_surname
    | `firstname -> string_cache get_first_name
    | `place ->
        let cache = ref StringSetAutoComplete.empty in
        (*
        let rec loop place =
          let (_, place) =
            try Api_util.split place ',' with
            [ Not_found -> ("", "") ]
          in
          if place = "" then ()
          else do {
            cache.val := StringSetAutoComplete.add place cache.val;
            loop place
          }
        in
        *)
        for i = 0 to nb_of_persons base - 1 do
          let p = poi base (Adef.iper_of_int i) in
          List.iter
            (fun evt ->
               let place = sou base evt.epers_place in
               if place = "" then ()
               else cache := StringSetAutoComplete.add place !cache)
            (get_pevents p)
        done;
        for i = 0 to nb_of_families base - 1 do
          let fam = foi base (Adef.ifam_of_int i) in
          List.iter
            (fun evt ->
               let place = sou base evt.efam_place in
               if place = "" then ()
               else cache := StringSetAutoComplete.add place !cache)
            (get_fevents fam)
        done;
        let list = StringSetAutoComplete.elements !cache in
        let list = List.sort Gutil.alphabetic_order list in Cache_string list
    | `source ->
        let cache = ref StringSetAutoComplete.empty in
        for i = 0 to nb_of_persons base - 1 do
          let p = poi base (Adef.iper_of_int i) in
          let psrc = sou base (get_psources p) in
          if psrc = "" then ()
          else cache := StringSetAutoComplete.add psrc !cache;
          List.iter
            (fun evt ->
               let src = sou base evt.epers_src in
               if src = "" then ()
               else cache := StringSetAutoComplete.add src !cache)
            (get_pevents p)
        done;
        for i = 0 to nb_of_families base - 1 do
          let fam = foi base (Adef.ifam_of_int i) in
          let fsrc = sou base (get_fsources fam) in
          if fsrc = "" then ()
          else cache := StringSetAutoComplete.add fsrc !cache;
          List.iter
            (fun evt ->
               let src = sou base evt.efam_src in
               if src = "" then ()
               else cache := StringSetAutoComplete.add src !cache)
            (get_fevents fam)
        done;
        let list = StringSetAutoComplete.elements !cache in
        let list = List.sort Gutil.alphabetic_order list in Cache_string list
  in
  match
    try Some (Secure.open_out_bin cache_file) with Sys_error _ -> None
  with
    Some oc -> Marshal.to_channel oc cache [Marshal.No_sharing]; close_out oc
  | None -> ()

let string_start_with ini s =
  let rec loop i1 i2 =
    if i1 = String.length ini then true
    else if i2 = String.length s then
      if ini.[i1] = '_' then loop (i1 + 1) i2 else false
    else if s.[i2] = ini.[i1] || s.[i2] = ' ' && ini.[i1] = '_' then
      loop (i1 + 1) (i2 + 1)
    else false
  in
  loop 0 0

(* ************************************************************************ *)
(*  [Fonc] print_auto_complete : config -> base -> AutoCompleteResult       *)
(** [Description] : Renvoie la liste unique d'un champ. Par exemple la liste
                    de nom de famille en fonction de ce qui est tapé.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - result : la liste de la recherche.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let get_list_from_cache conf base s max_res mode =
  let bfile = base_path conf.bname in
  let cache_file =
    match mode with
      `lastname -> Filename.concat bfile "cache_surname"
    | `firstname -> Filename.concat bfile "cache_first_name"
    | `place -> Filename.concat bfile "cache_place"
    | `source -> Filename.concat bfile "cache_src"
  in
  Lock.control cache_file false
    ~onerror:(fun () -> [])
    (fun () ->
       let stats = Unix.stat cache_file in
       let last_mod = conf.ctime -. stats.Unix.st_mtime in
       if stats.Unix.st_size = 0 || last_mod > 3600. then
         print_cache base mode cache_file;
       match
         try Some (Secure.open_in_bin cache_file) with Sys_error _ -> None
       with
         Some ic ->
         let cache =
           try Some (Marshal.from_channel ic) with
             End_of_file | Failure _ -> None
         in
         close_in ic;
         let ini = Mutil.tr '_' ' ' s in
         (* optim : on sait que la liste est triée. *)
         let rec loop list accu nb_res =
           match list with
             [] -> accu
           | name :: l ->
             let k = Mutil.tr '_' ' ' name in
             let (accu, nb_res) =
               if string_start_with (Name.lower ini) (Name.lower k) then
                 name :: accu, nb_res + 1
               else accu, nb_res
             in
             if nb_res < max_res then loop l accu nb_res
             else List.rev accu
         in
         begin match cache with
             Some (Cache_string list) -> loop list [] 0
           | _ -> []
         end
       | None -> [])
