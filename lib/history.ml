(* $Id: history.ml,v 5.14 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open TemplAst
open Util

let file_name conf =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  Filename.concat (Util.base_path [] bname) "history"

(* Record history when committing updates *)

let ext_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]


(* ********************************************************************** *)
(*  [Fonc] slash_name_of_key : string -> string -> int -> string          *)
(** [Description] : Renvoie la clé nom/prénom/occ.
    [Args] :
      - fn  : string
      - sn  : string
      - occ : int
    [Retour] : string
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let slash_name_of_key fn sn occ =
  let space_to_unders = Mutil.tr ' ' '_' in
  let fn = space_to_unders (Name.lower fn) in
  let sn = space_to_unders (Name.lower sn) in
  sn ^ "/" ^ fn ^ "/" ^ string_of_int occ


(* ********************************************************************** *)
(*  [Fonc] diff_visibility :
             config -> base -> gen_person -> gen_person -> string array   *)
(** [Description] : Si la visibilité de la personne a changé (entre
                    l'ancienne et la nouvelle), alors on revoie un tableau
                    avec la nouvelle visibilité.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - op   : la person avant les modifications
      - np   : la person après les modifications
    [Retour] : string array
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let diff_visibility conf base op np =
  let k = slash_name_of_key np.first_name np.surname np.occ in
  let empty_union = {family = [| |]} in
  let empty_ascend = {parents = None; consang = Adef.fix (-1)} in
  let op = Futil.map_person_ps (fun p -> p) (Gwdb.insert_string base) op in
  let np = Futil.map_person_ps (fun p -> p) (Gwdb.insert_string base) np in
  let o_p = Gwdb.person_of_gen_person base (op, empty_ascend, empty_union) in
  let n_p = Gwdb.person_of_gen_person base (np, empty_ascend, empty_union) in
  let tmp_conf = {conf with wizard = false; friend = false} in
  let old_visibility = Util.authorized_age tmp_conf base o_p in
  let new_visibility = Util.authorized_age tmp_conf base n_p in
  if old_visibility <> new_visibility then
    [| "VISIBLE"; k; string_of_bool new_visibility |]
  else [| |]


type kind_diff =
    Diff_person of (iper, iper, string) gen_person * (iper, iper, string) gen_person
  | Diff_string of (string * string * int) * (string * string * int)

(* ********************************************************************** *)
(*  [Fonc] diff_key : gen_person -> gen_person -> string array            *)
(** [Description] : Si la clé de la personne a changé, alors on renvoie un
                    tableau avec l'ancienne clé et la nouvelle clé.
    [Args] :
      - op   : la person avant les modifications
      - np   : la person après les modifications
    [Retour] : string array
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let diff_key d =
  match d with
    Diff_person (op, np) ->
      let o_key = slash_name_of_key op.first_name op.surname op.occ in
      let n_key = slash_name_of_key np.first_name np.surname np.occ in
      if o_key <> n_key then [| "KEY"; o_key; n_key |] else [| |]
  | Diff_string ((ofn, osn, oocc), (fn, sn, occ)) ->
      let o_key = slash_name_of_key ofn osn oocc in
      let n_key = slash_name_of_key fn sn occ in
      if o_key <> n_key then [| "KEY"; o_key; n_key |] else [| |]


(* ********************************************************************** *)
(*  [Fonc] diff_person :
             config -> base -> gen_person -> gen_person -> string array   *)
(** [Description] : Fonction qui ajouté des paramètres passés dans la
                    ligne de commande de notify_change. Elle permet de
                    savoir quelle genre de modifications ont été faites.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - op   : la person avant les modifications
      - np   : la person après les modifications
    [Retour] : string array
    [Rem] : Non exporté en clair hors de ce module.                       *)
(* ********************************************************************** *)
let diff_person conf base changed =
  match changed with
    U_Add_person _ | U_Delete_person _ -> [| |]
  | U_Modify_person (o, n) ->
      Array.append (diff_key (Diff_person (o, n)))
        (diff_visibility conf base o n)
  | U_Merge_person (p1, p2, p) ->
      let args_p1 =
        Array.append (diff_key (Diff_person (p1, p)))
          (diff_visibility conf base p1 p)
      in
      let args_p2 =
        Array.append (diff_key (Diff_person (p2, p)))
          (diff_visibility conf base p2 p)
      in
      Array.append args_p1 args_p2
  | U_Send_image _ | U_Delete_image _ | U_Add_family (_, _) |
    U_Modify_family (_, _, _) | U_Delete_family (_, _) |
    U_Invert_family (_, _) | U_Merge_family (_, _, _, _) |
    U_Add_parent (_, _) ->
      [| |]
  | U_Change_children_name (_, l) ->
      List.fold_left
        (fun accu ((ofn, osn, oocc, _), (fn, sn, occ, _)) ->
           Array.append accu
             (diff_key (Diff_string ((ofn, osn, oocc), (fn, sn, occ)))))
        [| |] l
  | U_Multi (o, n, _) -> diff_key (Diff_person (o, n))
  | U_Notes (_, _) | U_Kill_ancestors _ -> [| |]


(* ************************************************************************ *)
(*  [Fonc] notify_change : config -> base -> base_changed -> string -> unit *)
(** [Description] : Appel le script défini par la variable notify_change du
                    fichier gwf.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - changed : le type de modification (voir def.mli)
      - action : le code du type de modification
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let notify_change conf base changed action =
  if Sys.unix then
    match p_getenv conf.base_env "notify_change" with
      Some comm ->
        let base_args =
          match changed with
            U_Add_person p | U_Modify_person (_, p) | U_Delete_person p |
            U_Merge_person (_, _, p) | U_Send_image p | U_Delete_image p |
            U_Add_family (p, _) | U_Modify_family (p, _, _) |
            U_Delete_family (p, _) | U_Invert_family (p, _) |
            U_Merge_family (p, _, _, _) | U_Add_parent (p, _) |
            U_Kill_ancestors p | U_Change_children_name (p, _) |
            U_Multi (_, p, _) ->
              let key = slash_name_of_key p.first_name p.surname p.occ in
              [| key; string_of_iper p.key_index |]
          | U_Notes (Some num, file) -> [| file; string_of_int num |]
          | U_Notes (None, file) -> [| file |]
        in
        let optional_args = diff_person conf base changed in
        let args = Array.append base_args optional_args in
        let args =
          Array.append [| comm; conf.bname; conf.user; action |] args
        in
        begin match Unix.fork () with
          0 ->
            if Unix.fork () <> 0 then exit 0
            else begin (try Unix.execvp comm args with _ -> ()); exit 0 end
        | id -> ignore (Unix.waitpid [] id)
        end
    | None -> ()


(* ************************************************************************ *)
(*  [Fonc] gen_record : config -> base -> base_changed -> string -> unit    *)
(** [Description] : Enregistre dans le fichier historique si la variable
      "hitory" du fichier gwf est valorisée à "yes". Le fait qu'on ait des
      gen_person, nous permet de pouvoir faire un diff entre avant et après
      la modification d'une personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - changed : le type de modification (voir def.mli)
      - action : le code du type de modification
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let gen_record conf base changed action =
  begin match p_getenv conf.base_env "history" with
    Some "yes" when not conf.manitou ->
      let item =
        match changed with
          U_Add_person p | U_Modify_person (_, p) | U_Delete_person p |
          U_Merge_person (_, _, p) | U_Send_image p | U_Delete_image p |
          U_Add_family (p, _) | U_Modify_family (p, _, _) |
          U_Delete_family (p, _) | U_Invert_family (p, _) |
          U_Merge_family (p, _, _, _) | U_Add_parent (p, _) |
          U_Kill_ancestors p | U_Change_children_name (p, _) |
          U_Multi (_, p, _) ->
            p.first_name ^ "." ^ string_of_int p.occ ^ " " ^ p.surname
        | U_Notes (Some num, file) ->
            let s = string_of_int num in
            if file = "" then s else file ^ "/" ^ s
        | U_Notes (None, file) -> file
      in
      let fname = file_name conf in
      begin match
        begin try Some (Secure.open_out_gen ext_flags 0o644 fname) with
          Sys_error _ -> None
        end
      with
        Some oc ->
          let (hh, mm, ss) = conf.time in
          Printf.fprintf oc "%04d-%02d-%02d %02d:%02d:%02d [%s] %s %s\n"
            conf.today.year conf.today.month conf.today.day hh mm ss conf.user
            action item;
          close_out oc
      | None -> ()
      end
  | _ -> ()
  end;
  HistoryDiff.record_diff conf base changed;
  (* Effet de bord des modifications en masse : on peut facilement  *)
  (* créer 5000 nouveaux processus à chaque mise à jour.            *)
  (* Pour éviter cela, on n'appelle jamais notify_change lors de la *)
  (* mise à jour de l'historique.                                   *)
  match changed with
    U_Multi (_, _, _) -> ()
  | _ -> notify_change conf base changed action


(* ************************************************************************ *)
(*  [Fonc] record : config -> base -> base_changed -> string -> unit        *)
(** [Description] : Suite à la mise à jour de la base, on réalise les
      traitements suivant :
        - mise à jour (si nécessaire) du fichier gwf pour le sosa_ref
        - mise à jour du fichier historique
        - appel du script notify_change
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - changed : le type de modification (voir def.mli)
      - action : le code du type de modification
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let record conf base changed action =
  (* Mise à jour du fichier gwf si le sosa_ref a changé. *)
  begin match changed with
    U_Modify_person (_, p) ->
      let (fn, sn, occ, ip) = p.first_name, p.surname, p.occ, p.key_index in
      update_gwf_sosa conf base (ip, (fn, sn, occ))
  | U_Merge_person (p1, _, p) ->
      let (fn, sn, occ, ip) =
        p1.first_name, p1.surname, p1.occ, p1.key_index
      in
      update_gwf_sosa conf base (ip, (fn, sn, occ));
      (* On n'a pas besoin de faire un update sur "p2" *)
      (* parce qu'on le fait sur p dans tous les cas.  *)
      let (fn, sn, occ, ip) = p.first_name, p.surname, p.occ, p.key_index in
      update_gwf_sosa conf base (ip, (fn, sn, occ))
  | U_Change_children_name (_, l) ->
      List.iter
        (fun (_, (fn, sn, occ, ip)) ->
           update_gwf_sosa conf base (ip, (fn, sn, occ)))
        l
  | _ -> ()
  end;
  (* Mise à jour du fichier historique et appel de notify_change. *)
  gen_record conf base changed action


(* ************************************************************************ *)
(*  [Fonc] notify : config -> base -> string -> unit                        *)
(** [Description] : Appel explicite de notify_change suite à une modification
                    de masse de la base (typiquement, le dico des lieux).
                    On évite comme ça la création de 5000 processus.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - action : le code du type de modification
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let notify conf base action =
  let empty_person = Gwdb.empty_person base Gwdb.dummy_iper in
  let empty_person =
    Util.string_gen_person base (gen_person_of_person empty_person)
  in
  notify_change conf base (U_Multi (empty_person, empty_person, false)) action


(* Request for history printing *)

exception Begin_of_file

let buff_get_rev len =
  let get i = Bytes.get !(Buff.buff) (len - 1 - i) in String.init len get

let rev_input_char ic (rbuff, rpos) pos =
  if !rpos = 0 then
    begin
      if Bytes.length !rbuff < 65536 then
        begin let len =
          if Bytes.length !rbuff = 0 then 1024 else 2 * Bytes.length !rbuff
        in
          rbuff := Bytes.create len
        end;
      let ppos = max (pos - Bytes.length !rbuff) 0 in
      seek_in ic ppos;
      really_input ic !rbuff 0 (pos - ppos);
      rpos := pos - ppos
    end;
  decr rpos;
  Bytes.get !rbuff !rpos

let rev_input_line ic pos (rbuff, rpos) =
  if pos <= 0 then raise Begin_of_file
  else
    let rec loop len pos =
      if pos <= 0 then buff_get_rev len, pos
      else
        match rev_input_char ic (rbuff, rpos) pos with
          '\n' -> buff_get_rev len, pos
        | '\r' -> buff_get_rev len, (pos - 1)
        | c -> loop (Buff.store len c) (pos - 1)
    in
    loop 0 (pos - 1)

let line_tpl = "0000-00-00 00:00:00 xx ."

let line_fields line =
  if String.length line > String.length line_tpl then
    let time = String.sub line 0 19 in
    let (user, i) =
      match line.[20], String.index_opt line ']' with
        '[', Some i -> let user = String.sub line 21 (i - 21) in user, i + 2
      | _ -> "", 20
    in
    let action = String.sub line i 2 in
    let key =
      let i = i + 3 in
      if i >= String.length line then None
      else Some (String.sub line i (String.length line - i))
    in
    Some (time, user, action, key)
  else None

type hist_item =
    HI_notes of string * int option
  | HI_ind of person
  | HI_none

type 'a env =
    Vcnt of int ref
  | Vinfo of string * string * string * hist_item * string
  | Vpos of int ref
  | Vsearch of (bool * string * int) option
  | Vother of 'a
  | Vnone

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother =
  function
    Vother x -> Some x
  | _ -> None
let set_vother x = Vother x

let possibly_highlight env s =
  match get_env "search" env with
    Vsearch (Some (case_sens, h, _)) ->
      if in_text case_sens h s then html_highlight case_sens h s else s
  | _ -> s

let rec eval_var conf base env _ _ =
  function
    ["count"] ->
      begin match get_env "count" env with
        Vcnt c -> VVstring (string_of_int !c)
      | _ -> VVstring ""
      end
  | ["first_name"] ->
      begin match get_env "info" env with
        Vinfo (_, _, _, HI_ind p, _) -> VVstring (p_first_name base p)
      | _ -> VVstring ""
      end
  | ["found"] ->
      begin match get_env "search" env with
        Vsearch (Some _) -> VVbool true
      | _ -> VVbool false
      end
  | ["incr_count"] ->
      begin match get_env "count" env with
        Vcnt c -> incr c; VVstring ""
      | _ -> VVstring ""
      end
  | ["is_note"] ->
      begin match get_env "info" env with
        Vinfo (_, _, _, HI_notes (_, _), _) -> VVbool true
      | _ -> VVbool false
      end
  | ["key"] ->
      begin match get_env "info" env with
        Vinfo (_, _, _, _, s) -> VVstring (possibly_highlight env s)
      | _ -> raise Not_found
      end
  | "note" :: "page" :: sl ->
      begin match get_env "info" env with
        Vinfo (_, _, _, HI_notes (s, _), _) ->
          let s =
            match sl with
              ["v"] -> s
            | [] -> possibly_highlight env s
            | _ -> raise Not_found
          in
          VVstring s
      | _ -> raise Not_found
      end
  | ["note"; "part"] ->
      begin match get_env "info" env with
        Vinfo (_, _, _, HI_notes (_, Some x), _) -> VVstring (string_of_int x)
      | Vinfo (_, _, _, HI_notes (_, None), _) -> VVstring ""
      | _ -> raise Not_found
      end
  | ["occ"] ->
      begin match get_env "info" env with
        Vinfo (_, _, _, HI_ind p, _) -> VVstring (string_of_int (get_occ p))
      | _ -> VVstring ""
      end
  | "person" :: sl ->
      begin match get_env "info" env with
        Vinfo (_, _, _, HI_ind p, _) ->
          eval_person_field_var conf base env p sl
      | _ -> raise Not_found
      end
  | ["pos"] ->
      begin match get_env "pos" env with
        Vpos r -> VVstring (string_of_int !r)
      | _ -> raise Not_found
      end
  | ["reset_count"] ->
      begin match get_env "count" env with
        Vcnt c -> c := 0; VVstring ""
      | _ -> VVstring ""
      end
  | ["surname"] ->
      begin match get_env "info" env with
        Vinfo (_, _, _, HI_ind p, _) -> VVstring (p_surname base p)
      | _ -> VVstring ""
      end
  | ["time"] ->
      begin match get_env "info" env with
        Vinfo (s, _, _, _, _) -> VVstring (possibly_highlight env s)
      | _ -> raise Not_found
      end
  | "update" :: sl ->
      begin match get_env "info" env with
        Vinfo (_, u, _, _, _) -> eval_string u sl
      | _ -> raise Not_found
      end
  | "user" :: sl ->
      begin match get_env "info" env with
        Vinfo (_, _, s, _, _) ->
          let s =
            match sl with
              ["v"] -> s
            | [] -> possibly_highlight env s
            | _ -> raise Not_found
          in
          VVstring s
      | _ -> raise Not_found
      end
  | _ -> raise Not_found
and eval_string s =
  function
    ["var"] -> VVother (eval_string s)
  | [] -> VVstring s
  | _ -> raise Not_found
and eval_person_field_var conf base env p =
  function
    ["access"] -> VVstring (Util.acces conf base p)
  | ["dates"] -> VVstring (DateDisplay.short_dates_text conf base p)
  | ["has_history"] ->
      let fn = sou base (get_first_name p) in
      let sn = sou base (get_surname p) in
      let occ = get_occ p in
      let person_file = HistoryDiff.history_file fn sn occ in
      VVbool (Sys.file_exists (HistoryDiff.history_path conf person_file))
  | ["history_file"] ->
      let fn = sou base (get_first_name p) in
      let sn = sou base (get_surname p) in
      let occ = get_occ p in VVstring (HistoryDiff.history_file fn sn occ)
  | ["is_invisible"] ->
      let conf = {conf with wizard = false; friend = false} in
      VVbool (not (Util.authorized_age conf base p))
  | ["title"] -> VVstring (person_title conf base p)
  | [] -> VVstring (possibly_highlight env (simple_person_text conf base p))
  | _ -> VVstring "person..."
and simple_person_text conf base p =
  match main_title conf base p with
    Some t -> titled_person_text conf base p t
  | None -> person_text conf base p

let print_foreach conf base print_ast eval_expr =
  let eval_int_expr env ep e =
    let s = eval_expr env ep e in
    try int_of_string s with Failure _ -> raise Not_found
  in
  let rec print_foreach env xx _ s sl el al =
    match s, sl with
      "history_line", [] -> print_foreach_history_line env xx el al
    | _, _ -> raise Not_found
  and print_foreach_history_line env xx el al =
    match
      try Some (Secure.open_in_bin (file_name conf)) with Sys_error _ -> None
    with
      Some ic ->
        begin try
          let (k, pos, wiz) =
            match el with
              [[e1]; [e2]; [e3]] ->
                let k = eval_int_expr env xx e1 in
                let pos =
                  match get_env "search" env with
                    Vsearch (Some (_, _, pos)) -> pos
                  | Vsearch None -> in_channel_length ic
                  | _ ->
                      try eval_int_expr env xx e2 with
                        Not_found -> in_channel_length ic
                in
                let wiz = eval_expr env xx e3 in k, pos, wiz
            | [] -> 3, in_channel_length ic, ""
            | _ -> raise Not_found
          in
          let pos =
            let vv = ref (Bytes.create 0), ref 0 in
            let rec loop pos i =
              if i >= k then pos
              else
                match
                  try Some (rev_input_line ic pos vv) with
                    Begin_of_file -> None
                with
                  Some (line, pos) ->
                    let i = print_history_line2 env xx line wiz i al in
                    loop pos i
                | None -> pos
            in
            loop pos 0
          in
          begin match get_env "pos" env with
            Vpos r -> r := pos
          | _ -> ()
          end;
          close_in ic
        with e -> close_in ic; raise e
        end
    | None -> ()
  and print_history_line2 env xx line wiz i al =
    match line_fields line with
      Some (time, user, action, keyo) ->
        if wiz = "" || user = wiz then
          let hist_item =
            match keyo with
              Some key ->
                begin match action with
                  "mn" ->
                    let (i, j) =
                      try let i = String.rindex key '/' in i, i + 1 with
                        Not_found -> 0, 0
                    in
                    let pg = String.sub key 0 i in
                    let s = String.sub key j (String.length key - j) in
                    begin try HI_notes (pg, Some (int_of_string s)) with
                      Failure _ -> HI_notes (key, None)
                    end
                | _ ->
                    match Gutil.person_ht_find_all base key with
                      [ip] -> HI_ind (pget conf base ip)
                    | _ -> HI_none
                end
            | None -> HI_none
          in
          let not_displayed =
            match hist_item with
              HI_ind p ->
                is_hidden p ||
                is_hide_names conf p && not (authorized_age conf base p)
            | _ -> false
          in
          if not_displayed then i
          else
            let key =
              match keyo with
                Some s -> s
              | None -> ""
            in
            let env =
              ("info", Vinfo (time, action, user, hist_item, key)) :: env
            in
            List.iter (print_ast env xx) al; i + 1
        else i
    | None -> i
  in
  print_foreach

let gen_print conf base hoo =
  let env =
    let env = ["pos", Vpos (ref 0); "count", Vcnt (ref 0)] in
    match hoo with
      Some ho -> ("search", Vsearch ho) :: env
    | None -> env
  in
  Hutil.interp conf "updhist"
    {Templ.eval_var = eval_var conf base;
     Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
     Templ.eval_predefined_apply = (fun _ -> raise Not_found);
     Templ.get_vother = get_vother; Templ.set_vother = set_vother;
     Templ.print_foreach = print_foreach conf base}
    env ()

let print conf base = gen_print conf base None

(* searching *)

let search_text conf base s =
  let s = if s = "" then " " else s in
  let case_sens = p_getenv conf.env "c" = Some "on" in
  let found =
    match
      try Some (Secure.open_in_bin (file_name conf)) with Sys_error _ -> None
    with
      Some ic ->
        let pos =
          match p_getint conf.env "pos" with
            Some pos -> pos
          | None -> in_channel_length ic
        in
        let vv = ref (Bytes.create 0), ref 0 in
        let rec loop pos =
          match
            try Some (rev_input_line ic pos vv) with Begin_of_file -> None
          with
            Some (line, pos2) ->
              begin match line_fields line with
                Some (time, user, _, keyo) ->
                  let key =
                    match keyo with
                      Some key -> key
                    | None -> ""
                  in
                  if in_text case_sens s time || in_text case_sens s user ||
                     in_text case_sens s key
                  then
                    Some pos
                  else loop pos2
              | None -> None
              end
          | None -> None
        in
        loop pos
    | None -> None
  in
  let h =
    match found with
      Some pos -> Some (case_sens, s, pos)
    | None -> None
  in
  gen_print conf base (Some h)

let print_search conf base =
  if conf.wizard || conf.friend then
    match try Some (List.assoc "s" conf.env) with Not_found -> None with
      Some s -> search_text conf base (Mutil.gen_decode false s)
    | None -> print conf base
  else print conf base
