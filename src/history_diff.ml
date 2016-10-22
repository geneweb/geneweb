(* camlp5r ./pa_html.cmo *)
(* $Id: history_diff.ml,v 0.01 2012-12-20 14:34:44 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open TemplAst;
open Util;


type gen_record =
  { date : string;
    wizard : string;
    gen_p : gen_person iper string;
    gen_f : list (gen_family iper string);
    gen_c : list (array iper) }
;


(* Le nom du fichier historique (à partir de la clé personne). *)
value history_file fn sn occ =
  let space_to_unders = Mutil.tr ' ' '_' in
  let f = space_to_unders (Name.lower fn) in
  let s = space_to_unders (Name.lower sn) in
  f ^ "." ^ string_of_int occ ^ "." ^ s
;

(* Le chemin du dossier history_d. *)
value history_d conf =
  let path =
    match p_getenv conf.base_env "history_path" with
    [ Some path -> path
    | None -> "" ]
  in
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  List.fold_left
    Filename.concat path [Util.base_path [] bname; "history_d"]
;

(* Le chemin du fichier historique dans le dossier history_d. *)
value history_path conf fname =
  if String.length fname >= 6 then
    let dirs =
      [history_d conf; String.make 1 fname.[0]; String.make 1 fname.[1]]
    in
    List.fold_right Filename.concat dirs fname
  else Filename.concat (history_d conf) fname
;

(* Créé tous les dossiers intermédiaires. *)
value create_history_dirs conf fname =
  if String.length fname >= 6 then
    let dirs =
      [history_d conf; String.make 1 fname.[0]; String.make 1 fname.[1]]
    in
    Mutil.mkdir_p (List.fold_left Filename.concat "" dirs)
  else ()
;


(* ************************************************************************ *)
(*  [Fonc] write_history_file : config -> string -> gen_record -> unit      *)
(** [Description] : Enregistre la personne dans son fichier historique.
    [Args] :
      - fname : le chemin du fichier
      - gr : le contenu de la personne
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value write_history_file conf person_file fname gr =
  (* On créé toujours les dossiers nécessaires (changement de clé ...). *)
  let () = create_history_dirs conf person_file in
  let ext_flags =
    [Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname)
    with [ Sys_error _ -> None ]
  with
  [ Some oc -> do { output_value oc (gr : gen_record); close_out oc }
  | None -> () ]
;


(* ************************************************************************ *)
(*  [Fonc] make_gen_record :
             config -> base -> bool -> gen_person -> gen_record             *)
(** [Description] : Crée un gen_record à partir d'une personne.
    [Args] :
      - conf : configuratino de la base
      - base : base de donnée
      - first : booléen pour savoir si c'est la première entrée de
                l'historique. Si c'est le cas, on ne connait pas la date de
                modification, donc on met "environ" une seconde avant.
      - gen_p : gen_person
    [Retour] :
      - gen_record
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value make_gen_record conf base first gen_p =
  let (hh, mm, ss) = conf.time in
  let (hh, mm, ss) =
    (* On évite les calculs savant pour la date (ss - 1 avec une date *)
    (* autour de minuit ...). C'est simplement une indication.        *)
    if first then (hh, mm, min 0 ss) else (hh, mm, ss)
  in
  let date =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      conf.today.year conf.today.month conf.today.day hh mm ss
  in
  let p = poi base gen_p.key_index in
  let fam = get_family p in
  (* On fait en sorte qu'il y a une 'bijection' *)
  (* entre les familles et les enfants.         *)
  let (gen_f, gen_c) =
    List.fold_right
      (fun ifam (accu_fam, accu_child) ->
        let fam = foi base ifam in
        let children = get_children fam in
        let gen_f = gen_family_of_family fam in
        ([Util.string_gen_family base gen_f :: accu_fam],
         [children :: accu_child]))
      (Array.to_list fam) ([], [])
  in
  { date = date; wizard = conf.user; gen_p = gen_p;
    gen_f = gen_f; gen_c = gen_c }
;


(* ************************************************************************ *)
(*  [Fonc] record_diff : config -> base -> base_changed -> unit             *)
(** [Description] : Met à jour le fichier historique d'une personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - changed : le type de modification (voir def.mli)
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value record_diff conf base changed =
  match p_getenv conf.base_env "history_diff" with
  [ Some "yes" when not conf.manitou ->
      match changed with
      [ U_Add_person p ->
          let person_file = history_file p.first_name p.surname p.occ in
          let fname = history_path conf person_file in
          let gr = make_gen_record conf base False p in
          write_history_file conf person_file fname gr
      | U_Modify_person o p ->
          let o_person_file = history_file o.first_name o.surname o.occ in
          let person_file = history_file p.first_name p.surname p.occ in
          let ofname = history_path conf o_person_file in
          let fname = history_path conf person_file in
          do {
            (* La clé a changé, on reprend l'ancien historique. *)
            if o_person_file <> person_file && Sys.file_exists ofname then
              try Sys.rename ofname fname with [ Sys_error _ -> () ]
            else ();
            let gr = make_gen_record conf base False p in
            if Sys.file_exists fname then
              write_history_file conf person_file fname gr
            else do {
              let o_gr = make_gen_record conf base True o in
              write_history_file conf person_file fname o_gr;
              write_history_file conf person_file fname gr;
            }
          }
      | U_Delete_person _ -> () (* Faut-il supprimer l'historique ? *)
      | U_Merge_person _ o p ->
          let o_person_file = history_file o.first_name o.surname o.occ in
          let person_file = history_file p.first_name p.surname p.occ in
          let fname = history_path conf person_file in
          let gr = make_gen_record conf base False p in
          (* La clé a changé avec la fusion, on reprend l'ancien historique. *)
          if o_person_file <> person_file then do {
            let ofname = history_path conf o_person_file in
            try Sys.rename ofname fname with [ Sys_error _ -> () ];
            write_history_file conf person_file fname gr
          }
          else write_history_file conf person_file fname gr
      | U_Delete_family p f -> ()
      | U_Add_family p f | U_Modify_family p _ f
      | U_Merge_family p _ _ f | U_Add_parent p f ->
          let p_file = history_file p.first_name p.surname p.occ in
          let p_fname = history_path conf p_file in
          let cpl = foi base f.fam_index in
          let isp = Gutil.spouse p.key_index cpl in
          let sp = poi base isp in
          let sp_file =
            history_file
              (sou base (get_first_name sp))
              (sou base (get_surname sp))
              (get_occ sp)
          in
          let sp_fname = history_path conf sp_file in
          let gen_sp = gen_person_of_person sp in
          let gen_sp = Util.string_gen_person base gen_sp in
          do {
            let gr = make_gen_record conf base False p in
            write_history_file conf p_file p_fname gr;
            let gr = make_gen_record conf base False gen_sp in
            write_history_file conf sp_file sp_fname gr;
            (* Création des fichiers pour les enfants ajoutés. *)
            List.iter
              (fun ip ->
                let p = poi base ip in
                let person_file =
                  history_file
                    (sou base (get_first_name p))
                    (sou base (get_surname p))
                    (get_occ p)
                in
                let fname = history_path conf person_file in
                if Sys.file_exists fname then ()
                else
                  let gen_p = gen_person_of_person p in
                  let gen_p = Util.string_gen_person base gen_p in
                  let gr = make_gen_record conf base False gen_p in
                  write_history_file conf person_file fname gr)
              (Array.to_list (get_children cpl))
          }
      | U_Change_children_name _ list ->
          List.iter
            (fun ((ofn, osn, oocc, oip), (fn, sn, occ, ip)) ->
               let o_person_file = history_file ofn osn oocc in
               let person_file = history_file fn sn occ in
               if o_person_file <> person_file then
                 do {
                   let ofname = history_path conf o_person_file in
                   let fname = history_path conf person_file in
                   try Sys.rename ofname fname with [ Sys_error _ -> () ];
                   let p = poi base ip in
                   let p =
                     Futil.map_person_ps
                       (fun p -> p) (sou base) (gen_person_of_person p)
                   in
                   let gr = make_gen_record conf base False p in
                   write_history_file conf person_file fname gr
                 }
               else ())
            list
      | U_Multi p ->
          let person_file = history_file p.first_name p.surname p.occ in
          let fname = history_path conf person_file in
          let gr = make_gen_record conf base False p in
          write_history_file conf person_file fname gr
      | _ -> () ]
  | _ -> () ]
;


(* avec zip ? *)
(*
  let history = ref [] in
  let fname = history_path conf fname in
  if extract_zfile fname then
    do {
      read_history_file fname
      Sys.remove fname
    }
  else ();
  history.val
*)

(* ************************************************************************ *)
(*  [Fonc] load_person_history : config -> string -> gen_record list        *)
(** [Description] : Charge la liste des modifications pour une personne.
      L'avantage est que les versions les plus récentes se trouvent en
      tête de liste.
    [Args] :
      - conf  : configuration de la base
      - fname : le nom du fichier historique
    [Retour] :
      - gen_record list
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value load_person_history conf fname = do {
  let history = ref [] in
  let fname = history_path conf fname in
  match try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do {
        try
          while True do {
            let v : gen_record = input_value ic in
            history.val := [v :: history.val]
          }
        with [ End_of_file -> () | Failure "input_value: truncated object" -> () ]; (* https://caml.inria.fr/mantis/view.php?id=7142 *)
        close_in ic
      }
  | None -> () ];
  history.val
};


(* ************************************************************************ *)
(*  [Fonc] print_clean : config -> base -> unit                             *)
(** [Description] :
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value print_clean conf base =
  match p_getenv conf.env "f" with
  [ Some f when f <> "" ->
      do {
        let title _ =
          Wserver.wprint "%s" (capitale (transl conf "clean history"))
        in
        Hutil.header conf title;
        Hutil.print_link_to_welcome conf True;
        Util.gen_print_tips conf
          (capitale
             (transl conf
                "select the input you want to erase from the history"));
        let history = load_person_history conf f in
        tag "form" "method=\"post\" action=\"%s\"" conf.command begin
          xtag "input" "type=\"hidden\" name=\"m\" value=\"HIST_CLEAN_OK\"" ;
          xtag "input" "type=\"hidden\" name=\"f\" value=\"%s\"" f;
          tag "ul" begin
            loop 0 history where rec loop i = fun
              [ [] -> ()
              | [gr :: l] ->
                  do {
                    tag "li" begin
                      tag "label" begin
                        xtag "input" "type=\"checkbox\" name=\"i%d\" value=\"on\"" i;
                        Wserver.wprint "%s %s" gr.date gr.wizard;
                      end;
                    end;
                    loop (i + 1) l
                  } ];
          end;
          xtag "input" "type=\"submit\" value=\"Ok\"";
        end;
        Hutil.trailer conf
      }
  | _ -> Hutil.incorrect_request conf ]
;


(* avec zip ? *)
(*
  let history = clean_history in
  let fname = history_path conf fname in
  if compress_zfile fname then
    do {
      write_history_file fname history;
      Sys.remove fname
    }
  else ();
*)

(* ************************************************************************ *)
(*  [Fonc] print_clean_ok : config -> base -> unit                          *)
(** [Description] : Ré-écrit le fichier historique lié à une personne en
      ayant supprimé les entrées non désirées.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
value print_clean_ok conf base =
  let rec clean_history i history new_history =
    match history with
    [ [] -> new_history
    | [gr :: l] ->
        let lab = "i" ^ string_of_int i in
        if p_getenv conf.env lab = Some "on" then
          clean_history (i + 1) l new_history
        else clean_history (i + 1) l [gr :: new_history] ]
  in
  match p_getenv conf.env "f" with
  [ Some f when f <> "" ->
      do {
        let title _ =
          Wserver.wprint "%s" (capitale (transl conf "history cleaned"))
        in
        Hutil.header conf title;
        Hutil.print_link_to_welcome conf True;
        let history = load_person_history conf f in
        let new_history = clean_history 0 history [] in
        let fname = history_path conf f in
        if new_history = [] then
          try Sys.remove fname with [ Sys_error _ -> () ]
        else
          let ext_flags =
            [Open_wronly; Open_trunc; Open_creat; Open_binary; Open_nonblock]
          in
          match
            try Some (Secure.open_out_gen ext_flags 0o644 fname)
            with [ Sys_error _ -> None ]
          with
          [ Some oc -> do {
              List.iter (fun v -> output_value oc (v : gen_record)) new_history;
              close_out oc }
          | None -> () ];
        Hutil.trailer conf
      }
  | _ -> Hutil.incorrect_request conf ]
;


(**/**) (* Template *)


value person_of_gen_p_key base gen_p =
  match person_of_key base gen_p.first_name gen_p.surname gen_p.occ with
  [ Some ip -> poi base ip
  | None -> Gwdb.empty_person base (Adef.iper_of_int (-1)) ]
;

(* N'est pas forcément très précis. En effet, on enregistre que     *)
(* les ipers. Or lors d'un nettoyage de la base, il se peut que     *)
(* ces ipers changent. On peut donc pointer vers une autre persone. *)
value person_of_iper conf base ip =
  try
    let p = pget conf base ip in
    if authorized_age conf base p then Util.person_text conf base p
    else ""
  with _ -> ""
;

value person_of_iper_list conf base ipl =
  let list =
    List.fold_right
      (fun ip accu ->
        let p = person_of_iper conf base ip in
        if p = "" then accu
        else [p :: accu])
      ipl []
  in
  String.concat ", " list
;


value string_of_codate conf cod =
  match Adef.od_of_codate cod with
  [ Some d -> Date.string_slash_of_date conf d
  | None -> "" ]
;

value string_of_death conf death =
  match death with
  [ Death _ cd -> Date.string_slash_of_date conf (Adef.date_of_cdate cd)
  | _ -> "" ]
;

value string_of_burial conf burial =
  match burial with
  [ Buried cod | Cremated cod -> string_of_codate conf cod
  | _ -> "" ]
;

value string_of_title conf titles =
  let string_of_t_name t =
    match t.t_name with
    [ Tname s -> s
    | _ -> "" ]
  in
  let one_title t =
    let name = t.t_ident ^ " " ^ t.t_place in
    let name = if name = " " then "" else name in
    let dates =
      string_of_codate conf t.t_date_start ^ "-" ^
        string_of_codate conf t.t_date_end
    in
    let dates = if dates = "-" then "" else "(" ^ dates ^ ")" in
    let nth = if t.t_nth = 0 then "" else string_of_int t.t_nth in
    let nth =
      if string_of_t_name t = "" then nth
      else string_of_t_name t ^ " " ^ string_of_int t.t_nth
    in
    let nth = if nth = "" || nth = " " then "" else "[" ^ nth ^ "]" in
    name ^ (if name = "" then "" else " ") ^ nth ^
      (if nth = "" then "" else " ") ^ dates
  in
  List.fold_left
    (fun accu t ->
      if accu = "" then one_title t
      else accu ^ ", " ^ one_title t)
    "" titles
;

value string_of_related conf base ip related =
  let related =
    List.fold_right
      (fun ic accu ->
        let p = person_of_iper conf base ip in
        if p = "" then accu
        else
          (* Si l'enfant n'existe plus. *)
          let c = try pget conf base ic with _ -> Gwdb.empty_person base ic in
          let rel =
            loop (get_rparents c) where rec loop rp =
              match rp with
              [ [r :: l] ->
                  match r.r_fath with
                  [ Some ifath when ifath = ip ->
                      Util.rchild_type_text conf r.r_type 2
                  | _ -> loop l ]
              | [] -> "" ]
          in
          [capitale rel ^ ": " ^ p :: accu])
      related []
  in
  String.concat ", " related
;

value string_of_rparents conf base rparents =
  let rparents =
    List.fold_right
      (fun rp accu ->
        match (rp.r_fath, rp.r_moth) with
        [ (Some ip1, Some ip2) ->
            let rel = capitale (Util.relation_type_text conf rp.r_type 2) in
            let fath = person_of_iper conf base ip1 in
            let moth = person_of_iper conf base ip2 in
            match (fath, moth) with
            [ ("", "") -> accu
            | (p, "") -> [rel ^ ": " ^ p :: accu]
            | ("", p) -> [rel ^ ": " ^ p :: accu]
            | (p1, p2) -> [rel ^ ": " ^ p1 ^ ", " ^ p2 :: accu] ]
        | (Some ip, None) ->
            let p = person_of_iper conf base ip in
            if p = "" then accu
            else
              let rel = capitale (Util.relation_type_text conf rp.r_type 2) in
              [rel ^ ": " ^ p :: accu]
        | (None, Some ip) ->
            let p = person_of_iper conf base ip in
            if p = "" then accu
            else
              let rel = capitale (Util.relation_type_text conf rp.r_type 2) in
              [rel ^ ": " ^ p :: accu]
        | (None, None) -> accu ])
      rparents []
  in
  String.concat ", " rparents
;

value string_of_marriage conf marriage =
  match marriage with
  [ NotMarried | NoSexesCheckNotMarried -> transl conf "with"
  | Married | NoSexesCheckMarried -> transl conf "married"
  | Engaged -> transl conf "engaged"
  | NoMention -> transl conf "with" ]
;

value string_of_divorce conf divorce =
  match divorce with
  [ NotDivorced -> ""
  | Divorced cod -> transl conf "divorced" ^ " " ^ string_of_codate conf cod
  | Separated -> transl conf "separated" ]
;


(* ************************************************************************ *)
(*  [Fonc] array_of_string : string -> char array                           *)
(** [Description] : Converti une string en tableau de char afin de pouvoir
      faire un diff.
    [Args] :
      - s : string à convertir
    [Retour] :
      - char array
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value array_of_string s =
  let len = String.length s in
  let a = Array.make len ' ' in
  loop 0 where rec loop i =
    if i = len then a
    else do {
      a.(i) := s.[i];
      loop (i + 1)
    }
;


(* ************************************************************************ *)
(*  [Fonc] highlight_diff : char array -> bool array -> string              *)
(** [Description] : Converti un tableau de char en string, avec les parties
      modifiées encadrées par des balises <span>.
    [Args] :
      - arr : tableau à convertir
      - diff_arr : tableau des différences
    [Retour] :
      - string
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value highlight_diff arr diff_arr =
  loop 0 "" where rec loop i s =
    if i >= Array.length arr then s
    else if diff_arr.(i) then do {
      let j = ref i in
      let accu = ref s in
      accu.val := accu.val ^ "<span class=\"diff_highlight\">";
      while j.val < Array.length diff_arr && diff_arr.(j.val) do {
        accu.val := accu.val ^ Printf.sprintf "%c" arr.(j.val);
        incr j
      };
      accu.val := accu.val ^ "</span>";
      loop j.val accu.val
    }
    else
      loop (i + 1) (s ^ Printf.sprintf "%c" arr.(i))
;


(* ************************************************************************ *)
(*  [Fonc] diff_string : string -> string -> (string * string)              *)
(** [Description] : Renvoie les deux string avec mise en évidence des
      différences entre les deux.
    [Args] :
      - before : string avant modification
      - after  : string après modification
    [Retour] :
      - string * string
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value diff_string before after =
  if before = after then (before, after)
  else if before = "" then
    (before, "<span class=\"diff_highlight\">" ^ after ^ "</span>")
  else if after = "" then
    ("<span class=\"diff_highlight\">" ^ before ^ "</span>", after)
  else
    let aa = array_of_string after in
    let bb = array_of_string before in
    let (bef_d, aft_d) = Diff.f bb aa in
    let bef_s = highlight_diff bb bef_d in
    let aft_s = highlight_diff aa aft_d in
    (bef_s, aft_s)
;


type env 'a =
  [ Vgen_record of gen_record
  | Vfam of option (gen_family iper string) and option (gen_family iper string) and bool
  | Vchild of option (array iper) and option (array iper)
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vother of 'a
  | Vnone ]
;


value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];
value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;
value str_val x = VVstring x;
value bool_val x = VVbool x;

value rec eval_var conf base env (bef, aft, p_auth) loc sl =
  try eval_simple_var conf base env (bef, aft, p_auth) sl with
  [ Not_found -> eval_compound_var conf base env (bef, aft, p_auth) sl ]
and eval_simple_var conf base env (bef, aft, p_auth) =
  fun
  [ [s] -> str_val (eval_simple_str_var conf base env (bef, aft, p_auth) s)
  | _ -> raise Not_found ]
and eval_compound_var conf base env (bef, aft, p_auth) sl =
  let rec loop =
    fun
    [ [s] -> eval_simple_str_var conf base env (bef, aft, p_auth) s
    | ["evar"; s] ->
        match p_getenv conf.env s with
        [ Some s -> s
        | None -> "" ]
    | ["before" :: sl] ->
        fst (eval_gen_record conf base env (bef, aft, p_auth) sl)
    | ["after" :: sl] ->
        snd (eval_gen_record conf base env (bef, aft, p_auth) sl)
    | _ -> raise Not_found ]
  in
  str_val (loop sl)
and eval_gen_record conf base env (bef, aft, p_auth) =
  fun
  [ ["date"] -> (bef.date, aft.date)
  | ["wizard"] -> (bef.wizard, aft.wizard)
  | [s] -> eval_str_gen_record conf base env (bef, aft, p_auth) s
  | _ -> raise Not_found ]
and eval_str_gen_record conf base env (bef, aft, p_auth) =
  fun
  [ "first_name" ->
      if p_auth then
        let b = bef.gen_p.first_name in
        let a = aft.gen_p.first_name in
        diff_string b a
      else ("", "")
  | "surname" ->
      if p_auth then
        let b = bef.gen_p.surname in
        let a = aft.gen_p.surname in
        diff_string b a
      else ("", "")
  | "occ" ->
      if p_auth then
        let b = string_of_int bef.gen_p.occ in
        let a = string_of_int aft.gen_p.occ in
        diff_string b a
      else ("", "")
  | "image" ->
      if p_auth && not conf.no_image then
        let b = bef.gen_p.image in
        let a = aft.gen_p.image in
        diff_string b a
      else ("", "")
  | "public_name" ->
      if p_auth then
        let b = bef.gen_p.public_name in
        let a = aft.gen_p.public_name in
        diff_string b a
      else ("", "")
  | "qualifiers" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.qualifiers in
        let a = String.concat ", " aft.gen_p.qualifiers in
        diff_string b a
      else ("", "")
  | "aliases" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.aliases in
        let a = String.concat ", " aft.gen_p.aliases in
        diff_string b a
      else ("", "")
  | "first_names_aliases" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.first_names_aliases in
        let a = String.concat ", " aft.gen_p.first_names_aliases in
        diff_string b a
      else ("", "")
  | "surnames_aliases" ->
      if p_auth then
        let b = String.concat ", " bef.gen_p.surnames_aliases in
        let a = String.concat ", " aft.gen_p.surnames_aliases in
        diff_string b a
      else ("", "")
  | "titles" ->
      if p_auth then
        let b = string_of_title conf bef.gen_p.titles in
        let a = string_of_title conf aft.gen_p.titles in
        diff_string b a
      else ("", "")
  | "relations" ->
      if p_auth then
        let br =
          string_of_related conf base bef.gen_p.key_index bef.gen_p.related
        in
        let ar =
          string_of_related conf base aft.gen_p.key_index aft.gen_p.related
        in
        let brp = string_of_rparents conf base bef.gen_p.rparents in
        let arp = string_of_rparents conf base aft.gen_p.rparents in
        let b = if br = "" then brp else (br ^ ". " ^ brp) in
        let a = if ar = "" then arp else (ar ^ ". " ^ brp) in
        diff_string b a
      else ("", "")
  | "occupation" ->
      if p_auth then
        let b = bef.gen_p.occupation in
        let a = aft.gen_p.occupation in
        diff_string b a
      else ("", "")
  | "sex" ->
      if p_auth then
        let b =
          transl_nth
            conf "male/female/neuter" (Util.index_of_sex bef.gen_p.sex)
        in
        let a =
          transl_nth
            conf "male/female/neuter" (Util.index_of_sex aft.gen_p.sex)
        in
        diff_string b a
      else ("", "")
  | "access" ->
      if p_auth then
        let b =
          match bef.gen_p.access with
          [ IfTitles -> transl_nth conf "iftitles/public/private" 0
          | Public -> transl_nth conf "iftitles/public/private" 1
          | Private -> transl_nth conf "iftitles/public/private" 2 ]
        in
        let a =
          match aft.gen_p.access with
          [ IfTitles -> transl_nth conf "iftitles/public/private" 0
          | Public -> transl_nth conf "iftitles/public/private" 1
          | Private -> transl_nth conf "iftitles/public/private" 2 ]
        in
        diff_string b a
      else ("", "")
  | "birth" ->
      if p_auth then
        let b = string_of_codate conf bef.gen_p.birth in
        let a = string_of_codate conf aft.gen_p.birth in
        diff_string b a
      else ("", "")
  | "birth_place" ->
      if p_auth then
        let b = bef.gen_p.birth_place in
        let a = aft.gen_p.birth_place in
        diff_string b a
      else ("", "")
  | "birth_src" ->
      if p_auth then
        let b = bef.gen_p.birth_src in
        let a = aft.gen_p.birth_src in
        diff_string b a
      else ("", "")
  | "baptism" ->
      if p_auth then
        let b = string_of_codate conf bef.gen_p.baptism in
        let a = string_of_codate conf aft.gen_p.baptism in
        diff_string b a
      else ("", "")
  | "baptism_place" ->
      if p_auth then
        let b = bef.gen_p.baptism_place in
        let a = aft.gen_p.baptism_place in
        diff_string b a
      else ("", "")
  | "baptism_src" ->
      if p_auth then
        let b = bef.gen_p.baptism_src in
        let a = aft.gen_p.baptism_src in
        diff_string b a
      else ("", "")
  | "death" ->
      if p_auth then
        let b = string_of_death conf bef.gen_p.death in
        let a = string_of_death conf aft.gen_p.death in
        diff_string b a
      else ("", "")
  | "death_place" ->
      if p_auth then
        let b = bef.gen_p.death_place in
        let a = aft.gen_p.death_place in
        diff_string b a
      else ("", "")
  | "death_src" ->
      if p_auth then
        let b = bef.gen_p.death_src in
        let a = aft.gen_p.death_src in
        diff_string b a
      else ("", "")
  | "burial" ->
      if p_auth then
        let b = string_of_burial conf bef.gen_p.burial in
        let a = string_of_burial conf aft.gen_p.burial in
        diff_string b a
      else ("", "")
  | "burial_place" ->
      if p_auth then
        let b = bef.gen_p.burial_place in
        let a = aft.gen_p.burial_place in
        diff_string b a
      else ("", "")
  | "burial_src" ->
      if p_auth then
        let b = bef.gen_p.burial_src in
        let a = aft.gen_p.burial_src in
        diff_string b a
      else ("", "")
  | "notes" ->
      if p_auth && not conf.no_note then
        let b = bef.gen_p.notes in
        let a = aft.gen_p.notes in
        diff_string b a
      else ("", "")
  | "psources" ->
      if p_auth then
        let b = bef.gen_p.psources in
        let a = aft.gen_p.psources in
        diff_string b a
      else ("", "")
  | "spouse" ->
      match get_env "fam" env with
      [ Vfam f_bef f_aft m_auth ->
          if m_auth then
            (eval_string_env "spouse_bef" env,
             eval_string_env "spouse_aft" env)
          else ("", "")
      | _ -> raise Not_found ]
  | "marriage" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = string_of_codate conf b.marriage in
                let a = string_of_codate conf a.marriage in
                diff_string b a
            | (None, Some a) -> ("", string_of_codate conf a.marriage)
            | (Some b, None) -> (string_of_codate conf b.marriage, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "marriage_place" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = b.marriage_place in
                let a = a.marriage_place in
                diff_string b a
            | (None, Some a) -> ("", a.marriage_place)
            | (Some b, None) -> (b.marriage_place, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "marriage_src" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = b.marriage_src in
                let a = a.marriage_src in
                diff_string b a
            | (None, Some a) -> ("", a.marriage_src)
            | (Some b, None) -> (b.marriage_src, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "witnesses" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b =
                  person_of_iper_list conf base (Array.to_list b.witnesses)
                in
                let a =
                  person_of_iper_list conf base (Array.to_list a.witnesses)
                in
                diff_string b a
            | (None, Some a) ->
                ("", person_of_iper_list conf base (Array.to_list a.witnesses))
            | (Some b, None) ->
                (person_of_iper_list conf base (Array.to_list b.witnesses), "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "marriage_type" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = string_of_marriage conf b.relation in
                let a = string_of_marriage conf a.relation in
                diff_string b a
            | (None, Some a) -> ("", string_of_marriage conf a.relation)
            | (Some b, None) -> (string_of_marriage conf b.relation, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "divorce" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = string_of_divorce conf b.divorce in
                let a = string_of_divorce conf a.divorce in
                diff_string b a
            | (None, Some a) -> ("", string_of_divorce conf a.divorce)
            | (Some b, None) -> (string_of_divorce conf b.divorce, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "comment" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth && not conf.no_note then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = b.comment in
                let a = a.comment in
                diff_string b a
            | (None, Some a) -> ("", a.comment)
            | (Some b, None) -> (b.comment, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "origin_file" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = b.origin_file in
                let a = a.origin_file in
                diff_string b a
            | (None, Some a) -> ("", a.origin_file)
            | (Some b, None) -> (b.origin_file, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "fsources" ->
      match get_env "fam" env with
      [ Vfam bef aft m_auth ->
          if m_auth then
            match (bef, aft) with
            [ (Some b, Some a) ->
                let b = b.fsources in
                let a = a.fsources in
                diff_string b a
            | (None, Some a) -> ("", a.fsources)
            | (Some b, None) -> (b.fsources, "")
            | (None, None) -> ("", "") ]
          else ("", "")
      | _ -> raise Not_found ]
  | "children" ->
      match get_env "fam" env with
      [ Vfam _ _ m_auth ->
          if m_auth then
            match get_env "child" env with
            [ Vchild bef aft ->
                match (bef, aft) with
                [ (Some b, Some a) ->
                    let b = person_of_iper_list conf base (Array.to_list b) in
                    let a = person_of_iper_list conf base (Array.to_list a) in
                    diff_string b a
                | (None, Some a) ->
                    ("", person_of_iper_list conf base (Array.to_list a))
                | (Some b, None) ->
                    (person_of_iper_list conf base (Array.to_list b), "")
                | (None, None) -> ("", "") ]
            | _ -> raise Not_found ]
          else ("", "")
      | _ -> raise Not_found ]
  | _ -> raise Not_found ]
and eval_simple_str_var conf base env (bef, aft, p_auth) =
  fun
  [ "acces" ->
      let p = person_of_gen_p_key base aft.gen_p in
      acces conf base p
  | "date" -> eval_string_env "date" env
  | "history_len" -> eval_int_env "history_len" env
  | "line" -> eval_int_env "line" env
  | "nb_families" ->
      let nb_fam = max (List.length bef.gen_f) (List.length aft.gen_f) in
      string_of_int nb_fam
  | "person" ->
      if p_auth then
        let p = person_of_gen_p_key base aft.gen_p in
        Util.person_text conf base p
      else eval_string_env "history_file" env
  | "wizard" -> eval_string_env "wizard" env
  | _ -> raise Not_found ]
and eval_string_env s env =
  match get_env s env with
  [ Vstring s -> s
  | _ -> raise Not_found ]
and eval_int_env s env =
  match get_env s env with
  [ Vint i -> string_of_int i
  | _ -> raise Not_found ]
;

value print_foreach conf base print_ast eval_expr =
  let rec print_foreach env xx loc s sl el al =
    match [s :: sl] with
    [ ["family"] -> print_foreach_family env xx el al
    | ["history_line"] -> print_foreach_history_line env xx el al
    | _ -> raise Not_found ]
  and print_foreach_family env xx el al =
    let (bef, aft, p_auth) = xx in
    let rec loop bef_f bef_c aft_f aft_c =
      match (bef_f, aft_f) with
      [ ([], []) -> ()
      | ([], [gen_f :: l]) ->
          do {
            let fam = foi base gen_f.fam_index in
            let isp = Gutil.spouse aft.gen_p.key_index fam in
            let sp = person_of_iper conf base isp in
            let m_auth = authorized_age conf base (poi base isp) && p_auth in
            let vfam = Vfam None (Some gen_f) m_auth in
            let (vchild, c) =
              match (bef_c, aft_c) with
              [ ([], [gen_c :: l]) -> (Vchild None (Some gen_c), l)
              | _ -> (* pas normal*) (Vchild None None, []) ]
            in
            let env =
              [("fam", vfam); ("spouse_bef", Vstring "");
               ("spouse_aft", Vstring sp); ("child", vchild) :: env]
            in
            List.iter (print_ast env xx) al;
            loop [] bef_c l c
          }
      | ([gen_f :: l], []) ->
          do {
            let fam = foi base gen_f.fam_index in
            let isp = Gutil.spouse aft.gen_p.key_index fam in
            let sp = person_of_iper conf base isp in
            let m_auth = authorized_age conf base (poi base isp) && p_auth in
            let vfam = Vfam (Some gen_f) None m_auth in
            let (vchild, c) =
              match (bef_c, aft_c) with
              [ ([gen_c :: l], []) -> (Vchild (Some gen_c) None, l)
              | _ -> (* pas normal*) (Vchild None None, []) ]
            in
            let env =
              [("fam", vfam); ("spouse_bef", Vstring sp);
               ("spouse_aft", Vstring ""); ("child", vchild) :: env]
            in
            List.iter (print_ast env xx) al;
            loop l c [] aft_c
          }
      | ([gen_f1 :: l1], [gen_f2 :: l2]) ->
          do {
            let fam = foi base gen_f2.fam_index in
            let isp1 = Gutil.spouse bef.gen_p.key_index fam in
            let isp2 = Gutil.spouse aft.gen_p.key_index fam in
            let sp1 = person_of_iper conf base isp1 in
            let sp2 = person_of_iper conf base isp2 in
            let m_auth = authorized_age conf base (poi base isp2) && p_auth in
            let vfam = Vfam (Some gen_f1) (Some gen_f2) m_auth in
            let (vchild, c1, c2) =
              match (bef_c, aft_c) with
              [ ([gen_c1 :: l1], [gen_c2 :: l2]) ->
                  (Vchild (Some gen_c1) (Some gen_c2), l1, l2)
              | _ -> (* pas normal*) (Vchild None None, [], []) ]
            in
            let env =
              [("fam", vfam); ("spouse_bef", Vstring sp1);
               ("spouse_aft", Vstring sp2); ("child", vchild) :: env]
            in
            List.iter (print_ast env xx) al;
            loop l1 c1 l2 c2
          } ]
    in
    loop bef.gen_f bef.gen_c aft.gen_f aft.gen_c
  and print_foreach_history_line env xx el al =
    match get_env "history_file" env with
    [ Vstring fname ->
        let history = load_person_history conf fname in
        loop 0 history where rec loop i list =
          match list with
          [ [] -> ()
          | [gr :: l] ->
              let env =
                [("line", Vint i); ("date", Vstring gr.date);
                 ("wizard", Vstring gr.wizard) :: env]
              in
              do { List.iter (print_ast env xx) al; loop (i + 1) l } ]
    | _ -> () ]
  in
  print_foreach
;

value eval_predefined_apply conf env f vl =
  let vl = List.map (fun [ VVstring s -> s | _ -> raise Not_found ]) vl in
  match (f, vl) with
  [ ("transl_date", [date_txt]) ->
      (* date_tpl = "0000-00-00 00:00:00" *)
      try
        let year = int_of_string (String.sub date_txt 0 4) in
        let month = int_of_string (String.sub date_txt 5 2) in
        let day = int_of_string (String.sub date_txt 8 2) in
        let date =
          Dgreg
            {day = day; month = month; year = year; prec = Sure; delta = 0}
            Dgregorian
        in
        let time = String.sub date_txt 11 8 in
        Date.string_of_date conf date ^ ", " ^ time
      with [ Failure "int_of_string" -> date_txt ]
  | _ -> raise Not_found ]
;

value print conf base =
  match p_getenv conf.env "t" with
  [ Some ("SUM" | "DIFF") ->
      match p_getenv conf.env "f" with
      [ Some file when file <> "" ->
          let history = load_person_history conf file in
          let len = List.length history in
          let (before, after) =
            match (p_getint conf.env "old", p_getint conf.env "new") with
            [ (Some o, Some n) ->
                let o =
                  if o < 0 then 0 else if o > len - 1 then len - 1 else o
                in
                let n =
                  if n < 0 then 0 else if n > len - 1 then len - 1 else n
                in
                (o, n)
            | _ -> (0, 0) ]
          in
          let before = List.nth history before in
          let after = List.nth history after in
          let p = person_of_gen_p_key base after.gen_p in
          let p_auth = authorized_age conf base p in
          let env =
            [("history_file", Vstring file); ("history_len", Vint len)]
          in
          Hutil.interp conf base "updhist_diff"
            {Templ.eval_var = eval_var conf base;
             Templ.eval_transl _ = Templ.eval_transl conf;
             Templ.eval_predefined_apply = eval_predefined_apply conf;
             Templ.get_vother = get_vother; Templ.set_vother = set_vother;
             Templ.print_foreach = print_foreach conf base}
            env (before, after, p_auth)
      | _ -> Hutil.incorrect_request conf ]
  | _ -> Hutil.incorrect_request conf ]
;
