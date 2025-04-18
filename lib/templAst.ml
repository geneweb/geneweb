type ast =
  | Atext of loc * string
  | Avar of loc * string * string list
  | Atransl of loc * bool * string * string
  | Aconcat of loc * ast list
  | Awid_hei of string
  | Aif of ast * ast list * ast list
  | Aforeach of (loc * string * string list) * ast list list * ast list
  | Afor of string * ast * ast * ast list
  | Adefine of string * (string * ast option) list * ast list * ast list
  | Aapply of loc * string * (string option * ast list) list
  | Alet of string * ast list * ast list
  | Aop1 of loc * string * ast
  | Aop2 of loc * string * ast * ast
  | Aint of loc * string
  | Ainclude of string * ast list

and loc = string * int * int

type 'a expr_val =
  | VVbool of bool
  | VVstring of string
  | VVother of (string list -> 'a expr_val)

(* TODO: remove these duplications *)
(* DUPLICATION FROM UTIL *)

let search_in_path p s =
  let rec loop = function
    | d :: dl ->
        let f = Filename.concat d s in
        if Sys.file_exists f then f else loop dl
    | [] -> s
  in
  loop (p ())

let search_in_assets = search_in_path Secure.assets
let p_getenv env label = Option.map Mutil.decode (List.assoc_opt label env)

(* DUPLICATION FROM UTIL *)

(* ************************************************************************ *)
(*  [Fonc] etc_file_name : config -> string -> string                       *)

(* ************************************************************************ *)

(** [Description] : Renvoie le chemin vers le fichier de template passé
                    en paramètre.
    [Args] :
      - conf  : configuration de la base
      - fname : le fichier de template
    [Retour] :
      - string : le chemin vers le fichier de template

    On cherche le fichier dans cet ordre :
    etc_d vaut :
    - bases/etc/mybase       en mode classique
    - bases/mybase.gwb/etc/  en mode reorg
    on cherche dans :
    - etc_d/templx/name.txt  (base specific)
    - etc_d/name.txt         (base specific)
    - gw/etc/templx/name.txt (distribution)
    - gw/etc/name.txt        (distribution)

    [Rem] : Exporté en clair hors de ce module.                             *)

let etc_file_name conf fname =
  let open Config in
  (* On recherche si dans le nom du fichier, on a specifié son *)
  (* répertoire, i.e. si fname est écrit comme ceci : dir/file *)
  (* on le reconstitue avec le bon dir_separateur *)
  let fname =
    List.fold_left Filename.concat "" (String.split_on_char '/' fname)
  in
  let file_exist dir =
    (* etc_d/templx/name.txt or etc_d/name.txt *)
    let fn =
      String.concat Filename.dir_sep
        (if dir <> "" then [ !GWPARAM.etc_d conf.bname; dir; fname ^ ".txt" ]
        else [ !GWPARAM.etc_d conf.bname; fname ^ ".txt" ])
    in
    if Sys.file_exists fn then fn
    else
      (* etc_d/name.txt *)
      let fn =
        String.concat Filename.dir_sep
          [ !GWPARAM.etc_d conf.bname; fname ^ ".txt" ]
      in
      (* on a déjà testé le cas dir = "" *)
      if dir <> "" && Sys.file_exists fn then fn
      else
        (* assets/templx/name.txt or assets/name.txt *)
        let fn =
          search_in_assets
            (String.concat Filename.dir_sep
               (if dir <> "" then [ "etc"; dir; fname ^ ".txt" ]
               else [ "etc"; fname ^ ".txt" ]))
        in
        if Sys.file_exists fn then fn
        else
          (* assets/name.txt *)
          let fn =
            search_in_assets
              (String.concat Filename.dir_sep [ "etc"; fname ^ ".txt" ])
          in
          (* on a déjà testé le cas dir = "" *)
          if dir <> "" && Sys.file_exists fn then fn else ""
  in
  (* Recherche le fichier template par défaut dans la liste des      *)
  (* dossiers template définis par la variable gwf                   *)
  (* template = templ1,templ2,*                                      *)
  (* la valeur * autorise tous les templates                         *)
  let rec default_templ config_templ std_fname =
    match config_templ with
    | [] | [ "*" ] -> std_fname
    | x :: l -> (
        match file_exist x with "" -> default_templ l std_fname | s -> s)
  in
  let config_templ =
    try
      let s = List.assoc "template" conf.base_env in
      let rec loop list i len =
        if i = String.length s then List.rev (Buff.get len :: list)
        else if s.[i] = ',' then loop (Buff.get len :: list) (i + 1) 0
        else loop list (i + 1) (Buff.store len s.[i])
      in
      loop [] 0 0
    with Not_found -> [ conf.bname; "*" ]
  in
  (* the current template folder *)
  let dir =
    match p_getenv conf.env "templ" with
    | Some x when List.mem "*" config_templ -> x
    | Some x when List.mem x config_templ -> x
    | Some _ | None -> (
        match config_templ with [] | [ "*" ] -> "" | x :: _ -> x)
  in
  (* default template file (gw/etc/fname.txt) *)
  let std_fname = search_in_assets (Filename.concat "etc" (fname ^ ".txt")) in

  (* On cherche le fichier template dans l'ordre de file_exist.   *)
  (* Si on ne trouve rien, alors on cherche le premier template   *)
  (* par défaut tel que défini par la variable template du gwf    *)
  match file_exist dir with
  | "" -> default_templ config_templ std_fname
  | s -> s
