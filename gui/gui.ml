(* $Id: gui.ml,v 1.00 2011-12-25 15:36:35 flh Exp $ *)

open Printf;

type conf =
  { bases_dir : mutable string;
    port : mutable int;
    browser : mutable option string;
    log : mutable string;
    gui_arg : mutable list (string * string);
    gwd_arg : mutable list string;
    only_arg : mutable list string;
    server_running : mutable option int;
    waiting_pids : mutable list int }
;

value bin_dir =
  let path = Filename.dirname (Sys.argv.(0)) in
  if Filename.is_relative path then
    Filename.concat (Sys.getcwd ()) path
  else path
;

value trace = ref False;

value default_lang =
  try Sys.getenv "LC_ALL" with
  [ Not_found ->
      try Sys.getenv "LC_MESSAGES" with
      [ Not_found -> try Sys.getenv "LANG" with [ Not_found -> "en" ] ] ]
;

value lang = ref default_lang;
value lexicon_mtime = ref 0.0;
value lexicon_file = Filename.concat bin_dir "gui_lex.txt";

value config_gui_file = Filename.concat bin_dir "config.txt";
value config_gwd_file = Filename.concat bin_dir "gwd.arg";
value config_only_file = Filename.concat bin_dir "only.txt";


(**/**) (* Gestion du dictionnaire des langues pour GUI. *)

value input_lexicon lang = do {
  let ht = Hashtbl.create 501 in
  Mutil.input_lexicon lang ht (fun () -> open_in lexicon_file);
  ht
};

value unfreeze_lexicon =
  let lexicon = ref None in
  fun lang ->
    if Sys.file_exists lexicon_file then do {
      let stbuf = Unix.stat lexicon_file in
      if stbuf.Unix.st_mtime > lexicon_mtime.val then do {
        lexicon.val := None;
        lexicon_mtime.val := stbuf.Unix.st_mtime;
      }
      else ();
      match lexicon.val with
      [ Some lex -> Some lex
      | None -> do {
          let lex = input_lexicon lang in
          lexicon.val := Some lex;
          Some lex
        } ]
    }
    else None
;

value transl w =
  match unfreeze_lexicon lang.val with
  [ Some lex -> try Hashtbl.find lex w with [ Not_found -> "[" ^ w ^ "]" ]
  | None -> w ]
;

value capitale w = String.capitalize w ;


(**/**) (* Fonctions utiles. *)


value channel_redirector channel callback = do {
  let (cout,cin) = Unix.pipe () in
  Unix.dup2 cin channel ;
  let chan = GMain.Io.channel_of_descr cout in
  let len = 80 in
  let buf = Bytes.create len in
  GMain.Io.add_watch chan ~prio:0 ~cond:[`IN; `HUP; `ERR] ~callback:
    do { fun cond ->
      try
        if List.mem `IN cond then do {
	        (* On Windows, you must use Io.read *)
	        let len = GMain.Io.read chan ~buf ~pos:0 ~len in
	        len >= 1 && (callback (String.sub buf 0 len))
        }
        else False
      with [ _ -> False ]
  }
};


value exec prog args out err =
  Unix.create_process prog (Array.of_list [prog :: args]) Unix.stdin out err
;

value exec_wait conf prog args =
  do {
    let wnd = GWindow.window
      ~title:(capitale (transl "Processing"))
      ~position:`CENTER
      ~resizable:True
      ~width:600 ~height:300 ()
    in
    ignore (wnd#connect#destroy ~callback:(fun _ -> ()));
    wnd#show();
    let vbox = GPack.vbox
      ~spacing:5
      ~packing:wnd#add ()
    in
    let scrolled_window = GBin.scrolled_window ~border_width: 10
      ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
      ~packing:vbox#add ()
    in
    let vvbox = GPack.hbox
      ~border_width: 10
      ~packing:scrolled_window#add_with_viewport ()
    in
    vvbox #focus#set_vadjustment (Some scrolled_window#vadjustment);
    let redirect channel =
      let buffer = GText.buffer () in
      let _text = GText.view ~buffer ~editable:False ~packing:vvbox#add () in
      channel_redirector channel (fun c -> do {buffer#insert c; True})
    in
    ignore (redirect Unix.stderr);
    let pid = exec prog args Unix.stdout Unix.stderr in
    (* On voudrait bien attendre la fin du process
       mais sinon wnd ne s'affiche pas ...         *)
    let (_, _) = Unix.waitpid [] pid in
    ()
  }
;

value mkdir_p x =
  loop x where rec loop x =
    do  {
      let y = Filename.dirname x in
      if y <> x && String.length y < String.length x then loop y else ();
      try Unix.mkdir x 0o777 with [ Unix.Unix_error _ _ _ -> () ];
    }
;

value rmdir conf dir =
  (* Récupère tous les fichiers et dossier d'un dossier         *)
  (* et renvoie la liste des dossiers et la liste des fichiers. *)
  let read_files_folders fname =
    let list =
      List.map
        (fun file -> Filename.concat fname file)
        (Array.to_list (Sys.readdir fname))
    in
    List.partition Sys.is_directory list
  in
  (* Parcours récursif de tous les dossiers *)
  let rec loop l folders files =
    match l with
    [ [] -> (folders, files)
    | [x :: l] ->
        let (fd, fi) = read_files_folders x in
        let l = List.rev_append l fd in
        let folders = List.rev_append fd folders in
        let files = List.rev_append fi files in
        loop l folders files ]
  in
  (* Toute l'arborescence de dir *)
  let (folders, files) = loop [dir] [] [] in
  do {
    List.iter (fun f -> try Unix.unlink f with [ _ -> () ]) files;
    List.iter (fun f -> try Unix.rmdir f with [ _ -> () ]) folders;
    try Unix.rmdir dir with [ Unix.Unix_error _ _ _ -> () ]
  }
;

value rec cut_at_equal s =
  try
    let i = String.index s '=' in
    (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
  with
  [ Not_found -> (s, "") ]
;

value read_base_env conf bname =
  let fname = Filename.concat conf.bases_dir (bname ^ ".gwf") in
  match try Some (open_in fname) with [ Sys_error _ -> None] with
  [ Some ic ->
      let env =
        loop [] where rec loop env =
          match try Some (input_line ic) with [End_of_file -> None] with
          [ Some s ->
              if s = "" || s.[0] = '#' then loop env
              else loop [cut_at_equal s :: env]
          | None -> List.rev env ]
      in
      do { close_in ic; env }
  | None -> [] ]
;

value write_base_env conf bname env =
  let fname = Filename.concat conf.bases_dir (bname ^ ".gwf") in
  match try Some (open_out fname) with [ Sys_error _ -> None] with
  [ Some oc ->
      do {
        List.iter (fun (k, v) -> fprintf oc "%s=%s\n" k v) env;
        close_out oc
      }
  | None -> () ]
;

value write_config_file conf = do {
  let fname = Filename.concat bin_dir "config.txt" in
  match try Some (open_out fname) with [ Sys_error _ -> None] with
  [ Some oc ->
      do {
        List.iter (fun (k, v) -> fprintf oc "%s=%s\n" k v) conf.gui_arg;
        close_out oc;
      }
  | None -> () ]
};

value read_config_file () =
  match try Some (open_in config_gui_file) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop [] where rec loop env =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line ->
            let len = String.length line in
            if len = 0 then loop env
            else if line.[0] = '#' then loop env
            else
              let bind =
                try
                  let i = String.index line '=' in
                  (String.sub line 0 i,
                   String.sub line (i + 1) (len - i - 1))
                with
                [ Not_found -> (line, "") ]
              in
              loop [bind :: env]
        | None -> do { close_in ic; env } ]
  | None -> [] ]
;

value config_browser () =
  let default_browsers =
    match Sys.os_type with
    [ "Win32" | "Cygwin" ->
        ["C:\\Program Files\\Mozilla Firefox\\firefox.exe";
         "C:\\Program Files\\Internet Explorer\\iexplore.exe"]
    | _ -> ["/usr/bin/firefox"; "/usr/bin/mozilla"] ]
  in
  match List.filter Sys.file_exists default_browsers with
  [ [] -> None
  | [b :: l] -> Some b ]
;

value clean_waiting_pids conf =
  conf.waiting_pids :=
    List.filter
      (fun pid ->
         let (r, _) = Unix.waitpid [Unix.WNOHANG] pid in
         let _ =
           do {
             if r = pid then
               do { Printf.eprintf "--- pid ended %d\n" r; flush stderr }
             else ()
           }
         in
         r <> pid)
      conf.waiting_pids
;

value close_server conf =
  match conf.server_running with
  [ Some server_pid -> do {
      clean_waiting_pids conf;
      eprintf "Closing..."; flush stderr;
      (* Making a (empty) file STOP_SERVER to make the server stop. *)
      let stop_server =
        List.fold_left Filename.concat conf.bases_dir ["cnt"; "STOP_SERVER"]
      in
      let oc = open_out stop_server in
      close_out oc;
      (* Send a phony connection to unblock it. *)
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      try
        Unix.connect s (Unix.ADDR_INET Unix.inet_addr_loopback conf.port)
      with [ Unix.Unix_error _ _ _ -> () ];
      try Unix.close s with
      [ Unix.Unix_error _ _ _ -> () ];
      ignore (Unix.waitpid [] server_pid);
      conf.server_running := None;
      eprintf "\n"; flush stderr;
    }
  | None -> () ]
;


(**/**) (* Autres interfaces graphique ou utilitaires. *)

value select_dir parent initial_dir = do {
  let dialog = GWindow.file_chooser_dialog
    ~action:`SELECT_FOLDER
    ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  let new_dir =
    if dialog#run () = `OPEN then
      match dialog#filename with
      [ Some dir -> dir
      | _ -> initial_dir ]
    else initial_dir
  in
  dialog#destroy ();
  new_dir
};

value select_file parent initial_file = do {
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  let new_file =
    if dialog#run () = `OPEN then
      match dialog#filename with
      [ Some file -> file
      | _ -> initial_file ]
    else initial_file
  in
  dialog#destroy ();
  new_file
};

value error_popup msg = do {
  let wnd = GWindow.window
    ~title:(capitale (transl "error"))
    ~position:`CENTER
    ~resizable:True ()
  in
  ignore (wnd#connect#destroy ~callback:(fun _ -> ()));
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:wnd#add ()
  in
  let _label = GMisc.label
    ~text:msg
    ~packing:vbox#pack ()
  in
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `SPREAD
    ~packing: vbox#pack ()
  in
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:bbox#pack ()
  in
  ignore (btn_ok#connect#clicked (fun () -> wnd#destroy ()));
  wnd#show ();
};

value display_log conf = do {
  let wnd = GWindow.window
    ~title:(capitale (transl "log"))
    ~position:`CENTER
    ~resizable:True
    ~width:600 ~height:300 ()
  in
  ignore (wnd#connect#destroy ~callback:(fun _ -> ()));
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:wnd#add ()
  in
  let scrolled_window = GBin.scrolled_window ~border_width: 10
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
    ~packing:vbox#add ()
  in
  let vvbox = GPack.hbox
    ~border_width: 10
    ~packing:scrolled_window#add_with_viewport ()
  in
  vvbox #focus#set_vadjustment (Some scrolled_window#vadjustment);
  match try Some (open_in conf.log) with [Sys_error _ -> None] with
  [ Some ic ->
      do {
        let len = in_channel_length ic in
        (* TODO ! *)
        if len > 0 then
          do {
            let buf = Buffer.create len in
            Buffer.add_channel buf ic len;
            let text = GText.view ~packing:vvbox#add () in
            text#buffer#set_text (Buffer.contents buf)
          }
        else ();
        close_in ic;
      }
  | None -> () ];
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `EDGE
    ~packing: vbox#pack ()
  in
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:bbox#pack ()
  in
  ignore (btn_ok#connect#clicked (fun () -> wnd#destroy ()));
  wnd#show();
};

value delete_base conf bname = do {
  let wnd = GWindow.window
    ~title:(capitale (transl "Confirm"))
    ~position:`CENTER
    ~resizable:True
    ~width:300 ~height:50 ()
  in
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:wnd#add ()
  in
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `SPREAD
    ~packing: vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(capitale (transl "cancel"))
    ~packing:bbox#pack ()
  in
  ignore (btn_cancel#connect#clicked (fun () -> wnd#destroy ()));
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:bbox#pack ()
  in
  ignore
    (btn_ok#connect#clicked
      (fun () ->
        do {
          let base = Filename.concat conf.bases_dir (bname ^ ".gwb") in
          rmdir conf base;
          wnd#destroy ();
          (* TODO : revenir sur l'accueil *)
        }));
  wnd#show ();
};


(**/**) (* Autres binaires. *)

value read_error_cmd conf =
  match try Some (open_in conf.log) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop "" where rec loop msg =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line -> loop (msg ^ " " ^ line)
        | None -> do { close_in ic; msg } ]
  | None -> "error reading error_file" ]
;

value browse conf url = do {
  let pid =
    match conf.browser with
    [ Some browser -> exec browser [url] Unix.stdout Unix.stderr
    | None -> -1 ]
  in
  if pid = -1 then error_popup (transl "open url in your favorite browser")
  else conf.waiting_pids := [pid :: conf.waiting_pids];
  clean_waiting_pids conf;
};


(**/**) (* Binaires GeneWeb. *)
(*
   NB: Windows :
   let base = Filename.concat conf.bases_dir bname in
   Plante car il n'interprète pas les chemins avec espaces. On se place
   donc toujours dans le répertoire des bases dans launch_server.
*)

value gwc1 conf bname fname =
  (* Hack Windows, pas de Filename.concat mais juste bname *)
  let prog = Filename.concat bin_dir "gwc1" in
  let args = ["-v"; "-nc"; "-o"; bname] in
  let args = if fname <> "" then [fname :: args] else args in
  exec_wait conf prog args
;

value gwc2 conf bname fname =
  let prog = Filename.concat bin_dir "gwc2" in
  let args = ["-v"; "-nc"; "-o"; bname] in
  let args = if fname <> "" then [fname :: args] else args in
  exec_wait conf prog args
;

value ged2gwb conf bname fname =
  let prog = Filename.concat bin_dir "ged2gwb" in
  let args = ["-nc"; "-o"; bname] in
  let args = if fname <> "" then [fname :: args] else args in
  exec_wait conf prog args
;

value ged2gwb2 conf bname fname =
  let prog = Filename.concat bin_dir "ged2gwb2" in
  let args = ["-nc"; "-o"; bname] in
  let args = if fname <> "" then [fname :: args] else args in
  exec_wait conf prog args
;

value gwb2ged conf bname fname =
  let fname = fname ^ ".ged" in
  let prog = Filename.concat bin_dir "gwb2ged" in
  let args = [bname; "-o"; fname] in
  exec_wait conf prog args
;

value gwu conf bname fname =
  let fname = fname ^ ".gw" in
  let prog = Filename.concat bin_dir "gwu" in
  let args = [bname; "-o"; fname] in
  exec_wait conf prog args
;

value consang conf bname =
  let prog = Filename.concat bin_dir "consang" in
  let args = ["-i"; bname] in
  exec_wait conf prog args
;

value update_nldb conf bname =
  let prog = Filename.concat bin_dir "update_nldb" in
  let args = [bname] in
  (* TODO il faut effacer le fichier avant de le re-créer *)
  (* rm bname/notes_links *)
  exec_wait conf prog args
;

value check_base conf bname =
  let prog = Filename.concat bin_dir "check_base" in
  let args = [bname] in
  exec_wait conf prog args
;


(**/**) (* Fonctions utilies pour les binaires GeneWeb. *)

value print_default_gwf_file conf bname =
  let gwf =
    [ ("# File generated by GeneWeb", "");
      ("access_by_key", "yes");
      ("disable_forum", "yes");
      ("hide_private_names", "no");
      ("use_restrict", "no");
      ("show_consang", "yes");
      ("display_sosa", "yes");
      ("place_surname_link_to_ind", "yes");
      ("max_anc_level", "8");
      ("max_anc_tree", "7");
      ("max_desc_level", "12");
      ("max_desc_tree", "4");
      ("max_cousins", "2000");
      ("max_cousins_level", "5");
      ("latest_event", "20");
      ("template", "*");
      ("long_date", "no");
      ("counter", "no");
      ("full_siblings", "yes");
      ("hide_advanced_request", "no") ]
  in
  let fname = Filename.concat conf.bases_dir (bname ^ ".gwf") in
  if Sys.file_exists fname then ()
  else write_base_env conf bname gwf
;

value create_base conf bname src_file =
  if bname = "" then ()
  else do {
    if src_file = "" then gwc2 conf bname src_file
    else
      let fname = String.lowercase src_file in
      if Filename.check_suffix fname ".gw" then
        gwc2 conf bname src_file
      else if Filename.check_suffix fname ".ged" then
        ged2gwb2 conf bname src_file
      else error_popup (transl "Unknown file");
    let gwf_file = Filename.concat conf.bases_dir (bname ^ ".gwf") in
    if Sys.file_exists gwf_file then ()
    else print_default_gwf_file conf bname
};

value rename_base conf bname new_name =
  let databases =
    List.sort compare
      (List.filter (fun fn -> Filename.check_suffix fn ".gwb")
         (Array.to_list (Sys.readdir conf.bases_dir)))
  in
  let databases = List.map Filename.chop_extension databases in
  if List.mem new_name databases then
    error_popup (capitale (transl "database already exists"))
  else
    let old_base = Filename.concat conf.bases_dir (bname ^ ".gwb") in
    let new_base = Filename.concat conf.bases_dir (new_name ^ ".gwb") in
    try Sys.rename old_base new_base with [ _ -> error_popup "error rename" ]
;

value clean_database conf bname = do {
  gwu conf bname (bname ^ "_save");
  rename_base conf bname (bname ^ "_old");
  let fname = Filename.concat conf.bases_dir (bname ^ "_save.gw") in
  gwc2 conf bname fname;
  consang conf bname;
  update_nldb conf bname;
};

value merge conf bnames bname parameters = do {
  (* TODO : même méthode que clean *)
  List.iter
    (fun bname ->
      let bname = Filename.concat conf.bases_dir (bname ^ ".gwb") in
      let c =
        Filename.concat bin_dir "gwu " ^ bname ^ " -o " ^ bname ^ ".gw"
      in
      let rc = Sys.command c in
      if rc > 1 then error_popup c
      else ())
    bnames;
  let old_bases =
    List.fold_left
      (fun accu bname -> accu ^ " -sep " ^ bname ^ ".gw")
      "" bnames
  in
  let bname = Filename.concat conf.bases_dir bname in
  let c =
    Filename.concat bin_dir "gwc " ^ old_bases ^ " -f -o " ^ bname
  in
  let rc = Sys.command c in
  if rc > 1 then error_popup c
  else ();
};

(* fonctions pour faciliter le transport des bases suite à une mise à jour. *)
(* exporte toutes les bases au formet GW: base_name_date_today.gw *)
(* on copie toutes les base dans le nouveau dossier bases *)
(* on cherche tous les fichiers GW, et on les importe base_name *)
value export_all_bases conf =
  let databases =
    List.sort compare
      (List.filter (fun fn -> Filename.check_suffix fn ".gwb")
         (Array.to_list (Sys.readdir conf.bases_dir)))
  in
  let today = "05_10_2012" in
  List.iter
    (fun s -> gwu conf s (s ^ "_" ^ today))
    databases
;

value import_all_bases conf = ()
;

(**/**) (* UI pratique *)
value tmp_wnd conf bname f = do {
  let wnd = GWindow.window
    ~title:(capitale (transl "confirm"))
    ~position:`CENTER
    ~resizable:True ()
  in
  ignore (wnd#connect#destroy ~callback:(fun _ -> ()));
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:wnd#add ()
  in
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let _label = GMisc.label
    ~text:(transl "enter a name")
    ~packing:hbox#pack ()
  in
  let fname = ref "" in
  let entry = GEdit.entry
    ~text:""
    ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) ()
  in
  ignore (entry#connect#changed (fun () -> fname.val := entry#text));
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `SPREAD
    ~packing: vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(capitale (transl "cancel"))
    ~packing:bbox#pack ()
  in
  ignore (btn_cancel#connect#clicked (fun () -> wnd#destroy ()));
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:bbox#pack ()
  in
  ignore (btn_ok#connect#clicked (fun () -> do {
    wnd#destroy ();
    let fname = if fname.val = "" then "a" else fname.val in
    f conf bname fname }));
  wnd#show ();
};


(**/**) (* Interface graphique. *)

value main_window = do {
  ignore (GMain.init ());
  let wnd = GWindow.window
    ~title:("GeneWeb - " ^ Version.txt)
    ~position:`CENTER
    ~resizable:True
    ~width:640 ~height:480 ()
  in
  (* TODO : faire un close_server *)
  ignore (wnd#connect#destroy ~callback:GMain.quit);
  wnd
};

value rec show_main conf = do {
  ignore
    (main_window#connect#destroy
       ~callback:(fun () -> do {close_server conf; GMain.quit ()}));
  main_window#show ();
  clean_waiting_pids conf;
  let databases =
    List.sort compare
      (List.filter (fun fn -> Filename.check_suffix fn ".gwb")
         (Array.to_list (Sys.readdir conf.bases_dir)))
  in
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  let toolbar = GButton.toolbar
    ~orientation:`HORIZONTAL
    ~style:`BOTH
    ~packing:vbox#pack ()
  in
  let icon name =
    let file =
      List.fold_left Filename.concat bin_dir ["images"; name]
    in
    let info = GDraw.pixmap_from_xpm ~file:file () in
    (GMisc.pixmap info ())#coerce
  in
  let inser_toolbar text tooltip icon_file callback =
    toolbar#insert_button
      ~text:text
      ~tooltip:tooltip
      ~tooltip_private:"Private"
      ~icon:(icon icon_file)
      ~callback:callback ()
  in
  ignore
    (inser_toolbar
       (capitale (transl "create")) (capitale (transl "create a database"))
       "gui_create.png" (fun () -> do { vbox#destroy (); new_database conf; }));
  toolbar#insert_space ();
  ignore
    (inser_toolbar
       (capitale (transl "log")) (capitale (transl "view log")) "gui_log.png"
       (fun () -> display_log conf) );
  toolbar#insert_space ();
  ignore
    (inser_toolbar
       (capitale (transl "doc")) (capitale (transl "view doc")) "gui_doc.png"
       (fun () ->
         let url = "http://geneweb.tuxfamily.org/wiki/GeneWeb" in
         ignore (browse conf url)) );
  toolbar#insert_space ();
(* TOTO : gérer le restart
  ignore
    (inser_toolbar
       (capitale (transl "setup")) (capitale (transl "setup GeneWeb"))
       "gui_setup.png" (fun () -> do { vbox#destroy (); setup_gui conf }));
  toolbar#insert_space ();
*)
(*
  inser_toolbar
    (capitale (transl "restart")) (capitale (transl "restart GeneWeb"))
    "gtk.xpm"
    (fun () -> do { vbox#destroy (); close_server conf; launch_server conf });
  toolbar#insert_space ();
*)
  ignore
    (inser_toolbar
       (capitale (transl "quit")) (capitale (transl "quit GeneWeb"))
       "gui_quit.png"
       (fun () -> do { vbox#destroy (); close_server conf; GMain.quit () }));
  if databases = [] then
    ignore
      (GMisc.label
         ~text:(capitale (transl "no databases."))
         ~packing:vbox#pack ())
  else do {
    let _label = GMisc.label
      ~text:((capitale (transl "available databases")) ^
              " (" ^ (string_of_int (List.length databases) ^ "):"))
      ~packing:vbox#pack ()
    in
    let scrolled_window = GBin.scrolled_window ~border_width: 10
      ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
      ~packing:vbox#add () in
    let vvbox = GPack.vbox ~border_width: 10
      ~packing:scrolled_window#add_with_viewport () in
    vvbox #focus#set_vadjustment (Some scrolled_window#vadjustment);
    let table = GPack.table
      ~rows:3
      ~columns:3
      ~row_spacings: 5
      ~col_spacings: 5
      ~packing:vvbox#pack ()
    in
    loop 0 databases where rec loop i =
      fun
      [ [] -> ()
      | [bname :: l] -> do {
           let bn = Filename.chop_extension bname in
           let _label = GMisc.label
             ~text:bn
             ~packing:(table#attach ~left:0 ~top:i) ()
           in
           let bbut = GButton.button
             ~label:(capitale (transl "browse"))
             ~packing:(table#attach ~left:1 ~top:i) ()
           in
           ignore
             (bbut#connect#clicked
               (fun () ->
                 let url =
                   Printf.sprintf "http://localhost:%d/%s" conf.port bn
                 in
                 ignore (browse conf url)));
           let bbut = GButton.button
             ~label:(capitale (transl "tools"))
             ~packing:(table#attach ~left:2 ~top:i) ()
           in
           ignore
             (bbut#connect#clicked
               (fun () -> do { vbox#destroy (); tools conf bn }));
           loop (i+1) l }]
  }
}

and new_database conf = do {
  let vbox = GPack.box `VERTICAL
    ~spacing:5
    ~packing:main_window#add ()
  in
  let table = GPack.table
    ~rows:2
    ~columns:2
    ~row_spacings: 5
    ~col_spacings: 5
    ~packing:vbox#pack ()
  in
  let _label = GMisc.label
    ~text:(capitale (transl "name of the database:"))
    ~packing:(table#attach ~left:0 ~top:0) ()
  in
  let entry = GEdit.entry
    ~text:""
    ~packing:(table#attach ~left:1 ~top:0) ()
  in
  let _label= GMisc.label
    ~text:(capitale (transl "select a file"))
    ~packing:(table#attach ~left:0 ~top:1) ()
  in
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `SPREAD
    ~packing: (table#attach ~left:1 ~top:1) ()
  in
  let select = GButton.button
    ~stock:`OPEN
    ~packing:bbox#pack ()
  in
  let file = ref "" in
  ignore
    (select#connect#clicked
       (fun () -> file.val := select_file main_window ""));
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `SPREAD
    ~packing: vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(capitale (transl "cancel"))
    ~packing:bbox#pack ()
  in
  ignore
    (btn_cancel#connect#clicked
      (fun () -> do { vbox#destroy (); show_main conf } ) );
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:bbox#pack ()
  in
  ignore
    (btn_ok#connect#clicked
      (fun () ->
        do {
          create_base conf entry#text file.val;
          vbox#destroy ();
          show_main conf;
        }))
}

and setup_gui conf = do {
  let old_conf =
    { bases_dir = conf.bases_dir; port = conf.port;
      browser = conf.browser; log = conf.log; gui_arg = [];
      gwd_arg = [] ; only_arg = [];
      server_running = conf.server_running;
      waiting_pids = conf.waiting_pids }
  in
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  let _label = GMisc.label
    ~text:(transl "setup GeneWeb")
    ~packing:vbox#pack ()
  in
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let _label = GMisc.label
    ~text:("Bases dir : " ^ conf.bases_dir)
    ~packing:hbox#pack ()
  in
  let select = GButton.button
    ~stock:`OPEN
    ~packing:(hbox#pack ~expand:False) ()
  in
  ignore
    (select#connect#clicked
       (fun () -> conf.bases_dir := select_dir main_window conf.bases_dir)) ;
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let _label = GMisc.label
    ~text:"Port :"
    ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) ()
  in
  let entry = GEdit.entry
    ~text:(string_of_int conf.port)
    ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) ()
  in
  ignore
    (entry#connect#changed
       (fun () -> conf.port := int_of_string entry#text));
  let browser =
    match conf.browser with
    [ Some browser -> browser
    | None -> "" ]
  in
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let _label = GMisc.label
    ~text:("Browser : " ^ browser)
    ~packing:hbox#pack ()
  in
  let select = GButton.button
    ~stock:`OPEN
    ~packing:(hbox#pack ~expand:False) ()
  in
  ignore
    (select#connect#clicked
       (fun () ->
         let browser = select_file main_window bin_dir in
         let browser = if browser = "" then None else Some browser in
         conf.browser := browser)) ;
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `SPREAD
    ~packing: vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Cancel")
    ~packing:bbox#pack ()
  in
  ignore
    (btn_cancel#connect#clicked
       (fun () -> do { vbox#destroy (); show_main old_conf } ) ) ;
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:bbox#pack ()
  in
  ignore
    (btn_ok#connect#clicked
      (fun () -> do {
        vbox#destroy ();
        let browser =
          match conf.browser with
          [ Some b -> b
          | None -> "" ]
        in
        let gui_arg =
          [("bd", conf.bases_dir); ("port", string_of_int conf.port);
           ("browser", browser)]
        in
        let conf = {(conf) with gui_arg = gui_arg} in
        write_config_file conf;
        close_server conf;
        launch_server conf }));
}

and config_gwf conf bname = do {
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  let _label = GMisc.label
    ~text:(transl "Configuration gwf file of " ^ bname)
    ~packing:vbox#pack ()
  in
  let scrolled_window = GBin.scrolled_window ~border_width: 10
      ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
      ~packing:vbox#add () in
  let vvbox = GPack.vbox ~border_width: 10
      ~packing:scrolled_window#add_with_viewport () in
  vvbox #focus#set_vadjustment (Some scrolled_window#vadjustment);
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vvbox#pack ()
  in
  let vbox_list = GPack.vbox
    ~spacing:5
    ~packing:hbox#pack ()
  in
  let benv = read_base_env conf bname in
  let benv_new = ref (List.map (fun (k, v) -> (k, ref v)) benv) in
  List.iter
    (fun (k, v) ->
      do {
        let hbox = GPack.hbox
          ~spacing:5
          ~packing:vbox_list#pack ()
        in
        let _label = GMisc.label
          ~text: (k ^ " : ")
          ~width: 200
          ~packing:hbox#pack ()
        in
        (*
          TODO :
          si la valeur vaut "yes" ou "no", on ajoute un bouton radio
          sinon on ajoute un champ texte
          if v.val =
        *)
        let entry =
          GEdit.entry
            ~text: v.val
            ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) ()
        in
        ignore (entry#connect#changed (fun () -> v.val := entry#text))
      } )
    benv_new.val;
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `SPREAD
    ~packing: vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Cancel")
    ~packing:bbox#pack ()
  in
  ignore
    (btn_cancel#connect#clicked
       (fun () -> do { vbox#destroy (); show_main conf } ) );
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:bbox#pack ()
  in
  ignore
    (btn_ok#connect#clicked
       (fun () -> do {
         let new_benv = List.map (fun (k, v) -> (k, v.val)) benv_new.val in
         write_base_env conf bname new_benv;
         vbox#destroy ();
         show_main conf } ) )
}

and tools conf bname = do {
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  let _label = GMisc.label
    ~text:(transl "toolbox" ^ " " ^ bname)
    ~packing:vbox#pack ()
  in
  let scrolled_window = GBin.scrolled_window ~border_width: 10
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
    ~packing:vbox#add () in
  let vvbox = GPack.hbox ~border_width: 10
    ~packing:scrolled_window#add_with_viewport () in
  vvbox #focus#set_vadjustment (Some scrolled_window#vadjustment);
  let table = GPack.table
    ~rows:2
    ~columns:2
    ~row_spacings: 5
    ~col_spacings: 5
    ~packing:vvbox#pack ()
  in
  let _label = GMisc.label
    ~text:(transl "extract gw file")
    ~packing:(table#attach ~left:0 ~top:0) ()
  in
  let bbut = GButton.button
    ~label:(transl "Extract GW")
    ~packing:(table#attach ~left:1 ~top:0) ()
  in
  ignore (bbut#connect#clicked (fun () -> tmp_wnd conf bname gwu));
  let _label = GMisc.label
    ~text:(transl "extract ged file")
    ~packing:(table#attach ~left:0 ~top:1) ()
  in
  let bbut = GButton.button
    ~label:(transl "Extract GED")
    ~packing:(table#attach ~left:1 ~top:1) ()
  in
  ignore (bbut#connect#clicked (fun () -> tmp_wnd conf bname gwb2ged));
  let _label = GMisc.label
    ~text:(transl "setup base options")
    ~packing:(table#attach ~left:0 ~top:2) ()
  in
  let bbut = GButton.button
    ~label:(transl "setup gwf file")
    ~packing:(table#attach ~left:1 ~top:2) ()
  in
  ignore
    (bbut#connect#clicked
      (fun () -> do { vbox#destroy (); config_gwf conf bname })) ;
  let _label = GMisc.label
    ~text:(transl "Clean database")
    ~packing:(table#attach ~left:0 ~top:3) ()
  in
  let bbut = GButton.button
    ~label:(transl "Clean")
    ~packing:(table#attach ~left:1 ~top:3) ()
  in
  ignore (bbut#connect#clicked (fun () -> clean_database conf bname));
  let _label = GMisc.label
    ~text:(transl "Rename")
    ~packing:(table#attach ~left:0 ~top:4) ()
  in
  let bbut = GButton.button
    ~label:(transl "Rename")
    ~packing:(table#attach ~left:1 ~top:4) ()
  in
  ignore
    (bbut#connect#clicked
       (fun () -> do {
         tmp_wnd conf bname rename_base;
         vbox#destroy ();
         show_main conf}));
  let _label = GMisc.label
    ~text:(transl "Delete")
    ~packing:(table#attach ~left:0 ~top:5) ()
  in
  let bbut = GButton.button
    ~label:(transl "Delete")
    ~packing:(table#attach ~left:1 ~top:5) ()
  in
  ignore (bbut#connect#clicked (fun () -> delete_base conf bname));
(* TODO
  GMisc.label
    ~text:"merge"
    ~packing:(table#attach ~left:0 ~top:0) ();
  let bbut = GButton.button
    ~label:(transl "Merge")
    ~packing:(table#attach ~left:1 ~top:0) ()
  in
  ignore
    (bbut#connect#clicked
      (fun () -> ()));
*)
  let _label = GMisc.label
    ~text:(transl "Consang")
    ~packing:(table#attach ~left:0 ~top:6) ()
  in
  let bbut = GButton.button
    ~label:(transl "Consang")
    ~packing:(table#attach ~left:1 ~top:6) ()
  in
  ignore (bbut#connect#clicked (fun () -> consang conf bname));
  let _label = GMisc.label
    ~text:(transl "Update_nldb")
    ~packing:(table#attach ~left:0 ~top:7) ()
  in
  let bbut = GButton.button
    ~label:(transl "Update_nldb")
    ~packing:(table#attach ~left:1 ~top:7) ()
  in
  ignore (bbut#connect#clicked (fun () -> update_nldb conf bname));
  let bbox = GPack.button_box `HORIZONTAL
    ~border_width: 5
    ~layout: `EDGE
    ~packing: vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Home")
    ~packing:bbox#pack ()
  in
  ignore
    (btn_cancel#connect#clicked
       (fun () -> do { vbox#destroy (); show_main conf } ) );
}

and launch_server conf = do {
  (* On se place dans le répertoire des bases (obligatoire pour Windows). *)
  Sys.chdir conf.bases_dir;
  clean_waiting_pids conf;
  (* TODO *)
(*
  let cmd_log =
    Unix.openfile conf.log [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666
  in
*)
  let gwd_log =
    if trace.val then Unix.stdout
    else
      let fname = Filename.concat conf.bases_dir "gwd.log" in
      Unix.openfile fname [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666
  in
  let stop_server =
    List.fold_left Filename.concat conf.bases_dir ["cnt"; "STOP_SERVER"]
  in
  try Sys.remove stop_server with [ Sys_error _ -> () ];
  let prog = Filename.concat bin_dir "gwd" in
  let args =
    ["-hd"; bin_dir; "-bd"; conf.bases_dir; "-p"; sprintf "%d" conf.port]
  in
  let server_pid = exec prog args gwd_log gwd_log in
  let (pid, ps) = Unix.waitpid [Unix.WNOHANG] server_pid in
  if pid = 0 then ()
  else do {
    eprintf "Cannot launch the server:";
    eprintf " perhaps another server is running.\n";
    eprintf "You must close it, if you want to try again.\n";
    flush stderr;
    exit 2;
  };
  conf.server_running := Some server_pid;
  show_main conf;
};

value launch_config () =
  if Sys.file_exists config_gui_file then
    let gui_arg = read_config_file () in
    let bases_dir = List.assoc "bd" gui_arg in
    let port = int_of_string (List.assoc "port" gui_arg) in
    let browser = Some (List.assoc "browser" gui_arg) in
    let log = Filename.concat bases_dir "comm.log" in
    let conf =
      { bases_dir = bases_dir;
        port = port; browser = browser; log = log;
        gui_arg = gui_arg; gwd_arg = []; only_arg = [];
        server_running = None; waiting_pids = [] }
    in
    launch_server conf
  else do {
    let assistant = GAssistant.assistant () in
    assistant#misc#set_size_request ~width:450 ~height:300 ();
    assistant#set_title (transl "Setup GeneWeb");
    let page_0 = GMisc.label
      ~text:(transl "This assistant will help you to setup GeneWeb") ()
    in
    let bases_dir = ref "" in
    let page_1 = GPack.hbox ~spacing:5 () in
    let _label = GMisc.label
      ~text:(transl "select bases directory")
      ~packing:(page_1#pack ~expand:False ~fill:False ~padding:5) ()
    in
    let bbox = GPack.button_box `HORIZONTAL
      ~border_width: 5
      ~layout: `SPREAD
      ~packing:(page_1#pack ~expand:False ~fill:False ~padding:5) ()
    in
    let select = GButton.button
      ~stock:`OPEN
      ~packing:(bbox#pack ~expand:False ~fill:False ~padding:5) ()
    in
    ignore
      (select#connect#clicked
         (fun () -> do {
           bases_dir.val := select_dir assistant bases_dir.val;
           let num = assistant#current_page in
           let page = assistant#nth_page num in
           assistant#set_page_complete page (bases_dir.val <> "") }));
    let port = ref 2317 in
    let page_2 = GPack.hbox ~homogeneous:False ~spacing:5 () in
    let _label = GMisc.label
      ~text:(transl "enter port")
      ~packing:(page_2#pack ~expand:False ~fill:False ~padding:5) ()
    in
    let entry = GEdit.entry
      ~text:(string_of_int port.val)
      ~packing:(page_2#pack ~expand:False ~fill:False ~padding:5) ()
    in
    ignore
      (entry#connect#changed
         (fun () -> do {
           let txt = entry#text in
           port.val := int_of_string txt;
           let num = assistant#current_page in
           let page = assistant#nth_page num in
           assistant#set_page_complete page (String.length txt > 0) }));
    let browser = ref "" in
    let page_3 = GPack.hbox ~homogeneous:False ~spacing:5 () in
    let _label = GMisc.label
      ~text:(transl "select browser")
      ~packing:(page_3#pack ~expand:False ~fill:False ~padding:5) ()
    in
    let bbox = GPack.button_box `HORIZONTAL
      ~border_width: 5
      ~layout: `SPREAD
      ~packing:(page_3#pack ~expand:False ~fill:False ~padding:5) ()
    in
    let select = GButton.button
      ~stock:`OPEN
      ~packing:(bbox#pack ~expand:False ~fill:False ~padding:5) ()
    in
    ignore
      (select#connect#clicked
         (fun () -> do {
           browser.val := select_file assistant browser.val;
           let num = assistant#current_page in
           let page = assistant#nth_page num in
           assistant#set_page_complete page (browser.val <> "") }));
    match config_browser () with
    [ Some b ->
        let btn = GButton.check_button
          ~label:b
          ~packing:(page_3#pack ~expand:False ~fill:False ~padding:5) ()
        in
        ignore
          (btn#connect#toggled
             ~callback:(fun () -> do {
               browser.val := b;
               let num = assistant#current_page in
               let page = assistant#nth_page num in
               assistant#set_page_complete page btn#active }))
    | None -> () ];
    let page_4 = GMisc.label
      ~text:(transl "save preferences") ()
    in
    ignore
      (assistant#append_page
         ~title:(transl "Introduction")
         ~page_type:`INTRO
         ~complete:True
         page_0#as_widget);
    ignore
      (assistant#append_page
         ~title:(transl "Setup bases directory")
         ~page_type:`CONTENT
         page_1#as_widget);
    ignore
      (assistant#append_page
         ~title:(transl "Setup port")
         ~page_type:`CONTENT
         ~complete:True
         page_2#as_widget);
    ignore
      (assistant#append_page
         ~title:(transl "Setup browser")
         ~page_type:`CONTENT
         page_3#as_widget);
    ignore
      (assistant#append_page
         ~title:(transl "Finnish")
         ~page_type: `CONFIRM
         ~complete:True
         page_4#as_widget);
    let save_config () = do {
      let gui_arg =
        [("bd", bases_dir.val); ("port", string_of_int port.val);
         ("browser", browser.val)]
      in
      let conf =
        { bases_dir = bases_dir.val;
          port = port.val; browser = Some browser.val;
          log = Filename.concat bases_dir.val "comm.log";
          gui_arg = gui_arg; gwd_arg = []; only_arg = [];
          server_running = None; waiting_pids = [] }
      in
      (* TODO : On en a besoin avant ... *)
      (* mkdir_p conf.bases_dir; *)
      write_config_file conf;
      assistant#destroy ();
      launch_server conf }
    in
    ignore (assistant#connect#cancel ~callback:GMain.quit);
    ignore (assistant#connect#close ~callback:save_config);
    assistant#show ();
};


(**/**) (* main *)

value speclist = [("-trace", Arg.Set trace, " Trace server")];
value anon_fun s = raise (Arg.Bad (sprintf "Don't know what to do with %s" s));
value usage_msg = "Usage: gui [option]";

value main () = do {
  Arg.parse (Arg.align speclist) anon_fun usage_msg;
  launch_config ();
  let () = GMain.main () in
  Sys.catch_break True;
  eprintf "Bye\n"; flush stderr;
};

Printexc.print main ();
