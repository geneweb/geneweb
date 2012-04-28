(* $Id: gui.ml,v 1.00 2011-12-25 15:36:35 flh Exp $ *)

open Printf;

type conf =
  { config_env : mutable list (string * string);
    bin_dir : mutable string;
    bases_dir : mutable string;
    port : mutable int;
    browser : mutable option string;
    browser_lang : mutable bool;
    digest_auth : mutable bool;
    server_running : mutable option int;
    waiting_pids : mutable list int }
;

value abs_path fname =
  if Filename.is_relative fname then
    Filename.concat (Sys.getcwd ()) fname
  else fname
;

value trace = ref False;
value config_file = abs_path (Filename.concat "gw" "config.txt");
value lexicon_file = abs_path (Filename.concat "gw" "gui_lex.txt");
value lexicon_mtime = ref 0.0;

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

value default_lang =
  try Sys.getenv "LC_ALL" with
  [ Not_found ->
      try Sys.getenv "LC_MESSAGES" with
      [ Not_found -> try Sys.getenv "LANG" with [ Not_found -> "en" ] ] ]
;

value lang = ref default_lang;

value transl w =
  match unfreeze_lexicon lang.val with
  [ Some lex -> try Hashtbl.find lex w with [ Not_found -> "[" ^ w ^ "]" ]
  | None -> w ]
;

value read_config_env () =
  match try Some (open_in config_file) with [ Sys_error _ -> None ] with
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

value mkdir_p x =
  loop x where rec loop x =
    do  {
      let y = Filename.dirname x in
      if y <> x && String.length y < String.length x then loop y else ();
      try Unix.mkdir x 0o777 with [ Unix.Unix_error _ _ _ -> () ];
    }
;

value write_config_env env = do {
  mkdir_p (Filename.dirname config_file);
  let oc = open_out config_file in
  List.iter (fun (k, v) -> fprintf oc "%s=%s\n" k v) env;
  close_out oc;
};

value exec prog args out err =
  Unix.create_process prog (Array.of_list [prog :: args]) Unix.stdin out err
;

value close_server conf =
  match conf.server_running with
  [ Some server_pid -> do {
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
      with
      [ Unix.Unix_error _ _ _ -> () ];
      try Unix.close s with
      [ Unix.Unix_error _ _ _ -> () ];
      let _ : (int * _) = Unix.waitpid [] server_pid in
      conf.server_running := None;
      eprintf "\n"; flush stderr;
    }
  | None -> () ]
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

value browse conf browser port bname = do {
  let pid =
    match browser with
    [ Some browser ->
        exec browser [sprintf "http://localhost:%d/%s" port bname]
          Unix.stdout Unix.stderr
    | None -> -1 ]
  in
  if pid = -1 then do {
    eprintf "Open http://localhost:%d/%s in your favorite browser.\n"
      port bname;
    flush stderr;
  }
  else conf.waiting_pids := [pid :: conf.waiting_pids];
  clean_waiting_pids conf;
};

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
          | None -> env ]
      in
      do { close_in ic; env }
  | None -> [] ]
;

value rm_base dir =
  match
    try Some (Unix.opendir dir) with [ Unix.Unix_error _ _ _ -> None ]
  with
  [ Some dh ->
      let list = ref [] in
      do {
        try
          while True do {
            let file = Unix.readdir dh in
            if file = "." || file = ".." then ()
            else list.val := [file :: list.val]
          }
        with
        [ End_of_file -> () ];
        Unix.closedir dh;
        List.iter (fun file -> Unix.unlink (Filename.concat dir file))
          list.val;
        try Unix.rmdir dir with [ Unix.Unix_error _ _ _ -> () ]
      }
  | _ -> () ]
;

value clean_database conf bname = do {
  (* save *)
  let base = Filename.concat conf.bases_dir (bname ^ ".gwb") in
  let c = 
    Filename.concat conf.bin_dir "gwu" ^ " " ^ base ^ " -o tmp.gw" 
  in
  let rc = Sys.command c in
  if rc > 1 then 
    (* Ouvrir une fenêtre pop-up pour dire que la sauvegarde a échouée *)
    print_endline c
  else ();
  (* create *)
  (* Il faut regarder si la base était en gwc1 ou 2 pour appeler le même *)
  let c =
    Filename.concat conf.bin_dir "gwc" ^ " tmp.gw -f -o " ^ base ^ " > comm.log "
  in
  let rc = Sys.command c in
  if rc > 1 then 
    (* Ouvrir une fenêtre pop-up pour dire que la sauvegarde a échouée *)
    print_endline c
  else ();
};

value update_nldb conf bname = ()
(*
  let rc =
    let comm = stringify (Filename.concat bin_dir.val conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  do {
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file
  }
*)
;

value consang conf ok_file = ()
(*
  let rc =
    let comm = stringify (Filename.concat bin_dir.val conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  do {
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file
  }
*)
;

value consang conf bname parameters = do {
  let base = Filename.concat conf.bases_dir (bname ^ ".gwb") in
  let c = 
    Filename.concat conf.bin_dir "consang" ^ " " ^ base ^ " > comm.log"
  in
  let rc = Sys.command c in
  if rc > 1 then
    (* Ouvrir une fenêtre pop-up pour dire que la sauvegarde a échouée *)
    print_endline c
  else ();
};

value merge conf bnames bname parameters = do {
  (* tester quel gwc *)
  List.iter
    (fun bname ->
      let bname = Filename.concat conf.bases_dir (bname ^ ".gwb") in
      let c = 
        Filename.concat conf.bin_dir "gwu" ^ " " ^ bname ^ " -o " ^ bname ^ ".gw"
      in
      let rc = Sys.command c in
      if rc > 1 then
        (* Ouvrir une fenêtre pop-up pour dire que la sauvegarde a échouée *)
        print_endline c
      else () )
    bnames;
  let old_bases = 
    List.fold_left 
      (fun accu bname -> accu ^ " -sep " ^ bname ^ ".gw") 
      "" bnames
  in
  let bname = Filename.concat conf.bases_dir bname in
  let c =
    Filename.concat conf.bin_dir "gwc" ^ old_bases ^ " -f -o " ^ bname
  in 
  let rc = Sys.command c in
  if rc > 1 then
    (* Ouvrir une fenêtre pop-up pour dire que la sauvegarde a échouée *)
    print_endline c
  else ();
};

value save_to_ged conf bname fname = do {
  let bname = Filename.concat conf.bases_dir (bname ^ ".gwb") in
  let c =
    Filename.concat conf.bin_dir "gwb2ged" ^ bname ^ " -o " ^ fname ^ ".ged"
  in 
  let rc = Sys.command c in
  if rc > 1 then
    (* Ouvrir une fenêtre pop-up pour dire que la sauvegarde a échouée *)
    print_endline c
  else ();
};

value save_to_gw conf bname fname = do {
  let bname = Filename.concat conf.bases_dir (bname ^ ".gwb") in
  let c =
    Filename.concat conf.bin_dir "gwu" ^ bname ^ " -o " ^ fname ^ ".gw"
  in 
  let rc = Sys.command c in
  if rc > 1 then
    (* Ouvrir une fenêtre pop-up pour dire que la sauvegarde a échouée *)
    print_endline c
  else ();
};

value update_config_env conf s_conf v_conf = do {
  eprintf "%s = %s\n" s_conf v_conf;
  flush stderr;
  let new_config_env =
    try List.assoc s_conf conf.config_env with
    [ Not_found -> "" ]
  in
  if s_conf <> new_config_env || not (Sys.file_exists config_file)
  then do {
    conf.config_env :=
      List.filter (fun (v, _) -> v <> s_conf) conf.config_env @
      [(s_conf, v_conf)];
    write_config_env conf.config_env
  }
  else (); }
;

value main_window = do {
  GMain.init ();
  let wnd = GWindow.window 
    ~title:("GeneWeb - " ^ Version.txt)
    ~position:`CENTER 
    ~resizable:True 
    ~width:640 ~height:480 () in 
  wnd#connect#destroy ~callback:GMain.quit; 
  wnd 
};

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

(*
value create_menu depth tearoff = 
  let rec aux depth tearoff = do {
    let menu = GMenu.menu () and group = ref None in
    if tearoff then ignore (GMenu.tearoff_item ~packing: menu#append ()) else ();
    for i = 0 to 4 do {
      let menuitem = GMenu.radio_menu_item ?group:group.val
	  ~label:("item " ^ string_of_int depth ^ " - " ^ string_of_int (i+1))
	  ~packing:menu#append ~show_toggle:(depth mod 2 <> 0)
	  () in
      group.val := Some (menuitem #group);
      if i = 3 then menuitem #misc#set_sensitive False else ();
      if depth > 1 then
	menuitem #set_submenu (aux (depth-1) True)
          else ()
    };

    menu
}
  in aux depth tearoff
;
*)


value rec show_main conf = do {
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
  GMisc.label
    ~text:(transl "Server is running...")
    ~packing:vbox#pack ();
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let cbut = GButton.button
    ~label:"gwc1"
    ~packing:hbox#pack ()
  in
  ignore 
    (cbut#connect#clicked 
      (fun () -> do { vbox#destroy (); new_database_gwc1 conf } ) );
  let cbut = GButton.button
    ~label:"gwc2"
    ~packing:hbox#pack ()
  in
  ignore 
    (cbut#connect#clicked 
      (fun () -> do { vbox#destroy (); new_database_gwc2 conf } ) );
  let btn = GButton.button
    ~label:"ged2gwb"
    ~packing:hbox#pack ()
  in
  ignore 
    (btn#connect#clicked 
      (fun () -> ()) );
  let btn = GButton.button
    ~label:"setup"
    ~packing:hbox#pack ()
  in
  ignore 
    (btn#connect#clicked 
      (fun () -> do { vbox#destroy (); setup_gui conf } ) );
  let rbut = GButton.button
    ~label:(transl "Restart")
    ~packing:hbox#pack ()
  in
  ignore 
    (rbut#connect#clicked 
      (fun () -> do { vbox#destroy (); close_server conf; launch_server conf } ) );
  let wbut = GButton.button
    ~label:(transl "Quit")
    ~packing:hbox#pack ()
  in
  ignore 
    (wbut#connect#clicked 
      (fun () -> do { vbox#destroy (); GMain.quit () } ) );
  if databases = [] then 
    ignore (GMisc.label ~text:(transl "No databases.") ~packing:vbox#pack () )
  else do {
    GMisc.label
      ~text:(transl "Available databases:")
      ~packing:vbox#pack ();
    let scrolled_window = GBin.scrolled_window ~border_width: 10
        ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
        ~packing:vbox#add () in
    let vvbox = GPack.vbox ~border_width: 10
        ~packing:scrolled_window#add_with_viewport () in
    vvbox #focus#set_vadjustment (Some scrolled_window#vadjustment);
    List.iter
      (fun bname -> do {
         let bn = Filename.chop_extension bname in
         let hbox = GPack.hbox
           ~spacing:5
           ~packing:vvbox#pack ()
         in
         let blab = GMisc.label
           ~text:("- " ^ bn ^ " -")
           ~packing:hbox#pack ()
         in
         let bbut = GButton.button
           ~label:(transl "Browse")
           ~packing:hbox#pack () 
         in
         ignore 
           (bbut#connect#clicked 
             (fun () -> ignore (browse conf conf.browser conf.port bn)));
         let blab = GMisc.label
           ~text:(" - ")
           ~packing:hbox#pack ()
         in
         let bbut = GButton.button
           ~label:(transl "Tools")
           ~packing:hbox#pack () 
         in
         ignore 
           (bbut#connect#clicked 
             (fun () -> do { vbox#destroy (); tools conf bn })) })
      databases;
  }
}

and new_database_gwc1 conf = do {
  clean_waiting_pids conf;
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  GMisc.label
    ~text:(transl "Enter the name:")
    ~packing:vbox#pack ();
  let entry = GEdit.entry 
    ~text:""
    ~packing:(vbox#pack ~expand:False ~fill:False ~padding:5) () 
  in
  GMisc.label
    ~text:(transl "Choisir un fichier")
    ~packing:vbox#pack ();
  let select = GButton.button 
    ~stock:`OPEN 
    ~packing:(vbox#pack ~expand:False) () 
  in
  let file = ref "" in
  GMisc.label
    ~text:("actual : " ^ file.val)
    ~packing:vbox#pack ();
  select#connect#clicked
    (fun () -> file.val := select_file main_window "");
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Cancel")
    ~packing:hbox#pack ()
  in
  ignore 
      (btn_cancel#connect#clicked 
        (fun () -> do { vbox#destroy (); show_main conf } ) );
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:hbox#pack () 
  in
  ignore 
    (btn_ok#connect#clicked 
      (fun () -> 
        let s = entry#text in 
        if s = "" then ()
        else
             loop 0 where rec loop i =
               if i = String.length s then do {
                 let db = Filename.concat conf.bases_dir s in
                 if Sys.file_exists (db ^ ".gwb") then ()
                 else do {
                   let comm = Filename.concat conf.bin_dir "gwc" in
                   let pid = exec comm ["-o"; db] Unix.stdout Unix.stderr in
                   let (_, _) = Unix.waitpid [] pid in
                   vbox#destroy ();
                   show_main conf;
                 }
               }
               else
                 match s.[i] with
                 [ 'a'..'z' | 'A'..'Z' | '-' | '0'..'9' -> loop (i + 1)
                 | _ -> () ]) )
}

and new_database_gwc2 conf = do {
  clean_waiting_pids conf;
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  GMisc.label
    ~text:(transl "Enter the name:")
    ~packing:vbox#pack ();
  let entry = GEdit.entry 
    ~text:""
    ~packing:(vbox#pack ~expand:False ~fill:False ~padding:5) () 
  in
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Cancel")
    ~packing:hbox#pack ()
  in
  ignore 
      (btn_cancel#connect#clicked 
        (fun () -> do { vbox#destroy (); show_main conf } ) );
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:hbox#pack () 
  in
  ignore 
    (btn_ok#connect#clicked 
      (fun () -> 
        let s = entry#text in 
        if s = "" then ()
        else
             loop 0 where rec loop i =
               if i = String.length s then do {
                 let db = Filename.concat conf.bases_dir s in
                 if Sys.file_exists (db ^ ".gwb") then ()
                 else do {
                   let comm = Filename.concat conf.bin_dir "gwc2" in
                   let pid = exec comm ["-o"; db] Unix.stdout Unix.stderr in
                   let (_, _) = Unix.waitpid [] pid in
                   vbox#destroy ();
                   show_main conf;
                 }
               }
               else
                 match s.[i] with
                 [ 'a'..'z' | 'A'..'Z' | '-' | '0'..'9' -> loop (i + 1)
                 | _ -> () ]) )
}

and setup_gui conf = do {
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  GMisc.label
    ~text:(transl "Setup the gui")
    ~packing:vbox#pack ();
  let hbox = GPack.hbox 
    ~spacing:5 
    ~packing:vbox#pack () 
  in
  GMisc.label
    ~text:("actual : " ^ conf.bin_dir)
    ~packing:hbox#pack ();
  let select = GButton.button 
    ~stock:`OPEN 
    ~packing:(hbox#pack ~expand:False) () 
  in
  select#connect#clicked
    (fun () -> conf.bin_dir := select_dir main_window conf.bin_dir) ;
  let hbox = GPack.hbox 
    ~spacing:5 
    ~packing:vbox#pack () 
  in
  GMisc.label
    ~text:("actual : " ^ conf.bases_dir)
    ~packing:hbox#pack ();
  let select = GButton.button 
    ~stock:`OPEN 
    ~packing:(hbox#pack ~expand:False) () 
  in
  select#connect#clicked
    (fun () -> conf.bases_dir := select_dir main_window conf.bases_dir) ;
  let hbox = GPack.hbox 
    ~spacing:5 
    ~packing:vbox#pack () 
  in
  GMisc.label 
    ~text:"Port :" 
    ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) ();
  let entry = GEdit.entry 
    ~text:(string_of_int conf.port)
    ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) () 
  in
  conf.port := int_of_string entry#text;
  let hbox = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Cancel")
    ~packing:hbox#pack ()
  in
  ignore 
      (btn_cancel#connect#clicked 
        (fun () -> do { vbox#destroy (); show_main conf } ) );
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:hbox#pack () 
  in
  ignore 
    (btn_ok#connect#clicked 
      (fun () -> do { vbox#destroy (); 
                      update_config_env conf "bin_dir" conf.bin_dir;
                      update_config_env conf "bases_dir" conf.bases_dir;
                      update_config_env conf "port" (string_of_int conf.port);
                      show_main conf } ) );
}

and config_gwf conf bname = do {
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  GMisc.label
    ~text:(transl "Configuration gwf file of " ^ bname)
    ~packing:vbox#pack ();
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
  List.iter
    (fun (k, v) -> 
      ignore (
       GMisc.label
         ~text: (k ^ " : " ^ v)
         ~packing:vbox_list#pack () ))
    benv;
  let hbox_valid = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Cancel")
    ~packing:hbox_valid#pack ()
  in
  ignore 
    (btn_cancel#connect#clicked 
       (fun () -> do { vbox#destroy (); show_main conf } ) );
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:hbox_valid#pack () 
  in
  ignore 
    (btn_ok#connect#clicked 
       (fun () -> do { vbox#destroy (); show_main conf } ) )
}

and tools conf bname = do {
  let vbox = GPack.vbox
    ~spacing:5
    ~packing:main_window#add ()
  in
  GMisc.label
    ~text:(transl "Boîte à outil pour la base " ^ bname)
    ~packing:vbox#pack ();
  let scrolled_window = GBin.scrolled_window ~border_width: 10
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
    ~packing:vbox#add () in
  let vvbox = GPack.hbox ~border_width: 10
    ~packing:scrolled_window#add_with_viewport () in
  vvbox #focus#set_vadjustment (Some scrolled_window#vadjustment);
  let hbox = GPack.vbox
    ~spacing:5
    ~packing:vvbox#pack ()
  in
  let bbut = GButton.button
    ~label:(transl "Extract GW")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Extract GEDCOM")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Import GW")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Import GEDCOM")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Configure GWF")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> do { vbox#destroy (); config_gwf conf bname })) ;
  let bbut = GButton.button
    ~label:(transl "Extract GEDCOM")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Nettoyage")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> clean_database conf bname));
  let bbut = GButton.button
    ~label:(transl "Renommage")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Suppression")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Fusion")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Save & Restauration")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Consang")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let bbut = GButton.button
    ~label:(transl "Update_nldb")
    ~packing:hbox#pack () 
  in
  ignore 
    (bbut#connect#clicked 
      (fun () -> ()));
  let hbox_valid = GPack.hbox
    ~spacing:5
    ~packing:vbox#pack ()
  in
  let btn_cancel = GButton.button
    ~label:(transl "Cancel")
    ~packing:hbox_valid#pack ()
  in
  ignore 
    (btn_cancel#connect#clicked 
       (fun () -> do { vbox#destroy (); show_main conf } ) );
  let btn_ok = GButton.button
    ~label:(transl "OK")
    ~packing:hbox_valid#pack () 
  in
  ignore 
    (btn_ok#connect#clicked 
       (fun () -> do { vbox#destroy (); show_main conf } ) )
}

and launch_server conf = do {
  clean_waiting_pids conf;
  let only = Unix.gethostname () in
  let fd =
    if trace.val then Unix.stdout
    else
      Unix.openfile "gwd.log" [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
        0o666
  in
  let stop_server =
    List.fold_left Filename.concat conf.bases_dir ["cnt"; "STOP_SERVER"]
  in
  try Sys.remove stop_server with [ Sys_error _ -> () ];
  let rest_of_args =
    try
      let v = List.assoc "gwd_args" conf.config_env in
      if v = "" then []
      else
        loop 0 0 [] where rec loop ibeg i list =
          if i = String.length v then
            List.rev [String.sub v ibeg (i - ibeg) :: list]
          else if v.[i] = ' ' then
            let a = String.sub v ibeg (i - ibeg) in
            loop (i + 1) (i + 1) [a :: list]
          else loop ibeg (i + 1) list
    with
    [ Not_found -> [] ]
  in
  let rest_of_args =
    if conf.digest_auth then ["-digest" :: rest_of_args] else rest_of_args
  in
  let rest_of_args =
    if conf.browser_lang then ["-blang" :: rest_of_args] else rest_of_args
  in
  let comm = Filename.concat conf.bin_dir "gwd" in
  let args =
    ["-p"; sprintf "%d" conf.port; "-only"; "localhost"; "-only";
     "127.0.0.1"; "-only"; only; "-hd"; conf.bin_dir; "-bd";
     conf.bases_dir :: rest_of_args]
  in
  eprintf "%s" comm;
  List.iter (fun a -> eprintf " %s" a) args;
  eprintf "\n";
  flush stderr;
  let server_pid = exec comm args fd fd in
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


value config conf var_env create_frame set next = 
  match
    try Some (var_env ()) with
    [ Failure _ | Not_found -> None ]
  with
  [ Some v -> do { set conf v ; next conf }
  | None -> do { ignore (create_frame conf) } ]
;

value rec config_bin_dir conf =
  config
    conf 
    (fun () -> List.assoc "bin_dir" conf.config_env) 
    (fun conf -> do {
       let vbox = GPack.vbox
         ~spacing:5
         ~width:100 ~height:100
         ~packing:main_window#add ()
       in
       GMisc.label
         ~text:(transl "GeneWeb binary directory:")
         ~packing:vbox#pack ();
       let hbox = GPack.hbox 
         ~spacing:5 
         ~packing:vbox#pack () 
       in
       GMisc.label
         ~text:("actual : " ^ conf.bin_dir)
         ~packing:hbox#pack ();
       let select = GButton.button 
         ~stock:`OPEN 
         ~packing:(hbox#pack ~expand:False) () 
       in
       select#connect#clicked
         (fun () -> conf.bin_dir := select_dir main_window conf.bin_dir) ;
       let hbox = GPack.hbox 
         ~spacing:5 
         ~packing:vbox#pack () 
       in
       let next = GButton.button 
         ~label:(transl "Next")
         ~packing:hbox#pack () 
       in
       next#connect#clicked
         (fun () -> do { vbox#destroy ();   
                         update_config_env conf "bin_dir" conf.bin_dir;
                         ignore (config_bases_dir conf) } ) } )
    (fun conf bin_dir -> conf.bin_dir := bin_dir)
    (fun conf -> ignore (config_bases_dir conf))

and config_bases_dir conf = 
  config
    conf 
    (fun () -> List.assoc "bases_dir" conf.config_env) 
    (fun conf -> do {
       let vbox = GPack.vbox
         ~spacing:5
         ~packing:main_window#add ()
       in
       GMisc.label
         ~text:(transl "Databases directory:")
         ~packing:vbox#pack ();
       let hbox = GPack.hbox 
         ~spacing:5 
         ~packing:vbox#pack () 
       in
       GMisc.label
         ~text:("actual : " ^ conf.bases_dir)
         ~packing:hbox#pack ();
       let select = GButton.button 
         ~stock:`OPEN 
         ~packing:(hbox#pack ~expand:False) () 
       in
       select#connect#clicked
         (fun () -> conf.bases_dir := select_dir main_window conf.bases_dir) ;
       let hbox = GPack.hbox 
         ~spacing:5 
         ~packing:vbox#pack () 
       in
       let prev = GButton.button 
         ~label:(transl "Prev") 
         ~packing:hbox#pack () 
       in
       prev#connect#clicked
         (fun () -> do { vbox#destroy (); 
                         let conf = 
                           { (conf) with 
                             config_env = 
                               List.remove_assoc "bin_dir" conf.config_env }
                         in
                         ignore (config_bin_dir conf) } ) ;
       let next = GButton.button 
         ~label:(transl "Next")
         ~packing:hbox#pack () 
       in
       next#connect#clicked
         (fun () -> do { vbox#destroy ();
                         update_config_env conf "bases_dir" conf.bases_dir;
                         ignore (config_port conf) } ) } )
    (fun conf bases_dir -> conf.bases_dir := bases_dir)
    (fun conf -> ignore (config_port conf))

and config_port conf = 
  config
    conf 
    (fun () -> List.assoc "port" conf.config_env) 
    (fun conf -> do {
      let vbox = GPack.vbox 
        ~spacing:5 
        ~packing:main_window#add () 
      in
      GMisc.label 
        ~text:"Configuration du port"
        ~packing:vbox#pack ();
      let hbox = GPack.hbox 
        ~spacing:5 
        ~packing:vbox#pack () 
      in
      GMisc.label 
        ~text:"Port :" 
        ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) ();
      let entry = GEdit.entry 
        ~text:(string_of_int conf.port)
        ~packing:(hbox#pack ~expand:False ~fill:False ~padding:5) () 
      in
      conf.port := int_of_string entry#text;
      let hbox = GPack.hbox 
        ~spacing:5 
        ~packing:vbox#pack () 
      in
      let prev = GButton.button 
        ~label:(transl "Prev") 
        ~packing:hbox#pack () 
      in
      prev#connect#clicked
         (fun () -> do { vbox#destroy (); 
                         let conf = 
                           { (conf) with 
                             config_env = 
                               List.remove_assoc "bases_dir" conf.config_env }
                         in
                         ignore (config_bases_dir conf) } ) ;
      let next = GButton.button 
        ~label:(transl "Next")
        ~packing:hbox#pack () 
      in
      next#connect#clicked
        (fun () -> do { vbox#destroy ();   
                        update_config_env conf "port" (string_of_int conf.port);
                        ignore (config_browser conf) } ) } )
    (fun conf port -> conf.port := (int_of_string port))
    (fun conf -> ignore (config_browser conf))

and config_browser conf = 
  let default_sys_bin_dir =
    match Sys.os_type with
    [ "Win32" | "Cygwin" -> "C:\\Program Files"
    | _ -> "/usr/bin" ]
  in
  let browsers () =
    let defbrofil =
      try Sys.getenv "DEFAULT_BROWSERS_FILE" with
      [ Not_found -> Filename.concat "gw" "browsers.txt" ]
    in
    let browsers =
      match try Some (open_in defbrofil) with [ Sys_error _ -> None ] with
      [ Some ic ->
          loop [] where rec loop list =
            match try Some (input_line ic) with [ End_of_file -> None ] with
            [ Some name -> loop [name :: list]
            | None -> do { close_in ic; list } ]
      | None -> [] ]
    in
    let default_browsers =
      match Sys.os_type with
      [ "Win32" | "Cygwin" ->
          ["C:\\Program Files\\Mozilla Firefox\\firefox.exe";
           "C:\\Program Files\\Internet Explorer\\iexplore.exe"]
      | _ ->
          ["/usr/bin/firefox"; "/usr/bin/mozilla"] ]
    in
    List.filter Sys.file_exists (List.rev_append browsers default_browsers)
  in
  let browsers = lazy (browsers ()) in
  config
    conf
    (fun () -> List.assoc "browser" conf.config_env)
    (fun conf -> do {
      conf.browser := Some "/usr/bin/firefox" ;
      let to_string =
        fun
        [ Some s -> s
        | None -> "" ]
      in
      let vbox = GPack.vbox 
        ~spacing:5 
        ~packing:main_window#add () 
      in
      let hbox = GPack.hbox 
        ~spacing:5 
        ~packing:vbox#pack () 
      in
      let prev = GButton.button 
        ~label:(transl "Prev") 
        ~packing:hbox#pack () 
      in
      prev#connect#clicked
         (fun () -> do { vbox#destroy (); 
                         let conf = 
                           { (conf) with 
                             config_env = 
                               List.remove_assoc "port" conf.config_env }
                         in
                         ignore (config_bases_dir conf) } ) ;
      let next = GButton.button 
        ~label:(transl "Next")
        ~packing:hbox#pack () 
      in
      next#connect#clicked
        (fun () -> do { vbox#destroy ();   
                        update_config_env conf "browser" (to_string conf.browser);
                        ignore (launch_server conf) } ) } )
    (fun conf browser -> conf.browser := Some browser)
    (fun conf -> ignore (launch_server conf))
;

(**/**) (* main *)

value default_bin_dir = abs_path (Filename.concat (Sys.getcwd ()) "gw");
value default_bases_dir = abs_path (Filename.concat (Sys.getcwd ()) "bases");
value default_port = 2317;
value default_browser = None;

value speclist = [("-trace", Arg.Set trace, " Trace server")];

value anon_fun s =
  raise (Arg.Bad (sprintf "Don't know what to do with %s" s))
;

value usage_msg = "Usage: gui [option]";

value main () = do {
  Arg.parse (Arg.align speclist) anon_fun usage_msg;
  let config_env = read_config_env () in
  let conf =
    { config_env = config_env; bin_dir = default_bin_dir; 
      bases_dir = default_bases_dir; port = default_port; 
      browser = default_browser; browser_lang = True; 
      digest_auth = True; server_running = None; waiting_pids = [] }
  in  
  let () = main_window#show () in
  config_bin_dir conf;
  let () = GMain.main () in
  Sys.catch_break True;
  close_server conf;
  eprintf "Bye\n"; flush stderr;
};

Printexc.print main ();
