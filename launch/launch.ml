(* $Id: launch.ml,v 1.34 2007-06-06 15:22:35 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Camltk;
open Printf;

type state =
  { tk_win : Widget.widget;
    config_env : mutable list (string * string);
    bin_dir : mutable string;
    sys_dir : mutable string;
    doc_dir : mutable string;
    port : mutable int;
    browser : mutable option string;
    bases_dir : mutable string;
    browser_lang : mutable bool;
    digest_auth : mutable bool;
    server_running : mutable option int;
    waiting_pids : mutable list int }
;

value trace = ref False;
value config_file = Filename.concat "gw" "config.txt";
value lexicon_file = Filename.concat "gw" "launch_lex.txt";
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

value close_server state =
  match state.server_running with
  [ Some server_pid -> do {
      eprintf "Closing..."; flush stderr;
      (* Making a (empty) file STOP_SERVER to make the server stop. *)
      let stop_server =
        List.fold_left Filename.concat state.bases_dir ["cnt"; "STOP_SERVER"]
      in
      let oc = open_out stop_server in
      close_out oc;
      (* Send a phony connection to unblock it. *)
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      try
        Unix.connect s (Unix.ADDR_INET Unix.inet_addr_loopback state.port)
      with
      [ Unix.Unix_error _ _ _ -> () ];
      try Unix.close s with
      [ Unix.Unix_error _ _ _ -> () ];
      let _ : (int * _) = Unix.waitpid [] server_pid in
      state.server_running := None;
      eprintf "\n"; flush stderr;
    }
  | None -> () ]
;

value clean_waiting_pids state =
  state.waiting_pids :=
    List.filter
      (fun pid ->
         let (r, _) = Unix.waitpid [Unix.WNOHANG] pid in
  let _ = do { if r = pid then do { Printf.eprintf "--- pid ended %d\n" r; flush stderr } else () } in
         r <> pid)
      state.waiting_pids
;

value browse state browser port dbn = do {
  let pid =
    match browser with
    [ Some browser ->
        exec browser [sprintf "http://localhost:%d/%s" port dbn]
          Unix.stdout Unix.stderr
    | None -> -1 ]
  in
  if pid = -1 then do {
    eprintf "Open http://localhost:%d/%s in your favorite browser.\n"
      port dbn;
    flush stderr;
  }
  else state.waiting_pids := [pid :: state.waiting_pids];
  clean_waiting_pids state;
};

value window_centering win = do {
  let main_frame = Frame.create win [] in

  let frame1 = Frame.create main_frame [] in
  grid [frame1] [Row 0];
  Grid.row_configure main_frame 0 [Weight 1];

  let frame2 = Frame.create main_frame [] in
  grid [frame2] [Row 1];

  let frame3 = Frame.create main_frame [] in
  grid [frame3] [Row 2];
  Grid.row_configure main_frame 2 [Weight 1];

  pack [main_frame] [Expand True; Fill Fill_Both];

  (frame2, main_frame)
};

value tk_getOpenDir initialdir =
  let res =
    Protocol.tkEval
      [| Protocol.TkToken "tk_chooseDirectory";
         Protocol.TkToken "-initialdir"; Protocol.TkToken initialdir |]
  in
  if res = "" then initialdir else res
;

value button_create wid params comm =
  Button.create wid [Command comm :: params]
;

value config state title v def select to_string from_string set prev next =
  match
    try Some (from_string (v ())) with
    [ Failure _ | Not_found -> None ]
  with
  [ Some v -> do { set state v; next state }
  | None -> do {
      let (frame, gframe) = window_centering state.tk_win in
      let tit = Label.create frame [Text (title ())] in
      let var = Textvariable.create () in
      Textvariable.set var (to_string def);

      let sel = select frame var in
      let kont f state =
        match
          try Some (from_string (Textvariable.get var)) with
          [ Failure _ -> None ]
        with
        [ Some d -> do {
            Pack.forget [gframe];
            set state d;
            f state
          }
        | None -> () ]
      in

      let buts =
        match prev with
        [ Some (prev_var, prev) -> do {
            let buts = Frame.create frame [] in
            let kont_prev () = do {
              Pack.forget [gframe]; 
              let state =
                {(state) with
                 config_env =
                   List.remove_assoc prev_var state.config_env}
              in
              kont prev state
            }
            in
            let but1 = button_create buts [Text (transl "Prev")] kont_prev in
            let but2 =
              button_create buts [Text (transl "Next")]
                (fun _ -> kont next state)
            in
            pack [but1] [Side Side_Left];
            pack [but2] [Side Side_Right];
            buts
          }
        | None ->
            button_create frame [Text (transl "Next")]
              (fun _ -> kont next state) ]
      in
      pack [tit; sel; buts] [];
    } ]
;

value rec show_main state = do {
  clean_waiting_pids state;
  let databases =
    List.sort compare
      (List.filter (fun fn -> Filename.check_suffix fn ".gwb")
         (Array.to_list (Sys.readdir state.bases_dir)))
  in
  let (run_frame, gframe) = window_centering state.tk_win in
  let txt = Label.create run_frame [Text (transl "Server is running...")] in
  pack [txt] [];
  if databases = [] then do {
    let txt = Label.create run_frame [Text (transl "No databases.")] in
    pack [txt] [];
  }
  else do {
    let txt = Label.create run_frame [Text (transl "Available databases:")] in
    pack [txt] [];
    List.iter
      (fun dbn -> do {
         let bn = Filename.chop_extension dbn in
         let frame = Frame.create run_frame [] in
         let blab = Label.create frame [Text ("- " ^ bn ^ " -")] in
         let bbut =
           Button.create frame
             [Text (transl "Browse");
              Command (fun _ -> browse state state.browser state.port bn)]
         in
         pack [blab] [Side Side_Left];
         pack [bbut] [Side Side_Right];
         pack [frame] [Side Side_Top; Fill Fill_X];
       })
      databases;
  };
  let cbut =
    Button.create run_frame
      [Text (transl "Create a new database");
       Command
         (fun _ -> do {
            Pack.forget [gframe];
            new_database state;
          })]
  in
  let obut =
    Button.create run_frame
      [Text (transl "Change options");
       Command
         (fun _ -> do {
            Pack.forget [gframe];
            change_options state;
          })]
  in
  let rbut =
    Button.create run_frame
      [Text (transl "Restart");
       Command
         (fun _ -> do {
            Pack.forget [gframe];
            close_server state;
            launch_server state;
          })]
  in
  let wbut =
    Button.create run_frame [Text (transl "Quit"); Command closeTk]
  in
  pack [cbut; obut; rbut; wbut] [Fill Fill_X];
}

and change_options state = do {
  clean_waiting_pids state;
  let (frame, gframe) = window_centering state.tk_win in

  let tv2 = do {
    let opt = Frame.create frame [] in
    let tv = Textvariable.create () in
    let lab =
      Label.create opt [Text (transl "Select browser language if any:")]
    in
    let val1 =
      Radiobutton.create opt [Text (transl "yes"); Value "yes"; Variable tv]
    in
    let val2 =
      Radiobutton.create opt [Text (transl "no"); Value "no"; Variable tv]
    in
    Textvariable.set tv (if state.browser_lang then "yes" else "no");
    pack [lab] [Side Side_Left];
    pack [val1; val2] [Side Side_Right];
    pack [opt] [Fill Fill_X];
    tv
  }
  in

  let tv3 = do {
    let opt = Frame.create frame [] in
    let lab = Label.create opt [Text (transl "HTTP Authentication:")] in
    let tv = Textvariable.create () in
    let val1 =
      Radiobutton.create opt
        [Text (transl "basic"); Value "basic"; Variable tv]
    in
    let val2 =
      Radiobutton.create opt
        [Text (transl "digest"); Value "digest"; Variable tv]
    in
    Textvariable.set tv (if state.digest_auth then "digest" else "basic");
    pack [lab] [Side Side_Left];
    pack [val1; val2] [Side Side_Right];
    pack [opt] [Fill Fill_X];
    tv
  }
  in

  let buts = do {
    let buts = Frame.create frame [] in
    let but1 =
      button_create buts [Text (transl "Cancel")]
        (fun _ -> do {
           Pack.forget [gframe];
           show_main state;
         })
    in
    let but2 =
      button_create buts [Text (transl "Apply")]
        (fun _ -> do {
           state.browser_lang := Textvariable.get tv2 = "yes";
           state.digest_auth := Textvariable.get tv3 = "digest";
           Pack.forget [gframe];
           close_server state;
           launch_server state;
         })
    in
    pack [but1] [Side Side_Left];
    pack [but2] [Side Side_Right];
    buts
  }
  in
  pack [(*tit; sel;*) buts] [];
}

and new_database state = do {
  clean_waiting_pids state;
  let (frame, gframe) = window_centering state.tk_win in
  let tit = Label.create frame [Text (transl "Enter the name:")] in
  let var = Textvariable.create () in
  Textvariable.set var "";
  let sel = Entry.create frame [TextWidth 10; TextVariable var] in
  let buts = do {
    let buts = Frame.create frame [] in
    let but1 =
      button_create buts [Text (transl "Cancel")]
        (fun _ -> do {
           Pack.forget [gframe];
           show_main state;
         })
    in
    let but2 =
      button_create buts [Text (transl "OK")]
        (fun _ ->
           let s = Textvariable.get var in
           if s = "" then ()
           else
             loop 0 where rec loop i =
               if i = String.length s then do {
                 let db = Filename.concat state.bases_dir s in
                 if Sys.file_exists (db ^ ".gwb") then ()
                 else do {
                   let comm = Filename.concat state.bin_dir "gwc" in
                   let pid = exec comm ["-o"; db] Unix.stdout Unix.stderr in
                   let (_, _) = Unix.waitpid [] pid in
                   Pack.forget [gframe];
                   show_main state;
                 }
               }
               else
                 match s.[i] with
                 [ 'a'..'z' | 'A'..'Z' | '-' | '0'..'9' -> loop (i + 1)
                 | _ -> () ])
    in
    pack [but1] [Side Side_Left];
    pack [but2] [Side Side_Right];
    buts
  }
  in
  pack [tit; sel; buts] [];
}

and launch_server state = do {
  clean_waiting_pids state;
  let only = Unix.gethostname () in
  let fd =
    if trace.val then Unix.stdout
    else
      Unix.openfile "gwd.log" [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
        0o666
  in
  let stop_server =
    List.fold_left Filename.concat state.bases_dir ["cnt"; "STOP_SERVER"]
  in
  try Sys.remove stop_server with [ Sys_error _ -> () ];
  let rest_of_args =
    try
      let v = List.assoc "gwd_args" state.config_env in
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
    if state.digest_auth then ["-digest" :: rest_of_args] else rest_of_args
  in
  let rest_of_args =
    if state.browser_lang then ["-blang" :: rest_of_args] else rest_of_args
  in
  let comm = Filename.concat state.bin_dir "gwd" in
  let args =
    ["-p"; sprintf "%d" state.port; "-only"; "localhost"; "-only";
     "127.0.0.1"; "-only"; only; "-hd"; state.sys_dir; "-bd";
     state.bases_dir; "-dd"; state.doc_dir :: rest_of_args]
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
  state.server_running := Some server_pid;
  show_main state;
};

value rec config_bases_dir state =
  config state (fun () -> transl "Databases directory:")
    (fun () -> List.assoc "bases_dir" state.config_env) state.bases_dir
    (fun frame var -> do {
       let sframe = Frame.create frame [] in
       let lab = Label.create sframe [TextVariable var] in
       let but =
         button_create sframe [Text (transl "Select")]
           (fun () ->
              let d = Textvariable.get var in
              Textvariable.set var (tk_getOpenDir d))
       in
       pack [lab; but] [];
       sframe
     })
    (fun s -> s) (fun s -> s)
    (fun state bases_dir -> state.bases_dir := bases_dir)
    (Some ("browser", config_browser))
    (fun state -> do {
       let bases_dir = state.bases_dir in
       eprintf "bases_dir = %s\n" bases_dir;
       flush stderr;
       let config_env_bases_dir =
         try List.assoc "bases_dir" state.config_env with
         [ Not_found -> "" ]
       in
       if bases_dir <> config_env_bases_dir ||
          not (Sys.file_exists config_file)
       then do {
         state.config_env :=
           List.filter (fun (v, _) -> v <> "bases_dir") state.config_env @
           [("bases_dir", bases_dir)];
         write_config_env state.config_env
       }
       else ();
       mkdir_p bases_dir;
       launch_server state
     })

and config_browser state =
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
  let other_browser_var = lazy (Textvariable.create ()) in

  let select frame var = do {
    let browsers = Lazy.force browsers in
    let other_browser_var = Lazy.force other_browser_var in
    let other_browser_dir = Textvariable.create () in
    Textvariable.set other_browser_dir default_sys_bin_dir;
    let sframe = Frame.create frame [] in
    let list =
      if browsers = [] then do {
        let bro = Label.create sframe [TextVariable other_browser_var] in
        Textvariable.set var "other";
        bro
      }
      else do {
        let list = Frame.create sframe [] in
        List.iter
          (fun fn -> do {
             let frad = Frame.create list [] in
             let rad =
               Radiobutton.create frad [Text fn; Variable var; Value fn]
             in
             pack [rad] [Side Side_Left];
             pack [frad] [Fill Fill_X];
           })
           browsers;
        let frad = Frame.create list [] in
        Textvariable.set other_browser_var
          (match state.browser with
           [ Some s -> if List.mem s browsers then transl "other:" else s
           | None -> transl "other:" ]);
        let rad =
          Radiobutton.create frad
            [TextVariable other_browser_var; Variable var; Value "other"]
        in
        pack [rad] [Side Side_Left];
        pack [frad] [Fill Fill_X];
        Textvariable.set var
          (match state.browser with
           [ Some s -> if List.mem s browsers then s else "other"
           | None -> List.hd browsers ]);
        list
      }
    in
    let but =
      button_create sframe [Text (transl "Select")]
        (fun () -> do {
           Textvariable.set var "other";
           let ini_dir = Textvariable.get other_browser_dir in
           let br = Tk.getOpenFile [InitialDir ini_dir] in
           if br <> "" then do {
             Textvariable.set other_browser_var br;
             Textvariable.set other_browser_dir (Filename.dirname br);
           }
           else ();
         })
    in
    pack [list; but] [];
    sframe
  }
  in
  let to_string =
    fun
    [ Some s -> s
    | None -> "" ]
  in
  let from_string s =
    let s =
      if s = "other" then Textvariable.get (Lazy.force other_browser_var)
      else s
    in
    let s = if s = "other" then "" else s in
    if s = "" then None else Some s
  in
  config state
    (fun () ->
       if Lazy.force browsers = [] then transl "Browser:"
       else transl "Browser(s):")
    (fun () -> List.assoc "browser" state.config_env)
    None select to_string from_string
    (fun state browser -> state.browser := browser)
    (Some ("port", config_port))
    (fun state -> do {
       let browser = state.browser in
       match browser with
       [ Some browser -> eprintf "browser = %s\n" browser
       | None -> eprintf "no browser\n" ];
       flush stderr;
       match browser with
       [ Some browser ->
           let config_env_browser =
             try List.assoc "browser" state.config_env with
             [ Not_found -> "" ]
           in
           if browser <> "" && browser <> config_env_browser then do {
             state.config_env :=
               List.filter (fun (v, _) -> v <> "browser") state.config_env @
               [("browser", browser)];
             write_config_env state.config_env
           }
           else ()
       | None -> () ];
       config_bases_dir state
     })

and config_port state =
  config state (fun () -> transl "Port:")
    (fun () -> List.assoc "port" state.config_env) state.port
    (fun frame var -> do {
       let ent = Entry.create frame [TextWidth 5; TextVariable var] in
       Entry.selection_to ent End;
       ent
     })
    string_of_int
    (fun s ->
       let i = int_of_string s in
       if i < 1024 then failwith "bad value" else i)
    (fun state port -> state.port := port)
    (Some ("sys_dir", config_sys_dir))
    (fun state -> do {
       let port = state.port in
       eprintf "port = %d\n" port;
       flush stderr;
       let config_env_port =
         try int_of_string (List.assoc "port" state.config_env) with
         [ Failure _ | Not_found -> 0 ]
       in
       if port <> config_env_port || not (Sys.file_exists config_file)
       then do {
         state.config_env :=
           List.filter (fun (v, _) -> v <> "port") state.config_env @
           [("port", string_of_int port)];
         write_config_env state.config_env
       }
       else ();
       config_browser state
     })

and config_sys_dir state =
  config state (fun () -> transl "GeneWeb system directory:")
    (fun () -> List.assoc "sys_dir" state.config_env) state.sys_dir
    (fun frame var -> do {
       let sframe = Frame.create frame [] in
       let lab = Label.create sframe [TextVariable var] in
       let but =
         button_create sframe [Text (transl "Select")]
           (fun () ->
              let d = Textvariable.get var in
              Textvariable.set var (tk_getOpenDir d))
       in
       pack [lab; but] [];
       sframe
     })
    (fun s -> s) (fun s -> s)
    (fun state sys_dir -> state.sys_dir := sys_dir)
    (Some ("bin_dir", config_bin_dir))
    (fun state -> do {
       let sys_dir = state.sys_dir in
       eprintf "sys_dir = %s\n" sys_dir;
       flush stderr;
       let config_env_sys_dir =
         try List.assoc "sys_dir" state.config_env with
         [ Not_found -> "" ]
       in
       if sys_dir <> config_env_sys_dir || not (Sys.file_exists config_file)
       then do {
         state.config_env :=
           List.filter (fun (v, _) -> v <> "sys_dir") state.config_env @
           [("sys_dir", sys_dir)];
         write_config_env state.config_env
       }
       else ();
       config_port state
     })

and config_bin_dir state =
  config
    state (fun () -> transl "GeneWeb binary directory:")
    (fun () -> List.assoc "bin_dir" state.config_env) state.bin_dir
    (fun frame var -> do {
       let sframe = Frame.create frame [] in
       let lab = Label.create sframe [TextVariable var] in
       let but =
         button_create sframe [Text (transl "Select")]
           (fun () ->
              let d = Textvariable.get var in
              Textvariable.set var (tk_getOpenDir d))
       in
       pack [lab; but] [];
       sframe
     })
    (fun s -> s) (fun s -> s)
    (fun state bin_dir -> state.bin_dir := bin_dir)
    None
    (fun state -> do {
       let bin_dir = state.bin_dir in
       eprintf "bin_dir = %s\n" bin_dir;
       flush stderr;
       let config_env_bin_dir =
         try List.assoc "bin_dir" state.config_env with
         [ Not_found -> "" ]
       in
       if bin_dir <> config_env_bin_dir || not (Sys.file_exists config_file)
       then do {
         state.config_env :=
           List.filter (fun (v, _) -> v <> "bin_dir") state.config_env @
           [("bin_dir", bin_dir)];
         write_config_env state.config_env
       }
       else ();
       config_sys_dir state
     })
;

value default_bin_dir = "../src";
value default_sys_dir = "../hd";
value default_doc_dir = "../doc";
value default_port = 2317;
value default_browser = None;
value default_bases_dir = "../../gwbases";

value speclist = [("-trace", Arg.Set trace, " Trace server")];
value anon_fun s =
  raise (Arg.Bad (sprintf "Don't know what to do with %s" s))
;
value usage_msg = "Usage: launch [option]";

value main () = do {
  Arg.parse (Arg.align speclist) anon_fun usage_msg;
  let config_env = read_config_env () in
  let win = openTk () in
  let state =
    {config_env = config_env; tk_win = win; bin_dir = default_bin_dir;
     sys_dir = default_sys_dir; doc_dir = default_doc_dir;
     port = default_port; browser = default_browser;
     bases_dir = default_bases_dir; browser_lang = True; digest_auth = True;
     server_running = None; waiting_pids = []}
  in
  Encoding.system_set "utf-8";
  Wm.minsize_set state.tk_win 400 300;

  config_bin_dir state;

  Sys.catch_break True;
  try mainLoop () with [ Sys.Break -> () ];
  close_server state;
  eprintf "Bye\n"; flush stderr;
};

Printexc.catch main ();
