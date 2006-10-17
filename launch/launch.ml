(* $Id: launch.ml,v 1.13 2006-10-17 12:10:09 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Camltk;
open Printf;

type state =
  { tk_win : Widget.widget;
    config_env : mutable list (string * string);
    bin_dir : mutable string;
    sys_dir : mutable string;
    port : mutable int;
    browser : mutable option string;
    bases_dir : mutable string;
    server_running : mutable bool }
;

value config_file = Filename.concat "gw" "config.txt";

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
      let y = Filename.dirname x;
      if y <> x && String.length y < String.length x then loop y else ();
      try Unix.mkdir x 0o755 with [ Unix.Unix_error _ _ _ -> () ];
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

value close_server state = do {
  eprintf "Closing..."; flush stderr;
  (* Making a (empty) file STOP_SERVER to make the server stop. *)
  let stop_server =
    List.fold_left Filename.concat state.bases_dir ["cnt"; "STOP_SERVER"]
  in
  let oc = open_out stop_server in
  close_out oc;
  (* Send a phony connection to unblock it. *)
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  try Unix.connect s (Unix.ADDR_INET Unix.inet_addr_loopback state.port) with
  [ Unix.Unix_error _ _ _ -> () ];
  try Unix.close s with
  [ Unix.Unix_error _ _ _ -> () ];
  eprintf "\n"; flush stderr;
};

value browse browser port dbn () =
  let pid =
    match browser with
    [ Some browser ->
        let browser_pid =
          exec browser [sprintf "http://localhost:%d/%s" port dbn]
            Unix.stdout Unix.stderr
        in
        let (pid, _) = Unix.waitpid [Unix.WNOHANG] browser_pid in
        pid
    | None -> -1 ]
  in
  if pid = 0 then ()
  else do {
    eprintf "Open http://localhost:%d/%s in your favorite browser.\n"
      port dbn;
    flush stderr;
  }
;

value close_app state = do {
  if state.server_running then close_server state else ();
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
      Focus.force sel;

      let ev_seq = [([], KeyPressDetail "Return")] in
      let kont f state _ =
        match
          try Some (from_string (Textvariable.get var)) with
          [ Failure _ -> None ]
        with
        [ Some d -> do {
            bind sel ev_seq BindRemove;
            Pack.forget [gframe];
            set state d;
            f state
          }
        | None -> () ]
      in
      bind sel ev_seq (BindSet [] (kont next state));

      let buts =
        match prev with
        [ Some (prev_var, prev) -> do {
            let buts = Frame.create frame [] in
            let but1 =
              Button.create buts
                [Text "Prev";
                 Command
                   (fun () -> do {
                      bind sel ev_seq BindRemove;
                      Pack.forget [gframe]; 
                      let state =
                        {(state) with
                         config_env =
                           List.remove_assoc prev_var state.config_env}
                      in
                      kont prev state ()
                    })]
            in
            let but2 =
              Button.create buts
                [Text "Next"; Default Active; Command (kont next state)]
            in
            pack [but1] [Side Side_Left];
            pack [but2] [Side Side_Right];
            buts
          }
        | None ->
            Button.create frame
              [Text "Next"; Default Active; Command (kont next state)] ]
      in
      pack [tit; sel; buts] [];
    } ]
;

value launch_server state = do {
  let only = Unix.gethostname () in
  let fd =
    Unix.openfile "gwd.log" [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644
  in
  let stop_server =
    List.fold_left Filename.concat state.bases_dir ["cnt"; "STOP_SERVER"]
  in
  try Sys.remove stop_server with [ Sys_error _ -> () ];
  let server_pid =
    exec (Filename.concat state.bin_dir "gwd")
      ["-p"; sprintf "%d" state.port; "-only"; "localhost"; "-only";
       "127.0.0.1"; "-only"; only; "-hd"; state.sys_dir; "-bd";
       state.bases_dir; "-blang"] fd fd
  in
  let (pid, ps) = Unix.waitpid [Unix.WNOHANG] server_pid in
  if pid = 0 then ()
  else do {
    eprintf "Cannot launch the server:";
    eprintf " perhaps another server is running.\n";
    eprintf "You must close it, if you want to try again.\n";
    flush stderr;
    exit 2;
  };
  state.server_running := True;
  let databases =
    List.sort compare
      (List.filter (fun fn -> Filename.check_suffix fn ".gwb")
         (Array.to_list (Sys.readdir state.bases_dir)))
  in
  let (run_frame, _) = window_centering state.tk_win in
  let txt = Label.create run_frame [Text "Server is running..."] in
  pack [txt] [];
  if databases = [] then do {
    let txt = Label.create run_frame [Text "No databases."] in
    pack [txt] [];
  }
  else do {
    let txt = Label.create run_frame [Text "Available databases:"] in
    pack [txt] [];
    List.iter
      (fun dbn -> do {
         let frame = Frame.create run_frame [] in
         let blab = Label.create frame [Text dbn] in
         let bbut =
           let bn = Filename.chop_extension dbn in
           Button.create frame
             [Text "Browse"; Command (browse state.browser state.port bn)]
         in
         pack [blab] [Side Side_Left];
         pack [bbut] [Side Side_Right];
         pack [frame] [Side Side_Top; Fill Fill_X];
       })
      databases;
  };
  let wbut = Button.create run_frame [Text "Quit"; Command closeTk] in
  pack [wbut] [Fill Fill_X];
};

value rec config_bases_dir state =
  config state (fun () -> "Databases directory:")
    (fun () -> List.assoc "bases_dir" state.config_env) state.bases_dir
    (fun frame var -> do {
       let sframe = Frame.create frame [] in
       let lab = Label.create sframe [TextVariable var] in
       let but =
         Button.create sframe
           [Text "Select";
            Command
              (fun () ->
                 let d = Textvariable.get var in
                 Textvariable.set var (tk_getOpenDir d))]
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
       if bases_dir <> config_env_bases_dir || not (Sys.file_exists config_file)
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
           [ Some s -> if List.mem s browsers then "other:" else s
           | None -> "other:" ]);
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
      Button.create sframe
        [Text "Select";
         Command
           (fun () -> do {
              Textvariable.set var "other";
              let ini_dir = Textvariable.get other_browser_dir in
              let br = Tk.getOpenFile [InitialDir ini_dir] in
              if br <> "" then do {
                Textvariable.set other_browser_var br;
                Textvariable.set other_browser_dir (Filename.dirname br);
              }
              else ();
            })]
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
    let s = if s = "other:" then "" else s in
    if s = "" then None else Some s
  in
  config state
    (fun () -> if Lazy.force browsers = [] then "Browser:" else "Browser(s):")
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
  config state (fun () -> "Port:")
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
  config state (fun () -> "GeneWeb system directory:")
    (fun () -> List.assoc "sys_dir" state.config_env) state.sys_dir
    (fun frame var -> do {
       let sframe = Frame.create frame [] in
       let lab = Label.create sframe [TextVariable var] in
       let but =
         Button.create sframe
           [Text "Select";
            Command
              (fun () ->
                 let d = Textvariable.get var in
                 Textvariable.set var (tk_getOpenDir d))]
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
    state (fun () -> "GeneWeb binary directory:")
    (fun () -> List.assoc "bin_dir" state.config_env) state.bin_dir
    (fun frame var -> do {
       let sframe = Frame.create frame [] in
       let lab = Label.create sframe [TextVariable var] in
       let but =
         Button.create sframe
           [Text "Select";
            Command
              (fun () ->
                 let d = Textvariable.get var in
                 Textvariable.set var (tk_getOpenDir d))]
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
value default_port = 2317;
value default_browser = None;
value default_bases_dir = "../../gwbases";

value main () = do {
  let config_env = read_config_env () in
  let win = openTk () in
  let state =
    {config_env = config_env; tk_win = win; bin_dir = default_bin_dir;
     sys_dir = default_sys_dir; port = default_port;
     browser = default_browser; bases_dir = default_bases_dir;
     server_running = False}
  in
  Encoding.system_set "utf-8";
  Wm.minsize_set state.tk_win 400 300;

  config_bin_dir state;

  Sys.catch_break True;
  try mainLoop () with [ Sys.Break -> () ];
  close_app state;
  eprintf "Bye\n"; flush stderr;
};

Printexc.catch main ();
