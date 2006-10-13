(* $Id: launch.ml,v 1.4 2006-10-13 13:01:56 ddr Exp $ *)
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

value default_port = 2317;
value default_bin_dir = "../src";
value default_sys_dir = "../hd";
value default_bases_dir = "../../gwbases";

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
  (* If the server detects the presence of the file STOP_SERVER, it stops *)
  let oc = open_out "STOP_SERVER" in
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

value finish state = do {
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

value continue_with_bases_dir state bases_dir = do {
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
  state.bases_dir := bases_dir;
  mkdir_p bases_dir;
  let only = Unix.gethostname () in
  let fd =
    Unix.openfile "gwd.log" [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644
  in
  try Sys.remove "STOP_SERVER" with [ Sys_error _ -> () ];
  let server_pid =
    exec (Filename.concat state.bin_dir "gwd")
      ["-p"; sprintf "%d" state.port; "-only"; "localhost"; "-only";
       "127.0.0.1"; "-only"; only; "-hd"; state.sys_dir; "-bd"; bases_dir;
       "-blang"] fd fd
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
  finish state;
};

value continue_with_browser state browser = do {
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
  state.browser := browser;
  try
    continue_with_bases_dir state (List.assoc "bases_dir" state.config_env)
  with
  [ Not_found -> do {
      let (frame, gframe) = window_centering state.tk_win in
      let tit = Label.create frame [Text "Databases directory:"] in
      let lab = Label.create frame [Text default_bases_dir] in
      let but =
        Button.create frame
          [Text "OK";
           Command
             (fun () -> do {
                Pack.forget [gframe];
                continue_with_bases_dir state default_bases_dir
              })]
      in
      pack [tit; lab; but] [];
    } ]
};

value continue_with_port state port = do {
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
  state.port := port;
  try
    continue_with_browser state
      (Some (List.assoc "browser" state.config_env))
  with
  [ Not_found -> do {
      let browsers =
        match
          try Some (open_in (Filename.concat "gw" "browsers.txt")) with
          [ Sys_error _ -> None ]
        with
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
      let browsers =
        List.filter Sys.file_exists
          (List.rev_append browsers default_browsers)
      in
      let (frame, gframe) = window_centering state.tk_win in
      if browsers = [] then
        let lab = Label.create frame [Text "No default browser"] in
        pack [lab] []
      else do {
        let txt = Label.create frame [Text "Browser(s):"] in
        pack [txt] [Side Side_Top];
        List.iter
          (fun fn -> do {
             let frame1 = Frame.create frame [] in
             let blab = Label.create frame1 [Text fn] in
             pack [blab] [Side Side_Left];
             pack [frame1] [Side Side_Top; Fill Fill_X];
           })
           browsers;
      };
      let but =
        Button.create frame
          [Text "OK";
           Command
             (fun () -> do {
                Pack.forget [gframe];
                continue_with_browser state
                  (match browsers with
                   [ [b :: _] -> Some b
                   | [] -> None ])
              })]
      in
      pack [but] [];
    } ];
};

value continue_with_sys_dir state sys_dir = do {
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
  state.sys_dir := sys_dir;
  try
    continue_with_port state
      (int_of_string (List.assoc "port" state.config_env))
  with
  [ Not_found -> do {
      let (frame, gframe) = window_centering state.tk_win in
      let tit = Label.create frame [Text "Port:"] in
      let lab = Label.create frame [Text (string_of_int default_port)] in
      let but =
        Button.create frame
          [Text "OK";
           Command
             (fun () -> do {
                Pack.forget [gframe];
                continue_with_port state default_port
              })]
      in
      pack [tit; lab; but] [];
    } ]
};

value continue_with_bin_dir state bin_dir = do {
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
  state.bin_dir := bin_dir;
  try
    continue_with_sys_dir state (List.assoc "sys_dir" state.config_env)
  with
  [ Not_found -> do {
      let (frame, gframe) = window_centering state.tk_win in
      let tit = Label.create frame [Text "GeneWeb system directory:"] in
      let lab = Label.create frame [Text default_sys_dir] in
      let but =
        Button.create frame
          [Text "OK";
           Command
             (fun () -> do {
                Pack.forget [gframe];
                continue_with_sys_dir state default_sys_dir
              })]
      in
      pack [tit; lab; but] [];
    } ]
};

value main () = do {
  let config_env = read_config_env () in
  let win = openTk () in
  let state =
    {config_env = config_env; tk_win = win; bin_dir = ""; sys_dir = "";
     port = 0; browser = None; bases_dir = ""; server_running = False}
  in
  Encoding.system_set "utf-8";
  Wm.minsize_set state.tk_win 300 200;
  try continue_with_bin_dir state (List.assoc "bin_dir" config_env) with
  [ Not_found -> do {
      let (frame, gframe) = window_centering win in
      let tit = Label.create frame [Text "GeneWeb binary directory:"] in
      let lab = Label.create frame [Text default_bin_dir] in
      let but =
        Button.create frame
          [Text "OK";
           Command
             (fun () -> do {
                Pack.forget [gframe];
                continue_with_bin_dir state default_bin_dir
              })]
      in
      pack [tit; lab; but] [];
    } ];
  Sys.catch_break True;
  try mainLoop () with [ Sys.Break -> () ];
  close_app state;
  eprintf "Bye\n"; flush stderr;
};

Printexc.catch main ();
