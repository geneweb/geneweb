let () =
  match Sys.argv.(1) with
  | "Win32" -> print_string "(-lshell32 -lole32 -luuid)"
  | _ -> print_string "()"
