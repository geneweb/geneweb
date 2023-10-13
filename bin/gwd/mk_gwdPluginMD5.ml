let md5 plugin =
  let files =
    let rec loop result = function
      | [] -> result
      | f :: fs ->
          if Sys.file_exists f then
            if String.get f (String.length f - 1) = '~' then loop result fs
            else if Sys.is_directory f then
              Sys.readdir f |> Array.to_list
              |> List.rev_map (Filename.concat f)
              |> List.rev_append fs |> loop result
            else loop (f :: result) fs
          else loop result fs
    in
    loop []
      [
        Filename.concat plugin @@ "plugin_" ^ Filename.basename plugin ^ ".cmxs";
        Filename.concat plugin "assets";
      ]
  in
  let files = List.sort compare files in
  let b = Buffer.create 1024 in
  List.iter
    (fun f -> Digest.file f |> Digest.to_hex |> Buffer.add_string b)
    files;
  Buffer.contents b |> Digest.string |> Digest.to_hex

let () =
  print_endline
    {|let md5 plugin =
  let files =
    let rec loop result = function
      | [] -> result
      | f :: fs ->
        if Sys.file_exists f
        then
          if String.get f (String.length f - 1) = '~'
          then loop result fs
          else if Sys.is_directory f
          then
            Sys.readdir f
            |> Array.to_list
            |> List.rev_map (Filename.concat f)
            |> List.rev_append fs
            |> loop result
          else (loop (f :: result) fs)
        else (loop result fs)
    in
    loop [] [ Filename.concat plugin @@ "plugin_" ^ Filename.basename plugin ^ ".cmxs"
            ; Filename.concat plugin "assets"
            ]
  in
  let files = List.sort compare files in
  let b = Buffer.create 1024 in
  List.iter begin fun f ->
    Digest.file f
    |> Digest.to_hex
    |> Buffer.add_string b
  end files ;
  Buffer.contents b
  |> Digest.string
  |> Digest.to_hex
|};
  print_endline {|let allowed p = match Filename.basename p with|};
  Array.iter
    (fun p ->
      print_endline
      @@ Printf.sprintf {||"%s" -> md5 p = "%s"|} p
           (md5 @@ Filename.concat Sys.argv.(1) p))
    (Sys.readdir Sys.argv.(1));
  print_endline {||_ -> false|}
