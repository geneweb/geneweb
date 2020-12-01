let md5 plugin =
  let files =
    let rec loop result = function
      | f :: fs when Sys.file_exists f && Sys.is_directory f ->
        Sys.readdir f
        |> Array.to_list
        |> List.rev_map (Filename.concat f)
        |> List.rev_append fs
        |> loop result
      | f :: fs ->
        if Sys.file_exists f
        then loop (f :: result) fs
        else loop result fs
      | [] -> result
    in
    loop [] [ plugin ; Filename.(concat (dirname plugin)) "assets" ]
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

let () =
print_endline {|let md5 plugin =
  let files =
    let rec loop result = function
      | f :: fs when Sys.file_exists f && Sys.is_directory f ->
        Sys.readdir f
        |> Array.to_list
        |> List.rev_map (Filename.concat f)
        |> List.rev_append fs
        |> loop result
      | f :: fs ->
        if Sys.file_exists f
        then loop (f :: result) fs
        else loop result fs
      | [] -> result
    in
    loop [] [ plugin ; Filename.(concat (dirname plugin)) "assets" ]
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
  |> Digest.to_hex|};
  print_endline {|let allowed p = match Filename.basename p with|} ;
  Array.iteri begin fun i p ->
    if i > 0 then begin
      print_endline @@
      Printf.sprintf {||"%s" -> md5 p = "%s"|}
        (Filename.basename p) (md5 p)
    end
  end Sys.argv ;
  print_endline {||_ -> false|}
