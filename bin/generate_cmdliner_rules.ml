module Fs = Filesystem

let split_path = String.split_on_char Filename.dir_sep.[0]

let concat_path l =
  let rec loop l =
    match l with
    | [] -> assert false
    | [ x ] -> x
    | x :: xs ->
        let p = loop xs in
        Filename.concat x p
  in
  loop l

let pp_as ppf () = Fmt.pf ppf " as "

let () =
  let path = Sys.argv.(1) in
  let files =
    Filesystem.walk_folder ~recursive:true
      (fun e acc ->
        match e with
        | File s ->
            let dest =
              match split_path s with
              | "cmdliner-support" :: "share" :: l -> concat_path l
              | _ -> Fmt.failwith "Unexpected file %S" s
            in
            (s, dest) :: acc
        | Dir _ | Exn _ -> acc)
      path []
  in
  Fmt.pr "@[(%a)@]@."
    Fmt.(list ~sep:cut @@ parens @@ pair ~sep:pp_as string string)
    files
