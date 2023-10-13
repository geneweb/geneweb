type meta = {
  version : string;
  maintainers : string list;
  depends : string list;
}

let parse fname =
  let ic = open_in fname in
  let rec loop meta =
    match input_line ic with
    | exception End_of_file ->
        close_in ic;
        meta
    | s -> (
        match String.index_opt s ':' with
        | None -> loop meta
        | Some i -> (
            let v =
              String.trim @@ String.sub s (i + 1) (String.length s - i - 1)
            in
            let split_and_trim v =
              List.map String.trim @@ String.split_on_char ',' v
            in
            match String.trim @@ String.sub s 0 i with
            | "depends" -> loop { meta with depends = split_and_trim v }
            | "maintainers" -> loop { meta with maintainers = split_and_trim v }
            | "version" -> loop { meta with version = v }
            | _ -> loop meta))
  in
  loop { version = ""; maintainers = []; depends = [] }
