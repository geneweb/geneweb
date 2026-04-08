module Config = Geneweb.Config
module Compat = Geneweb_compat
module H = Digestif.SHA256
module Fpath = Geneweb_fs.Fpath
module File = Geneweb_fs.File

let assets = ref Fpath.empty
let registered = ref []

let ht : (string, string * (Config.config -> Fpath.t option -> bool)) Hashtbl.t
    =
  Hashtbl.create 0

let register ~ns list =
  assert (not @@ List.mem ns !registered);
  registered := ns :: !registered;
  List.iter
    (fun (m, fn) ->
      let fn = fn !assets in
      Hashtbl.add ht m (ns, fn))
    list

let se : (string * (Config.config -> Fpath.t option -> unit)) list ref = ref []
let register_se ~ns fn = Mutil.list_ref_append se (ns, fn !assets)

let feed_file ctx path =
  let buffer_size = 0x1000 in
  let bytes = Bytes.create buffer_size in
  let rec loop ctx ic =
    match Compat.In_channel.input ic bytes 0 buffer_size with
    | 0 -> ctx
    | read ->
        let ctx = H.feed_bytes ctx ~len:read bytes in
        loop ctx ic
  in
  Compat.In_channel.with_open_bin (Fpath.to_string path) (loop ctx)

let checksum path =
  (* FIXME: As [File.walk_folder] does not walk directories in
     independent-platform order, we sort its output. *)
  let files =
    File.walk_folder ~recursive:true
      (fun e acc ->
        match e with
        | File f -> if Fpath.extension f = ".cmxs" then f :: acc else acc
        | Dir _ -> acc
        | Exn { exn; bt; _ } -> Printexc.raise_with_backtrace exn bt)
      path []
  in
  let files = List.sort Fpath.compare files in
  let ctx = H.init () in
  let ctx = List.fold_left feed_file ctx files in
  H.to_hex @@ H.get ctx

module MS = Map.Make (String)

type mark = Unmarked | Visiting | Marked

let topological_sort (type a) (t : (a * a list) list) : (a list, a list) result
    =
  let exception Cycle of a list in
  let marks : (a, mark) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun (n, _) -> Hashtbl.replace marks n Unmarked) t;
  let outgoings : (a, a list) Hashtbl.t = Hashtbl.of_seq @@ List.to_seq t in
  let rec visit path acc n =
    match Hashtbl.find marks n with
    | Marked -> acc
    | Visiting -> raise_notrace (Cycle (n :: path))
    | Unmarked ->
        Hashtbl.replace marks n Visiting;
        let outgoings = Hashtbl.find outgoings n in
        let acc = List.fold_left (visit (n :: path)) acc outgoings in
        Hashtbl.replace marks n Marked;
        n :: acc
  in
  match List.fold_left (fun acc (n, _) -> visit [] acc n) [] t with
  | l -> Ok (List.rev l)
  | exception Cycle l -> Error (List.rev l)

let compute_dependencies path =
  let deps =
    File.walk_folder
      (fun e acc ->
        match e with
        | Dir d ->
            let meta_file = Fpath.(d // ~$"META") in
            MS.update (Fpath.basename d)
              (fun o ->
                match o with
                | Some _ -> o
                | None ->
                    if File.file_exists meta_file then
                      Some Meta.(parse meta_file).depends
                    else Some [])
              acc
        | Exn { path = _; exn; bt } -> Printexc.raise_with_backtrace exn bt
        | _ -> acc)
      path MS.empty
  in
  topological_sort @@ List.of_seq @@ MS.to_seq deps
