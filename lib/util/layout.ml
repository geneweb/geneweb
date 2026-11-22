type t = { bname : string; reorg : bool }
type mode = Reorg | Legacy | Detect

let ( // ) = Filename.concat

let bpath bname =
  if not (Mutil.good_name bname) then invalid_arg "Layout.bpath";
  Secure.bases_dir () // (bname ^ ".gwb")

let reorg_gwf bname = bpath bname // "config" // (bname ^ ".gwf")
let is_reorg_base bname = Sys.file_exists (reorg_gwf bname)

let of_bname ~mode bname =
  if not (Mutil.good_name bname) then invalid_arg "Layout.of_bname";
  let reorg =
    match mode with
    | Reorg -> true
    | Legacy -> false
    | Detect -> is_reorg_base bname
  in
  { bname; reorg }

let is_reorg { reorg; _ } = reorg

let gwf { bname; reorg } =
  if reorg then reorg_gwf bname else Secure.bases_dir () // (bname ^ ".gwf")

let config { bname; reorg } =
  if reorg then bpath bname // "config" else Secure.bases_dir ()

let cnt l = if l.reorg then config l // "cnt" else Secure.bases_dir () // "cnt"

let subdir dir { bname; reorg } =
  if reorg then bpath bname // dir else Secure.bases_dir () // dir // bname

let etc = subdir "etc"
let src = subdir "src"
let lang = subdir "lang"

let documents dir l =
  if l.reorg then bpath l.bname // "documents" // dir else src l // dir

let portraits l =
  if l.reorg then documents "portraits" l
  else Secure.bases_dir () // "images" // l.bname

let images = documents "images"
let albums = documents "albums"
