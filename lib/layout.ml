type dir = string
type file = string

type t = {
  gwf : file;
  cnt : dir;
  portraits : dir;
  src : dir;
  etc : dir;
  config : dir;
  lang : dir;
  images : dir;
  albums : dir;
}

type mode = Reorg | Legacy | Detect

let ( // ) = Filename.concat

(* Duplication of GWPARAM.bpath to avoid cyclic dependency. *)
let bpath bname =
  if not @@ Mutil.good_name bname then invalid_arg "bpath";
  Secure.bases_dir () // (bname ^ ".gwb")

let reorg_layout bname =
  if not @@ Mutil.good_name bname then invalid_arg "default";
  let cnt = bpath bname // "config" // "cnt" in
  {
    gwf = bpath bname // "config" // (bname ^ ".gwf");
    cnt;
    portraits = bpath bname // "documents" // "portraits";
    src = bpath bname // "src";
    etc = bpath bname // "etc";
    config = bpath bname // "config";
    lang = bpath bname // "lang";
    images = bpath bname // "documents" // "images";
    albums = bpath bname // "documents" // "albums";
  }

let legacy_layout bname =
  if not @@ Mutil.good_name bname then invalid_arg "legacy";
  let bases_dir = Secure.bases_dir () in
  let cnt = bases_dir // "cnt" in
  {
    gwf = bases_dir // (bname ^ ".gwf");
    cnt;
    portraits = bases_dir // "images" // bname;
    src = bases_dir // "src" // bname;
    etc = bases_dir // "etc" // bname;
    config = bases_dir;
    lang = bases_dir // "lang" // bname;
    images = bases_dir // "src" // bname // "images";
    albums = bases_dir // "src" // bname // "albums";
  }

let is_reorg_base bname = Sys.file_exists (reorg_layout bname).gwf

let of_bname ~mode bname =
  match mode with
  | Reorg -> reorg_layout bname
  | Legacy -> legacy_layout bname
  | Detect ->
      if is_reorg_base bname then reorg_layout bname else legacy_layout bname

let[@inline] gwf { gwf; _ } = gwf
let[@inline] cnt { cnt; _ } = cnt
let[@inline] portraits { portraits; _ } = portraits
let[@inline] src { src; _ } = src
let[@inline] etc { etc; _ } = etc
let[@inline] config { config; _ } = config
let[@inline] lang { lang; _ } = lang
let[@inline] images { images; _ } = images
let[@inline] albums { albums; _ } = albums
