type meta = private {
  version : string;
  maintainers : string list;
  depends : string list;
}

val parse : Geneweb_fs.Fpath.t -> meta
