type meta = {
  version : string;
  maintainers : string list;
  depends : string list;
}

val parse : string -> meta
