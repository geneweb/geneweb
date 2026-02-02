type t = Uri.t

let make ~conf ~query =
  Uri.make ~path:conf.Config.Trimmed.bname
    ~query:
      (("lang", [ conf.Config.Trimmed.lang ])
      :: List.map (fun (key, value) -> (key, [ value ])) query)
    ()

let to_string url = Uri.to_string url
