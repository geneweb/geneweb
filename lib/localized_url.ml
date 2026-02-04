type t = { lang : Lang.t; url : Uri.t }

let make ~conf ~lang ~query =
  let url =
    Uri.make ~path:conf.Config.Trimmed.bname
      ~query:
        (("lang", [ Lang.tag lang ])
        :: List.map (fun (key, value) -> (key, [ value ])) query)
      ()
  in
  { lang; url }

let lang localized_url = localized_url.lang
let url localized_url = localized_url.url
let to_string localized_url = localized_url |> url |> Uri.to_string
