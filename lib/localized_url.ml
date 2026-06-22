type t = { lang : Lang.t; url : Uri.t }

let make ?path ~conf ~lang ~query () =
  let url =
    Uri.make
      ~path:(Option.value ~default:conf.Config.Trimmed.bname path)
      ~query:(("lang", [ Lang.tag lang ]) :: query)
      ()
  in
  { lang; url }

let make' ?path ~conf ~lang ~query () =
  make ?path ~conf ~lang
    ~query:(List.map (fun (key, value) -> (key, [ value ])) query)
    ()

let lang localized_url = localized_url.lang
let url localized_url = localized_url.url
let to_string localized_url = localized_url |> url |> Ext_uri.to_string

let query localized_url =
  ("lang", [ Lang.tag localized_url.lang ]) :: Uri.query localized_url.url

let with_fragment localized_url fragment =
  { localized_url with url = Uri.with_fragment localized_url.url fragment }
