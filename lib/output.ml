let status conf s = conf.Config.output_conf.Config.status s
let header conf fmt = Printf.ksprintf conf.Config.output_conf.Config.header fmt
let print_sstring conf s = conf.Config.output_conf.Config.body s

let print_string conf (s : [< `encoded | `escaped | `safe ] Adef.astring) =
  conf.Config.output_conf.Config.body ((s :> Adef.safe_string) :> string)

let printf conf fmt = Printf.ksprintf conf.Config.output_conf.Config.body fmt
let flush conf = conf.Config.output_conf.Config.flush ()

let link_header ?(alternate_urls = []) conf canonical_url =
  let format_link (url, params) =
    let format_params params =
      String.concat "; "
        (List.map
           (fun (key, value) -> Printf.sprintf "%s=\"%s\"" key value)
           params)
    in
    Format.sprintf "<%s>; %s" url (format_params params)
  in
  let canonical_url =
    (Canonical_url.to_string canonical_url, [ ("rel", "canonical") ])
  in
  let alternate_urls =
    List.map
      (fun url ->
        ( Localized_url.to_string url,
          [
            ("rel", "alternate");
            ("hreflang", url |> Localized_url.lang |> Lang.tag);
          ] ))
      alternate_urls
  in
  header
    (Config.Trimmed.to_config conf)
    "Link: %s"
    (String.concat ", "
       (List.map format_link (canonical_url :: alternate_urls)))

let robots_tag_header ?(index = false) ?(follow = false) conf =
  let indexing_rules =
    [
      (if index then "index" else "noindex");
      (if follow then "follow" else "nofollow");
    ]
  in
  header
    (Config.Trimmed.to_config conf)
    "X-Robots-Tag: %s"
    (String.concat ", " indexing_rules)
