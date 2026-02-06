type t = Localized_url.t

let make ~conf ~query =
  Localized_url.make ~conf
    ~lang:
      (Option.value ~default:Lang.english
         (Lang.from_tag conf.Config.Trimmed.lang))
    ~query

let to_string url = Localized_url.to_string url
