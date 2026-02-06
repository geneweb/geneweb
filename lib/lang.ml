type t =
  | German
  | English
  | Spanish
  | Finnish
  | French
  | Italian
  | Dutch
  | Norwegian
  | Portuguese
  | Swedish

let english = English

let from_tag = function
  | "de" -> Some German
  | "en" -> Some English
  | "es" -> Some Spanish
  | "fi" -> Some Finnish
  | "fr" -> Some French
  | "it" -> Some Italian
  | "nl" -> Some Dutch
  | "no" -> Some Norwegian
  | "pt" -> Some Portuguese
  | "sv" -> Some Swedish
  | _ -> None

let tag = function
  | German -> "de"
  | English -> "en"
  | Spanish -> "es"
  | Finnish -> "fi"
  | French -> "fr"
  | Italian -> "it"
  | Dutch -> "nl"
  | Norwegian -> "no"
  | Portuguese -> "pt"
  | Swedish -> "sv"

let all =
  [
    German;
    English;
    Spanish;
    Finnish;
    French;
    Italian;
    Dutch;
    Norwegian;
    Portuguese;
    Swedish;
  ]
