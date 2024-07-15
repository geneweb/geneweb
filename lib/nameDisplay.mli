val hidden_name_txt : Adef.safe_string

val map_person_name_visibility :
  ?on_hidden_name:
    (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string) ->
  ?on_restricted_name:
    (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string) ->
  on_visible_name:
    (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string) ->
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string

val first_name_html_of_person :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string

val fullname_html_of_person :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string

val fullname_str_of_person :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
