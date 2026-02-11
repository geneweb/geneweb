val lexicon_files : string list ref

val all_lexicons :
  (string
  * ( string,
      ?kwargs:Jingoo.Jg_types.kwargs -> int -> Jingoo.Jg_types.tvalue )
    Hashtbl.t)
  list
  Lazy.t
