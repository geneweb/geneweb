val orphans : string -> unit
(** [orphans lexicon] prints the entries of [lexicon] that neither the OCaml
    sources of [lib], [bin] and [plugins] nor the templates of [hd/etc] and of
    the plugins use, then the keys these sources pass to the translation
    functions that no lexicon defines, plugin lexicons included. Paths are
    relative to the current directory.

    An OCaml string literal marks the entry of the same key as used, a literal
    holding [%d] the entries it formats, such as [cousin.%d.%d], and a literal
    prefix or suffix of a concatenation, such as ["NOTIF " ^ key] or
    [key ^ " (at)"], the entries it begins or ends; such an affix must have at
    least four characters, two of them letters. In a template, a key holding a
    parameter of its macro is substituted on application: it marks the entries
    it can produce, and a literal argument of [%apply] the entry of the same
    key. Brackets in [<script>] and [<style>] blocks and in [pattern] attributes
    are code: they are reported as missing keys only when they hold a phrase
    (words separated by spaces, without code punctuation). *)
