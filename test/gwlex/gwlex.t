Sorting orders the keys ignoring ASCII case and the translations by
language, keeps one blank line between entries and the values verbatim.

  $ printf '    zoom\nfr: zoomer\nen: zoom\n\n\n    Add\nfr: ajouter\nen: add\n' > lexicon.txt
  $ printf '    add a group\nen: add a group\nde: \n\n' >> lexicon.txt
  $ printf '    cousin.0.1\n->: a son/a daughter/a child\n\n' >> lexicon.txt
  $ printf '    (space before percent)\nfr: \302\240\nen:\n' >> lexicon.txt
  $ gwlex sort lexicon.txt
  $ sed -n l lexicon.txt
      (space before percent)$
  en:$
  fr: \302\240$
  $
      Add$
  en: add$
  fr: ajouter$
  $
      add a group$
  de:$
  en: add a group$
  $
      cousin.0.1$
  ->: a son/a daughter/a child$
  $
      zoom$
  en: zoom$
  fr: zoomer$
  $ gwlex check lexicon.txt

Duplicated keys and languages are reported, kept in their order, and left to
merge by hand.

  $ printf '    b\nen: b\nfr: b\nfr: bb\n\n    a\nen: a\n\n    b\nde: b\n' > dup.txt
  $ gwlex sort dup.txt
  dup.txt:4: duplicate language fr in [b], first at line 3
  dup.txt:9: duplicate key [b], first at line 1
  $ cat dup.txt
      a
  en: a
  
      b
  en: b
  fr: b
  fr: bb
  
      b
  de: b
  $ gwlex check dup.txt
  dup.txt:7: duplicate language fr in [b], first at line 6
  dup.txt:9: duplicate key [b], first at line 4
  [1]

Malformed lines stop the sort.

  $ printf 'fr: orphan\n\n    a\nen:a\nfoo\n' > bad.txt
  $ gwlex sort bad.txt
  bad.txt:1: line outside an entry
  bad.txt:4: missing space after the colon
  bad.txt:5: malformed translation line
  [1]

An unsorted lexicon fails the check.

  $ printf '    b\nen: b\n\n    a\nen: a\n' > unsorted.txt
  $ gwlex check unsorted.txt
  unsorted.txt: not sorted or not normalized, run gwlex sort
  [1]
  $ gwlex sort -o - unsorted.txt
      a
  en: a
  
      b
  en: b

The placeholders of the translations must match the English ones.

  $ printf '    %%d years\nde: %%s Jahre\nen: %%d years\nfr: %%d ans\n\n' > formats.txt
  $ printf '    him/her\nen: him/her\nfr: lui/elle\nit: lui\n' >> formats.txt
  $ gwlex check formats.txt
  $ gwlex check --formats formats.txt
  formats.txt:2: [%d years] de: placeholders differ from en
  [1]

Missing languages are listed empty, translations with another number of
variants than the English one with their text.

  $ gwlex missing --lang de,it formats.txt
      %d years
  en: %d years
  fr: %d ans
  it:
  
      him/her
  en: him/her
  fr: lui/elle
  de:
  it: lui
  

Orphans: entries used nowhere, and keys missing from the lexicon. A key
holding a parameter of its macro matches the entries it can produce, and a
literal argument of %apply marks its entry as used, as does a literal
prefix or suffix of a concatenation in OCaml. Brackets in scripts,
styles and pattern attributes are code unless they hold a phrase.

  $ mkdir -p repo/lib repo/hd/lang repo/hd/etc
  $ cd repo
  $ printf '    alias\n->: target\n\n    birth (at)\nen: birth\n\n    by apply\nen: x\n\n' > hd/lang/lexicon.txt
  $ printf '    cousin.1.2\nen: cousin\n\n    cousins.3.0\nen: cousins\n\n    from ml\nen: a\n\n' >> hd/lang/lexicon.txt
  $ printf '    from template\nen: b\n\n    in comment\nen: c\n\n    NOTIF by prefix\nen: n\n\n' >> hd/lang/lexicon.txt
  $ printf '    target\nen: t\n\n    unused\nen: u\n' >> hd/lang/lexicon.txt
  $ printf 'let f conf = Util.transl conf "from ml"\n' > lib/a.ml
  $ printf 'let g conf = transl_nth conf "not in lexicon" 0\n' >> lib/a.ml
  $ printf '(* transl conf "in comment" *)\n' >> lib/a.ml
  $ printf 'let h a b = Printf.sprintf "cousin.%%d.%%d" a b\n' >> lib/a.ml
  $ printf 'let n conf key = transl conf ("NOTIF " ^ key)\nlet e key = key ^ " (at)"\n' >> lib/a.ml
  $ printf '[*from template] [missing in lexicon]0 [alias]\n' > hd/etc/a.txt
  $ printf '%%define;f(xx)[cousins.xx.0]%%end;\n%%apply;f("by apply")\n' >> hd/etc/a.txt
  $ printf '<script>let a = b[i]; let t = "[missing in script]";</script>\n' >> hd/etc/a.txt
  $ printf '<input pattern="[a-z]">\n' >> hd/etc/a.txt
  $ gwlex check
  $ gwlex orphans
  Unused entries:
  hd/lang/lexicon.txt:22: in comment
  hd/lang/lexicon.txt:31: unused
  
  Keys missing from the lexicon:
  hd/etc/a.txt: missing in lexicon
  hd/etc/a.txt: missing in script
  lib/a.ml:2: not in lexicon
