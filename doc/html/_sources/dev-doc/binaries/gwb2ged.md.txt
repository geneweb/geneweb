# gwb2ged

## Documentation

Converts a Geneweb database to a GEDCOM 5.5.1 file. GEDCOM is a popular format
among genealogy enthusiasts.
[GEDCOM 5.5.1 documentation](http://www.ancestris.org/dl/ancestris/norme_gedcom/Gedcom_norm_551_2019_11_15.pdf)


```
Usage: geneweb.gwb2ged <BASE> [OPT]
  -indexes                          export indexes in gedcom
  -a <N>                            maximum generation of the root's ascendants
  -ad <N>                           maximum generation of the root's ascendants descendants
  -key <KEY>                        key reference of root person. Used for -a/-d options. Can be used multiple times. Key format is "First Name.occ SURNAME"
  -c <NUM>:                         when a person is born less than <num> years ago, it is not exported unless it is Public. All the spouses and descendants are also censored.
  -charset [ASCII|ANSEL|ANSI|UTF-8] set charset; default is UTF-8
  -d <N>                            maximum generation of the root's descendants.
  -mem                              save memory space, but slower.
  -nn                               no (database) notes.
  -nnn                              no notes (implies -nn).
  -nopicture                        don't extract individual picture.
  -o <GED>                          output file name (default: stdout).
  -parentship                       select individuals involved in parentship computation between pairs of keys. Pairs must be defined with -key option, descendant first: e.g. -key "Descendant.0 SURNAME" -key "Ancestor.0 SURNAME". If multiple pair are provided, union of persons are returned.
  -picture-path                     extract pictures path.
  -s <SN>                           select this surname (option usable several times, union of surnames will be used).
  -source <SRC>                     replace individuals and families sources. Also delete event sources.
  -v                                verbose
```
