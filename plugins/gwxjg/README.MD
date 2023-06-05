# gwxjg

`gwxjg` means GeneWeb x Jingoo. This package translate data from Geneweb
structure to Jingoo's type system. Below, see a quick reference of which
structures and which fields are accessible from a Jingoo template.

If you (approximately) know how to read OCaml code, the best is to read
[Data module](data.ml), where everything is defined.

Alternatively, here is a simplified view of what you get when using `gwxjg`

## boolean

Either `true` or `false`.

## burial

- `type`: [burial type]
- `date`: [date]

## burial type

A [burial type] is `"Buried"` or `"Cremated"`

## calendar

A [calendar] is one of these [string]:
- `"Dgregorian"`
- `"Djulian"`
- `"Dfrench"`
- `"Dhebrew"`

## date

- `calendar`: [calendar]
- `d2`: [date]
- `day`: [int]
- `month`: [int]
- `prec`: [prec]
- `year`: [int]

## death

- `date` : [date]
- `death_reason`: [death reason]

## death reason

A [death reason] is one of these [string]:
- `"Killed"`
- `"Murdered"`
- `"Executed"`
- `"Disappeared"`
- `"Unspecified"`
- `"DeadYoung"`
- `"DeadDontKnowWhen"`
- `"DontKnowIfDead"`
- `"OfCourseDead"`

## event

- `date`: [date]
- `kind`: [event kind]
- `name`: [string]
- `note`: [string]
- `place`: [place]
- `spouse`: [person]
- `src`: [string]
- `witnesses`: [witness] [list]

## event kind

A [event kind] is either *free text* or one of these [string]:
- `"EPERS_BIRTH"`
- `"EPERS_BAPTISM"`
- `"EPERS_DEATH"`
- `"EPERS_BURIAL"`
- `"EPERS_CREMATION"`
- `"EPERS_ACCOMPLISHMENT"`
- `"EPERS_ACQUISITION"`
- `"EPERS_ADHESION"`
- `"EPERS_BAPTISMLDS"`
- `"EPERS_BARMITZVAH"`
- `"EPERS_BATMITZVAH"`
- `"EPERS_BENEDICTION"`
- `"EPERS_CHANGENAME"`
- `"EPERS_CIRCUMCISION"`
- `"EPERS_CONFIRMATION"`
- `"EPERS_CONFIRMATIONLDS"`
- `"EPERS_DECORATION"`
- `"EPERS_DEMOBILISATIONMILITAIRE"`
- `"EPERS_DIPLOMA"`
- `"EPERS_DISTINCTION"`
- `"EPERS_DOTATION"`
- `"EPERS_DOTATIONLDS"`
- `"EPERS_EDUCATION"`
- `"EPERS_ELECTION"`
- `"EPERS_EMIGRATION"`
- `"EPERS_EXCOMMUNICATION"`
- `"EPERS_FAMILYLINKLDS"`
- `"EPERS_FIRSTCOMMUNION"`
- `"EPERS_FUNERAL"`
- `"EPERS_GRADUATE"`
- `"EPERS_HOSPITALISATION"`
- `"EPERS_ILLNESS"`
- `"EPERS_IMMIGRATION"`
- `"EPERS_LISTEPASSENGER"`
- `"EPERS_MILITARYDISTINCTION"`
- `"EPERS_MILITARYPROMOTION"`
- `"EPERS_MILITARYSERVICE"`
- `"EPERS_MOBILISATIONMILITAIRE"`
- `"EPERS_NATURALISATION"`
- `"EPERS_OCCUPATION"`
- `"EPERS_ORDINATION"`
- `"EPERS_PROPERTY"`
- `"EPERS_RECENSEMENT"`
- `"EPERS_RESIDENCE"`
- `"EPERS_RETIRED"`
- `"EPERS_SCELLENTCHILDLDS"`
- `"EPERS_SCELLENTPARENTLDS"`
- `"EPERS_SCELLENTSPOUSELDS"`
- `"EPERS_VENTEBIEN"`
- `"EPERS_WILL"`
- `"EFAM_MARRIAGE"`
- `"EFAM_NO_MARRIAGE"`
- `"EFAM_NO_MENTION"`
- `"EFAM_ENGAGE"`
- `"EFAM_DIVORCE"`
- `"EFAM_SEPARATED"`
- `"EFAM_ANNULATION"`
- `"EFAM_MARRIAGE_BANN"`
- `"EFAM_MARRIAGE_CONTRACT"`
- `"EFAM_MARRIAGE_LICENSE"`
- `"EFAM_PACS"`
- `"EFAM_RESIDENCE"`

## family

- `divorce_date`: [date]
- `children`: [person] [list]
- `father`: [person]
- `events`: [event] [list]
- `ifam`: [string]
- `marriage_date`: [date]
- `marriage_place`: [place]
- `marriage_note`: [string]
- `marriage_source`: [string]
- `mother`: [person]
- `origin_file`: [string]
- `relation`: [relation]
- `separation`: [separation]
- `spouse`: [person]
- `witnesses`: [persons] [list]

## float

A floating point number.

## int

An integer.

## list

A sequence of zero or more items.

## person

- `access`: [string]
- `baptism_date`: [date]
- `baptism_place`: [place]
- `birth_date`: [date]
- `birth_place`: [place]
- `burial`: [burial]
- `burial_place`: [string]
- `children`: [person] [list]
- `cremation_place`: [place]
- `consanguinity`: [float]
- `dates`: [string]
- `death`: [death]
- `death_place`: [place]
- `digest`: [string]
- `events`: [event] [list]
- `families`: [family] [list]
- `father`: [person]
- `first_name`: [string]
- `first_name_aliases`: [string] [list]
- `first_name_key`: [string]
- `first_name_key_val`: [string]
- `half_siblings`: [person] [list]
- `iper`: [string]
- `is_birthday`: [boolean]
- `is_visible_for_visitors`: [boolean]
- `linked_page`: [string]
- `mother`: [person]
- `occ`: [int]
- `occupation`: [string]
- `parents`: [family]
- `public_name`: [string]
- `qualifier`: [string]
- `qualifiers`: [string] [list]
- `relations`: [person] [list]
- `related`: [related] list
- `sex`: [int]
- `siblings`: [person] [list]
- `sosa`: [string]
- `sources`: [string]
- `spouses`: [person] [list]
- `surname`: [string]
- `surname_aliases`: [string] [list]
- `surname_key`: [string]
- `surname_key_val`: [string]
- `titles`: [title] [list]

## place

For now, [place] is an alias for [string],
but it will eventually become a real data structure.

## prec

A [prec] is one of these [string]:
- `"sure"`
- `"about"`
- `"maybe"`
- `"before"`
- `"after"`
- `"oryear"`
- `"yearint"`

## related

A related is a [person] with these extra fields:
- `sources`: [string]
- `kind`: [related kind]

## related kind

A [related kind] is one of these [string]:
- `"ADOPTION"`
- `"RECOGNITION"`
- `"CANDIDATEPARENT"`
- `"GODPARENT"`
- `"FOSTERPARENT"`

## string

This is just text.

## title

- `ident`: [string]
- `name`: [string]
- `place`: [place]
- `date_start`: [date]
- `date_end`: [date]
- `nth`: [int]


## witness

[boolean]: #boolean
[burial]: #burial
[burial type]: #burial-type
[calendar]: #calendar
[date]: #date
[death]: #death
[death reason]: #death
[event]: #event
[family]: #family
[float]: #float
[int]: #int
[list]: #list
[person]: #person
[place]: #place
[prec]: #prec
[related]: #related
[related kind]: #related-kind
[string]: #string
[title]: #title
[witness]: #witness
