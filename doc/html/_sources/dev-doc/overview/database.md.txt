# Database overview

## Gw files

Genealogy database could be created by Geneweb from one or from multiple source files with *.gw* extension. Those files describe structurally persons, families all kinds of relationships, different events, etc. You can read more about the file structure [here](https://geneweb.tuxfamily.org/wiki/gw). Binary executable `gwc` reads files *.gw*, extracts all persons and families information and passes it to the **Gwdb** module in order to create the database.

## Database entries

Transmitted to **Gwdb** information is composed mainly from:

- Array of all strings that could be any kind of information encoded as a string, like for example: person's name, birth place, marriage place, etc. Identifier `istr` allows to reference the string in the given array (index of an element inside the array).  

- Array of persons where each element encompasses information about one person. Every string field of a person (like his name, birthplace, etc.) is an identifier where the real string is stored in array mentioned before. Reference to other persons by means of identifier `iper` that reference person in the current array (index of an element inside the persons array). 

- Array of families where each element encompasses information about one family (couple, children, marriage date, etc.). Identifier `ifam` allows to reference the family in the given array (index of element inside the array).

Each array keeps a data structure defined in the module **Def**. Further, those entries will be the main source for every database request. 

## Storage

**Gwdb** is responsible for creating the database on the disk from the provided inputs. It creates a directory `dbname.gwb` containing several
files. The main file `base` contains marshalled representation of each array and `base.acc` stores offsets to every entry entry that allows to make constant time access. Additionally, it creates some index files that associate useful for requests information to the entry's identifier in the `base` file. That helps to requests to find instantly entry without iteration over all existing ones in the database. For example `strings.inx` is a string index that allows to find id for a searched string. One file is slightly different: the `patches` file. It stores every modification done inside the base (see [Modifications](#modifications) subsection). The storage manipulation interface is described in `lib/gwdb_driver.mli/gwdb_driver.mli`. This is a virtual module whose
current implementation is available on `gwdb-legacy`. Format and description for every database file is listed below: 

```text
base - the base itself
  magic number (magic_gwb)                 : string of length 8
  number of persons                        : binary_int
  number of families                       : binary_int
  number of strings                        : binary_int
  persons array offset in file             : binary_int
  ascends array offset in file             : binary_int
  unions array offset in file              : binary_int
  families array offset in file            : binary_int
  couples array offset in file             : binary_int
  descends array offset in file            : binary_int
  strings array offset in file             : binary_int
  notes origin file                        : value
  persons array                            : value
  ascends array                            : value
  unions array                             : value
  families array                           : value
  couples array                            : value
  descends array                           : value
  strings array                            : value

base.acc - direct accesses to arrays inside base
  persons offsets   : array of binary_ints
  ascends offsets   : array of binary_ints
  unions offsets    : array of binary_ints
  families offsets  : array of binary_ints
  couples offsets   : array of binary_ints
  descends offsets  : array of binary_ints
  strings offsets   : array of binary_ints

names.inx - index for names, strings of first names and surnames
  offset to sindex : binary_int
  offset to findex : binary_int
  1st index (mixes between names) : value 
    array, length = 16383, associating:
      - a hash value of a "crushed" (module "Name") name 
        (modulo length)
      - to the array of ids of the corresponding persons
  2nd index (surnames sub-strings) : value
    array, length = "table_size", associating:
      - a hash value of the "crushed" (module "Name") surname 
        sub-string (modulo length)
      - to the array of the corresponding surnnames (string ids) 
      that contain giving surname sub-string
  3rd index (first name sub-strings) : value 
    array, length = 16383, associating:
      - a hash value of the "crushed" (module "Name") first name 
      sub-string (modulo length)
      - to the array of the corresponding string ids that contains 
      giving first name sub-string

names.acc - direct accesses to values inside arrays in names.inx

strings.inx - index for all strings
  length of the strings offset array : binary_int
  strings hash table index           : 2 arrays of binary_ints
    strings offset array (length = prime after 10 * strings 
    array length)
      - associating a hash value of the string modulo length
      - to its id in the string array
    strings list array (length = string array length)
      - associating a string id
      - to the id of the next index (previous value) holding the 
      same hash value

snames.inx - index for surnames
  array ordered by surname  
    - associating the string id of a surname
    - to a pointer (offset) inside snames.dat

snames.dat - data associated with snames.inx
  array of list of persons holding a surname

fnames.inx - index for first names
  array ordered by first name 
    - associating the string id of a first name
    - to a pointer (offset) inside fnames.dat

fnames.dat - data associated with fnames.inx
  array of list of persons holding a first name

notes - text file containing data base notes.

notes_d - directory containing .txt for each extended page

particles.txt - text file with autorised name's particles

patches - modification inside the database
  When updated, none of the previous files are modified. 
  Only this one is written and rewritten. It holds a record 
  of type "patches", composed of association lists 
  "index" - "new value".

nb_persons - number of real persons (with those added by patches)

synchro_patches - timestamped history of base's modifications. 

restrict - defines visibility of each person in the base 

```

## Modifications

When a modification is requested, geneweb does not update `base` file itself. It
completes the `patches` file containing all the latest modifications on the
base. Every modification (patch) done is pended until patches are committed with `commit_patches` request.
Commit performs update of the `patches` file.

Patching signifies only operations that add or modify an entry. Entry suppression is done quite differently.
It is replaced by a *dummy* entry and then removed by Geneweb's garbage collector `gwgc` that performs compaction 
of database arrays. Another useful `fixbase` tool, locates and fixes inconsistencies on the base and updates all database files.

## Example

Here is an example how Geneweb displays birth dates of persons that have given name (let's say "Pierre") without considering caches:

- Firstly, it makes dichotomous search inside `fnames.inx` of a string id (`istr`) that references "Pierre"
- Then it reads (with associated to "Pierre" offset from `fnames.inx`) position in the file `fnames.data` where list of ids of persons (`iper`) with first name "Pierre" are stored.
- For every person's id it gets person's entry offset from `base.acc` file
- Then it reads person's entry with giving offset and get field associated to the birth date.
- Displays all extracted birth dates.
