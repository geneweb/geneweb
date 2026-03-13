  $ gwc -bd . foo.gw bar.gw
  Fatal error: exception Stdlib.Arg.Bad("In presence of several input files, we cannot infer the output database name. Please specify it with -o option.")
  [2]

  $ gwc -bd . foo.bar
  gwc: Don’t know what to do with "foo.bar".
  Usage: gwc [options] [files]
  where [files] are a list of files:
    source files end with .gw
    object files end with .gwo
  and [options] are:
    -bd <DIR>                     Specify where the “bases” directory with databases is installed (default if empty is "$XDG_DATA_HOME/geneweb/bases").
    -bnotes                       [drop|erase|first|merge] Behavior for base notes of the next file. [drop]: dropped. [erase]: erase the current content. [first]: dropped if current content is not empty. [merge]: concatenated to the current content. Default: merge
    -c                            Only compiling
    -cg                           Compute consanguinity
    -ds <str>                     Set the source field for persons and families without source data
    -f                            Remove database if already existing
    -gwo                          Suppress .gwo files after base creation
    -mem                          Save memory, but slower
    -nc                           No consistency check
    -ngrams <bi>[,<tri>[,<quad>]] N-gram indexing thresholds (e.g., '500,20,10' or '500')
    -nofail                       No failure in case of error
    -nolock                       Do not lock database
    -nopicture                    Do not create associative pictures
    -nowarn                       Do not show warnings during import
    -o <file>                     Output database (default: <input file name>.gwb, a.gwb if not available). Alphanumerics and -
    -particles <file>             Particles file (default = predefined particles)
    -prog                         show progress bar
    -q                            Quiet
    -reorg                        Mode reorg
    -rgpd <dir>                   Rgpd directory
    -roglo_special                Special treatment for Roglo (ignore multiple relations definitions)
    -sep                          Separate all persons in next file
    -sh <int>                     Shift all persons numbers in next files
    -stats                        Print statistics
    -v                            Verbose
    -help                         Display this list of options
    --help                        Display this list of options
  [2]

  $ gwc -bd .
  Fatal error: exception Stdlib.Arg.Bad("You must specify at least one input file.")
  [2]

  $ gwc -gwo -bd . ../galichet.gw
  Compilation: 0 min 0 sec
  Migration check for galichet:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: galichet.gwf
  
  pcnt 35 persons 63
  fcnt 15 families 15
  scnt 93 strings 127
  *** saving persons array
  *** saving ascends array
  *** saving unions array
  *** saving families array
  *** saving couples array
  *** saving descends array
  *** saving strings array
  *** create name index
  *** create strings of sname
  *** create strings of fname
  *** create string index
  *** create surname index
  *** create first name index
  *** ok
  Database generation: 0 min 0 sec

  $ gwc -gwo -bd . ../galichet.gw
  Compilation: 0 min 0 sec
  Database "galichet" already exists. Use -f to overwrite.
  [2]

  $ gwc -gwo -bd . ../galichet.gw -f
  Compilation: 0 min 0 sec
  Migration check for galichet:
    Classic .gwf exists: true (./galichet.gwf)
    Reorg .gwf exists: false
  Configuration kept
  
  pcnt 35 persons 63
  fcnt 15 families 15
  scnt 93 strings 127
  *** saving persons array
  *** saving ascends array
  *** saving unions array
  *** saving families array
  *** saving couples array
  *** saving descends array
  *** saving strings array
  *** create name index
  *** create strings of sname
  *** create strings of fname
  *** create string index
  *** create surname index
  *** create first name index
  *** ok
  Database generation: 0 min 0 sec

  $ gwc -gwo -bd . ../galichet.gw -o foo
  Compilation: 0 min 0 sec
  Migration check for foo:
    Classic .gwf exists: false
    Reorg .gwf exists: false
  Creating default configuration: foo.gwf
  
  pcnt 35 persons 63
  fcnt 15 families 15
  scnt 93 strings 127
  *** saving persons array
  *** saving ascends array
  *** saving unions array
  *** saving families array
  *** saving couples array
  *** saving descends array
  *** saving strings array
  *** create name index
  *** create strings of sname
  *** create strings of fname
  *** create string index
  *** create surname index
  *** create first name index
  *** ok
  Database generation: 0 min 0 sec

  $ gwc -bd . ../galichet.gwo -f
  Compilation: 0 min 0 sec
  Migration check for galichet:
    Classic .gwf exists: true (./galichet.gwf)
    Reorg .gwf exists: false
  Configuration kept
  
  pcnt 35 persons 63
  fcnt 15 families 15
  scnt 93 strings 127
  *** saving persons array
  *** saving ascends array
  *** saving unions array
  *** saving families array
  *** saving couples array
  *** saving descends array
  *** saving strings array
  *** create name index
  *** create strings of sname
  *** create strings of fname
  *** create string index
  *** create surname index
  *** create first name index
  *** ok
  Database generation: 0 min 0 sec

  $ gwc -bd . ../galichet.gw -f && cmp ../galichet.gwo galichet.gwo && rm galichet.gwo
  Compilation: 0 min 0 sec
  Migration check for galichet:
    Classic .gwf exists: true (./galichet.gwf)
    Reorg .gwf exists: false
  Configuration kept
  
  pcnt 35 persons 63
  fcnt 15 families 15
  scnt 93 strings 127
  *** saving persons array
  *** saving ascends array
  *** saving unions array
  *** saving families array
  *** saving couples array
  *** saving descends array
  *** saving strings array
  *** create name index
  *** create strings of sname
  *** create strings of fname
  *** create string index
  *** create surname index
  *** create first name index
  *** ok
  Database generation: 0 min 0 sec
  ../galichet.gwo galichet.gwo differ: char 16, line 1
  [1]

