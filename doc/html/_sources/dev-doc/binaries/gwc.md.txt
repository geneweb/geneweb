# gwc

## Documentation

Creates a new database.

```
Usage: geneweb.gwc [options] [files]
where [files] are a list of files:
  source files end with .gw
  object files end with .gwo
and [options] are:
  -bnotes [drop|erase|first|merge] Behavior for base notes of the next file. [drop]: dropped. [erase]: erase the current content. [first]: dropped if current content is not empty. [merge]: concatenated to the current content. Default: merge
  -c                               Only compiling
  -cg                              Compute consanguinity
  -ds <str>                        Set the source field for persons and families without source data
  -f                               Remove database if already existing
  -mem                             Save memory, but slower
  -nc                              No consistency check
  -nofail                          No failure in case of error
  -nolock                          Do not lock database
  -nopicture                       Do not create associative pictures
  -o <file>                        Output database (default: a.gwb)
  -particles <file>                Particles file (default = predefined particles)
  -q                               Quiet
  -sep                             Separate all persons in next file
  -sh <int>                        Shift all persons numbers in next files
  -stats                           Print statistics
  -v                               Verbose
```
