# connex

## Documentation

Calculates the connex components of a base and returns it in HTML.

```
usage: geneweb.connex <base>

  -gwd_p <number>: Specify the port number of gwd (default = 2317); > 1024 for normal users.
  -server <string>: Name of the server (default is 127.0.0.1).
  -a : all connex components
  -s : produce statistics
  -d <int> : detail for this length
  -i <file> : ignore this file
  -bf : by origin files
  -del <int> : ask for deleting branches whose size <= that value
  -cnt <int> : delete cnt branches whose size <= -del value
  -exact : delete only branches whose size strictly = -del value
  -o <file> : output to this file
  -help  Display this list of options
  --help  Display this list of options
```
