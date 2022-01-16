# setup

The database setup portal. It depends on `wserver`, a library defined in `bin/`.

```
Usage: setup.exe [options] where options are:
  -bd <dir>: Directory where the databases are installed.
  -gwd_p <number>: Specify the port number of gwd (default = 2317); > 1024 for normal users.
  -lang <string>: default lang
  -daemon : Unix daemon mode.
  -p <number>: Select a port number (default = 2316); > 1024 for normal users.
  -only <file>: File containing the only authorized address
  -gd <string>: gwsetup directory
  -bindir <string>: binary directory (default = value of option -gd)
```