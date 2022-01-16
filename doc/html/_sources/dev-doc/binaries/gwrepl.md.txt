# gwrepl

## Documentation

Starts an OCaml interactive top-level for writing scripts manipulating the
database. It loads all the necessary libraries to use the geneweb
libraries.
For script execution, run:
    cat <script.ml> | [ GWREPL_PPF=/dev/null ] [ GWREPL_VERBOSE=1 ] [ GWREPL_FORCE_UNPACK=1 ] [ GWREPL_NOPROMPT=1 ] gwrepl.exe [scrip_arg1] ...

For interactive top-level, run `gwdrepl.exe`.

