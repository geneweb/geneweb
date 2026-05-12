The following tests ensures that `gwd` never prints special characters into 
log files that are not tty.

Cmdliner prints special characters to the output even when the file descriptor 
is not a TTY. It appears this feature cannot be controlled via the Cmdliner API,
except through the NO_COLOR environment variable. If this test fails, try 
setting NO_COLOR before investigating further.
$ export NO_COLOR=1

  $ gwd --check --debug 2>&1 | tr -d '\r' | cat -v
  DEBUG GWD  End of check mode.

  $ gwd --check --debug --log '<stdout>' | tr -d '\r' | cat -v
  DEBUG GWD  End of check mode.

  $ gwd --check --debug --log '<stderr>' 2>&1 | tr -d '\r' | cat -v
  DEBUG GWD  End of check mode.

  $ gwd --check --debug --log foo && tr -d '\r' < foo | cat -v
  DEBUG GWD  End of check mode.
