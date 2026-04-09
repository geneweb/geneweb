The following tests ensures that `gwd` never prints special characters into 
log files that are not tty.
  $ gwd -check -debug 2>&1 | tr -d '\r' | cat -v
  [DEBUG]: End of check mode.

  $ gwd -check -debug -log '<stdout>' | tr -d '\r' | cat -v
  [DEBUG]: End of check mode.

  $ gwd -check -debug -log '<stderr>' 2>&1 | tr -d '\r' | cat -v
  [DEBUG]: End of check mode.

  $ gwd -check -debug -log foo && tr -d '\r' < foo | cat -v
  [DEBUG]: End of check mode.
