  $ gwd --cgi --daemon
  gwd: cannot activate daemon mode in CGI mode
  [124]

  $ QUERY_STRING="foo" gwd --daemon
  gwd: CGI mode was enabled via the QUERY_STRING environment variable.
  This implicit behavior is deprecated. Use the `--cgi` option.
  gwd: cannot activate daemon mode in CGI mode
  [124]

  $ QUERY_STRING="foo" gwd
  gwd: CGI mode was enabled via the QUERY_STRING environment variable.
  This implicit behavior is deprecated. Use the `--cgi` option.

  $ gwd --cgi --log -
  gwd: you cannot redirect the diagnostic output of the server into the
       standard output in CGI mode
  [124]

  $ gwd --cgi --log '<stdout>'
  gwd: you cannot redirect the diagnostic output of the server into the
       standard output in CGI mode
  [124]
