  $ gwd --daemon --cgi
  gwd: cannot activate deamon mode in CGI mode
  [124]

  $ QUERY_STRING="" gwd --daemon
  gwd: cannot activate deamon mode in CGI mode
  [124]

This test succeeds as the current implementation of gwd cannot detect 
the failure of the database loading before daemonizing the server.
  $ gwd --cache-database foo --daemon
