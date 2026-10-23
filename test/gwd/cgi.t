  $ gwc -bd . ../galichet.gw
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

  $ alias gwd-cgi="bash ./gwd-cgi.sh"
  $ gwd-cgi | grep '</html>'
  </html>

  $ gwd-cgi "m=S&pn=galichet&p=&n=" | grep 'Galichet'
  <title>Galichet</title>
  <h1><a href="wicked?b=galichet&m=N&t=A&v=Galichet">Galichet</a></h1>
  <div>Display by<i class="fa fa-code-fork ms-3 me-1"></i>line (2)<i class="fa fa-arrow-down-a-z ms-3 me-1"></i><a href="wicked?b=galichet&m=N&v=Galichet&o=i"
  </script></div><div id="surname_by_branch"><dl><dt><a href="wicked?b=galichet&m=N&v=Galichet&br=1"
  </li></ul></li></ul></dd><dt><a href="wicked?b=galichet&m=N&v=Galichet&br=2"

  $ gwd-cgi "m=S&pn=galichot&p=&n=" | grep 'Galichot'
  [1]
