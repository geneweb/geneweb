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

  $ export GWD_BIN="$(which gwd)"
  $ alias gwd-cgi=./gwd-cgi.sh
  $ gwd-cgi
  =========== QUERY_STRING:  =========
  INFO  ROB  === ROBOT SUMMARY ===
  INFO  ROB    Blocked IPs: 0, Monitored: 1
  INFO  ROB    Most active: 1 req by 
  INFO  ROB    Blocked robots detail:
  INFO  GWD  1970-01-01T00:00:00-00:00 (PID: UNPREDICTABLE) gwd?
    From: 
    Agent: Oz
  Content-type: text/html; charset=utf-8
  Date: UNPREDICTABLE
  Connection: close
  
  <!-- $Id: welcome.txt v7.1 04/10/2026 00:26:24 $ -->
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <title>GeneWeb – galichet</title>
  <meta name="robots" content="none">
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="icon" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/favicon_gwd.png">
  <script>window.GWPERMA={"q":"lang=en","r":2,"t":{"c":"Copy the permalink","p":"exterior","f":"friend","w":"wizard"}};</script>
  <script defer src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/permalink.min.js?hash=d8bb33b06468360963d0b4c7c3fd3d3a"></script><!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css.txt -->
    <!-- $Id: css.txt v7.1 23/05/2026 18:51:27 $ -->
  <meta name="format-detection" content="telephone=no">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/bootstrap.min.css?version=5.3.8">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/css.css?hash=c2143faa05629ffb12f566217aff153b">
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css.txt -->
  </head>
  <body>
  <main class="container" id="welcome">
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/hed.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/hed.txt -->
  <div class="d-flex flex-column flex-md-row align-items-center justify-content-lg-around mt-1 mt-lg-3">
  <div class="col-md-3 order-2 order-md-1 align-self-center mt-3 mt-md-0">
  <div class="d-flex justify-content-center">
  <img src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/arbre_start.png" alt="logo GeneWeb" width="180">
  </div>
  </div>
  <div class="col-12 col-md-3 order-1 order-md-3 ms-md-2 px-0 mt-xs-2 mt-lg-0 align-self-center">
  <div id="gw-login" class="d-flex flex-column col-md-10 ps-1 pe-0">
  <div class="btn-group btn-group-xs mt-1" role="group">
  </div>
  </div>
  </div>
  <div class="my-0 order-3 order-md-2 flex-fill text-lg-center align-self-md-center">
  <h1 class="display-5">Genealogical database galichet</h1>
  <div class="d-flex justify-content-center">
  <span class="text-center h4">35 individuals</span>
  </div>
  </div>
  </div>
  <div class="d-flex justify-content-center">
  <div class="d-flex flex-column col-8">
  </div>
  </div>
  <div id="welcome-search" class="d-flex flex-wrap justify-content-center mt-3 mt-lg-1">
  <div class="col col-md-10 col-xl-9">
  <form id="main-search" class="mt-2 mt-xl-4" method="get" action="gwd?b=galichet&">
  <input type="hidden" name="b" value="galichet">
  <input type="hidden" name="m" value="S" class="has_validation">
  <div class="d-flex justify-content-center">
  <div class="d-flex flex-column justify-content-center w-100">
  <div class="d-flex flex-column flex-md-row">
  <div class="w-100 w-md-auto flex-md-grow-1">
  <div class="d-flex flex-grow-1">
  <div class="d-flex align-items-center ms-1 me-2">
  <button type="button" class="btn btn-link p-0 border-0" aria-label="Search formats"
  data-bs-toggle="tooltip" data-bs-placement="top" data-bs-html="true" data-bs-trigger="hover focus"
  title="<div class='text-start fw-bold'><div class='h5'>Search formats:</div>
  1. Individual:<br>
  a. first name(s) surname ¹<br>
  <i class='fw-lighter'>John Doe</i><br><br>
  b. first name(s)/surname ²/occurence ³<br>
  <i class='fw-lighter'>James William/Smith-Johnson/2<br>       James William/Smith-Johnson<br>
  James William/<br>       /Smith-Johnson</i><br><br>
  2. Surname ¹: <i class='fw-lighter'>Doe</i><br><br>
  3. Public name: <i class='fw-lighter'>James Smith</i><br><br>
  4. Alias: <i class='fw-lighter'>Jimmy</i><br><br>
  5. Key: first name(s).occurence surname ²<br>
  <i class='fw-lighter'>James William.2 Smith-Johnson<br>       james william.2 smith johnson</i><br>
  <br><div class='fw-lighter small'>¹ only one word<br>² full surname<br>³ optional</div></div>">
  <i class="far fa-circle-question text-primary" aria-hidden="true"></i>
  </button>
  </div>
  <label for="fullname" class="visually-hidden">Search individual</label>
  <input type="search" id="fullname" class="form-control form-control-lg py-0 border border-top-0" autofocus
  name="pn" placeholder="Search individual, surname, public name, alias, key"
  >
  </div>
  <div class="d-flex mt-3">
  <div class="btn-group-vertical me-2">
  <a href="gwd?b=galichet&m=P&tri=A" data-bs-toggle="tooltip"
  title="First names, sort by alphabetical order"><i class="fa fa-arrow-down-a-z fa-fw" aria-hidden="true"></i></a>
  <a href="gwd?b=galichet&m=P&tri=F" data-bs-toggle="tooltip"
  title="Frequency first names, sort by number of individuals"><i class="fa fa-arrow-down-wide-short fa-fw" aria-hidden="true"></i></a>
  </div>
  <div class="d-flex flex-grow-1">
  <div class="flex-grow-1 align-self-center">
  <label for="firstname" class="visually-hidden form-label">First name(s)</label>
  <input type="search" id="firstname" class="form-control form-control-lg border-top-0"
  name="p" placeholder="First name(s)"
   list="datalist_fnames" data-book="fn">
  </div>
  </div>
  </div>
  <div class="d-flex mt-2">
  <div class="btn-group-vertical me-2">
  <a href="gwd?b=galichet&m=N&tri=A" data-bs-toggle="tooltip"
  title="Surnames, sort by alphabetical order"><i class="fa fa-arrow-down-a-z fa-fw" aria-hidden="true"></i></a>
  <a href="gwd?b=galichet&m=N&tri=F" data-bs-toggle="tooltip"
  title="Frequency surnames, sort by number of individuals"><i class="fa fa-arrow-down-wide-short fa-fw" aria-hidden="true"></i></a>
  </div>
  <div class="d-flex flex-grow-1">
  <div class="flex-grow-1 align-self-center">
  <label for="surname" class="visually-hidden form-label">Surname</label>
  <input type="search" id="surname" class="form-control form-control-lg border border-top-0"
  title="Full surname" data-bs-toggle="tooltip"
  name="n" placeholder="Surname"
   list="datalist_snames" data-book="sn">
  </div>
  </div>
  </div>
  </div>
  <div class="d-flex flex-column align-items-center justify-content-between mt-3 mt-md-0 mx-0 mx-md-1 px-0 px-md-3 col-md-auto">
  <div class="d-flex flex-row flex-md-column justify-content-start mb-3 mb-md-0">
  <div class="align-self-md-start fw-bold me-3 me-md-0 mb-0 mb-md-1">First names:</div>
  <div class="d-flex flex-row flex-md-column">
  <div class="form-check me-3 me-md-0 mb-md-1" data-bs-toggle="tooltip" data-bs-placement="left" title="Search all permutations (2 to 4 first names)">
  <input class="form-check-input" type="checkbox" name="p_order" id="p_order" value="on">
  <label class="form-check-label d-flex align-items-center" for="p_order">Permutated</label>
  </div>
  <div class="form-check" data-bs-toggle="tooltip" data-bs-placement="left" title="Partial matches, phonetic variants (quite long to load!)">
  <input class="form-check-input" type="checkbox" name="p_exact" id="p_exact" value="off">
  <label class="form-check-label d-flex align-items-center" for="p_exact">Partials</label>
  </div>
  </div>
  </div>
  <button id="global-search-inline" type="submit" class="d-none btn btn-outline-primary fw-bolder w-100 w-md-auto py-2 mb-1">
  <i class="fa fa-magnifying-glass fa-lg fa-fw" aria-hidden="true"></i>
  Search
  </button>
  </div>
  </div>
  </div>
  </div>
  </form>
  <div class="d-flex flex-wrap flex-md-nowrap justify-content-center align-items-md-center mt-1 mt-md-3">
  <form id="title-search" class="d-flex flex-column flex-md-row align-items-start align-items-md-center col-12 col-md-9 ms-md-4" method="get" action="gwd">
  <input type="hidden" name="b" value="galichet">
  <input type="hidden" name="m" value="TT">
  <div class="d-flex align-items-center w-100 mb-2 mb-md-0">
  <a class="me-2" data-bs-toggle="tooltip" data-bs-placement="bottom"
  href="gwd?b=galichet&m=TT" title="All the titles"><i class="fa fa-list-ul fa-fw" aria-hidden="true"></i></a>
  <label for="titles" class="visually-hidden form-label">Title</label>
  <input type="search" class="form-control border-top-0 border-end-0 border-start-0 w-100"
  name="t" id="titles" placeholder="Title"
  list="datalist_titles" data-book="title">
  </div>
  <div class="d-flex align-items-center w-100 mb-2 mb-md-0 ms-0 ms-md-2">
  <a class="me-2" data-bs-toggle="tooltip" data-bs-placement="bottom"
  href="gwd?b=galichet&m=TT&p=*" title="All the fiefs"><i class="fa fa-list-ul fa-fw" aria-hidden="true"></i></a>
  <label for="estates" class="visually-hidden form-label">Fief</label>
  <input type="search" class="form-control border-top-0 border-end-0 border-start-0 w-100"
  name="p" id="estates" placeholder="Fief"
  list="datalist_estates" data-book="domain">
  </div>
  <button class="d-none" type="submit"></button>
  </form>
  <button id="global-search" class="btn btn-outline-primary fw-bolder col-12 col-md-auto mt-1 mt-md-0 ms-md-auto me-md-2 py-2"
  type="button" data-bs-toggle="tooltip" data-bs-placement="right" title="Search">
  <i class="fa fa-magnifying-glass fa-lg fa-fw" aria-hidden="true"></i>
  Search
  </button>
  </div>
  </div>
  </div>
  <div class="d-flex flex-column justify-content-start justify-content-md-center mt-4 col-12 col-md-11 col-lg-10 mx-auto">
  <h2 class="h4 text-md-center"><i class="fas fa-screwdriver-wrench fa-sm fa-fw text-secondary" aria-hidden="true"></i>
  Tools</h2>
  <div class="d-flex flex-wrap justify-content-md-center">
  <a class="btn btn-outline-primary" href="gwd?b=galichet&m=NOTES"><i class="far fa-file-lines fa-fw me-1" aria-hidden="true"></i>Notes
  </a>
  <a class="btn btn-outline-primary" href="gwd?b=galichet&m=ISOLATED"><i class="fa fa-user fa-fw me-1" aria-hidden="true"></i>Isolated persons</a>
  <a class="btn btn-outline-primary" href="gwd?b=galichet&m=STAT"><i class="far fa-chart-bar fa-fw me-1" aria-hidden="true"></i>Statistics</a>
  <a class="btn btn-outline-primary" href="gwd?b=galichet&m=ANM"><i class="fa fa-cake-candles fa-fw me-1" aria-hidden="true"></i>Anniversaries</a>
  <a class="btn btn-outline-primary" href="gwd?b=galichet&m=PS&bi=on&ba=on&ma=on&de=on&bu=on"><i class="fas fa-globe fa-fw me-1" aria-hidden="true"></i>Places/surname</a>
  <a class="btn btn-outline-primary" href="gwd?b=galichet&m=AS"><i class="fa fa-magnifying-glass-plus fa-fw me-1" aria-hidden="true"></i>Advanced request</a>
  <a class="btn btn-outline-primary" href="gwd?b=galichet&m=CAL"><i class="far fa-calendar-days fa-fw me-1" aria-hidden="true"></i>Calendars</a>
  <a class="btn btn-outline-success" href="gwd?b=galichet&m=H&v=conf"><i class="fas fa-gear fa-fw me-1" aria-hidden="true"></i>Configuration</a>
  <a class="btn btn-outline-success" href="gwd?b=galichet&m=MOD_NOTES&new"><i class="fa fa-file-lines fa-fw me-1" aria-hidden="true"></i>Add note</a>
  <a class="btn btn-outline-success" href="gwd?b=galichet&m=ADD_FAM"><i class="fas fa-user-plus fa-fw me-1" aria-hidden="true"></i>Add family</a>
  </div>
  </div>
  <div class="d-flex flex-column justify-content-start justify-content-md-center mt-3 col-12 col-md-9 mx-auto">
  <h2 class="h4 text-md-center">
  <i class="fas fa-book fa-sm fa-fw text-success me-1" aria-hidden="true"></i>Dictionaries</h2>
  <div class="d-flex flex-wrap justify-content-md-center">
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=fn" title="Update book of first names (wizard)"><i class="fa fa-child me-1" aria-hidden="true"></i>First names
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=sn" title="Update book of surnames (wizard)"><i class="fa fa-user me-1" aria-hidden="true"></i>Surnames
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=fna" title="Update book of alternate first names (wizard)"><i class="fa fa-child me-1" aria-hidden="true"></i>Alternate first names
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=sna" title="Update book of alternate surnames (wizard)"><i class="fa fa-user me-1" aria-hidden="true"></i>Alternate surnames
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=pubn" title="Update book of public names (wizard)"><i class="fa fa-signature me-1" aria-hidden="true"></i>Public names
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=qual" title="Update book of qualifiers (wizard)"><i class="fa fa-comment me-1" aria-hidden="true"></i>Qualifiers
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=alias" title="Update book of aliases (wizard)"><i class="fa fa-mask me-1" aria-hidden="true"></i>Aliases
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=occu" title="Update book of occupations (wizard)"><i class="fa fa-briefcase me-1" aria-hidden="true"></i>Occupations
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=place" title="Update book of places (wizard)"><i class="fa fa-location-dot me-1" aria-hidden="true"></i>Places
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=src" title="Update book of sources (wizard)"><i class="fa fa-box-archive me-1" aria-hidden="true"></i>Sources
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=title" title="Update book of titles (wizard)"><i class="fa fa-crown me-1" aria-hidden="true"></i>Titles
  </a>
  <a class="btn btn-outline-success" data-bs-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=domain" title="Update book of domains (wizard)"><i class="fa fa-chess-rook me-1" aria-hidden="true"></i>Domains
  </a>
  <a href="gwd?b=galichet&m=CHK_DATA" class="btn btn-outline-success ms-2"
  title="Data typographic checker"><i class="fas fa-spell-check" aria-hidden="true"></i>
  </a>
  </div>
  </div>
  <div class="d-flex mt-3">
  <div class="col-11 col-md-auto mx-auto">
  There have been 1 accesses, 1 of
  them to this page, since hidden.</div>
  </div>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/trl.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/trl.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/copyr.txt -->
  <!-- $Id: copyr.txt v7.1 07/10/2026 17:09:29 $ -->
  <div class="d-flex flex-row flex-wrap justify-content-center justify-content-lg-end align-items-center my-2" id="copyr">
  <div class="d-flex flex-wrap justify-content-md-end align-items-center">
  <div class="d-flex flex-row align-items-center mt-0 ms-3">
  <div class="btn-group dropup" data-bs-toggle="tooltip" data-bs-placement="left"
  title="English – Select language">
  <button class="btn btn-link dropdown-toggle" type="button" id="dropdownMenu1"
  data-bs-toggle="dropdown" aria-expanded="false">
  <span class="text-uppercase" aria-hidden="true">en</span>
  <span class="visually-hidden">English, select language</span>
  </button>
  <div id="language-menu" class="dropdown-menu scrollable-lang"
  aria-labelledby="dropdownMenu1"></div>
  </div>
  <div class="d-flex flex-column align-items-center small ms-1 ms-md-3"
  aria-label="Connections" role="status">
  <a href="gwd?b=galichet&m=CONN_WIZ">1 wizard
  </a><span>1 connection
  </span>
  </div>
  </div>
  <div class="d-flex flex-column justify-content-md-end align-self-center ms-1 ms-md-3 ms-lg-4">
  <div class="text-nowrap">
  <a class="me-2"
  href="gwd?b=galichet&templ=templm"
  data-bs-toggle="tooltip"
  title="templm"><i class="fab fa-markdown" aria-hidden="true"></i><span class="visually-hidden">templm</span></a>GeneWeb UNPREDICTABLE</div>
  <div class="btn-group mt-0 ms-1">
  <span>&copy; <a href="https://www.inria.fr" target="_blank" rel="noreferrer noopener">INRIA</a> 1998-2007</span>
  <a href="https://geneweb.tuxfamily.org/wiki/GeneWeb"
  class="ms-1" target="_blank" rel="noreferrer noopener"
  data-bs-toggle="tooltip" title="GeneWeb Wiki"><i class="fab fa-wikipedia-w" aria-hidden="true"></i>
  <span class="visually-hidden">GeneWeb Wiki</span>
  </a>
  <a href="https://github.com/geneweb/geneweb"
  class="ms-1" target="_blank" rel="noreferrer noopener"
  data-bs-toggle="tooltip" title="GeneWeb Github"><i class="fab fa-github" aria-hidden="true"></i>
  <span class="visually-hidden">GeneWeb Github</span>
  </a>
  </div>
  </div>
  </div>
  </div>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/copyr.txt -->
  </main>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js.txt -->
  <!-- $Id: js.txt v7.1 03/10/2026 01:19:36 $ -->
  <script src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/bootstrap.bundle.min.js?version=5.3.8"></script>
  <script>
  const inputToBook = {
  addNavigation: function() {
  document.addEventListener('mousedown', function(event) {
  const input = event.target.closest('input[data-book]');
  if (input && event.ctrlKey && event.button === 0) {
  event.preventDefault();
  inputToBook.openBook(input.value, input.dataset.book);
  }
  });
  },
  openBook: function(value, book) {
  if (!value) return;
  let preVal = value;
  if (book === "place") {
  const place = value.split(/\]\s+[-–—]\s+/);
  preVal = place.length > 1 ? place[1] : value;
  }
  preVal = preVal.substring(0, Math.min(preVal.length, 12))
  .split(/[, ]+/)[0]
  .substring(0, Math.floor(value.length / 2));
  const encVal = encodeURIComponent(value);
  const encPreVal = encodeURIComponent(preVal);
  const url = `?b=galichet&m=MOD_DATA&data=${book}&s=${encPreVal}&s1=${encVal !== encPreVal ? encVal : ''}`;
  window.open(url, '_blank');
  }
  };
  inputToBook.addNavigation();</script>
  <script>
  function initializeWelcomeSearchFunctionality() {
  const mainForm = document.getElementById('main-search');
  const titleForm = document.getElementById('title-search');
  const searchBtn = document.getElementById('global-search');
  if (!searchBtn) return;
  const defaultText = `Search`;
  function hasInput(form) {
  return form && Array.from(form.querySelectorAll('input[type="search"]'))
  .some(input => input.value.trim() !== '');
  }
  function closeAlertsAndCleanURL() {
  document.querySelectorAll('.alert').forEach(el => {
  bootstrap.Alert.getOrCreateInstance(el).close();
  });
  const url = new URL(window.location.href);
  const bParam = url.searchParams.get('b');
  url.search = '';
  if (bParam !== null) url.searchParams.set('b', bParam);
  window.history.replaceState({}, '', url);
  }
  function updateTooltip() {
  const instance = bootstrap.Tooltip.getInstance(searchBtn);
  if (!instance) return;
  let text = defaultText;
  if (hasInput(mainForm)) text = `Search individual`;
  else if (titleForm && hasInput(titleForm)) text = `Search title/fief`;
  instance.setContent({ '.tooltip-inner': text });
  if (text !== defaultText) {
  instance.show();
  } else {
  instance.hide();
  }
  }
  [mainForm, titleForm].forEach(form => {
  if (!form) return;
  form.querySelectorAll('input[type="search"]').forEach(input => {
  input.addEventListener('input', () => {
  if (hasInput(form)) closeAlertsAndCleanURL();
  updateTooltip();
  });
  });
  form.addEventListener('keypress', e => {
  if (e.key === 'Enter') {
  e.preventDefault();
  if (hasInput(form)) form.submit();
  }
  });
  });
  searchBtn.addEventListener('click', () => {
  if (hasInput(mainForm)) mainForm.submit();
  else if (titleForm && hasInput(titleForm)) titleForm.submit();
  });
  }
  document.addEventListener('DOMContentLoaded', initializeWelcomeSearchFunctionality);
  </script>
  <script>
  (() => {
  const menu = document.getElementById('language-menu');
  if (!menu) return;
  const names = {'af':`Afrikaans`,'ar':`Arabic`,'bg':`Bulgarian`,'br':`Breton`,'ca':`Catalan`,'co':`Corsican`,'cs':`Czech`,'da':`Danish`,'de':`German`,'en':`English`,'eo':`Esperanto`,'es':`Spanish`,'et':`Estonian`,'fi':`Finnish`,'fr':`French`,'he':`Hebrew`,'is':`Icelandic`,'it':`Italian`,'lt':`Lithuanian`,'lv':`Latvian`,'nl':`Dutch`,'no':`Norwegian`,'oc':`Occitan`,'pl':`Polish`,'pt':`Portuguese`,'pt-br':`Brazilian-Portuguese`,'ro':`Romanian`,'ru':`Russian`,'sk':`Slovak`,'sl':`Slovenian`,'sv':`Swedish`,'tr':`Turkish`};
  const def = 'en';
  const base = 'en';
  const hrefFor = t => {
  const u = new URL(window.location.href);
  u.searchParams.delete('file');
  if (t === def) u.searchParams.delete('lang');
  else u.searchParams.set('lang', t);
  return u.pathname + u.search + u.hash;
  };
  const frag = document.createDocumentFragment();
  for (const l of Object.keys(names)) {
  const cur = l === 'en';
  const a = document.createElement('a');
  a.className = 'dropdown-item' + (l === base ? ' bg-warning' : '')
  + (cur ? ' disabled' : '');
  a.id = 'lang_' + l;
  if (cur) a.setAttribute('aria-current', 'true');
  else a.href = hrefFor(l);
  const code = document.createElement('code');
  code.textContent = (l === 'pt-br' ? l : l + '\u00a0\u00a0\u00a0') + ' ';
  a.appendChild(code);
  const n = names[l];
  a.appendChild(document.createTextNode(
  n.charAt(0).toUpperCase() + n.slice(1)));
  frag.appendChild(a);
  }
  menu.appendChild(frag);
  })();
  </script>
  <script>
  document.addEventListener('DOMContentLoaded', () => {
  document.querySelectorAll('[data-bs-toggle="tooltip"]').forEach(el => {
  if (!bootstrap.Tooltip.getInstance(el)) {
  new bootstrap.Tooltip(el, {
  trigger: el.dataset.bsTrigger || 'hover',
  delay: { show: 200, hide: 50 },
  container: 'body'
  });
  }
  });
  });
  </script>
  <script>
  function setupFloatingPlaceholders() {
  const inputs = document.querySelectorAll('input[type="text"][placeholder], input[type="number"][placeholder], input[type="search"][placeholder], textarea[placeholder]');
  inputs.forEach(input => {
  if (input.placeholder.trim() === '') return;
  let wrapper = input.parentNode;
  if (wrapper.classList.contains('clear-button-wrapper')) {
  wrapper.classList.add('input-wrapper');
  } else if (!wrapper.classList.contains('input-wrapper')) {
  const hadFocus = document.activeElement === input;
  wrapper = document.createElement('div');
  wrapper.className = 'input-wrapper';
  input.parentNode.insertBefore(wrapper, input);
  wrapper.appendChild(input);
  if (hadFocus) requestAnimationFrame(() => input.focus());
  }
  const placeholder = document.createElement('span');
  placeholder.className = 'floating-placeholder';
  placeholder.textContent = input.placeholder;
  wrapper.appendChild(placeholder);
  input.addEventListener('focus', () => placeholder.classList.add('active'));
  input.addEventListener('blur', () => placeholder.classList.remove('active'));
  if (input === document.activeElement || input.hasAttribute('autofocus')) {
  placeholder.classList.add('active');
  }
  });
  }
  document.addEventListener('DOMContentLoaded', setupFloatingPlaceholders);
  </script>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js.txt -->
  </body>
  </html>
  $ gwd-cgi "m=S&pn=galichet&p=&n="
  =========== QUERY_STRING: m=S&pn=galichet&p=&n= =========
  INFO  GWD  1970-01-01T00:00:00-00:00 (PID: UNPREDICTABLE) gwd?m=S&pn=galichet&p=&n=
    From: 
    Agent: Oz
  DEBUG SRCH  Search case=PersonName,first_name=<none>, surname=<none>, oc=<none>
  DEBUG SRCH  Search query=galichet,first_name=<none>, surname=<none>, oc=<none>
  DEBUG SRCH   Method Sosa: 0 results
  DEBUG SRCH   Method Key: 0 results
  DEBUG SRCH   Method FullName: skipped (empty fn)
  DEBUG SRCH   Method ApproxKey: 0 exact, 0 partial
  DEBUG SRCH     search_by_name: 0 candidates -> 0 after strict-sn filter
  DEBUG SRCH         partition_for_multiple_fn: 9 exact, 0 partial
  DEBUG SRCH   Method PartialKey: 9+0+0 results
  DEBUG SRCH Combined: 18+0+0 -> deduped 9+0+0
  Content-type: text/html; charset=utf-8
  Date: UNPREDICTABLE
  Connection: close
  
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <title>galichet: select</title>
  <meta name="robots" content="none">
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="shortcut icon" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/favicon_gwd.png">
  <script>window.GWPERMA={"q":"lang=en&m=S&pn=galichet","r":2,"t":{"c":"Copy the permalink","p":"exterior","f":"friend","w":"wizard"}};</script>
  <script defer src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/permalink.min.js?hash=d8bb33b06468360963d0b4c7c3fd3d3a"></script>  <!-- $Id: css.txt v7.1 23/05/2026 18:51:27 $ -->
  <meta name="format-detection" content="telephone=no">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/bootstrap.min.css?version=5.3.8">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/css.css?hash=c2143faa05629ffb12f566217aff153b">
  </head>
  <body>
  <!-- $Id: home.txt v7.1 03/04/2026 $ -->
  <div class="d-flex flex-column fix_top fix_left home-xs d-print-none">
  <a class="btn btn-sm btn-link p-0 border-0" href="gwd?b=galichet&" title="Home"><i class="fa fa-house fa-fw fa-xs" aria-hidden="true"></i><span class="visually-hidden">Home</span></a>
  <button type="button" class="btn btn-sm btn-link p-0 border-0" data-bs-toggle="modal" data-bs-target="#searchmodal"
  accesskey="S" title="Search"><i class="fa fa-magnifying-glass fa-fw fa-xs" aria-hidden="true"></i><span class="visually-hidden">Search</span></button>
  </div>
  <div class="modal fade" id="searchmodal" tabindex="-1" aria-labelledby="searchpopup" aria-hidden="true">
  <div class="modal-dialog modal-lg">
  <div class="modal-content">
  <form class="modal-body" method="get" action="gwd?b=galichet&">
  <h2 class="visually-hidden" id="searchpopup">Search individual</h2>
  <input type="hidden" name="b" value="galichet">
  <input type="hidden" name="m" value="S">
  <input type="search" id="hs_fullname" class="form-control form-control-lg no-clear-button"
  name="pn" aria-labelledby="searchpopup" autofocus
  placeholder="Search individual, surname, public name, alias or key">
  <div class="row g-2 mt-1">
  <div class="col-sm-6">
  <label class="visually-hidden" for="hs_p">First name(s)</label>
  <input type="search" id="hs_p" class="form-control form-control-lg no-clear-button"
  name="p" placeholder="First name(s)">
  </div>
  <div class="col-sm-6">
  <label class="visually-hidden" for="hs_n">Surname</label>
  <input type="search" id="hs_n" class="form-control form-control-lg no-clear-button"
  name="n" placeholder="Surname">
  </div>
  </div>
  <div class="d-flex flex-wrap align-items-center gap-2 mt-2">
  <span class="ms-1 me-2">First names</span>
  <div class="form-check form-check-inline" data-bs-toggle="tooltip" data-bs-placement="bottom" title="Search all permutations (2 to 4 first names)">
  <input class="form-check-input" type="checkbox" name="p_order" id="hs_p_order" value="on">
  <label class="form-check-label" for="hs_p_order">permutated</label>
  </div>
  <div class="form-check form-check-inline" data-bs-toggle="tooltip" data-bs-placement="bottom"
  title="Partial matches, phonetic variants (quite long to load!)">
  <input class="form-check-input" type="checkbox" name="p_exact" id="hs_p_exact" value="off">
  <label class="form-check-label" for="hs_p_exact">partials</label>
  </div>
  <button type="submit" class="btn btn-primary ms-auto"><i class="fa fa-magnifying-glass me-2" aria-hidden="true"></i>Search</button>
  </div>
  </form>
  </div>
  </div>
  </div>
  <div class="container">
  <h1>galichet: select</h1>
  <ul class="fa-ul">
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=jean+pierre&n=galichet">Jean Pierre Galichet</a> <bdo dir=ltr>†/1849</bdo>, married to Marie Elisabeth Loche.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=nicole&n=galichet">Nicole Galichet</a>, married to Louis Sutaine in 1570.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=jean+charles&n=galichet">Jean Charles Galichet</a> <bdo dir=ltr>1813</bdo>, son of Jean Pierre and Marie Elisabeth Loche.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=pierre&n=galichet">Pierre Galichet</a> <bdo dir=ltr>1814–1835</bdo>, son of Jean Pierre and Marie Elisabeth Loche.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=paul&n=galichet">Paul Galichet</a> <bdo dir=ltr>1816–1886</bdo>, son of Jean Pierre and Marie Elisabeth Loche, married to Florence Marty in 1835.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=lolo&n=galichet">lolo Galichet</a> <bdo dir=ltr>1818</bdo>, son of Jean Pierre and Marie Elisabeth Loche, married to prenom1 femme1 in 1840, &<sup><small>2</small></sup> prenom2 femme2 in 1845.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=therese+eugenie&n=galichet">Thérèse Eugénie Galichet</a> <bdo dir=ltr>1830</bdo>, daughter of Jean Pierre and Marie Elisabeth Loche.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=jean+paul&n=galichet">Jean-Paul Galichet</a> <bdo dir=ltr>1836</bdo>, son of Paul and Florence Marty.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=laure&n=galichet">Laure Galichet</a> <bdo dir=ltr>1839</bdo>, daughter of Paul and Florence Marty.</li>
  </ul>
  <!-- $Id: copyr.txt v7.1 07/10/2026 17:09:29 $ -->
  <div class="d-flex flex-row flex-wrap justify-content-center justify-content-lg-end align-items-center my-2" id="copyr">
  <div class="d-flex flex-wrap justify-content-md-end align-items-center">
  <div class="d-flex flex-row align-items-center mt-0 ms-3">
  <div class="btn-group dropup" data-bs-toggle="tooltip" data-bs-placement="left"
  title="English – Select language">
  <button class="btn btn-link dropdown-toggle" type="button" id="dropdownMenu1"
  data-bs-toggle="dropdown" aria-expanded="false">
  <span class="text-uppercase" aria-hidden="true">en</span>
  <span class="visually-hidden">English, select language</span>
  </button>
  <div id="language-menu" class="dropdown-menu scrollable-lang"
  aria-labelledby="dropdownMenu1"></div>
  </div>
  <div class="d-flex flex-column align-items-center small ms-1 ms-md-3"
  aria-label="Connections" role="status">
  <a href="gwd?b=galichet&m=CONN_WIZ">1 wizard
  </a><span>1 connection
  </span>
  </div>
  </div>
  <div class="d-flex flex-column justify-content-md-end align-self-center ms-1 ms-md-3 ms-lg-4">
  <div class="text-nowrap">
  <a class="me-2"
  href="gwd?b=galichet&templ=templm&m=S&pn=galichet"
  data-bs-toggle="tooltip"
  title="templm"><i class="fab fa-markdown" aria-hidden="true"></i><span class="visually-hidden">templm</span></a>GeneWeb UNPREDICTABLE</div>
  <div class="btn-group mt-0 ms-1">
  <span>&copy; <a href="https://www.inria.fr" target="_blank" rel="noreferrer noopener">INRIA</a> 1998-2007</span>
  <a href="https://geneweb.tuxfamily.org/wiki/GeneWeb"
  class="ms-1" target="_blank" rel="noreferrer noopener"
  data-bs-toggle="tooltip" title="GeneWeb Wiki"><i class="fab fa-wikipedia-w" aria-hidden="true"></i>
  <span class="visually-hidden">GeneWeb Wiki</span>
  </a>
  <a href="https://github.com/geneweb/geneweb"
  class="ms-1" target="_blank" rel="noreferrer noopener"
  data-bs-toggle="tooltip" title="GeneWeb Github"><i class="fab fa-github" aria-hidden="true"></i>
  <span class="visually-hidden">GeneWeb Github</span>
  </a>
  </div>
  </div>
  </div>
  </div>
  </div>
  <!-- $Id: js.txt v7.1 03/10/2026 01:19:36 $ -->
  <script src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/bootstrap.bundle.min.js?version=5.3.8"></script>
  <script>
  (() => {
  const menu = document.getElementById('language-menu');
  if (!menu) return;
  const names = {'af':`Afrikaans`,'ar':`Arabic`,'bg':`Bulgarian`,'br':`Breton`,'ca':`Catalan`,'co':`Corsican`,'cs':`Czech`,'da':`Danish`,'de':`German`,'en':`English`,'eo':`Esperanto`,'es':`Spanish`,'et':`Estonian`,'fi':`Finnish`,'fr':`French`,'he':`Hebrew`,'is':`Icelandic`,'it':`Italian`,'lt':`Lithuanian`,'lv':`Latvian`,'nl':`Dutch`,'no':`Norwegian`,'oc':`Occitan`,'pl':`Polish`,'pt':`Portuguese`,'pt-br':`Brazilian-Portuguese`,'ro':`Romanian`,'ru':`Russian`,'sk':`Slovak`,'sl':`Slovenian`,'sv':`Swedish`,'tr':`Turkish`};
  const def = 'en';
  const base = 'en';
  const hrefFor = t => {
  const u = new URL(window.location.href);
  u.searchParams.delete('file');
  if (t === def) u.searchParams.delete('lang');
  else u.searchParams.set('lang', t);
  return u.pathname + u.search + u.hash;
  };
  const frag = document.createDocumentFragment();
  for (const l of Object.keys(names)) {
  const cur = l === 'en';
  const a = document.createElement('a');
  a.className = 'dropdown-item' + (l === base ? ' bg-warning' : '')
  + (cur ? ' disabled' : '');
  a.id = 'lang_' + l;
  if (cur) a.setAttribute('aria-current', 'true');
  else a.href = hrefFor(l);
  const code = document.createElement('code');
  code.textContent = (l === 'pt-br' ? l : l + '\u00a0\u00a0\u00a0') + ' ';
  a.appendChild(code);
  const n = names[l];
  a.appendChild(document.createTextNode(
  n.charAt(0).toUpperCase() + n.slice(1)));
  frag.appendChild(a);
  }
  menu.appendChild(frag);
  })();
  </script>
  <script>
  document.addEventListener('DOMContentLoaded', () => {
  document.querySelectorAll('[data-bs-toggle="tooltip"]').forEach(el => {
  if (!bootstrap.Tooltip.getInstance(el)) {
  new bootstrap.Tooltip(el, {
  trigger: el.dataset.bsTrigger || 'hover',
  delay: { show: 200, hide: 50 },
  container: 'body'
  });
  }
  });
  });
  </script>
  <script>
  function initializeLazyModules() {
  function loadScriptOnce(buttonId, scriptUrl) {
  const btn = document.getElementById(buttonId);
  if (!btn) return;
  btn.addEventListener('click', function handler() {
  const script = document.createElement('script');
  script.src = scriptUrl;
  document.head.appendChild(script);
  btn.removeEventListener('click', handler);
  });
  }
  loadScriptOnce('load_once_p_mod', '/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/p_mod.min.js?hash=5bcafbd9290b2c1b63f59a4c20b1e9e9');
  loadScriptOnce('load_once_rlm_builder', '/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/rlm_builder.js?hash=469e86d912841061b96be1e39395006c');
  }
  document.addEventListener('DOMContentLoaded', initializeLazyModules);
  </script>
  <script>
  document.addEventListener('shown.bs.modal', function(event) {
  event.target.querySelector('[autofocus]')?.focus();
  });
  </script>
  <script>
  function setupFloatingPlaceholders() {
  const inputs = document.querySelectorAll('input[type="text"][placeholder], input[type="number"][placeholder], input[type="search"][placeholder], textarea[placeholder]');
  inputs.forEach(input => {
  if (input.placeholder.trim() === '') return;
  let wrapper = input.parentNode;
  if (wrapper.classList.contains('clear-button-wrapper')) {
  wrapper.classList.add('input-wrapper');
  } else if (!wrapper.classList.contains('input-wrapper')) {
  const hadFocus = document.activeElement === input;
  wrapper = document.createElement('div');
  wrapper.className = 'input-wrapper';
  input.parentNode.insertBefore(wrapper, input);
  wrapper.appendChild(input);
  if (hadFocus) requestAnimationFrame(() => input.focus());
  }
  const placeholder = document.createElement('span');
  placeholder.className = 'floating-placeholder';
  placeholder.textContent = input.placeholder;
  wrapper.appendChild(placeholder);
  input.addEventListener('focus', () => placeholder.classList.add('active'));
  input.addEventListener('blur', () => placeholder.classList.remove('active'));
  if (input === document.activeElement || input.hasAttribute('autofocus')) {
  placeholder.classList.add('active');
  }
  });
  }
  document.addEventListener('DOMContentLoaded', setupFloatingPlaceholders);
  </script>
  </body>
  </html>
  $ gwd-cgi "p=jean+pierre&n=galichet"
  =========== QUERY_STRING: p=jean+pierre&n=galichet =========
  INFO  GWD  1970-01-01T00:00:00-00:00 (PID: UNPREDICTABLE) gwd?p=jean+pierre&n=galichet
    From: 
    Agent: Oz
  Content-type: text/html; charset=utf-8
  Date: UNPREDICTABLE
  Connection: close
  
  <!-- $Id: perso.txt UNPREDICTABLE 04/04/2026 04:04:32 $ -->
  <!-- Copyright (c) 1998-2007 INRIA -->
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <title>Jean Pierre Galichet</title>
  <meta name="robots" content="none"><meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="icon" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/favicon_gwd.png">
  <script>window.GWPERMA={"q":"lang=en&p=jean+pierre&n=galichet","r":2,"t":{"c":"Copy the permalink","p":"exterior","f":"friend","w":"wizard"}};</script>
  <script defer src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/permalink.min.js?hash=d8bb33b06468360963d0b4c7c3fd3d3a"></script><!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css.txt -->
    <!-- $Id: css.txt v7.1 23/05/2026 18:51:27 $ -->
  <meta name="format-detection" content="telephone=no">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/bootstrap.min.css?version=5.3.8">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css/css.css?hash=c2143faa05629ffb12f566217aff153b">
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/css.txt -->
  </head>
  <body>
  <a href="#content" class="visually-hidden-focusable">Skip to main content</a>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/hed.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/hed.txt -->
  <div class="container">
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/perso_utils.txt -->
  <!-- $Id: perso_utils v7.1 04/04/2026 04:04:19 $ -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/perso_utils.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/menubar.txt -->
  <!-- $Id: menubar.txt v7.1 02/05/2024 19:03:42 $ -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/home.txt -->
  <!-- $Id: home.txt v7.1 03/04/2026 $ -->
  <div class="d-flex flex-column fix_top fix_left home-xs d-print-none">
  <a class="btn btn-sm btn-link p-0 border-0" href="gwd?b=galichet&" title="Home"><i class="fa fa-house fa-fw fa-xs" aria-hidden="true"></i><span class="visually-hidden">Home</span></a>
  <button type="button" class="btn btn-sm btn-link p-0 border-0" data-bs-toggle="modal" data-bs-target="#searchmodal"
  accesskey="S" title="Search"><i class="fa fa-magnifying-glass fa-fw fa-xs" aria-hidden="true"></i><span class="visually-hidden">Search</span></button>
  </div>
  <div class="modal fade" id="searchmodal" tabindex="-1" aria-labelledby="searchpopup" aria-hidden="true">
  <div class="modal-dialog modal-lg">
  <div class="modal-content">
  <form class="modal-body" method="get" action="gwd?b=galichet&">
  <h2 class="visually-hidden" id="searchpopup">Search individual</h2>
  <input type="hidden" name="b" value="galichet">
  <input type="hidden" name="m" value="S">
  <input type="search" id="hs_fullname" class="form-control form-control-lg no-clear-button"
  name="pn" aria-labelledby="searchpopup" autofocus
  placeholder="Search individual, surname, public name, alias or key">
  <div class="row g-2 mt-1">
  <div class="col-sm-6">
  <label class="visually-hidden" for="hs_p">First name(s)</label>
  <input type="search" id="hs_p" class="form-control form-control-lg no-clear-button"
  name="p" placeholder="First name(s)">
  </div>
  <div class="col-sm-6">
  <label class="visually-hidden" for="hs_n">Surname</label>
  <input type="search" id="hs_n" class="form-control form-control-lg no-clear-button"
  name="n" placeholder="Surname">
  </div>
  </div>
  <div class="d-flex flex-wrap align-items-center gap-2 mt-2">
  <span class="ms-1 me-2">First names</span>
  <div class="form-check form-check-inline" data-bs-toggle="tooltip" data-bs-placement="bottom" title="Search all permutations (2 to 4 first names)">
  <input class="form-check-input" type="checkbox" name="p_order" id="hs_p_order" value="on">
  <label class="form-check-label" for="hs_p_order">permutated</label>
  </div>
  <div class="form-check form-check-inline" data-bs-toggle="tooltip" data-bs-placement="bottom"
  title="Partial matches, phonetic variants (quite long to load!)">
  <input class="form-check-input" type="checkbox" name="p_exact" id="hs_p_exact" value="off">
  <label class="form-check-label" for="hs_p_exact">partials</label>
  </div>
  <button type="submit" class="btn btn-primary ms-auto"><i class="fa fa-magnifying-glass me-2" aria-hidden="true"></i>Search</button>
  </div>
  </form>
  </div>
  </div>
  </div>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/home.txt -->
  <nav aria-label="Navigation" class="navbar navbar-expand-md pt-0 px-0 mt-1 mt-md-0">
  <div class="container-fluid justify-content-center px-0">
  <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Navigation">
  <span class="navbar-toggler-icon"></span>
  </button>
  <div class="collapse navbar-collapse flex-grow-0" id="navbarSupportedContent">
  <ul class="nav nav-tabs">
  <li class="nav-item dropdown">
  <a id="load_once_p_mod" class="nav-link dropdown-toggle text-secondary dropdown-toggle-split"
  data-bs-toggle="dropdown" data-bs-auto-close="outside"
  href="#" role="button" aria-expanded="false" title="Modules"><span class="fas fa-address-card fa-fw me-1" aria-hidden="true"></span><span class="visually-hidden">Modules</span></a>
  <div class="dropdown-menu dropdown-menu-transl-pmod">
  <div class="d-flex justify-content-around mx-1">
  <form class="mx-1" name="upd_url" method="get" action="gwd">
  <div class="d-flex justify-content-between mx-2 mt-2 img-prfx" data-prfx="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/modules">
  <div class="d-flex align-items-center flex-grow-1">
  <input type="hidden" name="b" value="galichet">
  <label for="p_mod" class="visually-hidden">Select personalized modules</label>
  <div class="input-group p-mod-group me-2">
  <input type="text" pattern="^((?:([a-z][0-9]){1,15})|zz)" name="p_mod" id="p_mod" class="form-control"
  value=""
  placeholder="Select personalized modules" maxlength="30">
  <button type="submit" class="btn btn-outline-success" title="Submit"><i class="fa fa-check fa-lg" aria-hidden="true"></i></button>
  <button type="button" class="btn btn-outline-danger" id="p_mod_clear" title="Delete"><i class="fa fa-xmark fa-lg mx-1" aria-hidden="true"></i></button>
  </div>
  <input type="hidden" name="p" value="jean pierre">
  <input type="hidden" name="n" value="galichet">
  </div>
  <div class="ms-auto">
  <button type="button" class="btn btn-outline-danger ms-2" id="p_mod_rm" title="Remove the last added module" value=""><i class="fa fa-backward" aria-hidden="true"></i></button>
  <button type="submit" class="btn btn-outline-secondary ms-2 me-1" id='zz'
  title="Default template" data-bs-toggle="popover" data-bs-trigger="hover"
  data-bs-html="true" data-bs-content="<img class='w-100' src='/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/modules/zz_1.jpg'>"><i class="fa fa-arrow-rotate-left" aria-hidden="true"></i></button>
  <a class="btn btn-outline-primary ms-2"
  href="gwd?b=galichet&p=jean+pierre&n=galichet&wide=on"
  title="Full width display"><i class="fa fa-desktop fa-fw" aria-hidden="true"></i><span class="visually-hidden">Full width display</span></a>
  </div>
  </div>
  <div class="mx-2 mt-2">Select each module by clicking on the corresponding button (max 15).</div>
  <div class="alert alert-warning alert-dismissible fade show mt-1 mb-2 d-none" role="alert">
  <div class="d-none alert-opt">Invalid option <strong id="alert-option"></strong> for module <strong id="alert-module"></strong>. Please enter a valid option number.</div>
  <div class="d-none alert-mod">Invalid module <strong id="alert-module-2"></strong>. Please enter a valid module initial.</div>
  </div>
  <div id="p_mod_table"></div>
  </form>
  <div class="d-none d-md-block mx-1">
  <img src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/modules/menubar_1.jpg" alt="menubar for p_mod_builder" aria-hidden="true">
  <div id="p_mod_builder"></div>
  </div>
  </div>
  </div>
  </li>
  <li class="nav-item">
  <a class="nav-link " id="add_par"
  href="gwd?b=galichet&m=ADD_PAR&ip=0" title="Add parents of Jean Pierre Galichet (L)" accesskey="L"><sup><span class="fa fa-user male" aria-hidden="true"></span></sup><sub><span class="fa fa-plus male" aria-hidden="true"></span></sub><sup><span class="fa fa-user female" aria-hidden="true"></span></sup><span class="visually-hidden">Add parents of Jean Pierre Galichet (L)</span></a>
  </li>
  <li class="nav-item">
  <a class="nav-link active bg-light" aria-current="page" id="self"
  href="gwd?b=galichet&p=jean+pierre&n=galichet" title="Jean Pierre Galichet"><span class="fa fa-user-large fa-fw male" aria-hidden="true"></span>
  <span class="visually-hidden">Jean Pierre Galichet</span></a>
  </li>
  <li class="nav-item">
  <a class="nav-link " id="mod_ind" href="gwd?b=galichet&m=MOD_IND&i=0" title="Update individual Jean Pierre Galichet (P)" accesskey="P"><span class="fa fa-user-pen fa-fw male" aria-hidden="true"></span>
  <span class="visually-hidden">Update individual (P)</span></a>
  </li>
  <li class="nav-item dropdown">
  <a class="nav-link dropdown-toggle text-secondary "
  data-bs-toggle="dropdown" data-bs-auto-close="outside" href="#" role="button" aria-expanded="false" title="Tools individual"><span class="fa fa-user-gear text-body-secondary" aria-hidden="true"></span><span class="visually-hidden">Tools individual, visibility (if titles)</span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" href="gwd?b=galichet&m=H&v=visibility" title="Help for visibility of individuals" target="_blank">
  <i class="fa fa-person fa-fw text-body-secondary me-2" aria-hidden="true"></i>Visibility (if titles)</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item"
  href="gwd?b=galichet&pz=jean+pierre&nz=galichet&p=jean+pierre&n=galichet"
  title="Browse using Jean Pierre Galichet as Sosa reference" accesskey="S"><i class="far fa-circle-dot fa-fw male me-1" aria-hidden="true"></i> Update Sosa 1: Jean Pierre Galichet</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&cgl=on" target="_blank"><i class="fa fa-link-slash fa-fw me-2" title="Without GeneWeb links"></i>Without GeneWeb links</a><div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=CHG_EVT_IND_ORD&i=0" title="Change order of individual events"><span class="fa fa-sort fa-fw me-2" aria-hidden="true"></span>Reverse events</a>
  <a class="dropdown-item" href="gwd?b=galichet&m=SND_IMAGE&i=0">
  <i class="far fa-file-image fa-fw me-2" aria-hidden="true"></i>Add picture</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=MRG&i=0" title="Merge Jean Pierre Galichet with…"><span class="fa fa-compress fa-fw text-danger me-2" aria-hidden="true"></span>Merge individuals</a>
  <a class="dropdown-item" href="gwd?b=galichet&m=DEL_IND&i=0" title="Delete Jean Pierre Galichet"><span class="fa fa-trash-can fa-fw text-danger me-2" aria-hidden="true"></span>Delete individual</a>
  <div class="dropdown-divider"></div>
  <button class="dropdown-item simple-copy" type="button" title="Copy the wiki link to the clipboard"
  data-wikilink="[[Jean Pierre/Galichet]]"><i class="far fa-clipboard fa-fw me-2" aria-hidden="true"></i>[[Jean Pierre/Galichet]]</button>
  <button class="dropdown-item full-copy" type="button" title="Copy the wikitext link to the clipboard"
  data-wikilink="[[Jean Pierre/Galichet/0/Jean Pierre Galichet]]"><i class="far fa-clipboard fa-fw me-2" aria-hidden="true"></i>[[Jean Pierre/Galichet/0/Jean Pierre Galichet]]</button>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item"
  href="https://www.geneanet.org/fonds/individus/?sourcename=&size=50&sexe=&nom=Galichet&ignore_each_patronyme=&with_variantes_nom=1&prenom=Jean%20Pierre&prenom_operateur=or&ignore_each_prenom=&profession=&ignore_each_profession=&nom_conjoint=&ignore_each_patronyme_conjoint=&prenom_conjoint=&prenom_conjoint_operateur=or&ignore_each_prenom_conjoint=&place__0__=&zonegeo__0__=&country__0__=&region__0__=&subregion__0__=&place__1__=&zonegeo__1__=&country__1__=&region__1__=&subregion__1__=&place__2__=&zonegeo__2__=&country__2__=&region__2__=&subregion__2__=&place__3__=&zonegeo__3__=&country__3__=&region__3__=&subregion__3__=&place__4__=&zonegeo__4__=&country__4__=&region__4__=&subregion__4__=&type_periode=between&from=&to=1864&exact_day=&exact_month=&exact_year=&nom_pere=&ignore_each_patronyme_pere=&prenom_pere=&prenom_pere_operateur=or&ignore_each_prenom_pere=&nom_mere=&ignore_each_patronyme_mere=&with_variantes_nom_mere=1&prenom_mere=&prenom_mere_operateur=or&ignore_each_prenom_mere=&with_parents=0&go=1" target="_blank" rel="noreferrer noopener">
  <i class="fa fa-magnifying-glass fa-fw me-2" aria-hidden="true"></i>Search Jean Pierre Galichet on Geneanet
  </a>
  </div>
  </li>
  <li class="nav-item">
  <a class="nav-link " id="mod_fam_1" href="gwd?b=galichet&m=MOD_FAM&i=0&ip=0"
  title="Update family with Marie Elisabeth Loche (F)"
  accesskey="F"><span class="fa fa-user-pen male" aria-hidden="true"></span><span class="fa fa-user female" aria-hidden="true"></span><span class="visually-hidden"> Update family with Marie Elisabeth Loche</span></a>
  </li>
  <li class="nav-item dropdown">
  <a class="nav-link dropdown-toggle text-secondary
  "
  data-bs-toggle="dropdown" role="button" href="#" aria-expanded="false"
  title="Tools family"><span class="fa fa-user-plus" aria-hidden="true"></span><span class="fa fa-user-gear" aria-hidden="true"></span><span class="visually-hidden">Tools family</span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" id="add_fam" href="gwd?b=galichet&m=ADD_FAM&ip=0" title="Add family (A)" accesskey="A"><span class="fa fa-plus fa-fw me-2" aria-hidden="true"></span>Add family</a>
  <div class="dropdown-divider"></div>
  <span class="dropdown-header">Marriage with Marie Elisabeth Loche</span>
  <a class="dropdown-item" href="gwd?b=galichet&m=CHG_EVT_FAM_ORD&i=0&ip=0" title="Change order of family events"><span class="fa fa-sort fa-fw me-2" aria-hidden="true"></span>Reverse events</a>
  <a class="dropdown-item" href="gwd?b=galichet&m=DEL_FAM&i=0&ip=0"><span class="fa fa-trash fa-fw text-danger me-2" aria-hidden="true"></span>Delete family</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=CHG_CHN&ip=0"><span class="fa fa-child fa-fw me-1" aria-hidden="true"></span> Update surname of children</a>
  </div>
  </li>
  <li class="nav-item dropdown">
  <a class="nav-link dropdown-toggle text-secondary" data-bs-toggle="dropdown" role="button" href="#" aria-expanded="false" title="Descendants"><span class="fa fa-sitemap fa-fw" aria-hidden="true"></span><span class="visually-hidden">Descendants</span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D"><span class="fa fa-gear fa-fw me-2" aria-hidden="true"></span>Descendants</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=T&v=3" title="Tree (Y)" accesskey="Y"><span class="fa fa-sitemap fa-fw me-2" aria-hidden="true"></span>Descendants tree</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=TV&v=3" title="Tree (Y)" accesskey="Y"><span class="fa fa-sitemap fa-fw me-2" aria-hidden="true"></span>Compact descendants tree</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=D&v=2"><span class="fa fa-code-branch fa-rotate-90 fa-flip-vertical fa-fw me-2" aria-hidden="true"></span>Descendant tree view</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=I&v=2&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&gen=1&ns=1&hl=1"><span class="fa fa-table fa-fw me-2" aria-hidden="true"></span>Table descendants</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=L&v=3&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on"><span class="fa fa-newspaper fa-fw me-2" aria-hidden="true"></span>Full display</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=A&num=on&v=2"><span class="fa fa-code-branch fa-flip-vertical fa-fw me-2" aria-hidden="true"></span>D’Aboville</a>
  </div>
  </li>
  <li class="nav-item dropdown">
  <a id="load_once_rlm_builder" class="nav-link dropdown-toggle text-secondary"
  data-bs-toggle="dropdown" role="button" href="#" aria-expanded="false" title="Relationship"><span class="fa fa-user-group" aria-hidden="true"></span><span class="visually-hidden">Relationship</span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=R" title="Relationship computing (R)" accesskey="R">
  <span class="fa fa-gear fa-fw me-2" aria-hidden="true"></span>Relationship computing</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=C&p=jean+pierre&n=galichet&v=5"
  title="Relationship link with a parent"><span class="fa fa-elevator fa-fw me-2" aria-hidden="true"></span>Relationship</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=F&p=jean+pierre&n=galichet" title="Family">
  <span class="fa fa-users fa-fw me-2" aria-hidden="true"></span>Family</a>
  <div class="dropdown-divider"></div>
  <span class="dropdown-header pt-0" id="RLM" data-eb="galichet" data-accessbykey="1" data-i="0" data-p="jean+pierre" data-n="galichet" data-self="Jean Pierre Galichet †/1849">Multi relations graph</span>
  <div class="px-3 mb-2">
  <label for="description" class="visually-hidden">Description</label>
  <input type="text" id="description" class="form-control" placeholder="Description" title="&t=…">
  </div>
  <div class="d-flex pe-3">
  <button type="button" class="dropdown-item flex-grow-1" title="Add Jean Pierre Galichet to the data" id="saveButton">
  <i class="fa fa-plus fa-fw me-2" aria-hidden="true"></i>
  <span>Add to the data</span>
  </button>
  <button type="button" class="dropdown-item w-auto flex-shrink-0 px-0 d-none" title="Clear the data" id="clearGraphButton">
  <i class="fa fa-trash-can fa-fw text-danger" aria-hidden="true"></i>
  <span class="visually-hidden">Clear the data</span>
  </button>
  </div>
  <div class="d-none" id="graphButtons">
  <div class="d-flex pe-3" role="group" aria-label="[relationship graph actions]">
  <a class="dropdown-item flex-grow-1" id="generateGraphButton">
  <i class="fa fa-code-fork fa-rotate-180 fa-fw me-2" aria-hidden="true"></i>
  <span>Show the graph</span></a>
  <a class="dropdown-item w-auto flex-shrink-0 px-0" title="Edit the data" id="editGraphButton">
  <i class="far fa-pen-to-square fa-fw" aria-hidden="true"></i>
  <span class="visually-hidden">Edit the data</span></a>
  </div>
  </div>
  </div>
  </li>
  <li class="nav-item">
  <a class="nav-link text-secondary" href="gwd?b=galichet&m=SND_IMAGE_C&i=0" title="Add/delete pictures (I)" accesskey="I"><span class="fa fa-image fa-fw" aria-hidden="true"></span><span class="visually-hidden">Add/delete pictures</span></a>
  </li>
  </ul>
  </div>
  </div>
  </nav>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/menubar.txt -->
  <main id="content" tabindex="-1">
  <div class="row">
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/heading.txt -->
  <!-- heading.txt 04/03/2026 02:32:58 -->
  <h1 class="text-center text-md-start">
  <a href="gwd?b=galichet&m=P&v=jean+pierre#i0" data-updfield="first_name">Jean Pierre</a>  <a href="gwd?b=galichet&m=N&v=galichet#i0" data-updfield="surname">Galichet</a><input type="number" value="0"
  class="form-control upd-occ d-none ms-3" min="0" max="99999" step="1"
  title="Occurence number" aria-label="Occurence number"><span class="fw-light small"> <bdo dir=ltr>†/1849</bdo></span>
  <button type="button" class="btn p-0 ms-0 text-muted upd-wand"title="Edit mode (modify underlined data in-place, enter to validate)" aria-label="Edit mode (modify underlined data in-place, enter to validate)"><i class="fa fa-wand-magic-sparkles fa-sm"></i></button></h1><!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/heading.txt -->
  </div>
  <div class="row">
  <div class="col-lg-8">
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/individu.txt -->
  <!-- $Id: modules/individu.txt v7.1 04/03/2026 02:42:09 $ -->
  <div class="d-flex flex-column flex-md-row align-items-center align-items-md-start">
  <div class="flex-grow-1">
  <div class="text-center mb-2 mx-2">
  </div>
  <ul class="fa-ul ps-0 mb-0">
  <li class="mt-1" title="Death">
  <span class="fa-li"><i class="fas fa-skull-crossbones"></i></span>Died before&nbsp;1849</li>
  <li class="mt-1" title="Occupation">
  <span class="fa-li"><i class="fas fa-user-tie"></i></span><span data-updfield="occu"
   data-updraw="Marchand de bois">Marchand de bois
  </span></li>
  </ul>
  </div>
  </div>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/individu.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/unions.txt -->
  <!-- $Id: modules/unions.txt v7.1 17/11/2023 10:30:00 $ -->
  <h2 class="h3 mt-2 w-100">
  Marriage and children<span class="ms-2"><a href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=T&v=1"><img class="mx-1 mb-1" src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/images/gui_create.png" height="18" alt="tree desc."
  title="Descendants tree up to the children (with spouse)"></a>
  </span>
  </h2>
  <ul class="ps-4 py-0 fa-ul ms-0 mb-0">
  <li><span class="fa-li"><a href="gwd?b=galichet&m=MOD_FAM&i=0&ip=0"><i class="fa fa-wrench fa-sm text-success" data-bs-html="true" data-bs-toggle="tooltip"
  title="<div class='text-start'>Update family Galichet/Loche (wizard)<br>
  <b>Him </b><br>
  <b>Her </b><br>
  </div>"></i></a>
  </span><span  data-bs-toggle="tooltip" data-bs-html="true"
  title='<div class="text-wrap text-start">
  <div><span class="fw-bold">
  Notes of  marriage:</span> <p>
  Rajout de notes avec retour à la ligne avec des tag html
  <br>1880 - Habitent Yzernay naissance d&#39;une fille
  <br>1886 - Habitent la Verdelière (6 enfants) - Recensement Moulins: - Vue 23
  <br>1891 - N&#39;habitent plus Moulins, recensés au Temple (79) * Vue 7
  </p>.</div></div>'
  >Married to</span> <a href="gwd?b=galichet&p=marie+elisabeth&n=loche" class="female-underline">Marie Elisabeth Loche</a><a href="gwd?b=galichet&m=MOD_IND&i=1" title="Update Marie Elisabeth Loche " class="text-nowrap fst-italic"> <bdo dir=ltr>†/1849</bdo></a> (<p>
  Rajout de notes avec retour à la ligne avec des tag html
  <br>1880 - Habitent Yzernay naissance d&#39;une fille
  <br>1886 - Habitent la Verdelière (6 enfants) - Recensement Moulins: - Vue 23
  <br>1891 - N&#39;habitent plus Moulins, recensés au Temple (79) * Vue 7
  </p>), with five children:
  <ul>
  <li style="vertical-align: middle;list-style-type: circle;"><a href="gwd?b=galichet&m=MOD_IND&i=2" title="Update Jean Charles Galichet"><i class="fa fa-mars male fa-xs me-1"></i></a><a href="gwd?b=galichet&p=jean+charles&n=galichet" title="Display Jean Charles Galichet">Jean Charles</a><a href="gwd?b=galichet&m=MOD_IND&i=2"><span class="text-nowrap fst-italic"
  title="Update Jean Charles Galichet"> <bdo dir=ltr>1813</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: circle;"><a href="gwd?b=galichet&m=MOD_IND&i=3" title="Update Pierre Galichet"><i class="fa fa-mars male fa-xs me-1"></i></a><a href="gwd?b=galichet&p=pierre&n=galichet" title="Display Pierre Galichet">Pierre</a><a href="gwd?b=galichet&m=MOD_IND&i=3"><span class="text-nowrap fst-italic"
  title="Update Pierre Galichet (21 years)"> <bdo dir=ltr>1814–1835</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: square;"><a href="gwd?b=galichet&m=MOD_IND&i=4" title="Update Paul Galichet"><i class="fa fa-mars male fa-xs me-1"></i></a><a href="gwd?b=galichet&p=paul&n=galichet" title="Display Paul Galichet">Paul</a><a href="gwd?b=galichet&m=MOD_IND&i=4"><span class="text-nowrap fst-italic"
  title="Update Paul Galichet (70 years)"> <bdo dir=ltr>1816–1886</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: disc;"><a href="gwd?b=galichet&m=MOD_IND&i=5" title="Update lolo Galichet"><i class="fa fa-mars male fa-xs me-1"></i></a><a href="gwd?b=galichet&p=lolo&n=galichet" title="Display lolo Galichet">lolo</a><a href="gwd?b=galichet&m=MOD_IND&i=5"><span class="text-nowrap fst-italic"
  title="Update lolo Galichet"> <bdo dir=ltr>1818</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: circle;"><a href="gwd?b=galichet&m=MOD_IND&i=6" title="Update Thérèse Eugénie Galichet"><i class="fa fa-venus female fa-xs me-1"></i></a><a href="gwd?b=galichet&p=therese+eugenie&n=galichet" title="Display Thérèse Eugénie Galichet">Thérèse Eugénie</a><a href="gwd?b=galichet&m=MOD_IND&i=6"><span class="text-nowrap fst-italic"
  title="Update Thérèse Eugénie Galichet"> <bdo dir=ltr>1830</bdo></span></a></li>
  </ul>
  </li></ul>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/unions.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/chronologie.txt -->
  <!-- $Id: modules/chronologie.txt v7.1 04/04/2026 06:50:49 $ -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/chronologie.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/notes.txt -->
  <!-- $Id: modules/notes.txt v7.1 04/03/2026 02:42:31 $ -->
  <div id="mod_notes">
  <h2 class="h3 mt-2">Individual notes</h2>
  <div class="ms-4 ind-notes">
  <p>
  </p><ul>
  <li> j&#39;utilise la même image dans les notes du mari et de la femme</li>
  <li> avec un texte alternatif sans apostrophe pour le mari
  <br><img src="gwd?b=galichet&#38;m=IM;s=jean_pierre.0.galichet.jpg" width="50%" alt="texte alternatif sans apostrophe"></li>
  </ul>
  <ul>
  <li> et un texte alternatif <b>avec</b> apostrophe pour la femme <a id="p_1" href="gwd?b=galichet&#38;p=marie+elisabeth;n=loche">Marie Elisabeth Loche</a></li>
  </ul>
  <ul>
  <li> les vues individuelles sont correctes, mais la vue sous forme de table des ascendants est incorrecte à cause de cette apostrophe.</li>
  </ul>
  <ul>
  <li> ici je teste le tag html de prefix, dans une liste <b>wiki</b>
  <pre>une 1ere  ligne
  suivie d&#39;une 2eme ligne
  et d&#39;une 3eme
  et une ligne terminale</pre></li>
  <li>voir la fiche de <a id="p_2" href="gwd?b=galichet&#38;p=anthoine;n=geruzet">Anthoine Geruzet</a>
  pour les même ligne dans une liste html.</li>
  </ul>
  <ul>
  <li> About issue 140 and notes section access:
    <ul>
    <li> Able to access <a href="gwd?b=galichet&#38;m=NOTES#a_5">notes db section 5</a> with html syntax</li>
    <li> BUT do not try to access with three brackets wiki syntax, as would create empty notes file !</li>
    <li> the <a href="gwd?b=galichet&#38;m=NOTES&#38;f=copynotesdb">copynotesdb</a> MUST be referenced without the dash section !</li>
    <li> Nevertheless able to access section of a standard notes file with
  suggested <a href="https://fr.wikipedia.org/wiki/Aide:Lien_ancr%C3%A9_(wikicode)">wikipedia</a>
  syntax as per section <a href="gwd?b=galichet&#38;m=NOTES&#38;f=copynotesdb#a_5">5</a> pour test issue 140</li>
    </ul>
  </li>
  </ul>
  <p></p></div>
  <div class="fam-notes">
  <ul class="ps-0">
  </ul>
  </div>
  </div><!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/notes.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/sources.txt -->
  <!-- $Id: modules/sources.txt v7.1 04/03/2026 02:42:03 $ -->
  <div id="mod_sources">
  <h2 class="h3 mt-2">Source<span id="upd-wand-sources" class="ms-2"></span></h2>
  <ul class="ps-4 py-0">
  <li>Individual, family:
  déjà décédés au mariage de sa fille Thérèse Eugénie.
  </li>
  </ul>
  </div>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/sources.txt -->
  </div>
  <div class="col-lg-4">
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/arbre_3gen_photo.txt -->
  <!-- $Id: modules/arbre_3gen_photo.txt v7.1 22/04/2024 07:27:12 $ -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/arbre_3gen_photo.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/fratrie.txt -->
  <!-- $Id: modules/fratrie.txt v7.1 13/08/2023 16:47:40 $ -->
   <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/fratrie.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/relations.txt -->
  <!-- $Id: modules/relations.txt v7.1 17/11/2023 10:30:00 $ -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/modules/relations.txt -->
  </div>
  </div>
  </main>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/trl.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/trl.txt -->
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/copyr.txt -->
  <!-- $Id: copyr.txt v7.1 07/10/2026 17:09:29 $ -->
  <div class="d-flex flex-row flex-wrap justify-content-center justify-content-lg-end align-items-center my-2" id="copyr">
  <div class="d-flex flex-wrap justify-content-md-end align-items-center">
  <div class="d-flex flex-row align-items-center mt-0 ms-3">
  <div class="btn-group dropup" data-bs-toggle="tooltip" data-bs-placement="left"
  title="English – Select language">
  <button class="btn btn-link dropdown-toggle" type="button" id="dropdownMenu1"
  data-bs-toggle="dropdown" aria-expanded="false">
  <span class="text-uppercase" aria-hidden="true">en</span>
  <span class="visually-hidden">English, select language</span>
  </button>
  <div id="language-menu" class="dropdown-menu scrollable-lang"
  aria-labelledby="dropdownMenu1"></div>
  </div>
  <div class="d-flex flex-column align-items-center small ms-1 ms-md-3"
  aria-label="Connections" role="status">
  <a href="gwd?b=galichet&m=CONN_WIZ">1 wizard
  </a><span>1 connection
  </span>
  </div>
  </div>
  <div class="d-flex flex-column justify-content-md-end align-self-center ms-1 ms-md-3 ms-lg-4">
  <div class="text-nowrap">
  <a class="me-2"
  href="gwd?b=galichet&templ=templm&p=jean+pierre&n=galichet"
  data-bs-toggle="tooltip"
  title="templm"><i class="fab fa-markdown" aria-hidden="true"></i><span class="visually-hidden">templm</span></a>GeneWeb UNPREDICTABLE</div>
  <div class="btn-group mt-0 ms-1">
  <span>&copy; <a href="https://www.inria.fr" target="_blank" rel="noreferrer noopener">INRIA</a> 1998-2007</span>
  <a href="https://geneweb.tuxfamily.org/wiki/GeneWeb"
  class="ms-1" target="_blank" rel="noreferrer noopener"
  data-bs-toggle="tooltip" title="GeneWeb Wiki"><i class="fab fa-wikipedia-w" aria-hidden="true"></i>
  <span class="visually-hidden">GeneWeb Wiki</span>
  </a>
  <a href="https://github.com/geneweb/geneweb"
  class="ms-1" target="_blank" rel="noreferrer noopener"
  data-bs-toggle="tooltip" title="GeneWeb Github"><i class="fab fa-github" aria-hidden="true"></i>
  <span class="visually-hidden">GeneWeb Github</span>
  </a>
  </div>
  </div>
  </div>
  </div>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/copyr.txt -->
  </div>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js.txt -->
  <!-- $Id: js.txt v7.1 03/10/2026 01:19:36 $ -->
  <script src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/bootstrap.bundle.min.js?version=5.3.8"></script>
  <script src="/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/autosize.min.js?version=6.0.1"></script>
  <script>autosize(document.querySelectorAll('textarea'));</script>
  <script>
  (() => {
  const menu = document.getElementById('language-menu');
  if (!menu) return;
  const names = {'af':`Afrikaans`,'ar':`Arabic`,'bg':`Bulgarian`,'br':`Breton`,'ca':`Catalan`,'co':`Corsican`,'cs':`Czech`,'da':`Danish`,'de':`German`,'en':`English`,'eo':`Esperanto`,'es':`Spanish`,'et':`Estonian`,'fi':`Finnish`,'fr':`French`,'he':`Hebrew`,'is':`Icelandic`,'it':`Italian`,'lt':`Lithuanian`,'lv':`Latvian`,'nl':`Dutch`,'no':`Norwegian`,'oc':`Occitan`,'pl':`Polish`,'pt':`Portuguese`,'pt-br':`Brazilian-Portuguese`,'ro':`Romanian`,'ru':`Russian`,'sk':`Slovak`,'sl':`Slovenian`,'sv':`Swedish`,'tr':`Turkish`};
  const def = 'en';
  const base = 'en';
  const hrefFor = t => {
  const u = new URL(window.location.href);
  u.searchParams.delete('file');
  if (t === def) u.searchParams.delete('lang');
  else u.searchParams.set('lang', t);
  return u.pathname + u.search + u.hash;
  };
  const frag = document.createDocumentFragment();
  for (const l of Object.keys(names)) {
  const cur = l === 'en';
  const a = document.createElement('a');
  a.className = 'dropdown-item' + (l === base ? ' bg-warning' : '')
  + (cur ? ' disabled' : '');
  a.id = 'lang_' + l;
  if (cur) a.setAttribute('aria-current', 'true');
  else a.href = hrefFor(l);
  const code = document.createElement('code');
  code.textContent = (l === 'pt-br' ? l : l + '\u00a0\u00a0\u00a0') + ' ';
  a.appendChild(code);
  const n = names[l];
  a.appendChild(document.createTextNode(
  n.charAt(0).toUpperCase() + n.slice(1)));
  frag.appendChild(a);
  }
  menu.appendChild(frag);
  })();
  </script>
  <script>
  const UPD = {
  prefix: "gwd?b=galichet&",
  iper: "0",
  error: `Error`,
  editField: `Edit this data in-place`
  };
  (function () {
  const slot = document.getElementById('upd-wand-sources');
  const wand = document.querySelector('.upd-wand');
  if (slot && wand && !slot.querySelector('.upd-wand')) {
  slot.appendChild(wand.cloneNode(true));
  }
  })();
  (function () {
  let loaded = false;
  document.addEventListener('click', ev => {
  if (!ev.target.closest('.upd-wand')) return;
  ev.preventDefault();
  if (loaded) { if (window.InplaceEdit) InplaceEdit.toggle(); return; }
  loaded = true;
  const s = document.createElement('script');
  s.src = "/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/updateField.min.js?hash=0fa51e8823f94782323e14ca2d57e11f";
  s.onload = () => { InplaceEdit.init(); InplaceEdit.toggle(); };
  document.head.appendChild(s);
  });
  })();
  </script>
  <script>
  document.addEventListener('DOMContentLoaded', () => {
  document.querySelectorAll('[data-bs-toggle="tooltip"]').forEach(el => {
  if (!bootstrap.Tooltip.getInstance(el)) {
  new bootstrap.Tooltip(el, {
  trigger: el.dataset.bsTrigger || 'hover',
  delay: { show: 200, hide: 50 },
  container: 'body'
  });
  }
  });
  });
  </script>
  <script>
  function initializeLazyModules() {
  function loadScriptOnce(buttonId, scriptUrl) {
  const btn = document.getElementById(buttonId);
  if (!btn) return;
  btn.addEventListener('click', function handler() {
  const script = document.createElement('script');
  script.src = scriptUrl;
  document.head.appendChild(script);
  btn.removeEventListener('click', handler);
  });
  }
  loadScriptOnce('load_once_p_mod', '/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/p_mod.min.js?hash=5bcafbd9290b2c1b63f59a4c20b1e9e9');
  loadScriptOnce('load_once_rlm_builder', '/home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js/rlm_builder.js?hash=469e86d912841061b96be1e39395006c');
  }
  document.addEventListener('DOMContentLoaded', initializeLazyModules);
  </script>
  <script>
  document.addEventListener('shown.bs.modal', function(event) {
  event.target.querySelector('[autofocus]')?.focus();
  });
  </script>
  <script>
  function setupFloatingPlaceholders() {
  const inputs = document.querySelectorAll('input[type="text"][placeholder], input[type="number"][placeholder], input[type="search"][placeholder], textarea[placeholder]');
  inputs.forEach(input => {
  if (input.placeholder.trim() === '') return;
  let wrapper = input.parentNode;
  if (wrapper.classList.contains('clear-button-wrapper')) {
  wrapper.classList.add('input-wrapper');
  } else if (!wrapper.classList.contains('input-wrapper')) {
  const hadFocus = document.activeElement === input;
  wrapper = document.createElement('div');
  wrapper.className = 'input-wrapper';
  input.parentNode.insertBefore(wrapper, input);
  wrapper.appendChild(input);
  if (hadFocus) requestAnimationFrame(() => input.focus());
  }
  const placeholder = document.createElement('span');
  placeholder.className = 'floating-placeholder';
  placeholder.textContent = input.placeholder;
  wrapper.appendChild(placeholder);
  input.addEventListener('focus', () => placeholder.classList.add('active'));
  input.addEventListener('blur', () => placeholder.classList.remove('active'));
  if (input === document.activeElement || input.hasAttribute('autofocus')) {
  placeholder.classList.add('active');
  }
  });
  }
  document.addEventListener('DOMContentLoaded', setupFloatingPlaceholders);
  </script>
  <!-- /home/tiky/git/geneweb/add-cgi-test/_build/install/default/share/geneweb/hd/etc/js.txt -->
  </body>
  </html>
