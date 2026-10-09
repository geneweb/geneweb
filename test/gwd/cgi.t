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
  <link rel="icon" href="../../../../../install/default/share/geneweb/hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="../../../../../install/default/share/geneweb/hd/images/favicon_gwd.png">
  <script>window.GWPERMA={"q":"lang=en","r":2,"t":{"c":"Copy the permalink","p":"exterior","f":"friend","w":"wizard"}};</script>
  <script defer src="../../../../../install/default/share/geneweb/hd/etc/js/permalink.min.js?hash=d8bb33b06468360963d0b4c7c3fd3d3a"></script><!-- ../../../../../install/default/share/geneweb/hd/etc/css.txt -->
    <!-- $Id: css.txt v7.1 23/05/2026 18:51:27 $ -->
  <meta name="format-detection" content="telephone=no">
  <link rel="stylesheet" href="../../../../../install/default/share/geneweb/hd/etc/css/bootstrap.min.css?version=5.3.8">
  <link rel="stylesheet" href="../../../../../install/default/share/geneweb/hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="../../../../../install/default/share/geneweb/hd/etc/css/css.css?hash=c2143faa05629ffb12f566217aff153b">
  <!-- ../../../../../install/default/share/geneweb/hd/etc/css.txt -->
  </head>
  <body>
  <main class="container" id="welcome">
  <!-- ../../../../../install/default/share/geneweb/hd/etc/hed.txt -->
  <!-- ../../../../../install/default/share/geneweb/hd/etc/hed.txt -->
  <div class="d-flex flex-column flex-md-row align-items-center justify-content-lg-around mt-1 mt-lg-3">
  <div class="col-md-3 order-2 order-md-1 align-self-center mt-3 mt-md-0">
  <div class="d-flex justify-content-center">
  <img src="../../../../../install/default/share/geneweb/hd/images/arbre_start.png" alt="logo GeneWeb" width="180">
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
  <!-- ../../../../../install/default/share/geneweb/hd/etc/trl.txt -->
  <!-- ../../../../../install/default/share/geneweb/hd/etc/trl.txt -->
  <!-- ../../../../../install/default/share/geneweb/hd/etc/copyr.txt -->
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
  <!-- ../../../../../install/default/share/geneweb/hd/etc/copyr.txt -->
  </main>
  <!-- ../../../../../install/default/share/geneweb/hd/etc/js.txt -->
  <!-- $Id: js.txt v7.1 03/10/2026 01:19:36 $ -->
  <script src="../../../../../install/default/share/geneweb/hd/etc/js/bootstrap.bundle.min.js?version=5.3.8"></script>
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
  <!-- ../../../../../install/default/share/geneweb/hd/etc/js.txt -->
  </body>
  </html>

  $ gwd-cgi "m=S&pn=galichet&p=&n="
  =========== QUERY_STRING: m=S&pn=galichet&p=&n= =========
  INFO  GWD  1970-01-01T00:00:00-00:00 (PID: UNPREDICTABLE) gwd?m=S&pn=galichet&p=&n=
    From: 
    Agent: Oz
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
  <link rel="shortcut icon" href="../../../../../install/default/share/geneweb/hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="../../../../../install/default/share/geneweb/hd/images/favicon_gwd.png">
  <script>window.GWPERMA={"q":"lang=en&m=S&pn=galichet","r":2,"t":{"c":"Copy the permalink","p":"exterior","f":"friend","w":"wizard"}};</script>
  <script defer src="../../../../../install/default/share/geneweb/hd/etc/js/permalink.min.js?hash=d8bb33b06468360963d0b4c7c3fd3d3a"></script>  <!-- $Id: css.txt v7.1 23/05/2026 18:51:27 $ -->
  <meta name="format-detection" content="telephone=no">
  <link rel="stylesheet" href="../../../../../install/default/share/geneweb/hd/etc/css/bootstrap.min.css?version=5.3.8">
  <link rel="stylesheet" href="../../../../../install/default/share/geneweb/hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="../../../../../install/default/share/geneweb/hd/etc/css/css.css?hash=c2143faa05629ffb12f566217aff153b">
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
  <script src="../../../../../install/default/share/geneweb/hd/etc/js/bootstrap.bundle.min.js?version=5.3.8"></script>
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
  loadScriptOnce('load_once_p_mod', '../../../../../install/default/share/geneweb/hd/etc/js/p_mod.min.js?hash=5bcafbd9290b2c1b63f59a4c20b1e9e9');
  loadScriptOnce('load_once_rlm_builder', '../../../../../install/default/share/geneweb/hd/etc/js/rlm_builder.js?hash=469e86d912841061b96be1e39395006c');
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