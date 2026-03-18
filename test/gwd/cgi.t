  $ export GWD_BIN="$(which gwd)"
  $ alias gwd-cgi=./gwd-cgi.sh

  $ gwd-cgi
  =========== QUERY_STRING:  =========
  [1970-01-01T00:00:00-00:00][INFO]: No configuration file ./bases/galichet.gwf found, see ../../hd/a.gwf for example
  [1970-01-01T00:00:00-00:00][INFO]: === ROBOT SUMMARY ===
  [1970-01-01T00:00:00-00:00][INFO]:   Blocked IPs: 0, Monitored: 1
  [1970-01-01T00:00:00-00:00][INFO]:   Most active: 1 req by 
  [1970-01-01T00:00:00-00:00][INFO]:   Blocked robots detail:
  [1970-01-01T00:00:00-00:00][INFO]: (PID: UNPREDICTABLE) gwd?
    From: 
    Agent: Oz
  Content-type: text/html; charset=UTF-8
  Date: UNPREDICTABLE
  Connection: close
  
  <!-- $Id: welcome.txt v7.1 16/02/2025 17:11:01 $ -->
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <title>GeneWeb – galichet</title>
  <meta name="robots" content="none">
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
  <link rel="icon" href="../../hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="../../hd/images/favicon_gwd.png">
  <!-- ../../hd/etc/css.txt -->
    <!-- $Id: css.txt v7.1 06/08/2025 00:34:15 $ -->
  <link rel="stylesheet" href="../../hd/etc/css/bootstrap.min.css?version=4.6.2">
  <link rel="stylesheet" href="../../hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="../../hd/etc/css/css.css?hash=ab8c273eb60a9999baaac9da388e72f7">
  <!-- ../../hd/etc/css.txt -->
  </head>
  <body>
  <div class="container" id="welcome">
  <!-- ../../hd/etc/hed.txt -->
  <meta name="format-detection" content="telephone=no">
  <!-- ../../hd/etc/hed.txt -->
  <div class="d-flex flex-column flex-md-row align-items-center justify-content-lg-around mt-1 mt-lg-3">
  <div class="col-md-3 order-2 order-md-1 align-self-center mt-3 mt-md-0">
  <div class="d-flex justify-content-center">
  <img src="../../hd/images/arbre_start.png" alt="logo GeneWeb" width="180">
  </div>
  </div>
  <div class="col-12 col-md-3 order-1 order-md-3 ml-md-2 px-0 mt-xs-2 mt-lg-0 align-self-center">
  <div class="d-flex flex-column col-md-10 pl-1 pr-0">
  <div class="btn-group btn-group-xs mt-1" role="group">
  </div>
  </div>
  </div>
  <div class="my-0 order-3 order-md-2 flex-fill text-lg-center align-self-md-center">
  <h1 class="font-weight-bolder">Genealogical database galichet</h1>
  <div class="d-flex justify-content-center">
  <span class="text-center h4 font-weight-lighter">35 individuals</span>
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
  <input type="hidden" name="lang" value="en">
  <input type="hidden" name="m" value="S" class="has_validation">
  <div class="d-flex justify-content-center">
  <div class="d-flex flex-column justify-content-center w-100">
  <div class="d-flex flex-column flex-md-row">
  <div class="w-100 w-md-auto flex-md-grow-1">
  <div class="d-flex flex-grow-1">
  <div class="d-flex align-items-center ml-1 mr-2">
  <abbr data-toggle="tooltip" data-placement="top" data-html="true"
  title="<div class='text-left font-weight-bold'><h5>Search formats:</h5>
  1. Individual:<br>
  a. first name·s surname ¹<br>
  <i class='font-weight-lighter'>John Doe</i><br><br>
  b. first name·s/surname ²/occurence ³<br>
  <i class='font-weight-lighter'>James William/Smith-Johnson/2<br>       James William/Smith-Johnson<br>
  James William/<br>       /Smith-Jonhson</i><br><br>
  2. Surname ¹: <i class='font-weight-lighter'>Doe</i><br><br>
  3. Public name: <i class='font-weight-lighter'>James Smith</i><br><br>
  4. Alias: <i class='font-weight-lighter'>Jimmy</i><br><br>
  5. Key: first name·s.occurence surname ²<br>
  <i class='font-weight-lighter'>James William.2 Smith-Johnson<br>       james william.2 smith johnson</i><br>
  <br><div class='font-weight-lighter small'>¹ only one word<br>² full surname<br>³ optional</div></div>">
  <i class="far fa-circle-question text-primary text-primary"></i>
  </abbr>
  </div>
  <input type="search" id="fullname" class="form-control form-control-lg py-0 border border-top-0" autofocus tabindex="1"
  name="pn" placeholder="Search individual, surname, public name, alias, key"
  >
  </div>
  <div class="d-flex mt-3">
  <div class="btn-group-vertical mr-2">
  <a role="button" href="gwd?b=galichet&m=P&tri=A" data-toggle="tooltip"
  title="First names, sort by alphabetical order"><i class="fa fa-arrow-down-a-z fa-fw"></i></a>
  <a role="button" href="gwd?b=galichet&m=P&tri=F" data-toggle="tooltip"
  title="Frequency first names, sort by number of individuals"><i class="fa fa-arrow-down-wide-short fa-fw"></i></a>
  </div>
  <div class="d-flex flex-grow-1">
  <div class="flex-grow-1 align-self-center">
  <label for="firstname" class="sr-only col-form-label">First name·s</label>
  <input type="search" id="firstname" class="form-control form-control-lg border-top-0"
  name="p" placeholder="First name·s" tabindex="2"
   list="datalist_fnames" data-book="fn">
  </div>
  </div>
  </div>
  <div class="d-flex mt-2">
  <div class="btn-group-vertical mr-2">
  <a role="button" href="gwd?b=galichet&m=N&tri=A" data-toggle="tooltip"
  title="Surnames, sort by alphabetical order"><i class="fa fa-arrow-down-a-z fa-fw"></i></a>
  <a role="button" href="gwd?b=galichet&m=N&tri=F" data-toggle="tooltip"
  title="Frequency surnames, sort by number of individuals"><i class="fa fa-arrow-down-wide-short fa-fw"></i></a>
  </div>
  <div class="d-flex flex-grow-1">
  <div class="flex-grow-1 align-self-center">
  <label for="surname" class="sr-only col-form-label col-sm-2">Surname</label>
  <input type="search" id="surname" class="form-control form-control-lg border border-top-0"
  title="Full surname" data-toggle="tooltip"
  name="n" placeholder="Surname" tabindex="3"
   list="datalist_snames" data-book="sn">
  </div>
  </div>
  </div>
  </div>
  <div class="d-flex flex-column align-items-center justify-content-between mt-3 mt-md-0 mx-0 mx-md-1 px-0 px-md-3 col-md-auto small">
  <div class="d-flex flex-row flex-md-column justify-content-start mb-3 mb-md-0">
  <div class="align-self-md-start font-weight-bold mr-3 mr-md-0 mb-0 mb-md-1">First names:</div>
  <div class="d-flex flex-row flex-md-column">
  <div class="custom-control custom-checkbox mr-3 mr-md-0 mb-md-1" data-toggle="tooltip" data-placement="left" title="Search all permutations (2 to 4 first names)">
  <input class="custom-control-input" type="checkbox" name="p_order" id="p_order" value="on" tabindex="6">
  <label class="custom-control-label d-flex align-items-center" for="p_order">Permutated</label>
  </div>
  <div class="custom-control custom-checkbox" data-toggle="tooltip" data-placement="left" title="Partial matches, phonetic variants (quite long to load!)">
  <input class="custom-control-input" type="checkbox" name="p_exact" id="p_exact" value="off" tabindex="4">
  <label class="custom-control-label d-flex align-items-center" for="p_exact">Partials</label>
  </div>
  </div>
  </div>
  <button id="global-search-inline" class="d-none btn btn-outline-primary font-weight-bolder w-100 w-md-auto py-2 mb-1"
  type="submit">
  <i class="fa fa-magnifying-glass fa-lg fa-fw"></i>
  Search
  </button>
  </div>
  </div>
  </div>
  </div>
  </form>
  <div class="d-flex flex-wrap flex-md-no-wrap justify-content-center align-items-md-center mt-1 mt-md-3">
  <form id="title-search" class="d-flex flex-column flex-md-row align-items-start align-items-md-center col-12 col-md-9 ml-md-4" method="get" action="gwd">
  <input type="hidden" name="b" value="galichet">
  <input type="hidden" name="lang" value="en">
  <input type="hidden" name="m" value="TT">
  <div class="d-flex align-items-center w-100 mb-2 mb-md-0">
  <a class="mr-2" role="button" data-toggle="tooltip" data-placement="bottom"
  href="gwd?b=galichet&m=TT" title="All the titles"><i class="fa fa-list-ul fa-fw"></i></a>
  <label for="titles" class="sr-only col-form-label">Title</label>
  <input type="search" class="form-control border-top-0 border-right-0 border-left-0 w-100"
  name="t" id="titles" placeholder="Title" tabindex="7"
  list="datalist_titles" data-book="title">
  </div>
  <div class="d-flex align-items-center w-100 mb-2 mb-md-0 ml-0 ml-md-2">
  <a class="mr-2" role="button" data-toggle="tooltip" data-placement="bottom"
  href="gwd?b=galichet&m=TT&p=*" title="All the fiefs"><i class="fa fa-list-ul fa-fw"></i></a>
  <label for="estates" class="sr-only col-form-label">Fief</label>
  <input type="search" class="form-control border-top-0 border-right-0 border-left-0 w-100"
  name="p" id="estates" placeholder="Fief" tabindex="8"
  list="datalist_estates" data-book="domain">
  </div>
  <button class="d-none" type="submit"></button>
  </form>
  <button id="global-search" class="btn btn-outline-primary font-weight-bolder col-12 col-md-auto mt-1 mt-md-0 ml-md-auto mr-md-2 py-2"
  type="submit" tabindex="9"
  data-toggle="tooltip" data-placement="right">
  <i class="fa fa-magnifying-glass fa-lg fa-fw"></i>
  Search
  </button>
  </div>
  </div>
  </div>
  <div class="d-flex flex-column justify-content-start justify-content-md-center mt-4 col-12 col-md-11 col-lg-10 mx-auto">
  <div class="h4 text-md-center"><i class="fas fa-screwdriver-wrench fa-sm fa-fw text-secondary"></i>
  Tools</div>
  <div class="d-flex flex-wrap justify-content-md-center">
  <a role="button" class="btn btn-outline-primary" href="gwd?b=galichet&m=NOTES"><i class="far fa-file-lines fa-fw mr-1" aria-hidden="true"></i>Notes
  </a>
  <a role="button" class="btn btn-outline-primary" href="gwd?b=galichet&m=ISOLATED"><i class="fa fa-user fa-fw mr-1" aria-hidden="true"></i>Isolated persons</a>
  <a role="button" class="btn btn-outline-primary" href="gwd?b=galichet&m=STAT"><i class="far fa-chart-bar fa-fw mr-1" aria-hidden="true"></i>Statistics</a>
  <a role="button" class="btn btn-outline-primary" href="gwd?b=galichet&m=ANM"><i class="fa fa-cake-candles fa-fw mr-1" aria-hidden="true"></i>Anniversaries</a>
  <a role="button" class="btn btn-outline-primary" href="gwd?b=galichet&m=PPS&bi=on&ba=on&ma=on&de=on&bu=on"><i class="fas fa-globe fa-fw mr-1" aria-hidden="true"></i>Places/surname</a>
  <a role="button" class="btn btn-outline-primary" href="gwd?b=galichet&m=CAL"><i class="far fa-calendar-days fa-fw mr-1" aria-hidden="true"></i>Calendars</a>
  <a role="button" class="btn btn-outline-success" href="gwd?b=galichet&m=H&v=conf"><i class="fas fa-gear fa-fw mr-1" aria-hidden="true"></i>Configuration</a>
  <a role="button" class="btn btn-outline-success" href="gwd?b=galichet&m=MOD_NOTES&new"><i class="fa fa-file-lines fa-fw mr-1" aria-hidden="true"></i>Add note</a>
  <a role="button" class="btn btn-outline-success" href="gwd?b=galichet&m=ADD_FAM"><i class="fas fa-user-plus fa-fw mr-1" aria-hidden="true"></i>Add family</a>
  </div>
  </div>
  <div class="d-flex flex-column justify-content-start justify-content-md-center mt-3 col-12 col-md-9 mx-auto">
  <div class="h4 text-md-center">
  <i class="fas fa-book fa-sm fa-fw text-success mr-1"></i>Dictionaries</div>
  <div class="d-flex flex-wrap justify-content-md-center">
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=fn" title="Update book of first names (wizard)"><i class="fa fa-child mr-1"></i>First names
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=sn" title="Update book of surnames (wizard)"><i class="fa fa-user mr-1"></i>Surnames
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=fna" title="Update book of alternate first names (wizard)"><i class="fa fa-child mr-1"></i>Alternate first names
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=sna" title="Update book of alternate surnames (wizard)"><i class="fa fa-user mr-1"></i>Alternate surnames
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=pubn" title="Update book of public names (wizard)"><i class="fa fa-signature mr-1"></i>Public names
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=qual" title="Update book of qualifiers (wizard)"><i class="fa fa-comment mr-1"></i>Qualifiers
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=alias" title="Update book of aliases (wizard)"><i class="fa fa-mask mr-1"></i>Aliases
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=occu" title="Update book of occupations (wizard)"><i class="fa fa-briefcase mr-1"></i>Occupations
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=place" title="Update book of places (wizard)"><i class="fa fa-location-dot mr-1"></i>Places
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=src" title="Update book of sources (wizard)"><i class="fa fa-box-archive mr-1"></i>Sources
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=title" title="Update book of titles (wizard)"><i class="fa fa-crown mr-1"></i>Titles
  </a>
  <a role="button" class="btn btn-outline-success" data-toggle="tooltip"
  href="gwd?b=galichet&m=MOD_DATA&data=domain" title="Update book of domains (wizard)"><i class="fa fa-chess-rook mr-1"></i>Domains
  </a>
  <a href="gwd?b=galichet&m=CHK_DATA" class="btn btn-outline-success ml-2"
  title="Data typographic checker"><i class="fas fa-spell-check"></i>
  </a>    </div>
  </div>
  <div class="d-flex mt-3">
  <div class="col-11 col-md-auto mx-auto">
  There has been 1 accesses, 1 of
  them to this page, since hidden.</div>
  </div>
  <!-- ../../hd/etc/trl.txt -->
  <!-- ../../hd/etc/trl.txt -->
  <!-- ../../hd/etc/copyr.txt -->
  <!-- $Id: copyr.txt UNPREDICTABLE 18/12/2023 22:03:44 $ -->
  <div class="d-flex flex-row justify-content-center justify-content-lg-end my-2" id="copyr">
  <div class="d-flex flex-wrap justify-content-md-end align-items-center">
  <!-- legal notices -->
  <!-- Language selector and connections info -->
  <div class="d-flex flex-row align-items-lg-end mt-0 ml-3 border-0">
  <div class="btn-group dropup" data-toggle="tooltip" data-placement="left"
  title="English – Select language">
  <button class="btn btn-link dropdown-toggle" type="button" id="dropdownMenu1"
  data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
  <span class="sr-only">lang</span>
  <span class="text-uppercase">en</span>
  <span class="sr-only">, select language</span>
  </button>
  <div class="dropdown-menu scrollable-lang" aria-labelledby="dropdownMenu1">
  <a class="dropdown-item" id="lang_af" href="gwd?b=galichet&lang=af"><code>af&nbsp;&nbsp;&nbsp; </code>Afrikaans</a>
  <a class="dropdown-item" id="lang_ar" href="gwd?b=galichet&lang=ar"><code>ar&nbsp;&nbsp;&nbsp; </code>Arabic</a>
  <a class="dropdown-item" id="lang_bg" href="gwd?b=galichet&lang=bg"><code>bg&nbsp;&nbsp;&nbsp; </code>Bulgarian</a>
  <a class="dropdown-item" id="lang_br" href="gwd?b=galichet&lang=br"><code>br&nbsp;&nbsp;&nbsp; </code>Breton</a>
  <a class="dropdown-item" id="lang_ca" href="gwd?b=galichet&lang=ca"><code>ca&nbsp;&nbsp;&nbsp; </code>Catalan</a>
  <a class="dropdown-item" id="lang_co" href="gwd?b=galichet&lang=co"><code>co&nbsp;&nbsp;&nbsp; </code>Corsican</a>
  <a class="dropdown-item" id="lang_cs" href="gwd?b=galichet&lang=cs"><code>cs&nbsp;&nbsp;&nbsp; </code>Czech</a>
  <a class="dropdown-item" id="lang_da" href="gwd?b=galichet&lang=da"><code>da&nbsp;&nbsp;&nbsp; </code>Danish</a>
  <a class="dropdown-item" id="lang_de" href="gwd?b=galichet&lang=de"><code>de&nbsp;&nbsp;&nbsp; </code>German</a>
  <a class="dropdown-item" id="lang_eo" href="gwd?b=galichet&lang=eo"><code>eo&nbsp;&nbsp;&nbsp; </code>Esperanto</a>
  <a class="dropdown-item" id="lang_es" href="gwd?b=galichet&lang=es"><code>es&nbsp;&nbsp;&nbsp; </code>Spanish</a>
  <a class="dropdown-item" id="lang_et" href="gwd?b=galichet&lang=et"><code>et&nbsp;&nbsp;&nbsp; </code>Estonian</a>
  <a class="dropdown-item" id="lang_fi" href="gwd?b=galichet&lang=fi"><code>fi&nbsp;&nbsp;&nbsp; </code>Finnish</a>
  <a class="dropdown-item" id="lang_fr" href="gwd?b=galichet&lang=fr"><code>fr&nbsp;&nbsp;&nbsp; </code>French</a>
  <a class="dropdown-item" id="lang_he" href="gwd?b=galichet&lang=he"><code>he&nbsp;&nbsp;&nbsp; </code>Hebrew</a>
  <a class="dropdown-item" id="lang_is" href="gwd?b=galichet&lang=is"><code>is&nbsp;&nbsp;&nbsp; </code>Icelandic</a>
  <a class="dropdown-item" id="lang_it" href="gwd?b=galichet&lang=it"><code>it&nbsp;&nbsp;&nbsp; </code>Italian</a>
  <a class="dropdown-item" id="lang_lt" href="gwd?b=galichet&lang=lt"><code>lt&nbsp;&nbsp;&nbsp; </code>Lithuanian</a>
  <a class="dropdown-item" id="lang_lv" href="gwd?b=galichet&lang=lv"><code>lv&nbsp;&nbsp;&nbsp; </code>Latvian</a>
  <a class="dropdown-item" id="lang_nl" href="gwd?b=galichet&lang=nl"><code>nl&nbsp;&nbsp;&nbsp; </code>Dutch</a>
  <a class="dropdown-item" id="lang_no" href="gwd?b=galichet&lang=no"><code>no&nbsp;&nbsp;&nbsp; </code>Norwegian</a>
  <a class="dropdown-item" id="lang_oc" href="gwd?b=galichet&lang=oc"><code>oc&nbsp;&nbsp;&nbsp; </code>Occitan</a>
  <a class="dropdown-item" id="lang_pl" href="gwd?b=galichet&lang=pl"><code>pl&nbsp;&nbsp;&nbsp; </code>Polish</a>
  <a class="dropdown-item" id="lang_pt" href="gwd?b=galichet&lang=pt"><code>pt&nbsp;&nbsp;&nbsp; </code>Portuguese</a>
  <a class="dropdown-item" id="lang_pt-br" href="gwd?b=galichet&lang=pt-br"><code>pt-br </code>Brazilian-Portuguese</a>
  <a class="dropdown-item" id="lang_ro" href="gwd?b=galichet&lang=ro"><code>ro&nbsp;&nbsp;&nbsp; </code>Romanian</a>
  <a class="dropdown-item" id="lang_ru" href="gwd?b=galichet&lang=ru"><code>ru&nbsp;&nbsp;&nbsp; </code>Russian</a>
  <a class="dropdown-item" id="lang_sk" href="gwd?b=galichet&lang=sk"><code>sk&nbsp;&nbsp;&nbsp; </code>Slovak</a>
  <a class="dropdown-item" id="lang_sl" href="gwd?b=galichet&lang=sl"><code>sl&nbsp;&nbsp;&nbsp; </code>Slovenian</a>
  <a class="dropdown-item" id="lang_sv" href="gwd?b=galichet&lang=sv"><code>sv&nbsp;&nbsp;&nbsp; </code>Swedish</a>
  <a class="dropdown-item" id="lang_tr" href="gwd?b=galichet&lang=tr"><code>tr&nbsp;&nbsp;&nbsp; </code>Turkish</a>
  <a class="dropdown-item" id="lang_zh" href="gwd?b=galichet&lang=zh"><code>zh&nbsp;&nbsp;&nbsp; </code>Chinese</a>
  </div>
  </div>
  <!-- Connections info -->
  <div class="d-flex flex-column justify-items-center align-items-center small ml-1 ml-md-3">
  <a href="gwd?b=galichet&m=CONN_WIZ">1 wizard
  </a><span>1 connection
  </span>
  </div>
  </div>
  <!-- Footer links and copyright -->
  <div class="d-flex flex-column justify-content-md-end align-self-center ml-1 ml-md-3 ml-lg-4">
  <div class="ml-auto mb-0">
  <a role="button" class="mr-2"
  href="gwd?b=galichet&templ=templm"
  data-toggle="tooltip"
  title="templm"><i class="fab fa-markdown" aria-hidden="true"></i><span class="sr-only">switch to templm</span></a>GeneWeb UNPREDICTABLE</div>
  <div class="btn-group mt-0 ml-1">
  <span>&copy; <a href="https://www.inria.fr" target="_blank" rel="noreferrer, noopener">INRIA</a> 1998-2007</span>
  <a href="https://geneweb.tuxfamily.org/wiki/GeneWeb"
  class="ml-1" target="_blank" rel="noreferrer, noopener" data-toggle="tooltip" title="GeneWeb Wiki"><i class="fab fa-wikipedia-w"></i>
  </a>
  <a href="https://github.com/geneweb/geneweb"
  class="ml-1" target="_blank" rel="noreferrer, noopener" data-toggle="tooltip" title="GeneWeb Github"><i class="fab fa-github"></i>
  </a>
  </div>
  </div>
  </div>
  </div>
  <!-- ../../hd/etc/copyr.txt -->
  </div>
  <!-- ../../hd/etc/js.txt -->
  <!-- $Id: js.txt v7.1 17/01/2026 10:39:33 $ -->
  <script src="../../hd/etc/js/jquery.min.js?version=3.7.1"></script>
  <script src="../../hd/etc/js/bootstrap.bundle.min.js?version=4.6.1"></script>
  <script>
  // Manages enhanced input field behaviors (navigation, interaction)
  const inputToBook = {
  addNavigation: function() {
  // Use event delegation for dynamically added inputs
  $(document).on('mousedown', 'input[data-book]', function(event) {
  if (event.ctrlKey && event.button === 0) {
  event.preventDefault();
  inputToBook.openBook(this.value, $(this).data('book'));
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
  // Controls unified search functionality with a shared submit button
  // Handles both main person search and optional title search forms
  function initializeWelcomeSearchFunctionality() {
  const searchForms = {
  main: document.getElementById('main-search'),
  title: document.getElementById('title-search')
  };
  const searchBtn = document.getElementById('global-search');
  function hasInput(form) {
  return form && Array.from(form.querySelectorAll('input[type="search"]'))
  .some(input => input.value.trim() !== '');
  }
  function closeAlertsAndCleanURL() {
  $('.alert').alert('close');
  const url = new URL(window.location.href);
  const bParam = url.searchParams.get('b');
  url.search = '';
  if (bParam !== null) {
  url.searchParams.set('b', bParam); // on remet b=
  }
  window.history.replaceState({}, '', url);
  }
  function getTooltip() {
  if (hasInput(searchForms.main)) {
  return 'Search individual';
  }
  if (searchForms.title && hasInput(searchForms.title)) {
  return 'Search title/fief';
  }
  return '';
  }
  $(searchBtn).tooltip({
  title: getTooltip,
  trigger: 'hover'
  });
  Object.values(searchForms).forEach(form => {
  if (!form) return;
  form.querySelectorAll('input[type="search"]').forEach(input => {
  input.addEventListener('input', () => {
  if (hasInput(form)) {
  closeAlertsAndCleanURL();
  }
  $(searchBtn).tooltip('hide')
  .attr('data-original-title', getTooltip())
  .tooltip('show');
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
  if (hasInput(searchForms.main)) {
  searchForms.main.submit();
  }
  else if (searchForms.title && hasInput(searchForms.title)) {
  searchForms.title.submit();
  }
  });
  }
  // Floating placeholders for all textual inputs
  function setupFloatingPlaceholders() {
  const inputs = document.querySelectorAll('input[type="text"][placeholder], input[type="number"][placeholder], input[type="search"][placeholder], textarea[placeholder]');
  inputs.forEach(input => {
  // Ignore placeholders that are only non-breaking spaces
  if (input.placeholder.trim() === '') return;
  const hadFocus = document.activeElement === input;
  const wrapper = document.createElement('div');
  wrapper.className = 'input-wrapper';
  input.parentNode.insertBefore(wrapper, input);
  wrapper.appendChild(input);
  const placeholder = document.createElement('span');
  placeholder.className = 'floating-placeholder';
  placeholder.textContent = input.placeholder;
  wrapper.appendChild(placeholder);
  input.addEventListener('focus', () => placeholder.classList.add('active'));
  input.addEventListener('blur', () => placeholder.classList.remove('active'));
  if (hadFocus || input.hasAttribute('autofocus')) {
  requestAnimationFrame(() => {
  input.focus();
  placeholder.classList.add('active');
  });
  }
  });
  }
  const initTooltips = () => {
  const tooltipElements = document.querySelectorAll('[data-toggle="tooltip"]');
  if (tooltipElements.length > 0) {
  $(tooltipElements).tooltip({
  trigger: 'hover',
  delay: { show: 200, hide: 50 },
  container: 'body',
  });
  }
  };
  function safeInitialize(fn) {
  try {
  fn();
  } catch (error) {
  console.error('Initialization error:', error);
  }
  }
  document.addEventListener('DOMContentLoaded', () => {
  initializeWelcomeSearchFunctionality();
  inputToBook.addNavigation();
  initTooltips();
  setupFloatingPlaceholders();
  });
  </script>
  <!-- ../../hd/etc/js.txt -->
  </body>
  </html>

  $ gwd-cgi "m=S&pn=galichet&p=&n="
  =========== QUERY_STRING: m=S&pn=galichet&p=&n= =========
  [1970-01-01T00:00:00-00:00][INFO]: No configuration file ./bases/galichet.gwf found, see ../../hd/a.gwf for example
  [1970-01-01T00:00:00-00:00][INFO]: (PID: UNPREDICTABLE) gwd?m=S&pn=galichet&p=&n=
    From: 
    Agent: Oz
  [1970-01-01T00:00:00-00:00][DEBUG]: Print case PersonName (galichet)
  [1970-01-01T00:00:00-00:00][DEBUG]:     Method Key: 0 results
  [1970-01-01T00:00:00-00:00][DEBUG]:       search_fullname: 9 results
  [1970-01-01T00:00:00-00:00][DEBUG]:         search_for_multiple_fn: 
  [1970-01-01T00:00:00-00:00][DEBUG]: order: false, exact: true
  [1970-01-01T00:00:00-00:00][DEBUG]:           result: 9
  [1970-01-01T00:00:00-00:00][DEBUG]:         search_for_multiple_fn: 
  [1970-01-01T00:00:00-00:00][DEBUG]: order: false, exact: false
  [1970-01-01T00:00:00-00:00][DEBUG]:           result: 9
  [1970-01-01T00:00:00-00:00][DEBUG]:         spouses:
  [1970-01-01T00:00:00-00:00][DEBUG]:   → 9 results (9 exact, 0 phonetic)
  [1970-01-01T00:00:00-00:00][DEBUG]:         search_for_multiple_fn: 
  [1970-01-01T00:00:00-00:00][DEBUG]: order: false, exact: false
  [1970-01-01T00:00:00-00:00][DEBUG]:           result: 5
  [1970-01-01T00:00:00-00:00][DEBUG]:     Method FullName: 9+9+5 results
  [1970-01-01T00:00:00-00:00][DEBUG]:     Method ApproxKey: 0 exact, 0 partial
  [1970-01-01T00:00:00-00:00][DEBUG]:       search_partial_key: 0 results
  [1970-01-01T00:00:00-00:00][DEBUG]:         search_for_multiple_fn: 
  [1970-01-01T00:00:00-00:00][DEBUG]: order: false, exact: true
  [1970-01-01T00:00:00-00:00][DEBUG]:           result: 9
  [1970-01-01T00:00:00-00:00][DEBUG]:         search_for_multiple_fn: 
  [1970-01-01T00:00:00-00:00][DEBUG]: order: false, exact: false
  [1970-01-01T00:00:00-00:00][DEBUG]:           result: 9
  [1970-01-01T00:00:00-00:00][DEBUG]:     Method PartialKey: 9+9+0 results
  [1970-01-01T00:00:00-00:00][DEBUG]:   → 9 results (9 exact, 0 phonetic)
  [1970-01-01T00:00:00-00:00][DEBUG]: Print_person_list: 9
  [1970-01-01T00:00:00-00:00][DEBUG]: Print_person_list: 5
  Content-type: text/html; charset=UTF-8
  Date: UNPREDICTABLE
  Connection: close
  
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <title>galichet: select</title>
  <meta name="robots" content="none">
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
  <link rel="shortcut icon" href="../../hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="../../hd/images/favicon_gwd.png">
    <!-- $Id: css.txt v7.1 06/08/2025 00:34:15 $ -->
  <link rel="stylesheet" href="../../hd/etc/css/bootstrap.min.css?version=4.6.2">
  <link rel="stylesheet" href="../../hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="../../hd/etc/css/css.css?hash=ab8c273eb60a9999baaac9da388e72f7">
  </head>
  <body>
  <meta name="format-detection" content="telephone=no">
  <!-- $Id: home.txt v7.1 07/09/2023 00:49:55 $ -->
  <div class="d-flex flex-column fix_top fix_left home-xs">
  <a tabindex="1" role="button" class="btn btn-sm btn-link p-0 border-0" href="gwd?b=galichet&" title="Home"><i class="fa fa-house fa-fw fa-xs" aria-hidden="true"></i><i class="sr-only">Home</i></a>
  <a tabindex="3" role="button" class="btn btn-sm btn-link p-0 border-0" data-toggle="modal" data-target="#searchmodal"
  accesskey="S" title="Search"><i class="fa fa-magnifying-glass fa-fw fa-xs" aria-hidden="true"></i><span class="sr-only">Search</span></a>
  </div>
  <div class="modal" id="searchmodal" role="dialog" aria-labelledby="searchpopup" aria-hidden="true">
  <div class="modal-dialog modal-lg" role="document">
  <div class="modal-content">
  <div class="modal-body" id="ModalSearch">
  <form id="collapse_search" method="get" action="gwd?b=galichet&">
  <input type="hidden" name="b" value="galichet">
  <input type="hidden" name="lang" value="en">
  <input type="hidden" name="m" value="S">
  <div class="d-flex flex-column flex-md-row justify-content-center">
  <h3 class="rounded modal-title my-2 ml-3 ml-md-0 text-md-center w-md-50 align-self-md-center" id="searchpopup">Search individual</h3>
  <div class="col-12 col-md-8 mt-2 mt-md-0">
  <label class="sr-only" for="fullname">Search person name</label>
  <input type="search" id="fullname" class="form-control form-control-lg no-clear-button"
  name="pn" placeholder="Search individual, surname, public name, alias or key"
  autofocus>
  <label class="sr-only" for="p">Search firstname</label>
  <input type="search" id="p" class="form-control form-control-lg no-clear-button mt-2"
  name="p" placeholder="First name·s">
  <label class="sr-only" for="n">Search surname</label>
  <input type="search" id="n" class="form-control form-control-lg no-clear-button mt-2"
  name="n" placeholder="Surname">
  <div class="d-flex mt-2">
  <div class="ml-2">First names</div>
  <div class="custom-control custom-checkbox ml-2 mr-3 mr-md-0 mb-md-1" data-toggle="tooltip" data-placement="bottom" title="Search all permutations (2 to 4 first names)">
  <input class="custom-control-input" type="checkbox" name="p_order" id="p_order" value="on">
  <label class="custom-control-label d-flex align-items-center" for="p_order">permutated</label>
  </div>
  <div class="custom-control custom-checkbox ml-2" data-toggle="tooltip" data-placement="bottom" title="Partial matches, phonetic variants (quite long to load!)">
  <input class="custom-control-input" type="checkbox" name="p_exact" id="p_exact" value="off">
  <label class="custom-control-label d-flex align-items-center" for="p_exact">partials</label>
  </div>
  </div>
  </div>
  <button class="btn btn-outline-primary mx-3 mx-md-0 mt-4 my-2 mt-md-0" type="submit" title="Search"><i class="fa fa-magnifying-glass fa-lg mt-2 mt-md-0 mx-4 mx-md-2"></i> Search</button>
  </div>
  </form>
  </div>
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
  With spouse's name
  <ul class="fa-ul">
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=marie+elisabeth&n=loche">Marie Elisabeth Loche</a> <bdo dir=ltr>†/1849</bdo>, married to Jean Pierre Galichet.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=louis&n=sutaine&oc=1">Louis Sutaine</a>, married to Nicole Galichet in 1570.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=florence&n=marty">Florence Marty</a> <bdo dir=ltr>1815–1875</bdo>, married to Paul Galichet in 1835.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=prenom1&n=femme1">prenom1 femme1</a> <bdo dir=ltr>1818</bdo>, married to lolo Galichet in 1840.</li>
  <li><span class="fa-li">
  <span class="bullet">•</span></span><a href="gwd?b=galichet&p=prenom2&n=femme2">prenom2 femme2</a> <bdo dir=ltr>1825</bdo>, married to lolo Galichet in 1845.</li>
  </ul>
  <!-- $Id: copyr.txt UNPREDICTABLE 18/12/2023 22:03:44 $ -->
  <div class="d-flex flex-row justify-content-center justify-content-lg-end my-2" id="copyr">
  <div class="d-flex flex-wrap justify-content-md-end align-items-center">
  <!-- legal notices -->
  <!-- Language selector and connections info -->
  <div class="d-flex flex-row align-items-lg-end mt-0 ml-3 border-0">
  <div class="btn-group dropup" data-toggle="tooltip" data-placement="left"
  title="English – Select language">
  <button class="btn btn-link dropdown-toggle" type="button" id="dropdownMenu1"
  data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
  <span class="sr-only">lang</span>
  <span class="text-uppercase">en</span>
  <span class="sr-only">, select language</span>
  </button>
  <div class="dropdown-menu scrollable-lang short" aria-labelledby="dropdownMenu1">
  <!-- Language options remain the same -->
  <a class="dropdown-item" href="gwd?b=galichet&lang=af&m=S&pn=galichet">af</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=ar&m=S&pn=galichet">ar</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=bg&m=S&pn=galichet">bg</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=br&m=S&pn=galichet">br</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=ca&m=S&pn=galichet">ca</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=co&m=S&pn=galichet">co</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=cs&m=S&pn=galichet">cs</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=da&m=S&pn=galichet">da</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=de&m=S&pn=galichet">de</a>
  <a class="dropdown-item" href="gwd?b=galichet&m=S&pn=galichet">en</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=eo&m=S&pn=galichet">eo</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=es&m=S&pn=galichet">es</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=et&m=S&pn=galichet">et</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=fi&m=S&pn=galichet">fi</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=fr&m=S&pn=galichet">fr</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=he&m=S&pn=galichet">he</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=is&m=S&pn=galichet">is</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=it&m=S&pn=galichet">it</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=lt&m=S&pn=galichet">lt</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=lv&m=S&pn=galichet">lv</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=nl&m=S&pn=galichet">nl</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=no&m=S&pn=galichet">no</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=oc&m=S&pn=galichet">oc</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=pl&m=S&pn=galichet">pl</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=pt&m=S&pn=galichet">pt</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=pt-br&m=S&pn=galichet">pt-br</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=ro&m=S&pn=galichet">ro</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=ru&m=S&pn=galichet">ru</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=sk&m=S&pn=galichet">sk</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=sl&m=S&pn=galichet">sl</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=sv&m=S&pn=galichet">sv</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=tr&m=S&pn=galichet">tr</a>
  <a class="dropdown-item" href="gwd?b=galichet&lang=zh&m=S&pn=galichet">zh</a>
  </div>
  </div>
  <!-- Connections info -->
  <div class="d-flex flex-column justify-items-center align-items-center small ml-1 ml-md-3">
  <a href="gwd?b=galichet&m=CONN_WIZ">1 wizard
  </a><span>1 connection
  </span>
  </div>
  </div>
  <!-- Footer links and copyright -->
  <div class="d-flex flex-column justify-content-md-end align-self-center ml-1 ml-md-3 ml-lg-4">
  <div class="ml-auto mb-0">
  <a role="button" class="mr-2"
  href="gwd?b=galichet&templ=templm&m=S&pn=galichet"
  data-toggle="tooltip"
  title="templm"><i class="fab fa-markdown" aria-hidden="true"></i><span class="sr-only">switch to templm</span></a>GeneWeb UNPREDICTABLE</div>
  <div class="btn-group mt-0 ml-1">
  <span>&copy; <a href="https://www.inria.fr" target="_blank" rel="noreferrer, noopener">INRIA</a> 1998-2007</span>
  <a href="https://geneweb.tuxfamily.org/wiki/GeneWeb"
  class="ml-1" target="_blank" rel="noreferrer, noopener" data-toggle="tooltip" title="GeneWeb Wiki"><i class="fab fa-wikipedia-w"></i>
  </a>
  <a href="https://github.com/geneweb/geneweb"
  class="ml-1" target="_blank" rel="noreferrer, noopener" data-toggle="tooltip" title="GeneWeb Github"><i class="fab fa-github"></i>
  </a>
  </div>
  </div>
  </div>
  </div>
  </div>
  <!-- $Id: js.txt v7.1 17/01/2026 10:39:33 $ -->
  <script src="../../hd/etc/js/jquery.min.js?version=3.7.1"></script>
  <script src="../../hd/etc/js/bootstrap.bundle.min.js?version=4.6.1"></script>
  <script>
  function initializeLazyModules() {
  $('#load_once_p_mod').one('click', () => $.getScript('../../hd/etc/js/p_mod.min.js?hash=80e91f69b222614249d582e5f3b1d0fa'));
  $('#load_once_copylink').one('click', () => $.getScript('../../hd/etc/js/copylink.js?hash=ec0c078adb1aea49a1ef6db370d59384'));
  $('#load_once_rlm_builder').one('click', () => $.getScript('../../hd/etc/js/rlm_builder.js?hash=12dfd286fcbaef423ef77090b6f3bc71'));
  }
  // Focus on found autofocus input in opening BS modal
  function setupModalAutofocus() {
  $('.modal').on('shown.bs.modal', function() {
  $(this).find('[autofocus]').focus();
  });
  }
  // Floating placeholders for all textual inputs
  function setupFloatingPlaceholders() {
  const inputs = document.querySelectorAll('input[type="text"][placeholder], input[type="number"][placeholder], input[type="search"][placeholder], textarea[placeholder]');
  inputs.forEach(input => {
  // Ignore placeholders that are only non-breaking spaces
  if (input.placeholder.trim() === '') return;
  const hadFocus = document.activeElement === input;
  const wrapper = document.createElement('div');
  wrapper.className = 'input-wrapper';
  input.parentNode.insertBefore(wrapper, input);
  wrapper.appendChild(input);
  const placeholder = document.createElement('span');
  placeholder.className = 'floating-placeholder';
  placeholder.textContent = input.placeholder;
  wrapper.appendChild(placeholder);
  input.addEventListener('focus', () => placeholder.classList.add('active'));
  input.addEventListener('blur', () => placeholder.classList.remove('active'));
  if (hadFocus || input.hasAttribute('autofocus')) {
  requestAnimationFrame(() => {
  input.focus();
  placeholder.classList.add('active');
  });
  }
  });
  }
  const initTooltips = () => {
  const tooltipElements = document.querySelectorAll('[data-toggle="tooltip"]');
  if (tooltipElements.length > 0) {
  $(tooltipElements).tooltip({
  trigger: 'hover',
  delay: { show: 200, hide: 50 },
  container: 'body',
  });
  }
  };
  function safeInitialize(fn) {
  try {
  fn();
  } catch (error) {
  console.error('Initialization error:', error);
  }
  }
  document.addEventListener('DOMContentLoaded', () => {
  if (typeof initializeLazyModules === 'function') {
  initializeLazyModules();
  }
  setupModalAutofocus();
  setupFloatingPlaceholders();
  });
  </script>
  </body>
  </html>

  $ gwd-cgi "p=jean+pierre&n=galichet"
  =========== QUERY_STRING: p=jean+pierre&n=galichet =========
  [1970-01-01T00:00:00-00:00][INFO]: No configuration file ./bases/galichet.gwf found, see ../../hd/a.gwf for example
  [1970-01-01T00:00:00-00:00][INFO]: (PID: UNPREDICTABLE) gwd?p=jean+pierre&n=galichet
    From: 
    Agent: Oz
  Content-type: text/html; charset=UTF-8
  Date: UNPREDICTABLE
  Connection: close
  
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <!-- $Id: perso.txt UNPREDICTABLE 02/03/2026 23:04:39 $ -->
  <!-- Copyright (c) 1998-2007 INRIA -->
  <title>Jean Pierre Galichet</title>
  <meta name="robots" content="none"><meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
  <link rel="icon" href="../../hd/images/favicon_gwd.png">
  <link rel="apple-touch-icon" href="../../hd/images/favicon_gwd.png">
  <!-- ../../hd/etc/css.txt -->
    <!-- $Id: css.txt v7.1 06/08/2025 00:34:15 $ -->
  <link rel="stylesheet" href="../../hd/etc/css/bootstrap.min.css?version=4.6.2">
  <link rel="stylesheet" href="../../hd/etc/css/all.min.css?hash=19eac43a9fe1d373cc7ebca299f9f7e8">
  <link rel="stylesheet" href="../../hd/etc/css/css.css?hash=ab8c273eb60a9999baaac9da388e72f7">
  <!-- ../../hd/etc/css.txt -->
  </head>
  <body>
  <!-- ../../hd/etc/hed.txt -->
  <meta name="format-detection" content="telephone=no">
  <!-- ../../hd/etc/hed.txt -->
  <a href="#content" class="sr-only sr-only-focusable ml-4">Skip to main content</a>
  <div class="container">
  <!-- ../../hd/etc/perso_utils.txt -->
  <!-- $Id: perso_utils v7.1 13/08/2023 16:55:48 $ -->
  <!-- ../../hd/etc/perso_utils.txt -->
  <!-- ../../hd/etc/menubar.txt -->
  <!-- $Id: menubar.txt v7.1 02/05/2024 19:03:42 $ -->
  <nav class="navbar navbar-light justify-content-center navbar-expand-md pt-0 px-0 mt-1 mt-md-0">
  <div class="btn-toolbar" role="toolbar">
  <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarSupportedContent" aria-controls="navbarSupportedContent" aria-expanded="false" aria-label="Toggle navigation">
  <span class="navbar-toggler-icon"></span>
  </button>
  <div class="collapse navbar-collapse" id="navbarSupportedContent">
  <ul class="nav nav-tabs">
  <li class="nav-item dropdown">
  <a id="load_once_p_mod" class="nav-link dropdown-toggle text-secondary dropdown-toggle-split" data-toggle="dropdown"
  href="#" role="button" aria-haspopup="true" aria-expanded="false" title="Modules"><span class="fas fa-address-card fa-fw mr-1" aria-hidden="true"></span><span class="sr-only">Modules</span></a>
  <div class="dropdown-menu dropdown-menu-transl-pmod">
  <div class="d-flex justify-content-around mx-1">
  <form class="form-group mx-1" name="upd_url" method="get" action="gwd">
  <div class="d-flex justify-content-between mx-2 mt-2 img-prfx" data-prfx="../../hd/images/modules">
  <div class="d-flex align-items-center flex-grow-1">
  <input type="hidden" name="b" value="galichet">
  <div class="input-group p-mod-group mr-2">
  <input type="text" pattern="^((?:([a-z][0-9]){1,15})|zz)" name="p_mod" id="p_mod" class="form-control"
  value=""
  placeholder="Select personalized modules" maxlength="30">
  <div class="input-group-append">
  <button type="submit" class="btn btn-outline-success" title="Submit"><i class="fa fa-check fa-lg"></i></button>
  <button type="button" class="btn btn-outline-danger" id="p_mod_clear" title="Delete"><i class="fa fa-xmark fa-lg mx-1"></i></button>
  </div>
  </div>
  <input type="hidden" name="p" value="jean pierre">
  <input type="hidden" name="n" value="galichet">
  </div>
  <div class="ml-auto">
  <button type="button" class="btn btn-outline-danger ml-2" id="p_mod_rm" title="Remove the last added module" value=""><i class="fa fa-backward"></i></button>
  <button type="submit" class="btn btn-outline-secondary ml-2 mr-1" id='zz'
  title="Default template" data-toggle="popover" data-trigger="hover"
  data-html="true" data-content="<img class='w-100' src='../../hd/images/modules/zz_1.jpg'>"><i class="fa fa-arrow-rotate-left"></i></button>
  <a role="button" class="btn btn-outline-primary ml-2"
  href="gwd?b=galichet&p=jean+pierre&n=galichet&wide=on"
  title="Full width display"><i class="fa fa-desktop fa-fw" aria-hidden="true"></i><span class="sr-only">Full width display</span></a>
  </div>
  </div>
  <div class="mx-2 mt-2">Select each module by clicking on the corresponding button (max 15).</div>
  <div class="alert alert-warning alert-dismissible fade show mt-1 mb-2 d-none" role="alert">
  <div class="d-none alert-opt">Invalid option <strong id="alert-option"></strong> for module <strong id="alert-module"></strong>. Please enter a valid option number.</div>
  <div class="d-none alert-mod">Invalid module <strong id="alert-module-2"></strong>. Please enter a valid module initial.</div>
  </div>
  <div id="p_mod_table"></div>
  </form>
  <div class="form-group d-none d-md-block mx-1">
  <img src="../../hd/images/modules/menubar_1.jpg" alt="menubar for p_mod_builder" aria-hidden="true">
  <div id="p_mod_builder"></div>
  </div>
  </div>
  </div>
  </li>
  <li class="nav-item">
  <a class="nav-link " id="add_par"
  href="gwd?b=galichet&m=ADD_PAR&ip=0" title="Add parents of Jean Pierre Galichet (L)" accesskey="L"><sup><span class="fa fa-user male" aria-hidden="true"></span></sup><sub><span class="fa fa-plus male" aria-hidden="true"></span></sub><sup><span class="fa fa-user female" aria-hidden="true"></span></sup><span class="sr-only">Add parents of Jean Pierre Galichet (L)</span></a>
  </li>
  <li class="nav-item">
  <a class="nav-link active bg-light" id="self"
  href="gwd?b=galichet&p=jean+pierre&n=galichet" title="Jean Pierre Galichet"><span class="fa fa-user-large fa-fw male" aria-hidden="true"></span>
  <span class="sr-only">Jean Pierre Galichet</span></a>
  </li>
  <li class="nav-item">
  <a class="nav-link " id="mod_ind" href="gwd?b=galichet&m=MOD_IND&i=0" title="Update individual Jean Pierre Galichet (P)" accesskey="P"><span class="fa fa-user-pen fa-fw male" aria-hidden="true"></span>
  <span class="sr-only">Update individual (P)</span></a>
  </li>
  <li class="nav-item dropdown">
  <a id="load_once_copylink" class="nav-link dropdown-toggle text-secondary "
  data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="true" title="Tools individual"><span class="fa fa-user-gear
  text-muted" aria-hidden="true"></span><span class="sr-only">Tools individual</span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" href="gwd?b=galichet&m=H&v=visibility" title="Help for visibility of individuals" target="_blank">
  <i class="fa fa-person fa-fw text-muted mr-2"></i>Visibility (if titles)</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item"
  href="gwd?b=galichet&iz=0&p=jean+pierre&n=galichet"
  title="Browse using Jean Pierre Galichet as Sosa reference" accesskey="S"><i class="far fa-circle-dot fa-fw male mr-1"></i> Update Sosa 1: Jean Pierre Galichet</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&cgl=on" target="_blank"><i class="fa fa-link-slash fa-fw mr-2" title="Without GeneWeb links"></i>Without GeneWeb links</a><div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=CHG_EVT_IND_ORD&i=0" title="Change order of individual events"><span class="fa fa-sort fa-fw mr-2"></span>Reverse events</a>
  <a class="dropdown-item" href="gwd?b=galichet&m=SND_IMAGE&i=0">
  <i class="far fa-file-image fa-fw mr-2"></i>Add picture</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=MRG&i=0" title="Merge Jean Pierre.0 Galichet with…"><span class="fa fa-compress fa-fw text-danger mr-2"></span>Merge individuals</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=DEL_IND&i=0" title="Delete Jean Pierre Galichet"><span class="fa fa-trash-can fa-fw text-danger mr-2"></span>Delete individual</a>
  <div class="dropdown-divider"></div>
  <div class="btn-group pr-4" role="group"><button class="dropdown-item permalink-copy mr-1 pr-0" type="button"
  title="Copy the exterior link to the clipboard
  " 
  data-bname=""
  data-query="b=galichet&p=jean+pierre&n=galichet&lang=en"><i class="fas fa-link fa-fw mr-2"></i>Copy the permalink exterior</button>
  </div><div class="dropdown-divider"></div>
  <button class="dropdown-item simple-copy" type="button" title="Copy the wiki link to the clipboard"
  data-wikilink="[[Jean Pierre/Galichet]]"><i class="far fa-clipboard fa-fw mr-2"></i>[[Jean Pierre/Galichet]]</button>
  <button class="dropdown-item full-copy" type="button" title="Copy the wikitext link to the clipboard"
  data-wikilink="[[Jean Pierre/Galichet/0/Jean Pierre Galichet]]"><i class="far fa-clipboard fa-fw mr-2"></i>[[Jean Pierre/Galichet/0/Jean Pierre Galichet]]</button>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" role="button"
  href="https://www.geneanet.org/fonds/individus/?sourcename=&size=50&sexe=&nom=Galichet&ignore_each_patronyme=&with_variantes_nom=1&prenom=Jean%20Pierre&prenom_operateur=or&ignore_each_prenom=&profession=&ignore_each_profession=&nom_conjoint=&ignore_each_patronyme_conjoint=&prenom_conjoint=&prenom_conjoint_operateur=or&ignore_each_prenom_conjoint=&place__0__=" target="_blank" rel="noreferrer, noopener">
  <i class="fa fa-magnifying-glass fa-fw mr-2"></i>Search Jean Pierre Galichet on Geneanet
  </a>
  </div>
  </li>
  <li class="nav-item">
  <a class="nav-link " id="mod_fam_1" href="gwd?b=galichet&m=MOD_FAM&i=0&ip=0"
  title="Update family with Marie Elisabeth Loche (F)"
  accesskey="F"><span class="fa fa-user-pen male" aria-hidden="true"></span><span class="fa fa-user female" aria-hidden="true"></span><span class="sr-only"> Update family with Marie Elisabeth Loche (F)</span></a>
  </li>
  <li class="nav-item dropdown">
  <a class="nav-link dropdown-toggle text-secondary
  "
  data-toggle="dropdown" role="button" href="#" aria-haspopup="true" aria-expanded="false"
  title="Tools family"><span class="fa fa-user-plus" aria-hidden="true"></span><span class="fa fa-user-gear" aria-hidden="true"></span><span class="sr-only">Tools family</span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" id="add_fam" href="gwd?b=galichet&m=ADD_FAM&ip=0" title="Add family (A)" accesskey="A"><span class="fa fa-plus fa-fw mr-2"></span>Add family</a>
  <div class="dropdown-divider"></div>
  <span class="dropdown-header">Marriage with Marie Elisabeth Loche</span>
  <a class="dropdown-item" href="gwd?b=galichet&m=CHG_EVT_FAM_ORD&i=0&ip=0" title="Change order of family events"><span class="fa fa-sort fa-fw mr-2"></span>Reverse events</a>
  <a class="dropdown-item" href="gwd?b=galichet&m=DEL_FAM&i=0&ip=0"><span class="fa fa-trash fa-fw text-danger mr-2"></span>Delete family</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=CHG_CHN&ip=0"><span class="fa fa-child fa-fw mr-1"></span> Update surname of children</a>
  </div>
  </li>
  <li class="nav-item dropdown">
  <a class="nav-link dropdown-toggle text-secondary" data-toggle="dropdown" role="button" href="#" aria-haspopup="true" aria-expanded="false"><span class="fa fa-sitemap fa-fw" title="Descendants"></span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D"><span class="fa fa-gear fa-fw mr-2"></span>Descendants</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=V&v=3" title="Tree (Y)" accesskey="Y"><span class="fa fa-sitemap fa-fw mr-2"></span>Descendants tree</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=TV&v=3" title="Tree (Y)" accesskey="Y"><span class="fa fa-sitemap fa-fw mr-2"></span>Compact descendants tree</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=D&v=2"><span class="fa fa-code-branch fa-rotate-90 fa-flip-vertical fa-fw mr-2"></span>Descendant tree view</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=I&v=2&num=on&birth=on&birth_place=on&marr=on&marr_date=on&marr_place=on&child=on&death=on&death_place=on&age=on&occu=on&gen=1&ns=1&hl=1"><span class="fa fa-table fa-fw mr-2"></span>Table descendants</a>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=L&v=3&siblings=on&alias=on&parents=on&rel=on&witn=on&notes=on&src=on&hide=on"><span class="fa fa-newspaper fa-fw mr-2"></span>Full display</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=A&num=on&v=2"><span class="fa fa-code-branch fa-flip-vertical fa-fw mr-2"></span>D’Aboville</a>
  </div>
  </li>
  <li class="nav-item dropdown">
  <a id="load_once_rlm_builder" class="nav-link dropdown-toggle text-secondary" data-toggle="dropdown" role="button" href="#" aria-haspopup="true" aria-expanded="false" title="Relationship">
  <span class="fa fa-user-group"></span></a>
  <div class="dropdown-menu dropdown-menu-transl">
  <a class="dropdown-item" href="gwd?b=galichet&p=jean+pierre&n=galichet&m=R" title="Relationship computing (R)" accesskey="R">
  <span class="fa fa-gear fa-fw mr-2"></span>Relationship computing</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=C&p=jean+pierre&n=galichet&v=5"
  title="Relationship link with a parent"><span class="fa fa-elevator fa-fw mr-2"></span>Relationship</a>
  <div class="dropdown-divider"></div>
  <a class="dropdown-item" href="gwd?b=galichet&m=F&p=jean+pierre&n=galichet" title="Family">
  <span class="fa fa-users fa-fw mr-2"></span>Family</a>
  <!-- Relationship graph builder -->
  <div class="dropdown-divider"></div>
  <span class="dropdown-header pt-0" id="RLM" data-eb="galichet" data-i="0" data-p="jean+pierre" data-n="galichet" data-self="Jean Pierre Galichet †/1849">Multi relations graph</span>
  <div class="form-group px-3 mb-2">
  <label for="description" class="sr-only">Description</label>
  <input type="text" id="description" class="form-control" placeholder="Description" title="&t=…">
  </div>
  <div class="btn-group d-flex pr-3" role="group">
  <button class="dropdown-item flex-grow-1" title="Add Jean Pierre Galichet to the data" id="saveButton">
  <i class="fa fa-plus fa-fw mr-2" aria-hidden="true"></i>
  <span>Add to the data</span>
  </button>
  <button class="dropdown-item col-1 pl-1 d-none" title="Clear the data" id="clearGraphButton">
  <i class="fa fa-trash-can fa-fw text-danger" aria-hidden="true"></i>
  <span class="sr-only">Clear the data</span>
  </button>
  </div>
  <div class="d-none" id="graphButtons">
  <div class="btn-group d-flex pr-3" role="group" aria-label="Relationship graph actions">
  <a class="dropdown-item flex-grow-1 align-items-center" id="generateGraphButton">
  <i class="fa fa-code-fork fa-rotate-180 fa-fw mr-2" aria-hidden="true"></i>
  <span>Show the graph</span></a>
  <a class="dropdown-item col-1 pl-1" title="Edit the data" id="editGraphButton">
  <i class="far fa-pen-to-square fa-fw" aria-hidden="true"></i>
  <span class="sr-only">Edit the data</span></a>
  </div>
  </div>
  </div>
  </li>
  <li class="nav-item">
  <a class="nav-link text-secondary" href="gwd?b=galichet&m=SND_IMAGE_C&i=0" title="Add/delete pictures (I)" accesskey="I"><span class="fa fa-image fa-fw" aria-hidden="true"></span><span class="sr-only">icon button</span></a>
  </li>
  </ul>
  </div>
  </div>
  </nav>
  <!-- ../../hd/etc/home.txt -->
  <!-- $Id: home.txt v7.1 07/09/2023 00:49:55 $ -->
  <div class="d-flex flex-column fix_top fix_left home-xs">
  <a tabindex="1" role="button" class="btn btn-sm btn-link p-0 border-0" href="gwd?b=galichet&" title="Home"><i class="fa fa-house fa-fw fa-xs" aria-hidden="true"></i><i class="sr-only">Home</i></a>
  <a tabindex="3" role="button" class="btn btn-sm btn-link p-0 border-0" data-toggle="modal" data-target="#searchmodal"
  accesskey="S" title="Search"><i class="fa fa-magnifying-glass fa-fw fa-xs" aria-hidden="true"></i><span class="sr-only">Search</span></a>
  </div>
  <div class="modal" id="searchmodal" role="dialog" aria-labelledby="searchpopup" aria-hidden="true">
  <div class="modal-dialog modal-lg" role="document">
  <div class="modal-content">
  <div class="modal-body" id="ModalSearch">
  <form id="collapse_search" method="get" action="gwd?b=galichet&">
  <input type="hidden" name="b" value="galichet">
  <input type="hidden" name="lang" value="en">
  <input type="hidden" name="m" value="S">
  <div class="d-flex flex-column flex-md-row justify-content-center">
  <h3 class="rounded modal-title my-2 ml-3 ml-md-0 text-md-center w-md-50 align-self-md-center" id="searchpopup">Search individual</h3>
  <div class="col-12 col-md-8 mt-2 mt-md-0">
  <label class="sr-only" for="fullname">Search person name</label>
  <input type="search" id="fullname" class="form-control form-control-lg no-clear-button"
  name="pn" placeholder="Search individual, surname, public name, alias or key"
  autofocus>
  <label class="sr-only" for="p">Search firstname</label>
  <input type="search" id="p" class="form-control form-control-lg no-clear-button mt-2"
  name="p" placeholder="First name·s">
  <label class="sr-only" for="n">Search surname</label>
  <input type="search" id="n" class="form-control form-control-lg no-clear-button mt-2"
  name="n" placeholder="Surname">
  <div class="d-flex mt-2">
  <div class="ml-2">First names</div>
  <div class="custom-control custom-checkbox ml-2 mr-3 mr-md-0 mb-md-1" data-toggle="tooltip" data-placement="bottom" title="Search all permutations (2 to 4 first names)">
  <input class="custom-control-input" type="checkbox" name="p_order" id="p_order" value="on">
  <label class="custom-control-label d-flex align-items-center" for="p_order">permutated</label>
  </div>
  <div class="custom-control custom-checkbox ml-2" data-toggle="tooltip" data-placement="bottom" title="Partial matches, phonetic variants (quite long to load!)">
  <input class="custom-control-input" type="checkbox" name="p_exact" id="p_exact" value="off">
  <label class="custom-control-label d-flex align-items-center" for="p_exact">partials</label>
  </div>
  </div>
  </div>
  <button class="btn btn-outline-primary mx-3 mx-md-0 mt-4 my-2 mt-md-0" type="submit" title="Search"><i class="fa fa-magnifying-glass fa-lg mt-2 mt-md-0 mx-4 mx-md-2"></i> Search</button>
  </div>
  </form>
  </div>
  </div>
  </div>
  </div>
  <!-- ../../hd/etc/home.txt -->
  <!-- ../../hd/etc/menubar.txt -->
  <div id="content" tabindex="-1">
  <div class="rox col-12">
  <!-- ../../hd/etc/modules/heading.txt -->
  <!-- heading.txt 04/03/2026 02:32:58 -->
  <h1 class="text-xs-center text-md-left">
  <a href="gwd?b=galichet&m=P&v=jean+pierre#i0">Jean Pierre</a>  <a href="gwd?b=galichet&m=N&v=galichet#i0">Galichet</a><span class="font-weight-light small"> <bdo dir=ltr>†/1849</bdo></span>
  </h1>
  <!-- ../../hd/etc/modules/heading.txt -->
  </div>
  <div class="row col-12">
  <div class="col-lg-8">
  <!-- ../../hd/etc/modules/individu.txt -->
  <!-- $Id: modules/individu.txt v7.1 04/03/2026 02:42:09 $ -->
  <div class="d-flex flex-column flex-md-row align-items-center align-items-md-start">
  <div class="flex-grow-1">
  <div class="text-center mb-2 mx-2">
  </div>
  <ul class="fa-ul pl-0 mb-0">
  <li class="mt-1" title="Death">
  <span class="fa-li"><i class="fas fa-skull-crossbones"></i></span>Died before&nbsp;1849</li>
  <li class="mt-1" title="Occupation">
  <span class="fa-li"><i class="fas fa-user-tie"></i></span>Marchand de bois</li>
  </ul>
  </div>
  </div>
  <!-- ../../hd/etc/modules/individu.txt -->
  <!-- ../../hd/etc/modules/unions.txt -->
  <!-- $Id: modules/unions.txt v7.1 17/11/2023 10:30:00 $ -->
  <h2 class="mt-2 w-100">
  Marriage and children<span class="ml-2"><a href="gwd?b=galichet&p=jean+pierre&n=galichet&m=D&t=T&v=1"><img class="mx-1 mb-1" src="../../hd/images/gui_create.png" height="18" alt="tree desc."
  title="Descendants tree up to the children (with spouse)"></a>
  </span>
  </h2>
  <ul class="pl-4 py-0 fa-ul ml-0 mb-0">
  <li><span class="fa-li"><a href="gwd?b=galichet&m=MOD_FAM&i=0&ip=0"><i class="fa fa-wrench fa-sm text-success" data-html="true" data-toggle="tooltip"
  title="<div class='text-left'>Update family Galichet/Loche (wizard)<br>
  <b>Him </b><br>
  <b>Her </b><br>
  </div>"></i></a>
  </span><span  data-toggle="tooltip" data-html="true"
  title='<div class="text-wrap text-left">
  <div><span class="font-weight-bold">
  Notes of  marriage:</span> <p>
  Rajout de notes avec retour à la ligne avec des tag html
  <br>1880 - Habitent Yzernay naissance d&#39;une fille
  <br>1886 - Habitent la Verdelière (6 enfants) - Recensement Moulins: - Vue 23
  <br>1891 - N&#39;habitent plus Moulins, recensés au Temple (79) * Vue 7
  </p>.</div></div>'
  >Married to</span> <a  href="gwd?b=galichet&p=marie+elisabeth&n=loche" class="female-underline">Marie Elisabeth Loche</a><a href="gwd?b=galichet&m=MOD_IND&i=1" title="Update Marie Elisabeth Loche " class="text-nowrap font-italic"> <bdo dir=ltr>†/1849</bdo></a> (<p>
  Rajout de notes avec retour à la ligne avec des tag html
  <br>1880 - Habitent Yzernay naissance d&#39;une fille
  <br>1886 - Habitent la Verdelière (6 enfants) - Recensement Moulins: - Vue 23
  <br>1891 - N&#39;habitent plus Moulins, recensés au Temple (79) * Vue 7
  </p>), with five children:
  <ul>
  <li style="vertical-align: middle;list-style-type: circle;"><a href="gwd?b=galichet&m=MOD_IND&i=2" title="Update Jean Charles Galichet"><i class="fa fa-mars male fa-xs mr-1"></i></a><a  href="gwd?b=galichet&p=jean+charles&n=galichet" title="Display Jean Charles Galichet">Jean Charles</a><a href="gwd?b=galichet&m=MOD_IND&i=2"><span class="text-nowrap font-italic"
  title="Update Jean Charles Galichet"> <bdo dir=ltr>1813</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: circle;"><a href="gwd?b=galichet&m=MOD_IND&i=3" title="Update Pierre Galichet"><i class="fa fa-mars male fa-xs mr-1"></i></a><a  href="gwd?b=galichet&p=pierre&n=galichet" title="Display Pierre Galichet">Pierre</a><a href="gwd?b=galichet&m=MOD_IND&i=3"><span class="text-nowrap font-italic"
  title="Update Pierre Galichet (21 years)"> <bdo dir=ltr>1814–1835</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: square;"><a href="gwd?b=galichet&m=MOD_IND&i=4" title="Update Paul Galichet"><i class="fa fa-mars male fa-xs mr-1"></i></a><a  href="gwd?b=galichet&p=paul&n=galichet" title="Display Paul Galichet">Paul</a><a href="gwd?b=galichet&m=MOD_IND&i=4"><span class="text-nowrap font-italic"
  title="Update Paul Galichet (70 years)"> <bdo dir=ltr>1816–1886</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: disc;"><a href="gwd?b=galichet&m=MOD_IND&i=5" title="Update lolo Galichet"><i class="fa fa-mars male fa-xs mr-1"></i></a><a  href="gwd?b=galichet&p=lolo&n=galichet" title="Display lolo Galichet">lolo</a><a href="gwd?b=galichet&m=MOD_IND&i=5"><span class="text-nowrap font-italic"
  title="Update lolo Galichet"> <bdo dir=ltr>1818</bdo></span></a></li>
  </ul>
  <ul>
  <li style="vertical-align: middle;list-style-type: circle;"><a href="gwd?b=galichet&m=MOD_IND&i=6" title="Update Thérèse Eugénie Galichet"><i class="fa fa-venus female fa-xs mr-1"></i></a><a  href="gwd?b=galichet&p=therese+eugenie&n=galichet" title="Display Thérèse Eugénie Galichet">Thérèse Eugénie</a><a href="gwd?b=galichet&m=MOD_IND&i=6"><span class="text-nowrap font-italic"
  title="Update Thérèse Eugénie Galichet"> <bdo dir=ltr>1830</bdo></span></a></li>
  </ul>
  </li></ul>
  <!-- ../../hd/etc/modules/unions.txt -->
  <!-- ../../hd/etc/modules/chronologie.txt -->
  <!-- $Id: modules/chronologie.txt v7.1 02/02/2026 20:06:28 $ -->
  <!-- ../../hd/etc/modules/chronologie.txt -->
  <!-- ../../hd/etc/modules/notes.txt -->
  <!-- $Id: modules/notes.txt v7.1 04/03/2026 02:42:31 $ -->
  <div id="mod_notes">
  <h2 class="mt-2">Individual notes</h2>
  <div class="ml-4 ind-notes">
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
  <ul class="pl-0">
  </ul>
  </div>
  </div><!-- ../../hd/etc/modules/notes.txt -->
  <!-- ../../hd/etc/modules/sources.txt -->
  <!-- $Id: modules/sources.txt v7.1 04/03/2026 02:42:03 $ -->
  <div id="mod_sources">
  <h2 class="mt-2">Source</h2>
  <ul class="pl-4 pY-0">
  <li>Individual, family:
  déjà décédés au mariage de sa fille Thérèse Eugénie.
  </li>
  </ul>
  </div>
  <!-- ../../hd/etc/modules/sources.txt -->
  </div>
  <div class="col-lg-4">
  <!-- ../../hd/etc/modules/arbre_3gen_photo.txt -->
  <!-- $Id: modules/arbre_3gen_photo.txt v7.1 22/04/2024 07:27:12 $ -->
  <!-- ../../hd/etc/modules/arbre_3gen_photo.txt -->
  <!-- ../../hd/etc/modules/fratrie.txt -->
  <!-- $Id: modules/fratrie.txt v7.1 13/08/2023 16:47:40 $ -->
   <!-- ../../hd/etc/modules/fratrie.txt -->
  <!-- ../../hd/etc/modules/relations.txt -->
  <!-- $Id: modules/relations.txt v7.1 17/11/2023 10:30:00 $ -->
  <!-- ../../hd/etc/modules/relations.txt -->
  </div>
  </div>
  <!-- ../../hd/etc/trl.txt -->
  <!-- ../../hd/etc/trl.txt -->
  <!-- ../../hd/etc/copyr.txt -->
  <!-- $Id: copyr.txt UNPREDICTABLE 18/12/2023 22:03:44 $ -->
  <div class="d-flex flex-row justify-content-center justify-content-lg-end my-2" id="copyr">
  <div class="d-flex flex-wrap justify-content-md-end align-items-center">
  <!-- legal notices -->
  <!-- Language selector and connections info -->
  <div class="d-flex flex-row align-items-lg-end mt-0 ml-3 border-0">
  <div class="btn-group dropup" data-toggle="tooltip" data-placement="left"
  title="English – Select language">
  <button class="btn btn-link dropdown-toggle" type="button" id="dropdownMenu1"
  data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
  <span class="sr-only">lang</span>
  <span class="text-uppercase">en</span>
  <span class="sr-only">, select language</span>
  </button>
  <div class="dropdown-menu scrollable-lang" aria-labelledby="dropdownMenu1">
  <a class="dropdown-item" id="lang_af" href="gwd?b=galichet&lang=af&p=jean+pierre&n=galichet"><code>af&nbsp;&nbsp;&nbsp; </code>Afrikaans</a>
  <a class="dropdown-item" id="lang_ar" href="gwd?b=galichet&lang=ar&p=jean+pierre&n=galichet"><code>ar&nbsp;&nbsp;&nbsp; </code>Arabic</a>
  <a class="dropdown-item" id="lang_bg" href="gwd?b=galichet&lang=bg&p=jean+pierre&n=galichet"><code>bg&nbsp;&nbsp;&nbsp; </code>Bulgarian</a>
  <a class="dropdown-item" id="lang_br" href="gwd?b=galichet&lang=br&p=jean+pierre&n=galichet"><code>br&nbsp;&nbsp;&nbsp; </code>Breton</a>
  <a class="dropdown-item" id="lang_ca" href="gwd?b=galichet&lang=ca&p=jean+pierre&n=galichet"><code>ca&nbsp;&nbsp;&nbsp; </code>Catalan</a>
  <a class="dropdown-item" id="lang_co" href="gwd?b=galichet&lang=co&p=jean+pierre&n=galichet"><code>co&nbsp;&nbsp;&nbsp; </code>Corsican</a>
  <a class="dropdown-item" id="lang_cs" href="gwd?b=galichet&lang=cs&p=jean+pierre&n=galichet"><code>cs&nbsp;&nbsp;&nbsp; </code>Czech</a>
  <a class="dropdown-item" id="lang_da" href="gwd?b=galichet&lang=da&p=jean+pierre&n=galichet"><code>da&nbsp;&nbsp;&nbsp; </code>Danish</a>
  <a class="dropdown-item" id="lang_de" href="gwd?b=galichet&lang=de&p=jean+pierre&n=galichet"><code>de&nbsp;&nbsp;&nbsp; </code>German</a>
  <a class="dropdown-item" id="lang_eo" href="gwd?b=galichet&lang=eo&p=jean+pierre&n=galichet"><code>eo&nbsp;&nbsp;&nbsp; </code>Esperanto</a>
  <a class="dropdown-item" id="lang_es" href="gwd?b=galichet&lang=es&p=jean+pierre&n=galichet"><code>es&nbsp;&nbsp;&nbsp; </code>Spanish</a>
  <a class="dropdown-item" id="lang_et" href="gwd?b=galichet&lang=et&p=jean+pierre&n=galichet"><code>et&nbsp;&nbsp;&nbsp; </code>Estonian</a>
  <a class="dropdown-item" id="lang_fi" href="gwd?b=galichet&lang=fi&p=jean+pierre&n=galichet"><code>fi&nbsp;&nbsp;&nbsp; </code>Finnish</a>
  <a class="dropdown-item" id="lang_fr" href="gwd?b=galichet&lang=fr&p=jean+pierre&n=galichet"><code>fr&nbsp;&nbsp;&nbsp; </code>French</a>
  <a class="dropdown-item" id="lang_he" href="gwd?b=galichet&lang=he&p=jean+pierre&n=galichet"><code>he&nbsp;&nbsp;&nbsp; </code>Hebrew</a>
  <a class="dropdown-item" id="lang_is" href="gwd?b=galichet&lang=is&p=jean+pierre&n=galichet"><code>is&nbsp;&nbsp;&nbsp; </code>Icelandic</a>
  <a class="dropdown-item" id="lang_it" href="gwd?b=galichet&lang=it&p=jean+pierre&n=galichet"><code>it&nbsp;&nbsp;&nbsp; </code>Italian</a>
  <a class="dropdown-item" id="lang_lt" href="gwd?b=galichet&lang=lt&p=jean+pierre&n=galichet"><code>lt&nbsp;&nbsp;&nbsp; </code>Lithuanian</a>
  <a class="dropdown-item" id="lang_lv" href="gwd?b=galichet&lang=lv&p=jean+pierre&n=galichet"><code>lv&nbsp;&nbsp;&nbsp; </code>Latvian</a>
  <a class="dropdown-item" id="lang_nl" href="gwd?b=galichet&lang=nl&p=jean+pierre&n=galichet"><code>nl&nbsp;&nbsp;&nbsp; </code>Dutch</a>
  <a class="dropdown-item" id="lang_no" href="gwd?b=galichet&lang=no&p=jean+pierre&n=galichet"><code>no&nbsp;&nbsp;&nbsp; </code>Norwegian</a>
  <a class="dropdown-item" id="lang_oc" href="gwd?b=galichet&lang=oc&p=jean+pierre&n=galichet"><code>oc&nbsp;&nbsp;&nbsp; </code>Occitan</a>
  <a class="dropdown-item" id="lang_pl" href="gwd?b=galichet&lang=pl&p=jean+pierre&n=galichet"><code>pl&nbsp;&nbsp;&nbsp; </code>Polish</a>
  <a class="dropdown-item" id="lang_pt" href="gwd?b=galichet&lang=pt&p=jean+pierre&n=galichet"><code>pt&nbsp;&nbsp;&nbsp; </code>Portuguese</a>
  <a class="dropdown-item" id="lang_pt-br" href="gwd?b=galichet&lang=pt-br&p=jean+pierre&n=galichet"><code>pt-br </code>Brazilian-Portuguese</a>
  <a class="dropdown-item" id="lang_ro" href="gwd?b=galichet&lang=ro&p=jean+pierre&n=galichet"><code>ro&nbsp;&nbsp;&nbsp; </code>Romanian</a>
  <a class="dropdown-item" id="lang_ru" href="gwd?b=galichet&lang=ru&p=jean+pierre&n=galichet"><code>ru&nbsp;&nbsp;&nbsp; </code>Russian</a>
  <a class="dropdown-item" id="lang_sk" href="gwd?b=galichet&lang=sk&p=jean+pierre&n=galichet"><code>sk&nbsp;&nbsp;&nbsp; </code>Slovak</a>
  <a class="dropdown-item" id="lang_sl" href="gwd?b=galichet&lang=sl&p=jean+pierre&n=galichet"><code>sl&nbsp;&nbsp;&nbsp; </code>Slovenian</a>
  <a class="dropdown-item" id="lang_sv" href="gwd?b=galichet&lang=sv&p=jean+pierre&n=galichet"><code>sv&nbsp;&nbsp;&nbsp; </code>Swedish</a>
  <a class="dropdown-item" id="lang_tr" href="gwd?b=galichet&lang=tr&p=jean+pierre&n=galichet"><code>tr&nbsp;&nbsp;&nbsp; </code>Turkish</a>
  <a class="dropdown-item" id="lang_zh" href="gwd?b=galichet&lang=zh&p=jean+pierre&n=galichet"><code>zh&nbsp;&nbsp;&nbsp; </code>Chinese</a>
  </div>
  </div>
  <!-- Connections info -->
  <div class="d-flex flex-column justify-items-center align-items-center small ml-1 ml-md-3">
  <a href="gwd?b=galichet&m=CONN_WIZ">1 wizard
  </a><span>1 connection
  </span>
  </div>
  </div>
  <!-- Footer links and copyright -->
  <div class="d-flex flex-column justify-content-md-end align-self-center ml-1 ml-md-3 ml-lg-4">
  <div class="ml-auto mb-0">
  <a role="button" class="mr-2"
  href="gwd?b=galichet&templ=templm&p=jean+pierre&n=galichet"
  data-toggle="tooltip"
  title="templm"><i class="fab fa-markdown" aria-hidden="true"></i><span class="sr-only">switch to templm</span></a>GeneWeb UNPREDICTABLE</div>
  <div class="btn-group mt-0 ml-1">
  <span>&copy; <a href="https://www.inria.fr" target="_blank" rel="noreferrer, noopener">INRIA</a> 1998-2007</span>
  <a href="https://geneweb.tuxfamily.org/wiki/GeneWeb"
  class="ml-1" target="_blank" rel="noreferrer, noopener" data-toggle="tooltip" title="GeneWeb Wiki"><i class="fab fa-wikipedia-w"></i>
  </a>
  <a href="https://github.com/geneweb/geneweb"
  class="ml-1" target="_blank" rel="noreferrer, noopener" data-toggle="tooltip" title="GeneWeb Github"><i class="fab fa-github"></i>
  </a>
  </div>
  </div>
  </div>
  </div>
  <!-- ../../hd/etc/copyr.txt -->
  </div>
  </div>
  <!-- ../../hd/etc/js.txt -->
  <!-- $Id: js.txt v7.1 17/01/2026 10:39:33 $ -->
  <script src="../../hd/etc/js/jquery.min.js?version=3.7.1"></script>
  <script src="../../hd/etc/js/bootstrap.bundle.min.js?version=4.6.1"></script>
  <script>
  function initializeLazyModules() {
  $('#load_once_p_mod').one('click', () => $.getScript('../../hd/etc/js/p_mod.min.js?hash=80e91f69b222614249d582e5f3b1d0fa'));
  $('#load_once_copylink').one('click', () => $.getScript('../../hd/etc/js/copylink.js?hash=ec0c078adb1aea49a1ef6db370d59384'));
  $('#load_once_rlm_builder').one('click', () => $.getScript('../../hd/etc/js/rlm_builder.js?hash=12dfd286fcbaef423ef77090b6f3bc71'));
  }
  // Focus on found autofocus input in opening BS modal
  function setupModalAutofocus() {
  $('.modal').on('shown.bs.modal', function() {
  $(this).find('[autofocus]').focus();
  });
  }
  // Floating placeholders for all textual inputs
  function setupFloatingPlaceholders() {
  const inputs = document.querySelectorAll('input[type="text"][placeholder], input[type="number"][placeholder], input[type="search"][placeholder], textarea[placeholder]');
  inputs.forEach(input => {
  // Ignore placeholders that are only non-breaking spaces
  if (input.placeholder.trim() === '') return;
  const hadFocus = document.activeElement === input;
  const wrapper = document.createElement('div');
  wrapper.className = 'input-wrapper';
  input.parentNode.insertBefore(wrapper, input);
  wrapper.appendChild(input);
  const placeholder = document.createElement('span');
  placeholder.className = 'floating-placeholder';
  placeholder.textContent = input.placeholder;
  wrapper.appendChild(placeholder);
  input.addEventListener('focus', () => placeholder.classList.add('active'));
  input.addEventListener('blur', () => placeholder.classList.remove('active'));
  if (hadFocus || input.hasAttribute('autofocus')) {
  requestAnimationFrame(() => {
  input.focus();
  placeholder.classList.add('active');
  });
  }
  });
  }
  const initTooltips = () => {
  const tooltipElements = document.querySelectorAll('[data-toggle="tooltip"]');
  if (tooltipElements.length > 0) {
  $(tooltipElements).tooltip({
  trigger: 'hover',
  delay: { show: 200, hide: 50 },
  container: 'body',
  });
  }
  };
  function safeInitialize(fn) {
  try {
  fn();
  } catch (error) {
  console.error('Initialization error:', error);
  }
  }
  document.addEventListener('DOMContentLoaded', () => {
  if (typeof initializeLazyModules === 'function') {
  initializeLazyModules();
  }
  setupModalAutofocus();
  setupFloatingPlaceholders();
  });
  </script>
  <!-- ../../hd/etc/js.txt -->
  <script>
  $(function () {
  $('[data-toggle="tooltip"]').tooltip()
  })
  </script>
  </body>
  </html>
