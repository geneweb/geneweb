# gwd

## Documentation

The main web-server for geneweb. It depends on `wserver`, a library defined in `bin/`.

```
Usage: gwd [options] where options are:
  -a <ADDRESS>           Select a specific address (default = any address of this computer).
  -add_lexicon <FILE>    Add file as lexicon.
  -allowed_tags <FILE>   HTML tags which are allowed to be displayed. One tag per line in file.
  -auth <FILE>           Authorization file to restrict access. The file must hold lines of the form "user:password".
  -bd <DIR>              Directory where the databases are installed.
  -blang                 Select the user browser language if any.
  -cache_langs           Lexicon languages to be cached.
  -cgi                   Force CGI mode.
  -conn_tmout <SEC>      Connection timeout (default 120s; 0 means no limit).
  -daemon                Unix daemon mode.
  -debug                 Enable debug mode
  -digest                Use Digest authorization scheme (more secure on passwords)
  -friend <PASSWD>       Set a friend password.
  -hd <DIR>              Directory where the directory lang is installed.
  -images_dir <DIR>      Same than previous but directory name relative to current.
  -images_url <URL>      URL for GeneWeb images (default: gwd send them).
  -lang <LANG>           Set a default language (default: fr).
  -log <FILE>            Log trace to this file. Use "-" or "<stdout>" to redirect output to stdout or "<stderr>" to output log to stderr.
  -log_level <N>         Send messages with severity <= <N> to syslog (default: 7).
  -login_tmout <SEC>     Login timeout for entries with passwords in CGI mode (default 1800s).
  -max_clients <NUM>     Max number of clients treated at the same time (default: no limit) (not cgi).
  -min_disp_req          Minimum number of requests in robot trace (default: 6).
  -no_host_address       Force no reverse host by address.
  -nolock                Do not lock files before writing.
  -only <ADDRESS>        Only inet address accepted.
  -p <NUMBER>            Select a port number (default = 2317).
  -plugin <PLUGIN>.cmxs  load a safe plugin. Combine with -force to enable for every base. Combine with -unsafe to allow unverified plugins. e.g. "-plugin -unsafe -force".
  -plugins <DIR>         load all plugins in <DIR>. Combine with -force to enable for every base. Combine with -unsafe to allow unverified plugins. e.g. "-plugins -unsafe -force".
  -redirect <ADDR>       Send a message to say that this service has been redirected to <ADDR>.
  -robot_xcl <CNT>,<SEC> Exclude connections when more than <CNT> requests in <SEC> seconds.
  -setup_link            Display a link to local gwsetup in bottom of pages.
  -trace_failed_passwd   Print the failed passwords in log (except if option -digest is set).
  -wd <DIR>              Directory for socket communication (Windows) and access count.
  -wizard <PASSWD>       Set a wizard password.
  -wjf                   Wizard just friend (permanently).
```

It is build from eight files: `gwdLog.ml`, `gwdPlugin.ml`, `request.ml` (these
three files define `geneweb.gwd_lib`),, `gwdPluginDep.ml`,
`gwdPluginMD5.ml`, `gwdPluginMETA.ml`, `robot.ml` and `gwd.ml`.

### GwdLog module
Util functions for logging errors & infos.

### GwdPlugin module
Module used for registering plug-ins to Gwd.

### Request module
The main interface between the Geneweb core and the rest of the world.

### GedPluginDep module
Calculates the load order of the different plugins.

### GwdPluginMD5 module
Checks the plugins .cmxs files and assets did not change since compilation.
This module is written by mk_gwdPluginMD5.ml. After the plug-ins have been compiled,
mk_gwdPluginMD5.ml lists them in a pattern matching with their MD5 hash.

### GwdPluginMETA module
Handles the metadata of a plug-in: its version, its mainteners' list and
its dependencies.
Metadata is optional, and may contain the dependencies of the plugin (separated
by a comma), its maintainers (separated by a comma) and its version.

### Robot module
Handles the connection of robots to the server.

### Gwd module
The executable

## Requests
Gwd displays different web pages and perform different actions given a mode.
This mode is given as a GET argument with the key `m`.

Here is a quick documentation of each mode.

* No mode: displays the default page depending on other arguments. It can be
the index page (no base selected), a welcome page (only a base has been
selected) or a person page (a base and a person has been selected).

* Mode "A": displays the ascendants of the selected person (selection key is `i`).

* Mode "ADD_FAM": displays the form for adding families.

* Mode "ADD_FAM_OK": requests to add a new family.

* Mode "ADD_IND": displays a form for adding a new person

* Mode "ADD_IND_OK": requests to add a new person.

* Mode "ADD_PAR": associate parents to a person (person identifier must be set
with key `ip`)

* Mode "ADD_PAR_OK" : requests to add new parents.

* Mode "ANM" : displays the menu of anniversaries modification

* Mode "AN" : displays anniversaries ; with the `v` key for months, displays
the anniversaries of thay month (v=1 -> January, ...)

* Mode "AD": displays death anniversaries ; with the `v` key for months, displays
the anniversaries of thay month (v=1 -> January, ...)

* Mode "AM": displays marriage anniversaries ; with the `v` key for months, displays
the anniversaries of thay month (v=1 -> January, ...)

* Mode "AS_OK": displays the results of an advanced search

* Mode "C": displays the cousins menu

* Mode "CAL": displays the calendars; if no key set, it will use today's date.

* Mode "CHG_CHN": displays the form for changing children names of a person

* Mode "CHG_CHN_OK": requests to change the children names a new person.

* Mode "CHG_EVT_IND_ORD": displays the form for changing the order of events
for a person

* Mode "CHG_EVT_IND_ORD_OK": requests to change the evenement order of a
person.

* Mode "CHG_EVT_FAM_ORD": displays the form for changing the order of events
for a family

* Mode "CHG_EVT_FAM_ORD_OK": requests to change the evenement order of a
family.

* Mode "CHG_FAM_ORD": displays a menu to change the family order

* Mode "CHG_FAM_ORD_OK": requests the family order change

* Mode "CONN_WIZ": displays the connected wizards (the base admins)

* Mode "D": displays the descendants of the selected person (selection key is `i`).

* Mode "DAG": displays a relationship graph.

* Mode "DEL_FAM": displays a page for validating the deletion of the family in
argument (key is `i`).

* Mode "DEL_FAM_OK": requests to remove a family.

* Mode "DEL_IND": displays a page for validating the deletion of the person in
argument (key is `i`).

* Mode "DEL_IND_OK": requests to remove a person.

* Mode "F": displays the family tree.

* Mode "H": displays the file `fname.txt` where `fname` is register with the key
`v` and the file being in `hd/etc` (`hd` is the dir specified by option `-hd`).

* Mode "HIST": displays an history of updates.

* Mode "HIST_CLEAN": displays the history list associated to the history file
in argument (key is `f`).

* Mode "HIST_CLEAN_OK": requests to clean the history associated to the history
file in argument

* Mode "HIST_DIFF": displays the page that allows to select (with variable
`t = "SUM"`) and to view (with variable `t = "DIFF"`) the difference
between all revisions of history file of concerned person in variable `f`.
Intepretate the template file `updhist_diff.txt`

* Mode "HIST_SEARCH": same as "HIST", but with a default search

* Mode "IM": displays the image whose name is in argument (key is `s`)

* Mode "IMH": same than "IM", but returns HTML

* Mode "INV_FAM": displays a menu for inverting the order of two families (where
a family is given by the `f` key and the individual is given by the `i` key).

* Mode "INV_FAM_OK": requests to reverse the families.

* Mode "KILL_ANC": Undocumented feature; kill someone's ancestors.

* Mode "LB": lists the last births.

* Mode "LD": lists the last deaths.

* Mode "LINKED": displays links to pages assocaited to an individual.

* Mode "LL": lists the persons who lived the longest.

* Mode "LM": lists the last marriages.

* Mode "MISC_NOTES": displays a menu to search in notes.

* Mode "MISC_NOTES_SEARCH": same as "MISC_NOTES", but with a search argument
  (key is `s`)

* Mode "MOD_DATA": displays a menu for updating Geneweb's dictionary of names,
last names, locations, sources and professions.

* Mode "MOD_DATA_OK": requests a data modification.

* Mode "MOD_FAM": displays a form for updating a family (key is `i`).

* Mode "MOD_FAM_OK": requests a family modification.

* Mode "MOD_IND":  displays a form for updating a person (key is `i`).

* Mode "MOD_IND_OK": requests a person modification.

* Mode "MOD_NOTES": displays a text form for writing notes.

* Mode "MOD_NOTES_OK": requests a note update.

* Mode "MOD_WIZNOTES": displays the HTML page for editing wizard notes.
Fails if wizard authentification is incorrect or if current user cannot
edit.

* Mode "MOD_WIZNOTES_OK": requests the wizard note modification.

* Mode "MRG": displays a menu for merging two persons
(key for persons is `i`).

* Mode "MRG_DUP": displays a menu for merging possible duplications of persons
(key for persons is `ip`).

* Mode "MRG_DUP_IND_Y_N": either displays the merge dupliate menu if
`answer_y` is not a key of the request, or a form for merging two
person (whose keys are `i` and `i2`).

* Mod "MRG_DUP_FAM_Y_N": same than "MRG_DUP_IND_Y_N", but for families.

* Mode "MRG_FAM": displays a menu for merging families (family keys are `i`
and `i2`). Couples must be identical (modulo reversion).

* Mode "MRG_FAM_OK": requests the family merge

* Mode "MRG_MOD_FAM_OK": requests the family modification and merge

* Mode "MRG_IND": displays a form for merging two persons

* Mode "MRG_IND_OK": requests a merge of two persons

* Mode "MRG_MOD_IND_OK": requests a merge & modification of two persons

* Mode "N": lists all the surnames; if a surname is selected (key is `v`),
displays the list of pages where this surname is used.

* Mode "NG": same than `N`, but expects a name with the key `n`

* Mode "NOTES": displays the notes

* Mode "OA": displays the list of the oldest persons that are still alive or,
if unknown, whose death are not probable

* Mode "OE": same as "OA", but for engaged couples

* Mode "P": lists all the first names; if a surname is selected (key is `v`),
displays the list of pages where this name is used.

* Mode "PQP_PYR": displays a population pyramid (reachable from "STAT")

* Mode "PS": displays all the persons associated to a given place (`ma` key for
marriage, `bi` key for birth, `bp` key for baptism, `de` key for death and `bu`
key for burial).

* Mode "R": displays the relationship details between two persons

* Mode "REQUEST": displays the current request

* Mode "RL": displays the relationship link between two persons

* Mode "RLM": displays relation ship details between multiple persons

* Mode "S": displays the results of a search
(from, for example, the main page).

* Mode "SRC": displays the file in argument (key is `v`)

* Mode "STAT": displays several links for statistics: latest births, death,
marriages, the oldest couples, persons that are alive and who lives the longest.
There also is a population pyramid.

* Mode "CHANGE_WIZ_VIS": displays the connected wizards

* Mode "TT": displays the titles associated to persons

* Mode "U": displays the page associated to the template `updmenu.txt`

* Mode "VIEW_WIZNOTES": Prints the HTML page displaying wizard notes

* Mode "WIZNOTES": Same as VIEW_WIZNOTES, but fails if not authentificated

* Mode "WIZNOTES_SEARCH": Same as VIEW_WIZNOTES, but highlights HTML with
the specified string searched (key is `s`).

Any other mode leads to an incorrect request page (Error 400).

## Plug-ins
Plus-ins are additional features that can be added to `gwd`. They can register
two kind of services:

* additional modes: with `GwdPlugin.register`, a plug-in can register new
requests

* additional computations: with `GwdPlugin.register_se`, a plug-in can register
pre/post processors

## Customization
Each base can be customized with a `<BASENAME>.gwf` file. A documented example is
available on the `etc` directory at the root of the project.
Plug-ins are activated for a base with the `plugin=...` directive in the
configuration file.
