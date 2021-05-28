# Carrousel

Fanchart is a GeneWeb v7 plugin to allow display of fancharts.

## Installation (for users)

Activate the plugin mechanism at gwd launch time with
```
gwd ... -plugins path_to_plugins
```
This is probably already done as several v7 functions are provided
through a plugin mechanism (cgl, export, xhtml, ...)

Add fanchart to the active plugin list in your .gwf file:
```
plugins=cgl,export,forum,no_index,xhtml,fanchart
```

## Operations

Does not work properly in CGI mode (issue with .css and .js files)

Self explanatory buttons activate various display options.

## Copyright

Plugin written by H Gouraud (plugin) and Ludovix Ledieu (JavaScript)
