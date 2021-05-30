# Welcome

Welcome is a GeneWeb v7 plugin providing an alternative approach 
to search on the welcome page.

## Installation (for users)

Activate the plugin mechanism at gwd launch time with
```
gwd ... -plugins path_to_plugins
```
This is probably already done as several v7 functions are provided
through a plugin mechanism (cgl, export, xhtml, ...)

Add welcome to the active plugin list in your .gwf file:

```
plugins=cgl,export,forum,no_index,xhtml,welcome
```

## Operations

With the welcome plugin active, the semantic of the two capture lines of
the welcome page are inverted:

- The first line may contain "first_name surname", first_name", "alias", "public name" or "sosa".
- The second line may contain "surname"

When providing "first_name surname" in the first line, search will be limited to
the exact match of the first name.

When providing "first_name" and "surname" in the two separate lines, then the search
for both will return persons whose first name or surname contains the requested argument.
A button may restrict back one or the other to an exact match.
The search for "public name" and "surname" in two separate lines will return the
expected result.

To facilitate data capture, input in the two lines may be entered with
"first_name" <TAB> "surname" <CR>.

## Copyright

Plugin written by H Gouraud 
