# Carrousel

Carrousel is a GeneWeb v7 plugin to manage multiple images associated with a person.

## Installation (for users)

Activate the plugin mechanism at gwd launch time with
```
gwd ... -plugins path_to_plugins
```
This is probably already done as several v7 functions are provided
through a plugin mechanism (cgl, export, xhtml, ...)

Add carrousel to the active plugin list in your .gwf file:
```
plugins=cgl,export,forum,no_index,xhtml,carrousel
```

## Operations

Not fully functional in CGI mode yet.

Images are separated in two categories: portraits and others.
Portraits are displayed at the portrait location on perso pages.
They are kept in ```images/basename```
Carrousel maintains in ```images/basename/saved``` one saved copy of a
portrait after a change.
Carrousel provides a button (green) to swap the two portraits or to delete (red)
it permanently.
The (yellow) delete button next to the portrait removes it from the images folder
and places it in the saved folder, permanently deleting any file that may have
already been there

Other images are kept in ```src/basename/images/firstname.occ.surname```.
In the same folder, Carrousel keeps a .txt file containing a
caption for the image with the same name.
Images deleted (yellow) in this folder are kept in
```src/basename/images/firstname.occ.surname/saved```
Carrousel provides a button to return (green) a deleted image to the other images
folder, and a button to permanently delete (red) the image from the saved folder.

To display the carrousel, one must click on the image icon at the right of the menubar,
and then activate carrousel display. This two step operation is not ideal, but is a
constraint of the plugin mechanism which prevents carrousel features to be
readily available in the standard menubar. This may evolve over time.

## Copyright

Plugin written by H Gouraud and A2

