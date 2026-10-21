#!/bin/sh

echo 'Content-type: text/html'
echo  
echo '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"   '
echo '   "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">  '
echo '<html xmlns="http://www.w3.org/1999/xhtml">  '
echo '<body>'
echo 'This is a test for cgi commands<br>'
echo 'This should display Lenna:<br>'
substr="cgi-bin/test.cgi"
prefix=${SCRIPT_NAME%%$substr*}
echo '<img src="'$prefix'/Lenna.jpg">'
echo '<br>'
echo 'End of test'
echo '</body>'
echo '</html>'

