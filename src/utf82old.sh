#!/bin/sh

cat $1 |
sed -e 's/"/@/g' -e 's/\$/\$/g' |
awk '
BEGIN { enc="" }
function disp() {
    if (enc == "ar") ;
    else if (enc == "iso-8859-3")
        system("echo \"" $0 "\" | iconv -f utf-8 -t " enc " | sed -e 's/æ/cx/g' -e 's/ø/gx/g' -e 's/¼/jx/g' -e 's/þ/sx/g' -e 's/ý/ux/g'")
    else system("echo \"" $0 "\" | iconv -f utf-8 -t " enc);
    next;
}
function conv(t) { enc=t; disp() }
/^af: / { conv("iso-8859-1") }
/^ar: / { enc="ar"; next }
/^bg: / { conv("windows-1251") }
/^br: / { conv("iso-8859-1") }
/^ca: / { conv("iso-8859-1") }
/^cs: / { conv("iso-8859-2") }
/^da: / { conv("iso-8859-1") }
/^eo: / { conv("iso-8859-3") }
/^de: / { conv("iso-8859-1") }
/^es: / { conv("iso-8859-1") }
/^et: / { conv("iso-8859-15") }
/^fi: / { conv("iso-8859-1") }
/^fr: / { conv("iso-8859-1") }
/^fr-cr: / { conv("iso-8859-1") }
/^he: / { conv("iso-8859-8") }
/^is: / { conv("iso-8859-1") }
/^it: / { conv("iso-8859-1") }
/^lv: / { conv("iso-8859-1") }
/^nl: / { conv("iso-8859-1") }
/^no: / { conv("iso-8859-1") }
/^pl: / { conv("iso-8859-2") }
/^pt: / { conv("iso-8859-1") }
/^ru: / { conv("windows-1251") }
/^sl: / { conv("iso-8859-2") }
/^sv: / { conv("iso-8859-1") }
/^zh: / { conv("gb2312") }
enc== "" { print; next }
/^  / { disp(); next }
{ enc=""; print; next }
' |
sed -e 's/@/"/g'
