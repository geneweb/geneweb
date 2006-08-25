#!/bin/sh

cat $1 |
sed -e 's/"/@/g' -e 's/\$/\$/g' |
awk '
BEGIN { enc="" }
function disp() {
    if (enc == "ar") ;
    else if (enc == "iso-8859-3")
        system("echo \"" $0 "\" | iconv -f utf-8 -t " enc " | sed -e 's/æ/cx/g' -e 's/ø/gx/g' -e 's/¼/jx/g' -e 's/þ/sx/g' -e 's/ý/ux/g'")
    else if (enc == "iso-8859-8")
        system("echo \"" $0 "\" | iconv -f utf-8 -t " enc " | sed -e \"s/he: \\([âàéø]\\)/he:  \\1/\" -e \"s|/\\([âàéø]\\)|/ \\1|g\"")
    else if (enc == "lv")
        system("echo \"" $0 "\" | sed -e 's/Ä\\214/È/g' -e 's/Ä\\201/â/g' -e 's/Å\\206/ò/g' -e 's/Ä¼/ï/g' -e 's/Ä\\215/è/g' -e 's/Å¡/ð/g' -e 's/Å«/û/g' -e 's/Ä\\223/ç/g' -e 's/Ä¢/Ì/g' -e 's/Ä£/ì/g' -e 's/Ä¶/Í/g' -e 's/Ä«/î/g' -e 's/Ä·/í/g' -e 's/Ã¾/þ/g'")
    else system("echo \"" $0 "\" | iconv -f utf-8 -t " enc);
    fflush(stdout);
}
function conv(t) { enc=t; disp() }
/^af: / { conv("iso-8859-1"); next }
/^ar: / { enc="ar"; next }
/^bg: / { conv("windows-1251"); next }
/^br: / { conv("iso-8859-1"); next }
/^ca: / { conv("iso-8859-1"); next }
/^cs: / { conv("iso-8859-2"); next }
/^da: / { conv("iso-8859-1"); next }
/^eo: / { conv("iso-8859-3"); next }
/^de: / { conv("iso-8859-1"); next }
/^es: / { conv("iso-8859-1"); next }
/^et: / { conv("iso-8859-15"); next }
/^fi: / { conv("iso-8859-1"); next }
/^fr: / { conv("iso-8859-1"); next }
/^fr-cr: / { conv("iso-8859-1"); next }
/^he: / { conv("iso-8859-8"); next }
/^is: / { conv("iso-8859-1"); next }
/^it: / { conv("iso-8859-1"); next }
/^lv: / { conv("lv"); next }
/^nl: / { conv("iso-8859-1"); next }
/^no: / { conv("iso-8859-1"); next }
/^pl: / { conv("iso-8859-2"); next }
/^pt: / { conv("iso-8859-1"); next }
/^pt-br: / { conv("iso-8859-1"); next }
/^ru: / { conv("windows-1251"); next }
/^sl: / { conv("iso-8859-2"); next }
/^sv: / { conv("iso-8859-1"); next }
/^zh: / { conv("gb2312"); next }
enc== "" { print; next }
/^  / { disp(); next }
{ enc=""; print; next }
' |
sed -e 's/@/"/g'
