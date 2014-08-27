#include <stdint.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#include <caml/fail.h>


uint64_t piqi_strtoull(char *str)
{
    char *src, *dst;

    src = dst = str;

    /* check that input string contains only decimal digits and underscores and
     * remove underscores from the string */
    while (*src)
    {
        if (*src >= '0' && *src <= '9')
            *dst++ = *src++; /* copy digit */
        else if (*src == '_')
            src++; /* skip '_' character */
        else /* invalid character */
            goto fail;
    }
    if (dst == str) goto fail; /* no digits, i.e. string is empty */ 
    *dst = '\0'; /* terminate */

    /* convert input decimal string to uint64 value */
    errno = 0;
    uint64_t res = strtoull(str, NULL, 10);
    if (errno) goto fail;

    return res;

fail:
    caml_failwith("piqi_strtoull");
    return 0;
}


#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>

value camlidl_piqi_c_piqi_strtoull(
	value _v_str)
{
  char *str; /*in*/
  int _res;
  value _vres;

  str = String_val(_v_str);
  _res = piqi_strtoull(str);
  _vres = copy_int64(_res);
  return _vres;
}

