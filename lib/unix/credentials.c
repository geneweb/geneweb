#ifndef _WIN32
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <unistd.h>

CAMLprim value geneweb_unix_seteuid(value euid)
{
  if (seteuid(Int_val(euid)) == -1)
    uerror("geneweb_unix_seteuid", Nothing);
  return Val_unit;
}

CAMLprim value geneweb_unix_setegid(value egid)
{
  if (setegid(Int_val(egid)) == -1)
    uerror("geneweb_unix_setegid", Nothing);
  return Val_unit;
}

CAMLprim value geneweb_unix_setreuid(value ruid, value euid)
{
  if (setreuid(Int_val(ruid), Int_val(euid)) == -1)
    uerror("geneweb_unix_setreuid", Nothing);
  return Val_unit;
}

CAMLprim value geneweb_unix_setregid(value rgid, value egid)
{
  if (setregid(Int_val(rgid), Int_val(egid)) == -1)
    uerror("geneweb_unix_setregid", Nothing);
  return Val_unit;
}

#endif
