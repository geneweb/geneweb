#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifndef _WIN32
#include <unistd.h>
#endif

CAMLprim value geneweb_unix_linux()
{
#ifdef __linux__
  return Val_bool(1);
#else
  return Val_bool(0);
#endif
}

CAMLprim value geneweb_unix_seteuid(value euid)
{
#ifndef _WIN32
  if (seteuid(Int_val(euid)) == -1)
    uerror("geneweb_unix_seteuid", Nothing);
  return Val_unit;
#else
  caml_invalid_argument("Geneweb_unix.seteuid");
#endif
}

CAMLprim value geneweb_unix_setegid(value egid)
{
#ifndef _WIN32
  if (setegid(Int_val(egid)) == -1)
    uerror("geneweb_unix_setegid", Nothing);
  return Val_unit;
#else
  caml_invalid_argument("Geneweb_unix.setegid");
#endif
}

CAMLprim value geneweb_unix_setreuid(value ruid, value euid)
{
#ifdef __linux__
  if (setreuid(Int_val(ruid), Int_val(euid)) == -1)
    uerror("geneweb_unix_setreuid", Nothing);
  return Val_unit;
#else
  caml_invalid_argument("Geneweb_unix.setreuid");
#endif
}

CAMLprim value geneweb_unix_setregid(value rgid, value egid)
{
#ifdef __linux__
  if (setregid(Int_val(rgid), Int_val(egid)) == -1)
    uerror("geneweb_unix_setregid", Nothing);
  return Val_unit;
#else
  caml_invalid_argument("Geneweb_unix.setregid");
#endif
}
