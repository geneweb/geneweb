#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef _WIN32

#include <windows.h>

/* BCryptGenRandom is looked up in bcrypt.dll at run time, so that no import
   library has to be linked. */
typedef LONG (WINAPI *gen_random_t) (void *, PUCHAR, ULONG, ULONG);

#define SYSTEM_PREFERRED_RNG 0x00000002

CAMLprim value
geneweb_win32_random_bytes (value vlen)
{
  CAMLparam1 (vlen);
  CAMLlocal1 (res);
  static gen_random_t gen_random = NULL;
  ULONG len = (ULONG) Long_val (vlen);

  if (gen_random == NULL)
    {
      HMODULE bcrypt = LoadLibraryW (L"bcrypt.dll");
      if (bcrypt != NULL)
        gen_random = (gen_random_t) (void (*) (void))
          GetProcAddress (bcrypt, "BCryptGenRandom");
    }
  if (gen_random == NULL)
    caml_failwith ("BCryptGenRandom unavailable");
  res = caml_alloc_string (len);
  if (gen_random (NULL, (PUCHAR) Bytes_val (res), len, SYSTEM_PREFERRED_RNG)
      < 0)
    caml_failwith ("BCryptGenRandom failed");
  CAMLreturn (res);
}

#else

CAMLprim value
geneweb_win32_random_bytes (value vlen)
{
  (void) vlen;
  caml_failwith ("geneweb_win32_random_bytes: Windows only");
  return Val_unit;
}

#endif
