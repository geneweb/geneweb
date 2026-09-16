#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(_WIN32)
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/unixsupport.h>
#include <stdbool.h>
#include <windows.h>

static value Val_event (DWORD e) {
  CAMLparam0 ();

  if (WAIT_OBJECT_0 <= e && e < WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS)
    CAMLreturn (caml_alloc_1 (0, Val_int (e - WAIT_OBJECT_0)));
  else if (WAIT_ABANDONED_0 <= e && e < WAIT_ABANDONED_0 + MAXIMUM_WAIT_OBJECTS)
    CAMLreturn (caml_alloc_1 (1, Val_int (e - WAIT_ABANDONED_0)));
  else if (e == WAIT_TIMEOUT)
    CAMLreturn (Val_int (0));
  else if (e == WAIT_FAILED)
    CAMLreturn (Val_int (1));
  else
    abort ();
}

#endif

CAMLprim value
geneweb_wait_for_multiple_objects (value handles, value all, value timeout)
{
#if defined(_WIN32)
  CAMLparam3 (handles, all, timeout);
  bool s = Bool_val (all);
  int t = Int_val (timeout);
  mlsize_t len = Wosize_val (handles);
  HANDLE *a = malloc (len * sizeof (*a));

  for (mlsize_t i = 0; i < len; i++)
    a[i] = (HANDLE) Long_val (Field (handles, i));

  DWORD r = WaitForMultipleObjects (len, a, s, INFINITE);

  free (a);
  CAMLreturn (Val_event (r));
#else
  caml_invalid_argument ("geneweb_wait_for_multiple_objects: not supported");
#endif
}
