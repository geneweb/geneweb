#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/fail.h>

CAMLprim value geneweb_win32_fd_of_file_descr (value v) {
  CAMLparam1 (v);
  int fd;

#if defined (_WIN32)
  fd = caml_win32_CRT_fd_of_filedescr (v);
#else
  fd = Int_val (v);
#endif

  CAMLreturn (Val_int (fd));
}

CAMLprim value geneweb_win32_file_descr_of_fd (value v) {
  CAMLparam1 (v);
  CAMLlocal1 (fd);

#if defined (_WIN32)
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  if (h == INVALID_HANDLE_VALUE)
    caml_invalid_argument ("is not a handle!");

  fd = caml_win32_alloc_socket ((SOCKET) h);
#else
  fd = v;
#endif

  CAMLreturn (fd);
}
