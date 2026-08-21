#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/fail.h>

CAMLprim value geneweb_win32_fd_of_file_descr (value file_descr) {
  CAMLparam1 (file_descr);
  int fd;

#if defined (_WIN32)
  fd = caml_win32_CRT_fd_of_filedescr (file_descr);
#else
  fd = Int_val (file_descr);
#endif

  CAMLreturn (Val_int (fd));
}

CAMLprim value geneweb_win32_file_descr_of_fd (value fd) {
  CAMLparam1 (fd);
  CAMLlocal1 (file_descr);

#if defined (_WIN32)
  HANDLE h = (HANDLE) _get_osfhandle (Int_val (fd));
  if (h == INVALID_HANDLE_VALUE)
    caml_invalid_argument ("is not a handle!");

  file_descr = caml_win32_alloc_socket ((SOCKET) h);
#else
  file_descr = fd;
#endif

  CAMLreturn (file_descr);
}
