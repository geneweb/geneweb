#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

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
  SOCKET s = (SOCKET) _get_osfhandle (fd);
  fd = caml_win32_alloc_socket (s);
#else
  fd = v;
#endif

  CAMLreturn (fd);
}
