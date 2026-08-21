#include <caml/mlvalues.h>
#include <caml/fail.h>

#if defined(_WIN32)
#include <winsock2.h>
#include <windows.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#endif

CAMLprim value
geneweb_win32_socket_to_addr (value socket) {
#if defined(_WIN32)
  CAMLparam1 (socket);
  SOCKET s;

  switch (Descr_kind_val (socket)) {
  case KIND_SOCKET:
    s = Socket_val (socket);
    break;
  default:
    caml_invalid_argument ("socket_to_addr: unexpected file descriptor type");
  }

  CAMLreturn (caml_copy_int64 (s));
#else
  caml_invalid_argument ("socket_to_addr: not supported");
#endif
}

CAMLprim value
geneweb_win32_socket_of_addr (value addr) {
#if defined(_WIN32)
  CAMLparam1 (addr);
  SOCKET s = (SOCKET) Int64_val (addr);
  CAMLreturn (caml_win32_alloc_socket (s));
#else
  caml_invalid_argument ("addr_of_socket: not supported");
#endif
}
