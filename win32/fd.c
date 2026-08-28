#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(_WIN32)
#include <assert.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <windows.h>
#include <winsock2.h>

static value
Fd_handle (value handle)
{
  CAMLparam1 (handle);
  CAMLlocal1 (result);

  result = caml_alloc_small (2, 0);
  Field (result, 0) = caml_copy_int64 ((int64_t) Handle_val (handle));
  Field (result, 1) = KIND_HANDLE;

  CAMLreturn (result);
}

// Replace `win_alloc_handle` with `caml_win32_alloc_handle` after
// dropping support for OCaml 4.14.
static value
Handle_fd (value fd)
{
  return win_alloc_handle ((HANDLE) Int64_val (Field (fd, 0)));
}

static value
Fd_socket (value socket)
{
  CAMLparam1 (socket);
  CAMLlocal1 (result);

  result = caml_alloc_small (2, 0);
  Field (result, 0) = caml_copy_int64 ((int64_t) Socket_val (socket));
  Field (result, 1) = KIND_SOCKET;

  CAMLreturn (result);
}

// Replace `win_alloc_socket` with `caml_win32_alloc_socket` after
// dropping support for OCaml 4.14.
static value
Socket_fd (value fd)
{
  return win_alloc_socket ((SOCKET) Int64_val (Field (fd, 0)));
}

#endif

CAMLprim value
geneweb_win32_file_descr_to_fd (value socket)
{
  CAMLparam1 (socket);
#if defined(_WIN32)
  CAMLlocal1 (result);

  switch (Descr_kind_val (socket))
    {
    case KIND_SOCKET:
      result = Fd_socket (socket);
      break;
    case KIND_HANDLE:
      result = Fd_handle (socket);
      break;
    default:
      caml_invalid_argument (
          "file_descr_to_fd: unexpected file descriptor type");
    }

  CAMLreturn (result);
#else
  CAMLreturn (socket);
#endif
}

CAMLprim value
geneweb_win32_file_descr_of_fd (value fd)
{
  CAMLparam1 (fd);
#if defined(_WIN32)
  CAMLlocal1 (result);

  switch (Field (fd, 1))
    {
    case KIND_HANDLE:
      result = Handle_fd (fd);
      break;
    case KIND_SOCKET:
      result = Socket_fd (fd);
      break;
    default:
      assert (FALSE);
    }

  CAMLreturn (result);
#else
  CAMLreturn (fd);
#endif
}
