#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#if defined(_WIN32)
#ifndef UNICODE
#define UNICODE 1
#endif

#include <winsock2.h>
#include <windows.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/unixsupport.h>

#include <assert.h>
#include <stdbool.h>

static void
raise_error (int code)
{
  caml_raise_with_arg (*caml_named_value ("geneweb_wsa.error"),
                       Val_int (code));
}

static const struct custom_fixed_length protocol_info_fixed_length = {
  .bsize_32 = sizeof (WSAPROTOCOL_INFO), .bsize_64 = sizeof (WSAPROTOCOL_INFO)
};

static WSAPROTOCOL_INFO *
Protocol_info_val (value v)
{
  return (WSAPROTOCOL_INFO *) Data_custom_val (v);
}

void
protocol_info_serialize (value v, uintnat *bsize_32, uintnat *bsize_64)
{
  caml_serialize_block_1 (Protocol_info_val (v), sizeof (WSAPROTOCOL_INFO));
  *bsize_32 = protocol_info_fixed_length.bsize_32;
  *bsize_64 = protocol_info_fixed_length.bsize_64;
}

uintnat
protocol_info_deserialize (void *dst)
{
#ifdef ARCH_SIXTYFOUR
  caml_deserialize_block_1 (dst, protocol_info_fixed_length.bsize_64);
  return protocol_info_fixed_length.bsize_64;
#else
  caml_deserialize_block_1 (dst, protocol_info_fixed_length.bsize_32);
  return protocol_info_fixed_length.bsize_32;
#endif
}

CAMLexport const struct custom_operations protocol_info_ops = {
  "fr.roglo.geneweb.protocol_info", custom_finalize_default,
  custom_compare_default,           custom_hash_default,
  &protocol_info_serialize,         &protocol_info_deserialize,
  custom_compare_ext_default,       &protocol_info_fixed_length
};

static value
alloc_protocol_info (void)
{
  return caml_alloc_custom (&protocol_info_ops, sizeof (WSAPROTOCOL_INFO), 0,
                            1);
}

static DWORD
pid_of_pseudo_handle (value ph)
{
  // The input is a pseudo handle produced by [Unix.getpid]. It is valid to
  // cast it to HANDLE directly.
  return GetProcessId ((HANDLE) Long_val (ph));
}

#endif // _WIN32

CAMLprim value
geneweb_protocol_info_init (value unit)
{
#if defined(_WIN32)
  CAMLparam1 (unit);
  caml_register_custom_operations (&protocol_info_ops);
  CAMLreturn (Val_unit);
#else
  caml_invalid_argument ("geneweb_protocol_info_init: not supported");
#endif
}

CAMLprim value
geneweb_protocol_info_duplicate_socket (value s, value ph)
{
#if defined(_WIN32)
  CAMLparam2 (s, ph);
  CAMLlocal1 (r);
  SOCKET socket = INVALID_SOCKET;
  int pid = pid_of_pseudo_handle (ph);

  r = alloc_protocol_info ();
  WSAPROTOCOL_INFO *pi = NULL;

  switch (Descr_kind_val (s))
    {
    case KIND_SOCKET:
      socket = Socket_val (s);
      pi = Protocol_info_val (r);
      break;
    case KIND_HANDLE:
      caml_invalid_argument (
          "geneweb_protocol_info_duplicate_socket: expected a socket");
    default:
      assert (false);
      abort ();
    }

  if (WSADuplicateSocket (socket, pid, pi) != 0)
    raise_error (WSAGetLastError ());

  CAMLreturn (r);
#else
  caml_invalid_argument (
      "geneweb_protocol_info_duplicate_socket: not supported");
#endif
}

CAMLprim value
geneweb_protocol_info_to_socket (value v)
{
#if defined(_WIN32)
  CAMLparam1 (v);
  DWORD flags = 0;
  SOCKET s = INVALID_SOCKET;
  WSAPROTOCOL_INFO *pi = Protocol_info_val (v);

  s = WSASocket (FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO, FROM_PROTOCOL_INFO,
                 pi, 0, flags);
  if (s == INVALID_SOCKET)
    raise_error (WSAGetLastError ());

  CAMLreturn (win_alloc_socket (s));
#else
  caml_invalid_argument ("geneweb_protocol_info_to_socket: not supported");
#endif
}
