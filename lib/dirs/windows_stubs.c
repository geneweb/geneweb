// This code is extracted from the xdg library of the Dune project. It is
// distrubuted under the terms of the MIT license.
//
// Copyright (c) 2026 OCamlPro SAS, <contact@ocamlpro.com>
//
// Copyright (c) 2016 Jane Street Group, LLC <opensource@janestreet.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#ifdef _WIN32

/*  Windows Vista functions enabled */

#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600

#include <windows.h>
#include <knownfolders.h>
#include <shlobj.h>

value geneweb__get_known_folder_path(value v_known_folder)
{
  CAMLparam1(v_known_folder);
  CAMLlocal2(v_res, v_path);
  WCHAR* wcp = NULL;
  HRESULT res;
  int wlen, len;
  const KNOWNFOLDERID *rfid;

  v_res = Val_int(0);

  switch (Int_val(v_known_folder)) {
    case 0:
      rfid = &FOLDERID_InternetCache;
      break;
    case 1:
      rfid = &FOLDERID_LocalAppData;
      break;
    case 2:
      rfid = &FOLDERID_Profile;
      break;
    case 3:
      rfid = &FOLDERID_Documents;
      break;
    default:
      caml_invalid_argument("get_known_folder_path");
      break;
  }

  res = SHGetKnownFolderPath(rfid, 0, NULL, &wcp);

  if (res != S_OK)
    goto done;

  wlen = wcslen(wcp);
  len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wcp, wlen, NULL, 0, NULL, NULL);

  if (!len)
    goto done;

  v_path = caml_alloc_string(len);

  if (!WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, wcp, wlen, (char *)String_val(v_path), len, NULL, NULL))
    goto done;

  v_res = caml_alloc_small(1, 0);
  Field(v_res, 0) = v_path;

 done:
  CoTaskMemFree(wcp);
  CAMLreturn(v_res);
}

#else /* _WIN32 */

value geneweb__get_known_folder_path(value v_unit) {
  (void)v_unit;
  caml_invalid_argument("get_known_folder_path: not implemented");
}

#endif /* _WIN32 */
