#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <brotli/encode.h>
#include <stdlib.h>
#include <string.h>

CAMLprim value ml_brotli_compress(value quality_v, value mode_v, value input_v)
{
  CAMLparam3(quality_v, mode_v, input_v);
  CAMLlocal1(output_v);

  int quality = Int_val(quality_v);
  int mode = Int_val(mode_v);
  const uint8_t *input = (const uint8_t *)String_val(input_v);
  size_t input_size = caml_string_length(input_v);

  size_t output_size = BrotliEncoderMaxCompressedSize(input_size);
  uint8_t *output = (uint8_t *)malloc(output_size);
  if (!output)
    caml_failwith("brotli: malloc failed");

  BROTLI_BOOL ok = BrotliEncoderCompress(
      quality, BROTLI_DEFAULT_WINDOW, mode,
      input_size, input, &output_size, output);

  if (ok) {
    output_v = caml_alloc_string(output_size);
    memcpy(Bytes_val(output_v), output, output_size);
    free(output);
    CAMLreturn(output_v);
  } else {
    free(output);
    caml_failwith("brotli: compression failed");
  }
}