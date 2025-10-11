import io
import logging
from typing import Any, BinaryIO, Union

from lib.old_db.unmarshall.header import caml_parse_header_f
from lib.old_db.unmarshall.v2.intern_rec import read_bin_caml_input, unmarshall_ocaml_data
from lib.old_db.unmarshall.v2.ocaml_input import OCamlInput
import lib.old_db.unmarshall.v2.intext as Intext
from lib.old_db.v2 import mutil


def input_value(oi: Union[OCamlInput, BinaryIO]) -> Any:
    """Read an OCaml value from an OCamlInput stream."""
    if isinstance(oi, OCamlInput):
        return caml_input_val(oi.f)
    if isinstance(oi, (BinaryIO, io.BufferedReader)):
        return caml_input_val(oi)
    raise TypeError(f"input_value expects OCamlInput or BinaryIO, got {type(oi)}")


def caml_input_val(chan: BinaryIO) -> Any:
    """Read an OCaml value from a binary channel."""
    # NOTE: The original C code puts the header in a buffer and then parses it.
    # Here, we directly parse from the channel using OCamlInput.
    # The following commented code is a reference to the original C logic.

    # pos = chan.tell() # Save current position
    # oi = OCamlInput(chan)
    # header = oi.read_bytes(5)
    # print(f"Initial header bytes: {header.hex()}")
    # if len(header) == 0:
    #     raise EOFError("Unexpected end of file while reading OCaml value header")
    # elif len(header) < 5:
    #     raise EOFError("input_value: truncated object")
    # chan.seek(pos)  # Rewind to start. s->intern_src = (unsigned char *) header;
    # hlen = 0
    # match oi.read_uint32():
    #     case Intext.MagicNumbers.BIG.value:
    #         hlen = 32
    #     case Intext.MagicNumbers.COMPRESSED.value:
    #         hlen = oi.read_byte() & 0x3F
    #     case a:
    #         print(f"Detected magic number: {a:#08x}")
    #         hlen = 20
    # # Read the reminder of the header
    # assert hlen > 5
    # header += oi.read_bytes(hlen - 5)
    # if len(header) < (hlen - 5):
    #     raise EOFError("input_value: truncated object")
    # # Parse the full header
    # chan.seek(pos)  # Rewind to start. s->intern_src = (unsigned char *) header;

    header = caml_parse_header_f(chan)
    # Read block from channel
    oi = OCamlInput(chan)
    logger = mutil.get_logger("v2.caml_input_val")
    # ch = logging.StreamHandler()
    # ch.setLevel(logging.DEBUG)
    # ch.setFormatter(logging.Formatter("(origin/%(name)s) [%(levelname)s]\t%(message)s"))
    # logger.setLevel(logging.DEBUG)
    # if not logger.hasHandlers():
    #     logger.addHandler(ch)
    item = read_bin_caml_input(oi, logger)
    return item
