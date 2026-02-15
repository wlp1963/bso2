#!/usr/bin/env python3
"""Send a raw binary payload to bso2 via `L B ADDR LEN`."""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path

try:
    import serial
except ImportError as exc:  # pragma: no cover
    raise SystemExit(
        "pyserial is required. Install with: pip install pyserial"
    ) from exc


READY_TOKEN = b"L B READY"
DONE_TOKEN = b"L B LOAD COMPLETE"
ABORT_TOKEN = b"L B ABORTED"


def parse_hex_u16(value: str) -> int:
    text = value.strip().upper()
    if text.startswith("$"):
        text = text[1:]
    elif text.startswith("0X"):
        text = text[2:]
    if not text:
        raise argparse.ArgumentTypeError("empty value")
    try:
        number = int(text, 16)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(f"invalid hex value: {value}") from exc
    if not 0 <= number <= 0xFFFF:
        raise argparse.ArgumentTypeError(f"out of range (0000..FFFF): {value}")
    return number


def read_until(
    ser: serial.Serial,
    tokens: tuple[bytes, ...],
    timeout_s: float,
) -> tuple[bytes, bytes | None]:
    end = time.monotonic() + timeout_s
    buf = bytearray()
    while time.monotonic() < end:
        chunk = ser.read(256)
        if chunk:
            buf.extend(chunk)
            for token in tokens:
                if token in buf:
                    return bytes(buf), token
        else:
            time.sleep(0.01)
    return bytes(buf), None


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Stream a .bin file to bso2 using L B ADDR LEN."
    )
    parser.add_argument("bin_file", type=Path, help="Path to binary payload")
    parser.add_argument(
        "--port",
        default="COM3",
        help="Serial port (default: COM3)",
    )
    parser.add_argument(
        "--baud",
        type=int,
        default=115200,
        help="Baud rate (default: 115200)",
    )
    parser.add_argument(
        "--addr",
        type=parse_hex_u16,
        required=True,
        help="Load address in hex (examples: 1000, $1000, 0x1000)",
    )
    parser.add_argument(
        "--ready-timeout",
        type=float,
        default=5.0,
        help="Seconds to wait for 'L B READY' (default: 5.0)",
    )
    parser.add_argument(
        "--done-timeout",
        type=float,
        default=5.0,
        help="Seconds to wait for completion text (default: 5.0)",
    )
    parser.add_argument(
        "--no-wait-done",
        action="store_true",
        help="Do not wait for 'L B LOAD COMPLETE'",
    )
    args = parser.parse_args()

    payload = args.bin_file.read_bytes()
    length = len(payload)
    if length == 0:
        print("error: payload is empty", file=sys.stderr)
        return 2
    if length > 0xFFFF:
        print("error: payload too large for L B LEN (max FFFF bytes)", file=sys.stderr)
        return 2
    if args.addr + length > 0x10000:
        print("error: address range exceeds 64K memory space", file=sys.stderr)
        return 2

    cmd = f"L B {args.addr:04X} {length:04X}\r".encode("ascii")

    with serial.Serial(args.port, args.baud, timeout=0.1) as ser:
        ser.reset_input_buffer()
        ser.write(cmd)
        ser.flush()

        prelude, token = read_until(
            ser, (READY_TOKEN, ABORT_TOKEN), args.ready_timeout
        )
        if prelude:
            sys.stdout.write(prelude.decode("ascii", errors="replace"))
            sys.stdout.flush()

        if token != READY_TOKEN:
            print("\nerror: did not receive L B READY", file=sys.stderr)
            return 1

        ser.write(payload)
        ser.flush()

        if args.no_wait_done:
            return 0

        trailer, token = read_until(
            ser, (DONE_TOKEN, ABORT_TOKEN), args.done_timeout
        )
        if trailer:
            sys.stdout.write(trailer.decode("ascii", errors="replace"))
            sys.stdout.flush()

        if token == DONE_TOKEN:
            return 0

        print("\nerror: load did not complete", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
