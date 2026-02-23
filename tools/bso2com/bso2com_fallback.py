#!/usr/bin/env python3
# SPDX-License-Identifier: MIT
# Copyright (c) 2026 95west.us
# See LICENSE for full license text.

from __future__ import annotations

import argparse
import os
import sys
import time

try:
    import serial  # type: ignore
except Exception as exc:  # pragma: no cover
    print(
        "error: pyserial is required. Install with: python -m pip install pyserial",
        file=sys.stderr,
    )
    print(f"detail: {exc}", file=sys.stderr)
    raise SystemExit(2)

TOKEN_READY = b"L S READY"
TOKEN_DONE = b"L S LOAD COMPLETE"
TOKEN_ABORT = b"L S ABORTED"


def read_until_token(ser: serial.Serial, token_ok: bytes, token_abort: bytes | None, timeout_s: float) -> int:
    end_at = time.monotonic() + timeout_s
    buf = bytearray()
    while time.monotonic() < end_at:
        chunk = ser.read(256)
        if not chunk:
            continue
        sys.stdout.buffer.write(chunk)
        sys.stdout.buffer.flush()
        buf.extend(chunk)
        if token_ok in buf:
            return 1
        if token_abort is not None and token_abort in buf:
            return 2
        if len(buf) > 16384:
            del buf[:8192]
    return 0


def send_srec(
    *,
    port: str,
    baud: int,
    file_path: str,
    monitor_cmd: bytes,
    ready_timeout: float,
    done_timeout: float,
    no_wait_done: bool,
) -> int:
    if not os.path.isfile(file_path):
        print(f"error: file not found: {file_path}", file=sys.stderr)
        return 2
    with open(file_path, "rb") as f:
        payload = f.read()
    if not payload:
        print("error: input file is empty", file=sys.stderr)
        return 2

    try:
        ser = serial.Serial(port=port, baudrate=baud, timeout=0.1, write_timeout=2.0)
    except Exception as exc:
        print(f"error: failed to open serial port {port}: {exc}", file=sys.stderr)
        return 1

    with ser:
        ser.reset_input_buffer()
        ser.reset_output_buffer()
        ser.write(monitor_cmd)
        ser.flush()

        state = read_until_token(ser, TOKEN_READY, TOKEN_ABORT, ready_timeout)
        if state == 2:
            print("\nerror: monitor aborted before S-record ready", file=sys.stderr)
            return 1
        if state != 1:
            print("\nerror: did not receive L S READY", file=sys.stderr)
            return 1

        ser.write(payload)
        if payload[-1:] not in (b"\n", b"\r"):
            ser.write(b"\r\n")
        ser.flush()

        if not no_wait_done:
            state = read_until_token(ser, TOKEN_DONE, TOKEN_ABORT, done_timeout)
            if state != 1:
                print("\nerror: S-record load did not complete", file=sys.stderr)
                return 1
    return 0


def default_port() -> str:
    return "COM3" if os.name == "nt" else "/dev/ttyUSB0"


def main() -> int:
    p = argparse.ArgumentParser(description="Python fallback sender for bso2 S-record loads.")
    p.add_argument("--port", default=default_port(), help="serial port")
    p.add_argument("--baud", type=int, default=115200, help="baud rate")
    sub = p.add_subparsers(dest="cmd", required=True)

    p_ls = sub.add_parser("ls-send", help='send S-record file via monitor "L S"')
    p_ls.add_argument("srec_file")
    p_ls.add_argument("--ready-timeout", type=float, default=5.0)
    p_ls.add_argument("--done-timeout", type=float, default=10.0)
    p_ls.add_argument("--no-wait-done", action="store_true")

    p_lgs = sub.add_parser("lgs-send", help='send S-record file via monitor "L G S"')
    p_lgs.add_argument("srec_file")
    p_lgs.add_argument("--ready-timeout", type=float, default=5.0)
    p_lgs.add_argument("--done-timeout", type=float, default=10.0)
    p_lgs.add_argument("--no-wait-done", action="store_true")

    args = p.parse_args()
    if args.cmd == "ls-send":
        return send_srec(
            port=args.port,
            baud=args.baud,
            file_path=args.srec_file,
            monitor_cmd=b"L S\r",
            ready_timeout=args.ready_timeout,
            done_timeout=args.done_timeout,
            no_wait_done=args.no_wait_done,
        )
    if args.cmd == "lgs-send":
        return send_srec(
            port=args.port,
            baud=args.baud,
            file_path=args.srec_file,
            monitor_cmd=b"L G S\r",
            ready_timeout=args.ready_timeout,
            done_timeout=args.done_timeout,
            no_wait_done=args.no_wait_done,
        )
    print(f"error: unknown command: {args.cmd}", file=sys.stderr)
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
