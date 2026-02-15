# bso2

`bso2` is a serial monitor for the WDC W65C02EDU platform.

It provides boot flow control, memory tools, an interactive mini-assembler, disassembly, execution/resume debug flow, and vector inspection for a 65C02-based system.

## Name

- `bso2` = `Basic System Operations/2`
- The `/2` references the 6502 lineage.
- Stylized glyph note: `6` can stand in for lowercase `b`, and `5` can stand in for lowercase `s`.
- That makes `6502` a visual shorthand for `bso2` (`6`=`b`, `5`=`s`, `0`=`o`, `2`=`/2`).

## Features

- Target CPU: `W65C02` (`CHIP 65C02`)
- Table-driven command dispatcher
- Ring-buffer command parser
- Commands: `Z C W M D U A X R N F L Q V H ?`
- Protected low RAM (`$0000-$03FF`) with force prefix `!`
- BRK debug context output with previous and next instruction lines

## Project Notes

- Code size is not a current optimization target. This project currently assumes `32K RAM` and `32K ROM`.
- Some code in this repository was generated with AI assistance. The `SXB/EDU` board is being used as a blank canvas, and AI assistance was used to accelerate delivery of the primary project goals.

## Repository Layout

- `SRC/bso2.asm`: main monitor source
- `SRC/macros.inc`: assembler macros
- `SRC/Makefile`: build/upload/clean targets
- `DOCS/monitor_usage.html`: detailed command reference
- `DOCS/monitor_usage.pdf`: printable/offline command reference

## Prerequisites

- WDC toolchain in `PATH`:
  - `WDC02AS`
  - `WDCLN`
- For `make upload`:
  - Python
  - `C:\Program Files\wdc\Tools\bin\wdc_interface.py`
- Serial port in `Makefile` is currently `COM3`

## Quick Start

```powershell
make -C SRC all
```

Upload:

```powershell
make -C SRC upload
```

Clean:

```powershell
make -C SRC clean
```

## Host Helper (L B)

Use the included sender to stream a binary file for `L B ADDR LEN`:

```powershell
python tools/send_lb.py payload.bin --port COM3 --addr 1000
```

Requirements:

- Python
- `pyserial` (`pip install pyserial`)

## Monitor Commands

- `?` short help
- `H` full help
- `Z` clear RAM (confirm `Y/N`)
- `W` warm start to monitor
- `D [START [END]]` dump memory (`END` inclusive)
- `U START END` disassemble a 65C02 range (`END` inclusive)
- `A START [MNEMONIC OPERANDS]` tiny interactive 65C02 assembler (`.` exits)
- `X START` execute at address
- `R [A=HH] [X=HH] [Y=HH]` resume last debug context (optional `A/X/Y` overrides)
- `N` run to next sequential instruction using a temporary RAM breakpoint (`ROM/I/O` targets are rejected)
- `M [START [B0..B15]]` modify/deposit memory
- `F START END B0..B15` fill memory with repeating pattern
- `L S` load Motorola S-records from serial (stops at `S7/S8/S9`, abort with `SX`)
- `L B ADDR LEN` load raw serial bytes to memory at `ADDR` for `LEN` bytes (no CRC)
- `C SRC_START SRC_END DST_START` copy memory (overlap-safe)
- `Q` halt with `WAI` (resume via NMI/Reset)
- `V` print vector chain information
- `!<CMD> ...` force-enable protected low-RAM access for `F/M/C/A/N/L`

Notes:
- In monitor command mode, up-arrow (`ESC [ A`) repeats the previous command.
- `A` supports relative-branch target entry via absolute hex address (range checked).
- `A` supports explicit accumulator syntax such as `INC A`.
- See `DOCS/monitor_usage.html` for full behavior, macro parameters, and callable function API.

## Development

- Contributor guide: `CONTRIBUTING.md`
- Security policy: `SECURITY.md`
- Community conduct: `CODE_OF_CONDUCT.md`
- Release checklist: `RELEASING.md`

## Future / TODO

- Add `S` search command family.
- Parser shape (planned): `S C START END <text>` and `S B START END <pattern...>`.
- `S C` (C-string text mode):
  - Unquoted text stops at first whitespace.
  - Quoted text supports delimiters like `"`, `'`, and `` ` ``.
  - Delimiter escaping is by doubling the delimiter character.
- `S B` (binary mode):
  - `HH` byte token (`00..FF`).
  - `HHHH` word/address token, searched little-endian (example: `8000` => `00 80`).
  - `HL` nibble wildcard token where each nibble is hex or `?` (`?A`, `A?`, `??`).
  - `*` single-byte wildcard (`*` and `??` are equivalent wildcard bytes).
- Future (not yet implemented): `S P` (Pascal length-prefixed strings), `S H` (high-bit-set ASCII strings).

## Legal

- `WDC`, `W65C02`, and `W65C02EDU` are names associated with Western Design Center, Inc.
- This project is independent and not affiliated with or endorsed by Western Design Center, Inc.
- This repository does not redistribute WDC tool binaries or WDC ROM images.
- Third-party references are listed in `THIRD_PARTY_NOTICES.md`.

## License

MIT. See `LICENSE`.
