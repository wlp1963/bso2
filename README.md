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
- Commands: `Z C W M D U A X G R N F L Q V H ?`
- Protected low RAM (`$0000-$03FF`) with force prefix `!`
- BRK debug context output with previous and next instruction lines

## Project Notes

- Code size is not a current optimization target. This project currently assumes `32K RAM` and `32K ROM`.
- Some code in this repository was generated with AI assistance. The `SXB/EDU` board is being used as a blank canvas, and AI assistance was used to accelerate delivery of the primary project goals.
- This project does not claim heavy up-front design forethought. The long-term direction is known, but current implementation work is experimental and ideas are actively being tested.
- Suggestions are welcome.
- The only constant is that nothing is safe from change.

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
- `G` play a simple 3-try number guess game (`1..10`)
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
- Author stories/history: `STORIES.md`

## Future / TODO

### Roadmap Status

- This section captures planned architecture and upcoming work.
- The project is intentionally experimental. Items may be reordered, replaced, or removed as implementation progresses.

### Command Model (Approved Direction)

- The preferred grammar is `noun verb [args...]` (namespace first, action second).
- Primary namespaces currently approved for growth: `B I T J X S M O`.
- `TIME` is moving under `INFO`: `I T ...`.
- `T` is being repurposed for terminal-related commands.
- Top-level `P` and `V` are being freed for namespace pressure.
- PIA and VIA are moving under `INFO -> IO`:
- `I O P ...` for PIA operations.
- `I O V ...` for VIA operations.
- VIA timers are planned under the VIA tree (for example `I O V T ...` shape).

### Parser and Input Compatibility

- Keep short monitor UX, but normalize equivalent forms internally.
- `X S` and `XS` should resolve to the same command key.
- `M D` and `MD` should resolve to the same command key.
- `I O V` and `IOV` should resolve to the same command key.
- One canonical dispatch key format will be used internally to avoid duplicated handlers.
- Alias windows will be used during migration so old commands still run while new forms are introduced.

### Search Family (`S`) Plan

- Add and expand the `S` search family.
- Planned parser shape: `S C START END <text>` and `S B START END <pattern...>`.
- `S C` (C-string text mode):
- Unquoted text stops at first whitespace.
- Quoted text supports delimiters such as `"`, `'`, and `` ` ``.
- Delimiter escaping uses doubled delimiter characters.
- Wildcards are enabled in `S C` text patterns.
- `?` matches exactly one character.
- `*` matches zero or more characters.
- `??` matches a literal `?` character.
- `**` matches a literal `*` character.
- `S B` (binary mode):
- `HH` byte token (`00..FF`).
- `HHHH` word/address token searched little-endian (example: `8000` => `00 80`).
- `HL` nibble wildcard token where each nibble is hex or `?` (`?A`, `A?`, `??`).
- `*` single-byte wildcard (`*` and `??` are equivalent wildcard bytes).
- Later candidates: `S P` (Pascal length-prefixed strings), `S H` (high-bit-set ASCII strings).

### Transfer / XMODEM Plan

- At least XMODEM receive and send are required before publish.
- Current direction is to keep transfer commands grouped under `X`.
- `X R` for receive and `X S` for send are the preferred forms.
- Space and fused forms should both work (`X S` == `XS`, `X R` == `XR`).

### Flash / Bank Plan

- `B` is reserved for Bank/FLASH access.
- FLASH flows should include read/program/erase/verify style operations under `B` subverbs.
- FLASH operations are considered critical sections and must integrate with vector/NMI safety rules.

### Vector + Safety Direction (Required Before Publish)

- Vector updates must support dynamic and atomic behavior.
- Critical windows (vector commit and FLASH routines) should visibly warn the user.
- During critical windows, all EDU LEDs should flash as a "do not press NMI" signal.
- NMI handling should be guarded/deferred during critical windows rather than entering the normal debug flow.
- A staged update plus atomic commit mechanism is the current direction for vector changes.

### Open Design Item (Deferred)

- `O` command purpose is intentionally deferred to a later decision.
- Candidate use: chaining multiple operations on one line as an execution wrapper.
- If adopted, `O` still needs explicit policy for error handling (`stop on error` vs `continue`) and guard semantics.

### Additional Backlog Ideas

- Possible parser fork path (future): full string-driven command grammar instead of strict single-letter roots.
- Games backlog and demos: `Mastermind`, `Conway's Life`, `Tic-Tac-Toe`.

## Legal

- `WDC`, `W65C02`, and `W65C02EDU` are names associated with Western Design Center, Inc.
- This project is independent and not affiliated with or endorsed by Western Design Center, Inc.
- This repository does not redistribute WDC tool binaries or WDC ROM images.
- Third-party references are listed in `THIRD_PARTY_NOTICES.md`.

## License

MIT. See `LICENSE`.
