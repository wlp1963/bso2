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
- BRK debug context output with current/next instruction line and state line

## Project Notes

- Code size is not a current optimization target. This project currently assumes `32K RAM` and `32K ROM`.
- Some code in this repository was generated with AI assistance. The `SXB/EDU` board is being used as a blank canvas, and AI assistance was used to accelerate delivery of the primary project goals.
- This project does not claim heavy up-front design forethought. The long-term direction is known, but current implementation work is experimental and ideas are actively being tested.
- Suggestions are welcome.
- If you build something useful from this project, I'd love to hear about it and follow your work.
- The only constant is that nothing is safe from change.
- "Perfect is the enemy of good." - Voltaire
- Milestone: David Gray (WDC) invited to this repository at `2026-02-17T14:21:00-06:00`.
- Milestone: historical RLE design-note rationale documented at `2026-02-16T00:45:50-06:00`.

## Repository Layout

- `SRC/bso2.asm`: main monitor source
- `SRC/macros.inc`: assembler macros
- `SRC/Makefile`: build/upload/clean targets
- `tools/bso2com/bso2com.c`: unified Linux GNU C serial utility
- `tools/bso2com/Makefile`: build rules for `bso2com`
- `DOCS/monitor_usage.html`: detailed command reference
- `DOCS/monitor_usage.pdf`: printable/offline command reference
- `DOCS/demo_showcase.md`: curated demo script from live session
- `bso2 demo.log`: raw PuTTY capture used for demo/showcase

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

## Host Tool (`bso2com`)

Use the unified Linux GNU C helper for terminal mode, `L B` loading, and flash protocol actions:

```bash
make -C tools/bso2com
tools/bso2com/bso2com --port /dev/ttyUSB0 --baud 115200 term
tools/bso2com/bso2com lb-send payload.bin --addr 1000
tools/bso2com/bso2com flash-check
tools/bso2com/bso2com flash-clear --force --confirm ERASE
```

### Usage Examples (`bso2com`)

```bash
# 1) Build the host tool
make -C tools/bso2com

# 2) Serial terminal mode (quit with Ctrl-])
tools/bso2com/bso2com --port /dev/ttyUSB0 --baud 115200 term

# 3) Send raw payload to monitor L B path
tools/bso2com/bso2com --port /dev/ttyUSB0 lb-send app.bin --addr 1000

# 4) Flash maintenance commands
tools/bso2com/bso2com --port /dev/ttyUSB0 flash-check
tools/bso2com/bso2com --port /dev/ttyUSB0 flash-clear --force --confirm ERASE
tools/bso2com/bso2com --port /dev/ttyUSB0 flash-write --addr 8000 --in app.bin --force --confirm WRITE
tools/bso2com/bso2com --port /dev/ttyUSB0 flash-read --addr 8000 --len 0100 --out dump.bin
tools/bso2com/bso2com --port /dev/ttyUSB0 flash-update-monitor --in monitor.bin --force --confirm UPDATE
```

Requirements:

- Linux/Unix serial device (for example `/dev/ttyUSB0`)
- GNU C toolchain (`gcc`, `make`)
- POSIX userspace headers/libs (`termios`, `select`, `unistd`)

Linux dependency examples:

```bash
# Debian/Ubuntu
sudo apt-get install build-essential

# Fedora
sudo dnf install gcc make glibc-devel
```

### WSL2 Quick Start (`Ubuntu` on `Win11 Pro`)

```powershell
# Windows PowerShell (admin): attach USB serial adapter to WSL
usbipd list
usbipd bind --busid <BUSID>
usbipd attach --wsl --busid <BUSID>
```

```bash
# Ubuntu (WSL): verify device, build, run
ls /dev/ttyUSB* /dev/ttyACM* /dev/ttyS* 2>/dev/null
make -C tools/bso2com
tools/bso2com/bso2com --port /dev/ttyUSB0 --baud 115200 term
```

If needed for permissions:

```bash
sudo usermod -aG dialout $USER
```

## Monitor Commands

- `?` short help
- `H [A|P|M|S]` help index/all/protection/memory-steering sections
- `Z` clear RAM (confirm `Y/N`)
- `W` warm start to monitor
- `D [START [END]]` dump memory (`END` inclusive)
- `U [START END]` disassemble a 65C02 range (`END` inclusive; bare `U` repeats from saved next-instruction address)
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

### Usage Examples (`bso2` Monitor)

```text
# Basic memory workflow
D 1000 10FF
M 1000 A9 01 8D 00 20
F 1100 11FF 00
C 1000 10FF 1200

# Load and run
L B 1000 0200
X 1000

# Debug/resume flow
N
R

# Utility
V
H A
```

Notes:
- In monitor command mode, up-arrow (`ESC [ A`) repeats the previous command.
- Special case: if the previous command was a `D ...` or `U ...` form, up-arrow replays bare `D`/`U`; `D` continues by saved span, `U` continues by saved next-instruction address.
- On boot selection flow, terminal width prompt accepts single-key `4/8/1` for `40/80/132`; `W/M` restore prior width before prompt, while `C` starts from default `80`.
- `A` supports relative-branch target entry via absolute hex address (range checked).
- `A` supports explicit accumulator syntax such as `INC A`.
- Fixed-address contract: PAGE0 is anchored at `$0040`; pinned bytes include `GAME_ASK_PENDING=$0088`, `RST_HOOK=$0089`, `NMI_HOOK=$008C`, `IRQ_HOOK=$008F`, `BRK_FLAG=$0092`, `TERM_COLS=$0093`; hardware vectors remain `NMI=$FFFA`, `RST=$FFFC`, `IRQ/BRK=$FFFE`.
- `V` vector-chain format uses spaced arrows (`RST: FFFC > F818 > 8004 > 9F31 > [0089] > 800D`); bracketed links use `[addr16]` and indicate a patchable 16-bit RAM trampoline address.
- Vector naming contract (draft): each patchable target must export both `<HANDLER>` (entry) and `<HANDLER>_NAME` (ASCIIZ label); vector retarget operations must update both target address and name pointer as one logical transaction.
- Safety warning: direct live writes with `!M` to hook bytes (`$0089-$0091`, especially `$008C-$008E`) are non-atomic and debug-only. Possible side effects include mixed-byte jumps, hangs/crashes, wrong routine dispatch, and vector-name display mismatch while bytes are mid-update.
- See `DOCS/monitor_usage.html` for full behavior, macro parameters, and callable function API.

## Demo Showcase

- Raw log: `bso2 demo.log` (PuTTY capture)
- Curated walkthrough: `DOCS/demo_showcase.md`

Quick excerpt:

```text
POWER ON
TERM WIDTH 4=40 8=80 1=132 [8]?
RAM CLEARED
...
-H
HELP:? H  CTRL:Q W Z  EXEC:G N R X  MEM:A C D F L M P U V
...
-X1000
CURR: 1006: BRK #$00    NEXT: 1008: INY
STATE:[BRK] 00 PC:1008 A:3E X:FF Y:7F P:35 [nv-BdIzC] S:F9
```

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
- Process note: current host/WSL2 hardening, ergonomics, and safety polish are tracked as TODO items for now because there is already running functional code.

### Command Model (Approved Direction)

- The preferred grammar is `noun verb [args...]` (namespace first, action second).
- Primary namespaces currently approved for growth: `B I T J X S M O`.
- `TIME` is moving under `INFO`: `I T ...`.
- Calculator is planned under `INFO`: `I C ...` (prefer RPN input style).
- `T` is being repurposed for terminal-related commands.
- Top-level `P` and `V` are being freed for namespace pressure.
- PIA and VIA are moving under `INFO -> IO`:
- `I O P ...` for PIA operations.
- `I O V ...` for VIA operations.
- VIA timers are planned under the VIA tree (for example `I O V T ...` shape).

### Parser and Input Compatibility

- Keep short monitor UX, but normalize equivalent forms internally.
- Parser policy: token 1 selects namespace and stays locked for the line; no cross-namespace fallback after root selection.
- `X S` and `XS` should resolve to the same command key.
- `M D` and `MD` should resolve to the same command key.
- `I O V` and `IOV` should resolve to the same command key.
- `I C` and `IC` should resolve to the same command key.
- One canonical dispatch key format will be used internally to avoid duplicated handlers.
- Aliases are spelling variants only (same meaning); command override semantics are not used.
- Alias windows will be used during migration so old commands still run while new forms are introduced.

### Active TODO (Pressing)

#### Now

- TODO: extend `V` IRQ display to show sub-dispatch targets on separate lines (`BRK:` and `HW:`).
- TODO: add post-link map check that enforces `END_KDATA < $F000`.

#### Before Publish

- TODO: provide XMODEM receive and send paths before publish (`X R`/`XR`, `X S`/`XS`).
- TODO: implement staged vector update + atomic commit flow for runtime retargeting.
- TODO: enforce critical-window behavior for FLASH/vector mutation paths (LED warning + NMI guard/defer).
- TODO: enforce dangerous `B` operation policy (`!` required + explicit confirmation + fail-closed without mutation).
- TODO: add deterministic status reporting for dangerous operations (`OK`, `ABORTED`, `VERIFY_FAIL`, `FLASH_FAIL`, `DENIED` plus status byte).
- Deferred (not current TODO): text compression/decompression, RLE/tokenization, and TX ring architecture are postponed while 32K FLASH headroom is sufficient.

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
- `bso2` will use `WDCMONv2` FLASH routines through wrappers/trampolines.
- Integration intent is behavioral/protocol compatibility via wrapper entry points, not direct source copy/paste.

### Flash / Bank Safety Policy (Critical, Non-Negotiable)

- `B` must not execute dangerous operations by default.
- Any `B` operation that mutates FLASH state or vector state requires both:
- force prefix `!`
- explicit user confirmation
- Dangerous operations include at minimum: erase, program/write, monitor self-update, vector commit, and bank-activation/commit transitions.
- If `!` is absent for a dangerous operation, the command must fail closed with no side effects.
- Confirmation must be operation-specific (for example typed intent token), not a generic implicit continue.
- During dangerous `B` operations:
- enter critical guard mode before mutation starts
- visibly indicate critical mode (all LEDs flashing)
- guard/defer NMI debug flow until critical mode exits
- On any verify/check failure, abort the operation, exit critical mode cleanly, and report explicit status.
- Mandatory status reporting for dangerous `B` operations:
- final status code byte
- textual result (`OK`, `ABORTED`, `VERIFY_FAIL`, `FLASH_FAIL`, `DENIED`)

### Board Self-Update Policy (FLASH/Monitor Update Path)

- Board self-update is always treated as a dangerous operation.
- Self-update requires `!` plus explicit confirmation before erase/program.
- Self-update must present pre-commit details before confirmation:
- target region
- byte count
- integrity value (checksum/hash) when available
- Self-update flow should be staged and commit-oriented:
- preflight validation
- stage payload
- erase/program
- verify
- commit/activate
- Any update path must preserve a recovery strategy (do not rely on in-place blind overwrite as the only path).
- Self-update path is in scope of the non-changing mandate:
- any operation that mutates FLASH/vector state must assert critical indication/guard behavior
- this includes module/transient load and activation flows

### Host Tooling Direction (Linux GNU C)

- Critical FLASH workflows are expected to have a Linux GNU C host path.
- Python helper scripts may exist for convenience, but they are not the required path for critical FLASH operations.
- Preferred host implementation model:
- raw serial + protocol wrappers in C
- explicit timeout/error handling
- deterministic status reporting matching monitor status codes

### Memory Residency Policy (Working Contract)

- Keep `CODE` anchored at `$8000`.
- Let `KDATA` float immediately behind `CODE`.
- Treat bank 0 low 64K as always-resident core: reset path, ISR stubs, vector/dispatch tables, bank-switch glue, and minimal monitor.
- Keep `KDATA` and other core tables below `$F000` (`$F000-$FFFF` remains reserved for ROM/vector/system assumptions).
- Place feature-heavy code/data in flash bank 1/2 as callable modules.
- Keep stable entry trampolines in bank 0 for cross-bank calls.
- Keep vector targets in bank 0; vectors must not point directly into switchable banks.
- Add an enforced build check later for `END_KDATA < $F000` (post-link map check is the reliable method with this toolchain).

### Vector + Safety Direction (Required Before Publish)

- Vector updates must support dynamic and atomic behavior.
- Critical windows (vector commit and FLASH routines) should visibly warn the user.
- During critical windows, all EDU LEDs should flash as a "do not press NMI" signal.
- NMI handling should be guarded/deferred during critical windows rather than entering the normal debug flow.
- A staged update plus atomic commit mechanism is the current direction for vector changes.
- NMI retargeting direction: avoid in-place patching; use two complete slots and commit with a single-byte active-slot selector flip.
- Direct `!M` edits to live vector hook bytes remain available for debug bring-up, but are explicitly unsafe for production/runtime patch flow.
- Mandate (non-changing requirement): any operation that updates FLASH state or vector state must assert critical indication/guard behavior, including module/transient load paths; implementation details may change, but this requirement does not.

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
- `WDCMONv2` usage in this project is intended as wrapper/trampoline integration; any direct source reuse must be covered by upstream license/permission and documented in `THIRD_PARTY_NOTICES.md`.

## License

MIT. See `LICENSE`.
