# bso2

`bso2` is a serial monitor for the WDC `W65C02EDU` platform.

It provides monitor commands, memory inspection/edit tools, mini-assembly/disassembly flow, debug/resume flow, and vector/IRQ sub-dispatch control for a 65C02-based system.

Demo first: [DOCS/demo_showcase.md](DOCS/demo_showcase.md)  
Current release marker: `R0M0V2I01` (`I` = `INTERNAL`)

## Features

- Target CPU: `W65C02` (`CHIP 65C02`)
- Table-driven command dispatcher and ring-buffer parser
- Startup prompts: reset-cookie `C/W/M`, power-on `C/M`; `W` enters warm-recovery monitor (terse restart hints), `M` enters clean monitor
- Commands:
  -  `?`     Short Help
  -  `H`     Long Help
  -  `Z`     Zero Memory
  -  `T`     Terminal (`T`, `T C`, `T 20|40|80|132`)
  -  `D`     Dump Memory
  -  `U`     Unassemble
  -  `A`     Assemble
  -  `X`     Reserved
  -  `G`     Game
  -  `I`     Info/Inspect 
  -  `R`     Run (`R addr`) or Resume (in DEBUG); use `!R addr` to force-run when context is active
  -  `N`     Next (in DEBUG)
  -  `M`     Modify Memory, bulk and Interactive
  -  `F`     Fill Memory
  -  `S`     Search Memory
  -  `L`     Load
  -  `Q`     Quit
  -  `V`     Vectors

- S28 Support
- Dynamically updatable vector chains for `RST/NMI` and `IRQ/BRK`
- BRK debug context output with `CURR` / `NEXT` / `STATE` lines
- Default runtime posture at boot: `I T0 1` (Timer1 free-run ON) and `I I 1` (CPU IRQ enabled)
- EDU heartbeat timing: T0 base `122.0703125 Hz` (`8.192 ms`), LED overlay toggle `0.476837158 Hz` (~`2.097 s`/edge), full blink cycle `0.238418579 Hz` (~`4.194 s`)

## Quick Start

Requirements:
- WDC tools in `PATH`: `WDC02AS`, `WDCLN`
- Python for `make -C SRC upload`
- `SRC/Makefile` serial config set for your host (currently `COM3`)

Build:

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

Docs regen:

```powershell
powershell -ExecutionPolicy Bypass -File tools/docs/regenerate-docs.ps1
```

PuTTY note: disable `Auto wrap mode initially on` for clean monitor output alignment.

## Documentation

- Docs index: `DOCS/README.md`
- Command reference (canonical): `DOCS/reference/monitor-usage.html`
- Command reference (PDF): `DOCS/reference/monitor-usage.pdf`
- Zero-page map (canonical): `DOCS/reference/zero-page-usage.md`
- Zero-page map (PDF): `DOCS/reference/zero-page-usage.pdf`
- Warmstart resume test plan: `DOCS/reference/warmstart-test-plan.md`
- Demo walkthrough: `DOCS/demo_showcase.md`
- Milestone `R0M0V2I01`: `DOCS/milestones/R0M0V2I01.md`
- Hello World transcript: `DOCS/transcripts/hello-world.txt`
- Mastermind userland reference: `DOCS/reference/mastermind.md`
- Life userland reference: `DOCS/reference/life.md`

## Repository Layout

- `SRC/bso2.asm`: main monitor source
- `SRC/lib.asm`: shared monitor routines
- `SRC/mastermind.asm`: standalone userland Mastermind game (`$1000`)
- `SRC/life.asm`: standalone userland Life game (`$5000`)
- `INCLUDES/equates.inc`: hardware/address equates and constants
- `INCLUDES/macros.inc`: assembler macros
- `SRC/Makefile`: build/upload rules
- `tools/bso2com/`: host-side serial helper utility

## Development

- Changelog: `CHANGELOG.md`
- Contributing: `CONTRIBUTING.md`
- Security: `SECURITY.md`
- Code of conduct: `CODE_OF_CONDUCT.md`
- Releasing: `RELEASING.md`
- Stories / roadmap tracking: `STORIES.md`

## Roadmap

Active milestones and TODOs are maintained in `STORIES.md`.

## Legal

- `WDC`, `W65C02`, and `W65C02EDU` are names associated with Western Design Center, Inc.
- This project is independent and is not affiliated with or endorsed by Western Design Center, Inc.
- Third-party references and notices: `THIRD_PARTY_NOTICES.md`

## Host Tool (`bso2com`)

`bso2com` is an experimental host utility for terminal and transfer workflows.

```bash
make -C tools/bso2com
tools/bso2com/bso2com --port /dev/ttyUSB0 --baud 115200 term
tools/bso2com/bso2com ls-send game.s28
tools/bso2com/bso2com lgs-send game.s28
```

Details: `tools/bso2com/README.md`

## License

MIT. See `LICENSE`.
