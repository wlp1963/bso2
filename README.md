# bso2

`bso2` is a serial monitor for the WDC `W65C02EDU` platform.

It provides monitor commands, memory inspection/edit tools, mini-assembly/disassembly flow, debug/resume flow, and vector/IRQ sub-dispatch control for a 65C02-based system.

Demo first: [DOCS/demo_showcase.md](DOCS/demo_showcase.md)  
Current release marker: `R0M0V1I00`

## Features

- Target CPU: `W65C02` (`CHIP 65C02`)
- Table-driven command dispatcher and ring-buffer parser
- Command set: `? H Z T W D U A X G I R N M F S L C Q V`
- `L G S` / `LGS` load-go support for Motorola S-record workflows
- Dynamically updatable vector chains for `RST/NMI` and `IRQ/BRK`
- BRK debug context output with `CURR` / `NEXT` / `STATE` lines
- An `A`ssembler and `U`nassembler
- `G` Guess a number game

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

PuTTY note: disable `Auto wrap mode initially on` for clean monitor output alignment.

## Documentation

- Docs index: `DOCS/README.md`
- Command reference (canonical): `DOCS/reference/monitor-usage.html`
- Command reference (PDF): `DOCS/reference/monitor-usage.pdf`
- Zero-page map (canonical): `DOCS/reference/zero-page-usage.md`
- Zero-page map (PDF): `DOCS/reference/zero-page-usage.pdf`
- Demo walkthrough: `DOCS/demo_showcase.md`
- Hello World transcript: `DOCS/transcripts/hello-world.txt`

## Repository Layout

- `SRC/bso2.asm`: main monitor source
- `SRC/lib.asm`: shared monitor routines
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
```

Details: `tools/bso2com/README.md`

## License

MIT. See `LICENSE`.
