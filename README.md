# bso2

`bso2` is a serial monitor for the WDC W65C02EDU platform.

It provides boot flow control, memory tools, an interactive mini-assembler, disassembly, execution/resume debug flow, and vector inspection for a 65C02-based system.

> **Demo first:** [Open the curated `bso2` demo walkthrough](DOCS/demo_showcase.md)

## Name

- `bso2` = `Basic System Operations/2`
- The `/2` references the 6502 lineage.
- Stylized glyph note: `6` can stand in for lowercase `b`, and `5` can stand in for lowercase `s`.
- That makes `6502` a visual shorthand for `bso2` (`6`=`b`, `5`=`s`, `0`=`o`, `2`=`/2`).

## Features

- Target CPU: `W65C02` (`CHIP 65C02`)
- Table-driven command dispatcher
- Ring-buffer command parser
- Commands: `? H Z W D U A X G R N M F S L C Q V` ([reference](DOCS/monitor_usage.html))
- ZP map reference: [ZERO_PAGE_USAGE.md](ZERO_PAGE_USAGE.md) ([PDF](ZERO_PAGE_USAGE.pdf))
- Protected low RAM (`$0000-$03FF`) with force prefix `!`
- BRK debug context output with `CURR`/`NEXT` instruction lines and `STATE` line

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
- `DOCS/monitor_usage.html`: detailed command reference
- `DOCS/monitor_usage.pdf`: printable/offline command reference
- `ZERO_PAGE_USAGE.md`: zero-page map and reference index
- `ZERO_PAGE_USAGE.pdf`: printable zero-page reference
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

## Monitor Commands

- Quick list: `? H Z W D U A X G R N M F S L C Q V`
- Full reference (commands, edge cases, safety behavior): `DOCS/monitor_usage.html`
- Printable reference: `DOCS/monitor_usage.pdf`
- Zero-page reference: `ZERO_PAGE_USAGE.md`
- Printable zero-page reference: `ZERO_PAGE_USAGE.pdf`

## Demo Showcase

- **Curated demo walkthrough:** [DOCS/demo_showcase.md](DOCS/demo_showcase.md)
- Raw terminal capture: [bso2 demo.log](./bso2%20demo.log)

## Development

- Contributor guide: `CONTRIBUTING.md`
- Security policy: `SECURITY.md`
- Community conduct: `CODE_OF_CONDUCT.md`
- Release checklist: `RELEASING.md`
- Author stories/history: `STORIES.md`

## Roadmap / TODO

- Active milestones and TODOs: `STORIES.md`
- Detailed monitor policy/reference: `DOCS/monitor_usage.html` (section `9`)
- Current implementation is experimental; priorities may change as work progresses.

### Snapshot

- `Now`: keep pushing terminal/console I/O flow, extend `V` IRQ output with sub-dispatch lines (`BRK`/`HW`), and add post-link check for `END_KDATA < $F000`.
- `Soon`: get the ACIA port on the EDU board running.
- `Before publish`: complete XMODEM send/receive and staged vector commit flow.
- `Deferred`: compression/RLE/TX-ring architecture is postponed while `32K` FLASH headroom is sufficient.

## Legal

- `WDC`, `W65C02`, and `W65C02EDU` are names associated with Western Design Center, Inc.
- Official WDC sites: `https://wdc65xx.com/` and `https://westerndesigncenter.com/`
- This project is independent and not affiliated with or endorsed by Western Design Center, Inc.
- Disclosure: this is an independent personal project; no compensation, sponsorship, or endorsement has been received from Western Design Center, Inc.
- This repository does not redistribute WDC tool binaries or WDC ROM images.
- Third-party references are listed in `THIRD_PARTY_NOTICES.md`.
- `WDCMONv2` usage in this project is intended as wrapper/trampoline integration; any direct source reuse must be covered by upstream license/permission and documented in `THIRD_PARTY_NOTICES.md`.

## License

MIT. See `LICENSE`.

## Host Tool (`bso2com`)

Poor, poor, poor man's 5250 data stream / terminal emulation / file transfer for `bso2`.

Long-term direction: terminal/console/I-O first, with IBM-ish data-stream and file-transfer experiments.

```bash
make -C tools/bso2com
tools/bso2com/bso2com --port /dev/ttyUSB0 --baud 115200 term
```

- Source: `tools/bso2com/bso2com.c`
- Build rules: `tools/bso2com/Makefile`
- Full protocol/policy details: `DOCS/monitor_usage.html`
