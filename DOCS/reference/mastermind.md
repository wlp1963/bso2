# Mastermind (Userland) Reference

## Summary

`SRC/mastermind.asm` is a standalone 6502 userland game for `bso2`.

- Game type: 4-peg hex Mastermind (`0-9`, `A-F`)
- Build target: `make -C SRC mastermind`
- Link/load address: `$1000`
- Output artifact: `SRC/mastermind.s28`

## Build and Run

Build:

```powershell
make -C SRC mastermind
```

Load via monitor:

1. Enter `L S`
2. Paste `SRC/mastermind.s28`
3. Run `R1000`

Notes:

- `L G S` can auto-go depending on record entry/fallback behavior.
- `R1000` is the deterministic launch path for this target.

## Controls

- Input: hex keys only (`0-9`, `A-F`, case-insensitive)
- Delimiters: Enter is optional; any non-hex separator may submit/reset input state
- Guess length: exactly 4 hex digits
- Tries: 8 per round

## Scoring

- `Exact`: right digit, right position
- `Near`: right digit, wrong position

Round ends on:

- Win: `Exact=4`
- Lose: tries exhausted; code is printed, then new game starts

## Implementation Notes

- Uses shared `lib.lib` routines for text I/O and conversion.
- Uses timer registers as lightweight entropy for per-round secret generation.
- Source file header includes SPDX/license/provenance markers.
