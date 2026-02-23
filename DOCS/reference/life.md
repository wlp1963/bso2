# Life (Userland) Reference

## Summary

`SRC/life.asm` is a standalone Conway's Game of Life program for `bso2`.

- Game type: Conway's Life
- Board: `8x8`, toroidal wraparound
- Build target: `make -C SRC life`
- Link/load address: `$5000`
- Output artifact: `SRC/life.s28`

## Build and Run

Build:

```powershell
make -C SRC life
```

Load via monitor:

1. Enter `L S`
2. Paste `SRC/life.s28`
3. Run `R5000`

Notes:

- `L G S` can auto-go depending on record entry/fallback behavior.
- `R5000` is the deterministic launch path for this target.

## Controls

- `N` or Enter: advance one generation
- `C`: clear board
- `G`: seed glider
- `B`: seed blinker
- `R`: randomize board
- `Q`: break/exit path

## Display

- `#` = live cell
- `.` = dead cell
- `GEN $hh` prints generation counter in hex

## Implementation Notes

- Standard Life rules: survival with 2 or 3 neighbors; birth with 3 neighbors.
- Two-board step model: `BOARD_CUR` -> `BOARD_NEXT` then copy.
- Uses shared `lib.lib` routines for terminal output.
- Source file header includes SPDX/license/provenance markers.
