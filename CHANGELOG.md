# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased] - 2026-02-21

### Added
- Added independently assembled Hello World demo artifacts (`SRC/hello-world.asm`, `SRC/hello-world.s28`) and verification transcript (`DOCS/transcripts/hello-world.txt`) for S28 load-and-run flow.
- Added `L G S` / `LGS` load-go workflow: load Motorola S-records and auto-execute on successful load.
- Added monitor command `T` for terminal clear (`LF` x24 then `CR`).
- Added boot decision truth-table documentation for power-on and reset startup paths.
- Added patchable IRQ sub-dispatch trampolines:
  - `BRK_HOOK` at `$0089-$008B`
  - `HW_HOOK` at `$008C-$008E`
- Added `MONITOR_CLEAN` entry path for clean operator startup.
- Added `WARMSTART` banner marker on warm-no-vector startup path.
- Added `TERM_WIDTH_TIMEOUT` policy and prompt timing controls (`$007B`).
- Added `DELAY_333MS` helper for prompt timing cadence.

### Changed
- Standardized project release marker format to `R#M#V#I##` and set current marker to `R0M0V1I00`.
- Broke hardware/constants definitions out into `INCLUDES/equates.inc`; `SRC/bso2.asm` now includes equates through a single include.
- `INCLUDES/equates.inc` now serves as the include entry point for monitor builds and automatically includes `macros.inc`.
- Split startup selection behavior:
  - Power-on uses `C/M`, 6-second wait, `>` ticks, timeout defaults to `C`.
  - Reset-cookie uses `C/W/M`, 6-second wait, `<` ticks, timeout defaults to `M`.
- Boot-choice and clear-confirm prompt input now echoes uppercase.
- `POWER ON -> M` now writes reset cookie (`WDC\0`) so next reset follows cookie-aware path.
- `V` output now shows IRQ sub-dispatch path with explicit BRK/HW targets and updated dispatch label text.
- Search hit rendering now marks cross-row matches with `*` and prints aligned context rows.

### Fixed
- Fixed prompt LED blink state corruption by moving blink state/counter handling to stable ZP scratch.
- Clarified and documented row-boundary search hit behavior.
- Fixed `LGS` auto-run behavior for S-records with zero termination entry (`S7/S8/S9 = 0000`) by falling back to the first data-record address.

### Docs
- Updated runtime banner and documentation banner/transcript references from `v0 . 9` to `R0M0V0I00`.
- Documented `I T0 0|1` (Timer1 heartbeat toggle) and `I I 0|1` (CPU IRQ enable/disable) in `DOCS/reference/monitor-usage.html`.
- Updated command lists and command reference docs for `T` terminal clear behavior.
- Documented include model: prefer `INCLUDE EQUATES.INC`; macros are included automatically via equates.
- Updated startup behavior, IRQ dispatch details, and fixed-byte maps in `DOCS/reference/monitor-usage.html`.
- Updated zero-page reference in `DOCS/reference/zero-page-usage.md` for new prompt/IRQ hook allocations.
- Regenerated tracked PDFs (`DOCS/reference/monitor-usage.pdf`, `DOCS/reference/zero-page-usage.pdf`) to match current docs.
- Updated `README.md` feature wording to include boot policy and IRQ sub-dispatch visibility.
