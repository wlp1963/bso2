# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased] - 2026-02-18

### Added
- Added boot decision truth-table documentation for power-on and reset startup paths.
- Added patchable IRQ sub-dispatch trampolines:
  - `BRK_HOOK` at `$0089-$008B`
  - `HW_HOOK` at `$008C-$008E`
- Added `MONITOR_CLEAN` entry path for clean operator startup.
- Added `WARMSTART` banner marker on warm-no-vector startup path.
- Added `TERM_WIDTH_TIMEOUT` policy and prompt timing controls (`$007B`).
- Added `DELAY_333MS` helper for prompt timing cadence.

### Changed
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

### Docs
- Updated startup behavior, IRQ dispatch details, and fixed-byte maps in `DOCS/monitor_usage.html`.
- Updated zero-page reference in `ZERO_PAGE_USAGE.md` for new prompt/IRQ hook allocations.
- Regenerated tracked PDFs (`DOCS/monitor_usage.pdf`, `ZERO_PAGE_USAGE.pdf`) to match current docs.
- Updated `README.md` feature wording to include boot policy and IRQ sub-dispatch visibility.
