# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased] - 2026-02-23

### Added
- Added terminal width option `20` (`T 20`) and updated terminal command usage to `T [C|20|40|80|132]`.
- Added input overflow handling: overflow now drops the in-flight line to CR/LF resync and reports `INPUT OVERFLOW; LINE DROPPED`.
- Added independently assembled Hello World demo artifacts (`SRC/hello-world.asm`, `SRC/hello-world.s28`) and verification transcript (`DOCS/transcripts/hello-world.txt`) for S28 load-and-run flow.
- Added `L G S` / `LGS` load-go workflow: load Motorola S-records and auto-execute on successful load.
- Added monitor command `T` for terminal clear (`LF` x24 then `CR`).
- Added boot decision truth-table documentation for power-on and reset startup paths.
- Added reusable utility library sources/artifacts (`SRC/lib.asm`, `SRC/lib.lib`) for shared monitor/demo helper routines.
- Added patchable IRQ sub-dispatch trampolines:
  - `BRK_HOOK` at `$0089-$008B`
  - `HW_HOOK` at `$008C-$008E`
- Added `MONITOR_CLEAN` entry path for clean operator startup.
- Added `WARMSTART` banner marker on warm-no-vector startup path.
- Added `TERM_WIDTH_TIMEOUT` policy and prompt timing controls (`$007B`).
- Added `DELAY_333MS` helper for prompt timing cadence.
- Added full regression transcript capture `DOCS/transcripts/regression testing 1.txt`.
- Added curated regression report `DOCS/transcripts/regression-testing-1-curated.html`.

### Changed
- Bumped current release marker to `R0M0V2I01` (`I` = `INTERNAL`).
- Removed monitor command-mode warmstart (`W`) from command dispatch; warm path remains available via reset startup selection (`C/W/M`).
- Expanded command parser limit from `CMD_MAX_LEN=31` to `CMD_MAX_LEN=64`.
- `T` command now owns terminal control: clear (`T`/`T C`) plus width set (`20/40/80/132`).
- Output wrapping now enforces configured `TERM_COLS` globally via `WRITE_BYTE`.
- Standardized project release marker format to `R#M#V#I##`; bumped current marker to `R0M0V1I01` (`I` = `INTERNAL`).
- Changed startup defaults to `I T0 1` and `I I 1`:
- Timer1 free-run heartbeat is now enabled by default at boot.
- CPU IRQ handling is now enabled by default at boot.
- `I T0` and `I I` now report current state when `0|1` is omitted.
- Separated include files out of `SRC/` into `INCLUDES/` and updated assembler include paths for monitor and demo builds.
- Broke hardware/constants definitions out into `INCLUDES/equates.inc`; `SRC/bso2.asm` now includes equates through a single include.
- `INCLUDES/equates.inc` now serves as the include entry point for monitor builds and automatically includes `macros.inc`.
- Extracted stable utility routines from `SRC/bso2.asm` into `SRC/lib.asm`; build now creates/links `lib.lib` for `bso2` and `hello-world`.
- Split startup selection behavior:
  - Power-on uses `C/M`, 6-second wait, `>` ticks, timeout defaults to `C`.
  - Reset-cookie uses `C/W/M`, 6-second wait, `<` ticks, timeout defaults to `M`.
- Boot-choice and clear-confirm prompt input now echoes uppercase.
- `POWER ON -> M` now writes reset cookie (`WDC\0`) so next reset follows cookie-aware path.
- `V` output now shows IRQ sub-dispatch path with explicit BRK/HW targets and updated dispatch label text.
- Search hit rendering now marks cross-row matches with `*` and prints aligned context rows.

### Fixed
- Fixed repeated game prompt re-arming by separating `RNG_STATE` from `GAME_ASK_PENDING` (`$0078`) and removing NMI-based re-arm.
- Updated game prompt text to include latch address: `WANT TO PLAY A GAME (@$78)?`.
- Fixed `GAME_ASK_PENDING` reset policy: default/set to `01` only on power-on/invalid-cookie path; warm reset now preserves live `$0078` state.
- Fixed reset clear-memory startup policy: `C` then confirm `Y` now returns through cold-start monitor entry (no `WARMSTART` marker on that path).
- Differentiated reset startup monitor path (`M`) with explicit `MONITORSTART` marker so it is visibly distinct from reset warm path (`W`).
- Fixed `R` context ergonomics: address-form `R START` now emits explicit context hint (`R CTX ACTIVE, R [A/X/Y=] or !R START`) and `!R START` force-runs from address while dropping stale debug context.
- Fixed prompt LED blink state corruption by moving blink state/counter handling to stable ZP scratch.
- Clarified and documented row-boundary search hit behavior.
- Fixed `LGS` auto-run behavior for S-records with zero termination entry (`S7/S8/S9 = 0000`) by falling back to the first data-record address.

### Docs
- Updated canonical docs to match current startup/command behavior (`T` forms, no command `W`, `$0000-$0FFF` protection, `TERM_COLS` values including 20-column mode, `CMD_MAX_LEN=64`, overflow policy).
- Updated warmstart test plan to use reset startup warm path (`C/W/M -> W`) instead of removed command-mode `W`.
- Updated runtime banner and documentation banner/transcript references from `v0 . 9` to `R0M0V0I00`.
- Documented `I T0 [0|1]` and `I I [0|1]` query/toggle behavior in `DOCS/reference/monitor-usage.html`.
- Documented default startup posture (`I T0 1`, `I I 1`) and heartbeat timing math:
- T0 base rate `122.0703125 Hz` (`8.192 ms` period).
- LED overlay toggle rate `0.476837158 Hz` (~`2.097 s` per edge).
- Full LED blink cycle `0.238418579 Hz` (~`4.194 s` period).
- Updated command lists and command reference docs for `T` terminal clear behavior.
- Documented include model: prefer `INCLUDE EQUATES.INC`; macros are included automatically via equates.
- Updated startup behavior, IRQ dispatch details, and fixed-byte maps in `DOCS/reference/monitor-usage.html`.
- Updated zero-page reference in `DOCS/reference/zero-page-usage.md` for new prompt/IRQ hook allocations.
- Updated `R` command docs and warmstart test plan to cover active-context warning text and `!R START` force-run override.
- Regenerated tracked PDFs (`DOCS/reference/monitor-usage.pdf`, `DOCS/reference/zero-page-usage.pdf`) to match current docs.
- Updated `README.md` feature wording to include boot policy and IRQ sub-dispatch visibility.
- Added docs index links for regression transcript artifacts.

### Complete Delta Since `fbd69e0` (`Cleanup edits`)
- `CHANGELOG.md`
- `DOCS/README.md`
- `DOCS/demo_showcase.md`
- `DOCS/reference/monitor-usage.html`
- `DOCS/reference/monitor-usage.pdf`
- `DOCS/reference/zero-page-usage.md`
- `DOCS/reference/zero-page-usage.pdf`
- `DOCS/reference/warmstart-test-plan.md` (new)
- `DOCS/transcripts/regression testing 1.txt` (new)
- `DOCS/transcripts/regression-testing-1-curated.html` (new)
- `INCLUDES/equates.inc`
- `README.md`
- `SRC/bso2.asm`
- `SRC/hello-world.s28`
- `SRC/lib.asm`
- `SRC/lib.lib`
- `STORIES.md`
