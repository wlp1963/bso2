# SPEC-0001: bso2 Capacity Expansion, Banking, and IVI Policy

Status: Draft (Rev 2)
Revision: 2
Date: 2026-02-23

## Contributors

- Researcher / Experimental Author: Walter (project author)

## Adoption Note

This document is a proposal, not an implementation commitment.
No immediate action is required by the author.
Any interested follower/contributor may adopt this work for their own use, or help advance it in collaboration with the author.

## Problem

`32K RAM / 32K ROM` is no longer sufficient for planned monitor growth.

## Goals

- Keep a stable recovery-capable monitor core.
- Expand capacity with banked design.
- Support safe vector mutation through staged/atomic workflows.
- Keep unsafe live-byte vector patching available only when explicitly policy-allowed.

## Deployment Profiles

### DEV_LAB

- Purpose: bring-up, experimentation, rapid diagnostics.
- Live-byte patching: allowed.
- Staged atomic path: recommended.
- Recovery requirement: best effort.

### OS_RUNTIME

- Purpose: dynamic runtime behavior and extensibility.
- Live-byte patching: allowed only under explicit guard/critical-window policy.
- Staged atomic path: required for normal operations.
- Recovery requirement: strong (rollback and rescue path required).

### CONTROLLER_SAFE

- Purpose: deterministic embedded control.
- Live-byte patching: disabled in normal operation.
- Staged atomic path: allowed via authorized API/programmatic command channel during commissioning; typically locked after commissioning.
- Recovery requirement: strong and simple.

### RECOVERY_SERVICE

- Purpose: unbrick/recovery/minimal service monitor.
- Live-byte patching: disabled.
- Staged atomic path: limited and audited.
- Recovery requirement: highest priority.

## VIA Timer1 Reference (8 MHz)

Timing model for W65C22 Timer1 free-run:

- Period: `T = (N + 1) * 125 ns`
- IRQ rate: `F = 8,000,000 / (N + 1) Hz`

Requested baseline values:

| N | Period | IRQ rate |
|---:|---:|---:|
| `0` | `125 ns` | `8,000,000 Hz` |
| `1` | `250 ns` | `4,000,000 Hz` |
| `2` | `375 ns` | `2,666,667 Hz` |
| `3` | `500 ns` | `2,000,000 Hz` |
| `4` | `625 ns` | `1,600,000 Hz` |
| `5` | `750 ns` | `1,333,333 Hz` |
| `6` | `875 ns` | `1,142,857 Hz` |
| `7` | `1.000 us` | `1,000,000 Hz` |
| `8` | `1.125 us` | `888,889 Hz` |
| `15` | `2.000 us` | `500,000 Hz` |
| `31` | `4.000 us` | `250,000 Hz` |
| `63` | `8.000 us` | `125,000 Hz` |
| `127` | `16.000 us` | `62,500 Hz` |
| `255` | `32.000 us` | `31,250 Hz` |
| `511` | `64.000 us` | `15,625 Hz` |
| `1023` | `128.000 us` | `7,812.5 Hz` |
| `2047` | `256.000 us` | `3,906.25 Hz` |
| `4095` | `512.000 us` | `1,953.125 Hz` |

Three values from `0xFFFF` downward at even-millisecond periods:

| N (hex) | Period | IRQ rate |
|---|---:|---:|
| `0xF9FF` | `8.000 ms` | `125 Hz` |
| `0xBB7F` | `6.000 ms` | `166.667 Hz` |
| `0x7CFF` | `4.000 ms` | `250 Hz` |

### Idea: Timer "Trim" Helper (No Implementation Commitment)

Proposed utility concept for operator speed during timer tuning:

- Accept a base timer value and a trim delta/step.
- Return nearby candidate values (for example odd-only or even-only increments) without requiring manual arithmetic each time.
- Support bounded output within `0x0000..0xFFFF`.
- Intended as an optional info/calibration aid, not a required runtime control path.

## User-Owned Memory Reservation Guidance

Reserve explicit user-owned zones and keep them stable across releases.

- Controller-safe baseline: `32-64` bytes of zero page and `512B-1KB` RAM scratch.
- OS/runtime-flex baseline: `64-128` bytes of zero page and `1-2KB` RAM scratch.
- bso2 current convention: user ZP at `$0090-$00FF` (`112` bytes).

Historical context (varies by ROM/firmware/configuration and should not be treated as fixed guarantees):

- Apple II family: zero page is heavily consumed by ROM/BASIC/OS use; typically only small pockets are safely free.
- Commodore 64: zero page is shared heavily with BASIC/KERNAL; practical free space is configuration-dependent.
- Ohio Scientific systems: some maps show higher ZP pockets available, but monitor/ROM usage still reduces practical free bytes.

Design requirement:

- Publish and maintain a stable "monitor-owned vs user-owned" memory contract.
- Changes to reserved ranges require explicit migration notes.

## Strategy Options

1. Refactor only (single bank).
2. Refactor plus bank access (`0/1/2` and free space).
3. Keep rescue compatibility path while moving bso2 primary behavior into banks.
4. Full replacement path for monitor baseline.
5. Hybrid transition: `3` now, `4` later.

## Recommended Direction

Adopt the hybrid path:

1. Near-term: bank expansion + stable rescue path.
2. Mid-term: staged/atomic vector tooling (`IVI` family).
3. Long-term: evaluate full replacement with clean provenance process.

## IVI Command Family (Draft)

- Naming provenance: `IVI` / `IVS` / `IVC` / `IVR` / `IVZ` were born from an AI suggestion phrase: **Inspect Vector Indirection**.
- `IVI`: inspect live/staged/previous vector state.
- `IVS <slot> <addr> [name]`: stage target.
- `IVC <slot|ALL>`: commit staged targets.
- `IVR <slot|ALL>`: rollback to previous committed targets.
- `IVZ <slot>`: clear staged target.

Slots: `RST`, `NMI`, `IRQ`, `BRK`, `HW`.

## Policy Statement

Unsafe live-byte vector patching is not the normal operations path.
It is profile-controlled and use-case-dependent.
