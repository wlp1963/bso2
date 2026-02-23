# Warmstart Restartability Test Plan

This plan validates reset warmstart behavior through startup selection (`C/W/M`) with the rule:
`RESET` + `W` must return to a clean monitor state and may print terse recovery hints for manual restart.

## Goals

1. Confirm reset-path warm (`C/W/M` -> `W`) does not implicitly resume interrupted context.
2. Confirm reset-path warm returns a clean command state (`D/U/M` repeat state cleared).
3. Confirm reset-path warm prints terse hints for interrupted safe commands (`D/U/S/F/C`) and last run entry (`R START`) when available.
4. Confirm `R` is context-sensitive:
   - no debug context: `R <START>` runs code,
   - debug context present: `R [A=..] [X=..] [Y=..]` resumes,
   - debug context present + explicit override: `!R <START>` force-runs and drops context.
5. Confirm `X` is reserved (no execute behavior).
6. Confirm power-on/invalid-cookie paths also avoid stale resume and stale hints.

## Preconditions

1. Build and load current monitor image.
2. Terminal connected and stable.
3. Hardware controls available: `NMI` and `RESET`.
4. Optional safety:

```text
I T0 0
```

## TC-01 Reset During Long Dump, Then Warm (`C/W/M` -> `W`)

1. Start long dump (example):

```text
D 4200 7EFF
```

2. Press `RESET` during output.
3. At startup prompt, select `W`.
4. Verify banner + `WARMSTART` + monitor prompt.
5. Re-run dump:

```text
D 42C0 439F
```

Pass:

1. Command executes as normal dump output.
2. No unexpected debug trap lines (`CURR/NEXT/STATE`) are printed.
3. If interrupted metadata was captured, a terse hint line appears (example: `HINT: D NEXT=$xxxx`).

## TC-02 Reset During Running Code, Then Warm (`C/W/M` -> `W`)

1. Load simple loop and run:

```text
M 3000 EA EA EA 4C 00 30
R 3000
```

2. Press `RESET`.
3. Select `W`.
4. At prompt, run:

```text
R
```

Pass:

1. `R` without args does not resume stale context.
2. Usage requires `R START` when no context.

## TC-03 `R` Dual-Mode Behavior

1. No debug context path:

```text
R 3000
```

Expect program starts.

2. Create debug context:

```text
R 3000
<press NMI>
```

3. Context hint path:

```text
R 3000
```

Expect:

```text
R CTX ACTIVE, R [A/X/Y=] or !R START
```

4. Resume path:

```text
R
```

5. Forced run path:

```text
!R 3000
```

Pass:

1. `R START` runs when no context.
2. Bare `R` resumes when debug context exists.
3. `R START` in active context prints the context hint unless forced.
4. `!R START` force-runs from address and clears old context.

## TC-04 `X` Reserved

1. At prompt:

```text
X
```

Pass:

1. Command is not treated as execute.
2. Monitor reports reserved/unknown behavior consistently.

## TC-05 Power-On / Invalid Cookie

1. Power cycle (or otherwise invalidate reset cookie).
2. At startup `C/M`, choose `M`.
3. Verify monitor stability; no automatic resume.

Pass:

1. No stale context resume attempt.
2. Commands can be issued normally.

## Evidence Capture

For each test case, record:

1. Timestamp.
2. Command transcript.
3. Observed prompt/output sequence.
4. Final verdict (`PASS`/`FAIL`) and short note.

## Regression Cadence

Run this matrix after changes touching:

1. `SYS_RST` / startup branch selection.
2. `WARM_NO_VECT` behavior.
3. `CMD_DO_RESUME` / run-resume parsing.
4. Command table / reserved command routing.
