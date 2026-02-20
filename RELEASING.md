# Releasing / Public Repo Checklist

Use this checklist before tagging a release or making the repository public.

## 1) Legal and Attribution

- Confirm `LICENSE` is present and correct (MIT).
- Confirm project headers use SPDX and `Copyright (c) 2026 95west.us`.
- Confirm third-party notice file is present:
  - `THIRD_PARTY_NOTICES.md`
- Confirm wording states this repo is independent from WDC and does not redistribute WDC binaries/ROMs.

## 2) Artifact Policy

Track only source and maintained docs.

Do **not** track generated tool outputs:

- `*.bin`
- `*.obj`
- `*.sym`
- `*.map`
- `*.lst`
- `*.log`
- `*.zip`

`DOCS/monitor_usage.pdf` is intentionally tracked as a distributable document generated from `DOCS/monitor_usage.html`.
`ZERO_PAGE_USAGE.pdf` is intentionally tracked as a distributable document generated from `ZERO_PAGE_USAGE.md`.

Quick check:

```powershell
git ls-files
```

## 3) Build and Docs

- Build passes:

```powershell
make -C SRC all
```

- If command behavior/output changed:
  - update `DOCS/monitor_usage.html`
  - regenerate `DOCS/monitor_usage.pdf` (headless Edge)
- If zero-page map/docs changed:
  - update `ZERO_PAGE_USAGE.md`
  - regenerate `ZERO_PAGE_USAGE.pdf`

## 4) Final Review

- `git status` has only intended files.
- No secrets, local paths, or private notes in tracked files.
- README reflects current commands and behavior.
