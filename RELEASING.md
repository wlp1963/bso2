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

`DOCS/reference/monitor-usage.pdf` is intentionally tracked as a distributable document generated from `DOCS/reference/monitor-usage.html`.
`DOCS/reference/zero-page-usage.pdf` is intentionally tracked as a distributable document generated from `DOCS/reference/zero-page-usage.md`.

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
  - update `DOCS/reference/monitor-usage.html`
  - regenerate tracked docs:

```powershell
powershell -ExecutionPolicy Bypass -File tools/docs/regenerate-docs.ps1
```

- If zero-page map/docs changed:
  - update `DOCS/reference/zero-page-usage.md`
  - optional: regenerate zero-page HTML (requires `pandoc`):

```powershell
powershell -ExecutionPolicy Bypass -File tools/docs/regenerate-docs.ps1 -RegenerateMdHtml
```

## 4) Final Review

- `git status` has only intended files.
- No secrets, local paths, or private notes in tracked files.
- README reflects current commands and behavior.
