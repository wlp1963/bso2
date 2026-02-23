# Contributing

Thanks for contributing to `bso2`.

## Prerequisites

- WDC assembler/linker available in `PATH` (`WDC02AS`, `WDCLN`)
- Windows PowerShell environment (current scripts/Makefile are Windows-oriented)

## Workflow

1. Create a feature branch.
2. Small commits are preferred (one behavior change per commit when practical).
3. This is guidance, not strict enforcement; the maintainer does not always follow it perfectly.
4. If you plan to work in large, sweeping changes as your normal style, please fork and maintain that style there.
5. Update docs if command behavior or monitor output changes.
6. Run a full build before opening a PR:

```powershell
make -C SRC all
```

## Coding Notes

- Keep source ASCII unless there is a strong reason not to.
- Preserve existing assembly style and label naming patterns.
- Prefer `INCLUDE EQUATES.INC` for monitor-oriented sources; it already includes `MACROS.INC`.
- Include `MACROS.INC` directly only for standalone code that intentionally skips project equates.
- Add comments for non-obvious logic, not for line-by-line narration.
- Avoid broad refactors unrelated to the requested behavior.

## Testing Expectations

- Build must pass (`make -C SRC all`).
- If behavior changed, include at least one monitor transcript snippet in the PR description.
- Regenerate tracked docs with:

```powershell
powershell -ExecutionPolicy Bypass -File tools/docs/regenerate-docs.ps1
```

- If zero-page Markdown changed and you want refreshed HTML too, run:

```powershell
powershell -ExecutionPolicy Bypass -File tools/docs/regenerate-docs.ps1 -RegenerateMdHtml
```

## Pull Request Checklist

- [ ] Build succeeds locally
- [ ] Relevant docs updated
- [ ] No unrelated files changed
- [ ] Commit messages explain what changed and why
