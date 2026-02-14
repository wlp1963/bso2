# Contributing

Thanks for contributing to `bso2`.

## Prerequisites

- WDC assembler/linker available in `PATH` (`WDC02AS`, `WDCLN`)
- Windows PowerShell environment (current scripts/Makefile are Windows-oriented)

## Workflow

1. Create a feature branch.
2. Keep commits focused (one behavior change per commit when practical).
3. Update docs if command behavior or monitor output changes.
4. Run a full build before opening a PR:

```powershell
make -C SRC all
```

## Coding Notes

- Keep source ASCII unless there is a strong reason not to.
- Preserve existing assembly style and label naming patterns.
- Add comments for non-obvious logic, not for line-by-line narration.
- Avoid broad refactors unrelated to the requested behavior.

## Testing Expectations

- Build must pass (`make -C SRC all`).
- If behavior changed, include at least one monitor transcript snippet in the PR description.
- If help/output text changed, update `DOCS/monitor_usage.html` and regenerate `DOCS/monitor_usage.pdf`.

## Pull Request Checklist

- [ ] Build succeeds locally
- [ ] Relevant docs updated
- [ ] No unrelated files changed
- [ ] Commit messages explain what changed and why
