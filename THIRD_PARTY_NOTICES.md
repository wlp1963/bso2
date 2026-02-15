# Third-Party Notices

This project is MIT-licensed for original code in this repository.

## Western Design Center (WDC)

- `WDC`, `W65C02`, and `W65C02EDU` are names associated with Western Design Center, Inc.
- This project references WDC hardware/platform behavior and toolchain usage.
- This repository does **not** include or redistribute WDC tool binaries or WDC ROM images.

### WDC EDU sample provenance

- Upstream archive source used during development:
  - `W65C02SXB (1).zip` -> `02EDU_GettingStarted.zip` -> `EDU_GettingStarted/ASM/`
- Upstream files referenced/derived from:
  - `EDUGS_FLASH.asm`
  - `02EDU.inc`
- Repository file with upstream-derived/adapted portions:
  - `SRC/bso2.asm` (includes adapted header/provenance text and selected hardware symbol definitions)
- Upstream-derived portions remain subject to the original WDC notices/terms distributed with those archives.
- Project-original additions/modifications are MIT-licensed as stated in this repository.

## Tooling References

Build and upload instructions may reference local installations such as:

- `WDC02AS`
- `WDCLN`
- `wdc_interface.py`

Those tools remain subject to their own licenses/terms from their respective owners.
