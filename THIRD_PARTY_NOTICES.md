# Third-Party Notices

This project is MIT-licensed for original code in this repository.

## Western Design Center (WDC)

- `WDC`, `W65C02`, and `W65C02SXB/EDU` are names associated with Western Design Center, Inc.
- Official site references: `https://wdc65xx.com/` and `https://westerndesigncenter.com/`
- Disclosure: references to WDC products/sites are informational only and do not imply sponsorship or paid promotion.
- This project references WDC hardware/platform behavior and toolchain usage.
- This repository does **not** include or redistribute WDC tool binaries or WDC ROM images.

### WDCMONv2 Integration Note

- If direct source from `WDCMONv2` is ever imported, that import must include:
- upstream license/permission verification
- explicit attribution/provenance entry in this file
- clear file-level notice describing what was copied vs. newly written

### WDC EDU sample provenance

- Upstream archive source used during development:
  - `W65C02SXB.zip` -> `02EDU_GettingStarted.zip` -> `EDU_GettingStarted/ASM/`
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

## Educational / Community References

- Special recognition to Ben Eater for accessible 6502 education and practical learning resources.
- Ben Eater 6502: `https://eater.net/6502`
- Community reference hub: `https://6502.org/`

## Book-Sourced Material (Template)

Use this section when code or algorithms are derived from printed books/manuals.

Important:

- Citation/attribution is required for provenance.
- Citation alone does **not** grant copyright permission.
- Verbatim or closely adapted text/code should only be included when rights/permission allow it.

### Template Entry

Copy, fill, and keep one entry per source:

- Source title: `<Book Title>`
- Author(s): `<Author Name(s)>`
- Publisher / edition / year: `<Publisher, Edition, Year>`
- Pages / chapter / section used: `<Page range or section>`
- Repository files affected: `<path1>, <path2>, ...`
- Usage type: `<verbatim excerpt | adapted code | concept-only reimplementation>`
- Copyright / permission status: `<licensed | permission granted | public domain | unknown>`
- Notes: `<what was taken, what was changed, and why>`

If copyright/permission status is unknown, prefer concept-only reimplementation and avoid verbatim copying.

### Book Source Entry: Hex-to-ASCII Routine Reference

- Source title: `6502 Assembly Language Programming`
- Author(s): `Lance A. Leventhal`
- Publisher / edition / year: `Osborne/McGraw-Hill, 1979` 
- Pages / chapter / section used: `p. 7-3 (hex to ASCII method)`
- Repository files affected: `SRC/bso2.asm` (`CVT_NIBBLE`)
- Reference status: `on hand`
- Usage type: `adapted code / algorithmic pattern`
- Copyright / permission status: `unknown (citation/provenance recorded; no license grant implied)`
- Notes: `Method/code reference appears on Leventhal p. 7-3 (bottom) with superscript 1. The bibliographic details for Chapter 7 reference #1 are listed on p. 7-15: D. R. Allison, "A Design Philosophy for Microcomputer Architectures.", Computer, February 1977, pp. 35-41.`

### Book Source Entry: Programming the 65816 (Reference on Hand)

- Source title: `Programming the 65816: Including the 6502, 65C02, and 65802`
- Author(s): `David Eyes`; `Ron Lichty`
- ISBN: `0-89303-789-3`
- Publisher / edition / year: `Prentice Hall Press`
- Pages / chapter / section used: `none yet (reference copy only)`
- Repository files affected: `none yet`
- Reference status: `on hand`
- Usage type: `none yet (future reference candidate)`
- Copyright / permission status: `unknown (no license grant implied)`
- Notes: `Book recorded for provenance tracking. If any code/algorithms are derived from this source, update this entry with exact pages/sections and file paths.`
