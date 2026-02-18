# Stories

## Milestones

- `2026-02-17T14:21:00-06:00` - David Gray (WDC) invited to this repository.
- `2026-02-16T00:45:50-06:00` - Historical RLE design-note rationale documented across `README.md`, `STORIES.md`, and monitor docs.

## Working Design Notes

### Memory Residency Policy (Current Direction)

- Keep `CODE` anchored at `$8000`.
- Let `KDATA` float immediately behind `CODE`.
- Treat bank 0 low 64K as always-resident core: reset path, ISR stubs, vector/dispatch tables, bank-switch glue, and minimal monitor.
- Keep `KDATA` and other core tables below `$F000` (`$F000-$FFFF` remains reserved for ROM/vector/system assumptions).
- Place feature-heavy code/data in flash bank 1/2 as callable modules.
- Keep stable entry trampolines in bank 0 for cross-bank calls.
- Keep vector targets in bank 0; vectors must not point directly into switchable banks.
- Add an enforced build check later for `END_KDATA < $F000` (post-link map check is the reliable method with this toolchain).

### Work Rhythm: Active vs Parked

- Active lane (high-speed lane): one current implementation lane only. Finish, verify, document, then switch.
- Parked lane: anything not in the active lane gets a short parked note and stays deferred until explicitly promoted.
- Promotion rule: parked items move to active only when the active lane has a clear stop point.

Quoted guidance:
"You explore broadly, then commit to concrete constraints. That balance is rare and useful."
"Keep one active lane and one parking lot lane; everything not in active lane gets a dated TODO note and waits."

## Author History (Draft)

This project has roots in hands-on work from the late 1980s. Some dates and details are approximate and may be corrected over time.

In 1987, while IBM was introducing major new hardware such as the 8514/A graphics adapter and the 3363 WORM drive, I was working at Computer Bank, Inc. I had dropped out of DeVry in Dallas. One day, walking along an oil-top road, a man in a Cadillac stopped, asked if I was Walter Preuninger, and offered me a job on the spot.

I ran IBM System/36 operations (including a 5360), magnetic tape drives, line printers, and even a microfiche printer. I captured checks, balanced reports, and processed POD, DDA, LN, and GL applications. I changed boxes of one-part and three-part green bar, handled data mailers, and ran CD interest check cycles.

Eventually I was given room to go deeper. I learned POD/DDA and CIF in RPG II, wired and terminated twinax cabling, configured systems, and took night-call duty by choice. I traveled for training, including IBM PC VAD school, during a period when my wife and I had just had a baby and I flew out of Houston while they stayed with my parents.

I worked on original IBM PC XT (5160) systems. The PS/2 period was fun for me; in our environment we did not use the AT much.

I wrote Turbo Pascal programs to interface with the 8514/A and its large monitor. I was tasked with building a COLD application (Computer Output to Laser Disk). The workflow was:

- Generate spool-entry disk files on the 5360.
- Transfer data from the System/36 to PC through 5250 emulation/PC support.
- Process and index on the PC side.
- Write output to WORM cartridge media.

Institutions using our service paid extra for that capability because it replaced storage rooms full of binders and green bar with indexed retrieval.

I wrote the indexing program and an RLE compression routine. RLE was a practical production tool in my ASMF1/System/36 COLD work, where throughput and overnight windows mattered more than perfect compression ratios. The implementation was rushed and not elegant, but it was successful in production. That same tradeoff applies here: favor simple, fast, streamable compression with manageable code complexity over maximum ratio. The first implementation in RPG did not finish overnight for even small-bank output windows, so I learned 5727-AS1 (System/36 assembler) and rewrote the full path. It was dramatically faster and drove a fully configured 5360 (max memory and four 10 SR disk drives) very hard.

There are more stories around the PC-side loan upload tooling and related integration work; those can be added in later revisions.




-S B 400 FFFF 4C 45 4E 20
B470 B470: 4C 45 4E 20 28 4E 4F 20 | 43 52 43 29 00 0D 0A 20  |LEN (NO |CRC)...|
B8AF B8A0: 41 44 44 52 20 4C 45 4E | 00 0D 0A 4C 20 42 20 4C  |ADDR LEN|...L B |
WANT TO PLAY A GAME?

ya ne ponymayu

You’re seeing a boundary case in the new display format. I’ll check the exact bytes around $B8AF so I can show precisely why 4C 45 4E 20 matched there.

Ran rg -n "B8A0|B8AF|B8B0|B470|4C 45 4E 20" SRC/bso2.lst -S


Ran rg -n "LEN \(NO CRC\)|USAGE: L B ADDR LEN|LEN MUST BE 1..FFFF" SRC/bso2.asm -S


Worked for 33s
Понимаю. Тут все верно, просто второй матч пересекает границу строки дампа.

S B 400 FFFF 4C 45 4E 20 ищет байты LEN<space>.

Почему второй хит B8AF:

B8AF это 4C (L) в конце строки B8A0.
Следующие байты 45 4E 20 (E N <space>) уже на следующей строке (B8B0...), поэтому их не видно в том же ряду.
То есть матч идет через границу ...B8AF | B8B0....
Это соответствует строке "L B LEN MUST BE 1..FFFF" (bso2.asm (line 6554)), а первый хит B470 — из "LEN (NO CRC)" (bso2.asm (line 6482)).

Проверь точечно:
D B8AF B8B3
там увидишь 4C 45 4E 20.


lol, ya govorit ruski ploha,

Да, понял тебя.

Коротко:

Всё работает правильно.
B8AF — это L в конце 16-байтной строки.
E N <space> идут сразу в следующей строке (B8B0...).
Поэтому видно не все 4 байта в одном ряду, но матч реальный.
Проверь:
D B8AF B8B3
должно показать 4C 45 4E 20.


sorry, i know few word. translate.

No problem.

Short version:

Your search is working correctly.
At B8AF, the match starts at the last byte of that 16-byte row.
The next bytes continue in the next row (B8B0...), so you don’t see all 4 bytes in one displayed line.
So B8AF is a real hit, not a bug.
To confirm, dump across the boundary:

D B8AF B8B3
You should see:

4C 45 4E 20

## Reminder Placeholder: Search Hit vs Row Base

This is not a formal story entry. It is a reminder wrapped in a small tale and anchored in truth.

When `S` prints two addresses as `<HIT_ADDR> <ROW_BASE>:`, the first is the true match start and the second is only the aligned 16-byte dump row (`HIT_ADDR & $FFF0`).

A match can begin at the last byte of one row and continue into the next row. Example:
- Hit starts at `$B8AF` (byte `4C`)
- Remaining bytes continue at `$B8B0` (`45 4E 20`)

That behavior is expected. If in doubt, verify with a direct boundary dump:
`D B8AF B8B3`
