# bso2 Demo Transcript

Source: `DOCS/bso2 demo.log`

Homage: this demo honors your Ohio Scientific `C1P`, later expanded into a `C2P` with disk and `bso2` startup.

Clarification: older help text in this captured log says `POST ASK`; this means the game prompt appears once after Reset/NMI. Flag `$0088` controls pending state (`!M 88 01` set, `!M 88 00` clear).

```text
=~=~=~=~=~=~=~=~=~=~=~= PuTTY log 2026.02.17 03:49:55 =~=~=~=~=~=~=~=~=~=~=~=

POWER ON
TERM WIDTH 4=40 8=80 1=132 [8]?
RAM CLEARED

     **** basic system operations/2 ****
     ****       b s o / 2  v0 . 9   ****
     ****         6 5 0 2           ****


RST: FFFC > F818 > 8004 > A0BD > [0089] > 800D **RST_DEFAULT_PLACEHOLDER**
NMI: FFFA > 8007 > [008C] > A0D7               **NMI_DEFAULT_PLACEHOLDER**
IRQ: FFFE > 800A > [008F] > A138               **IRQ_DEFAULT_PLACEHOLDER**

-H
HELP:? H  CTRL:Q W Z  EXEC:G N R X  MEM:A C D F L M P U V
-H
HELP:? H  CTRL:Q W Z  EXEC:G N R X  MEM:A C D F L M P U V
H A=ALL  H P=PROTECTION  H M=MEMORY  H S=STEERING  H -=AUTOHELP OFF  H +=AUTOHELP ON
WANT TO PLAY A GAME?
-H ALL  
MONITOR HELP
  [HELP]
  ?                SHORT HELP
  H [A|P|M|S|-|+]  INDEX / ALL / PROT / MEM / STEER  (ALSO -H / +H)
  [CONTROL]
  Q                WAI HALT; RESUME VIA NMI/RESET
  W                WARM START (MONITOR)
  Z                CLEAR RAM (CONFIRM Y/N)
  [EXECUTION/DEBUG]
  N                NEXT (RAM PATCH; NO ROM/I/O)
  R [A/X/Y=HH]     RESUME LAST DEBUG CONTEXT
  X S              EXECUTE; NMI BREAKS TO MONITOR
  G                GUESS NUMBER (1-10, 3 TRIES)
  [MEMORY]
  A S [INSN]       TINY ASM; '.' EXITS
  C S E DST        COPY (OVERLAP-SAFE)
  D [S [E]]        DUMP (E IS INCLUSIVE)
  F S E B0..B15    FILL (NO INTERACTIVE MODE)
  M [S [B0..B15]]  MODIFY / DEPOSIT
    M INTERACTIVE: CR/LF=NEXT, '.'=END
    ENTER HEX PAIRS (00..FF) TO STORE BYTES
    NOTE: CRLF PAIR COUNTS AS ONE NEXT
  L S              LOAD MOTOROLA S-RECORDS
  L B A L          LOAD RAW BYTES TO ADDR/LEN (NO CRC)
  P                RESERVED / DEPRECATED (USE I O P)
  U [S E]          DISASSEMBLE 65C02 RANGE
  V                SHOW VECTOR CHAINS (DEPRECATING)
  [PROTECTION]
  F/M/C/A/N/L      PROTECT $0000-$03FF BY DEFAULT
  !<CMD> ...       FORCE-ENABLE LOW-RAM ACCESS
  [STEERING] (PLANNED/PROVISIONAL)
  STYLE            NOUN VERB (M D, X S, I O V)
  WILL CHANGE      X->J EXEC, TIME->I T, T->TERMINAL
  DEPRECATED       TOP-LEVEL P/V; USE I O P / I O V
  PROVISO          CHANGE IS CONSTANT
  POST ASK         ONE-SHOT AFTER RESET/NMI
  FLAG @ $0088     FIXED: !M 88 01=SET  !M 88 00=CLEAR
  HOOKS @          $0089/$008C/$008F FIXED
  HW VECTORS @     $FFFA/$FFFC/$FFFE
  TERM COL @ $0093 !M 93 28/50/84 (40/80/132)
-D 88
0088: 00 4C 0D 80 4C D7 A0 4C | 38 A1 00 50 ED FD AB BD  |.L..L..L|8..P....|
0098: 01 80 90 1C E8 5D 7D FB | 51 12 0C 80 B2 3F 75 FB  |.....]}.|Q....?u.|
00A8: 40 55 12 48 3D FF FD 9E | B4 CC 84 02 AF 5F DF FF  |@U.H=...|....._..|
00B8: 20 59 56 00 BD 5A C2 3D | 41 2A 41 18 FE FB DD C9  | YV..Z.=|A*A.....|
00C8: FE 10 33 89 6A DA BD 76 | 14 01 42 70 BF A7 EB 1B  |..3.j..v|..Bp....|
00D8: 60 1E 79 60 CF 65 FF 4F | 90 10 09 D9 DB AF BE 1B  |`.y`.e.O|........|
00E8: 01 78 48 16 7F 9F BF F3 | 04 80 19 00 FE 9F FF EF  |.xH.....|........|
00F8: 50 A0 89 99 BA 4A D9 5B | 39 01 2D 0A F6 FA 6E 56  |P....J.[|9.-...nV|
0108: 21 26 08 00 1F FF FF EF | 06 30 58 48 A7 AE DF F3  |!&......|.0XH....|
0118: 10 C0 48 0A F3 FC 3F F7 | 66 45 28 07 9C E5 EB F6  |..H...?.|fE(.....|
0128: 00 12 C1 E2 FF 7E F7 EB | 82 24 48 0F 7A 9F D4 CB  |.....~..|.$H.z...|
0138: 41 44 19 00 FF FC FF 16 | 73 32 40 18 6D 26 1F F1  |AD......|s2@.m&..|
0148: C0 28 86 81 DF FD FF A9 | 26 22 11 D4 DE B1 D3 ED  |.(......|&"......|
0158: 00 00 18 84 F3 D7 BE D5 | 40 30 41 00 DD EC E6 FF  |........|@0A.....|
0168: 21 00 20 A0 DF ED 7B 7B | 90 5A 90 40 FF AF DD AE  |!. ...{{|.Z.@....|
0178: 01 46 80 00 E7 FC F7 FE | 07 80 43 C6 B8 66 5F 7F  |.F......|..C..f_.|

-D 88 88
0088: 00                      |                          |.|

-D
0089: 4C                      |                          |L|

-D 88 89
0088: 00 4C                   |                          |.L|

-D
008A: 0D 80                   |                          |..|

-M88 01
PROTECTED RANGE ($0000-$03FF). USE ! TO FORCE
-!M88 01
WANT TO PLAY A GAME?
-G
I AM THINKING OF A NUMBER (1-10)
? 4
TOO LOW
? 8
CORRECT
-M400
0400: 00 01
0401: 00 02
0402: 00 03
0403: 00 .
-D 400 402
0400: 01 02 03                |                          |...|

-D
0403: 00 00 00                |                          |...|

-F400 40F 55 CC AA
-D400 40F
0400: 55 CC AA 55 CC AA 55 CC | AA 55 CC AA 55 CC AA 55  |U..U..U.|.U..U..U|

-H
HELP:? H  CTRL:Q W Z  EXEC:G N R X  MEM:A C D F L M P U V
H A=ALL  H P=PROTECTION  H M=MEMORY  H S=STEERING  H -=AUTOHELP OFF  H +=AUTOHELP ON
-A1000
A 1000: LDX #FF
A 1002: LDY #$7F
A 1004: LDA #3E
A 1006: BRK 00
A 1008: INC Y
A 1009: BRK FF
A 100B: .
-U1000 100B
1000: LDX #$FF
1002: LDY #$7F
1004: LDA #$3E
1006: BRK #$00
1008: INY
1009: BRK #$FF
100B: BRK #$00


-X1000
CURR: 1006: BRK #$00    NEXT: 1008: INY
STATE:[BRK] 00 PC:1008 A:3E X:FF Y:7F P:35 [nv-BdIzC] S:F9

-N
CURR: 1009: BRK #$FF    NEXT: 100B: BRK #$00
STATE:[BRK] FF PC:100B A:3E X:FF Y:80 P:B5 [Nv-BdIzC] S:F9

-RA=42
CURR: 100B: BRK #$00    NEXT: 100D: BRK #$00
STATE:[BRK] 00 PC:100D A:42 X:FF Y:80 P:B5 [Nv-BdIzC] S:F9

-Q
Q HALT - RESET/NMI TO RESUME
[NMI] PC:8D09 A:02 X:29 Y:00 P:67 [nV-bdIZC] S:F5
8D09: LDA #$02

-
-?
HELP:? H  CTRL:Q W Z  EXEC:G N R X  MEM:A C D F L M P U V
WANT TO PLAY A GAME?
-
```
