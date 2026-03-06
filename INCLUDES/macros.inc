; -----------------------------------------------------------------------------
; 65C02 Assembler Macros for WDC Toolchain: PUSH / PULL
; -----------------------------------------------------------------------------
; SPDX-License-Identifier: MIT
; Copyright (c) 2026 95west.us
; See LICENSE for full license text.
    ARGCHK OFF              ; Disable argument count checking to allow 1-3 args
; -----------------------------------------------------------------------------
; Macro: PUSH
; Usage: PUSH p1 [,p2] [,p3] [,p4]
; -----------------------------------------------------------------------------
PUSH    MACRO   V1,V2,V3,V4
        ; --- Process Parameter 1 ---
        IFMA    1           ; Check if Argument 1 exists
            IFSAME  V1,"A"
                PHA
            ENDIF
            IFSAME  V1,"a"
                PHA
            ENDIF
            IFSAME  V1,"X"
                PHX
            ENDIF
            IFSAME  V1,"x"
                PHX
            ENDIF
            IFSAME  V1,"Y"
                PHY
            ENDIF
            IFSAME  V1,"y"
                PHY
            ENDIF
            IFSAME  V1,"P"
                PHP
            ENDIF
            IFSAME  V1,"p"
                PHP
            ENDIF
        ENDIF

        ; --- Process Parameter 2 ---
        IFMA    2           ; Check if Argument 2 exists
            IFSAME  V2,"A"
                PHA
            ENDIF
            IFSAME  V2,"a"
                PHA
            ENDIF
            IFSAME  V2,"X"
                PHX
            ENDIF
            IFSAME  V2,"x"
                PHX
            ENDIF
            IFSAME  V2,"Y"
                PHY
            ENDIF
            IFSAME  V2,"y"
                PHY
            ENDIF
            IFSAME  V2,"P"
                PHP
            ENDIF
            IFSAME  V2,"p"
                PHP
            ENDIF
        ENDIF

        ; --- Process Parameter 3 ---
        IFMA    3           ; Check if Argument 3 exists
            IFSAME  V3,"A"
                PHA
            ENDIF
            IFSAME  V3,"a"
                PHA
            ENDIF
            IFSAME  V3,"X"
                PHX
            ENDIF
            IFSAME  V3,"x"
                PHX
            ENDIF
            IFSAME  V3,"Y"
                PHY
            ENDIF
            IFSAME  V3,"y"
                PHY
            ENDIF
            IFSAME  V3,"P"
                PHP
            ENDIF
            IFSAME  V3,"p"
                PHP
            ENDIF
        ENDIF


        ; --- Process Parameter 4 ---
        IFMA    4           ; Check if Argument 4 exists
            IFSAME  V4,"A"
                PHA
            ENDIF
            IFSAME  V4,"a"
                PHA
            ENDIF
            IFSAME  V4,"X"
                PHX
            ENDIF
            IFSAME  V4,"x"
                PHX
            ENDIF
            IFSAME  V4,"Y"
                PHY
            ENDIF
            IFSAME  V4,"y"
                PHY
            ENDIF
            IFSAME  V4,"P"
                PHP
            ENDIF
            IFSAME  V4,"p"
                PHP
            ENDIF
        ENDIF

    ENDM

; -----------------------------------------------------------------------------
; Macro: PULL
; Usage: PULL p1 [,p2] [,p3] [,p4]
; -----------------------------------------------------------------------------
PULL    MACRO   V1,V2,V3,V4
        ; --- Process Parameter 1 ---
        IFMA    1
            IFSAME  V1,"A"
                PLA
            ENDIF
            IFSAME  V1,"a"
                PLA
            ENDIF
            IFSAME  V1,"X"
                PLX
            ENDIF
            IFSAME  V1,"x"
                PLX
            ENDIF
            IFSAME  V1,"Y"
                PLY
            ENDIF
            IFSAME  V1,"y"
                PLY
            ENDIF
            IFSAME  V1,"P"
                PLP
            ENDIF
            IFSAME  V1,"p"
                PLP
            ENDIF
        ENDIF

        ; --- Process Parameter 2 ---
        IFMA    2
            IFSAME  V2,"A"
                PLA
            ENDIF
            IFSAME  V2,"a"
                PLA
            ENDIF
            IFSAME  V2,"X"
                PLX
            ENDIF
            IFSAME  V2,"x"
                PLX
            ENDIF
            IFSAME  V2,"Y"
                PLY
            ENDIF
            IFSAME  V2,"y"
                PLY
            ENDIF
            IFSAME  V2,"P"
                PLP
            ENDIF
            IFSAME  V2,"p"
                PLP
            ENDIF
        ENDIF

        ; --- Process Parameter 3 ---
        IFMA    3
            IFSAME  V3,"A"
                PLA
            ENDIF
            IFSAME  V3,"a"
                PLA
            ENDIF
            IFSAME  V3,"X"
                PLX
            ENDIF
            IFSAME  V3,"x"
                PLX
            ENDIF
            IFSAME  V3,"Y"
                PLY
            ENDIF
            IFSAME  V3,"y"
                PLY
            ENDIF
            IFSAME  V3,"P"
                PLP
            ENDIF
            IFSAME  V3,"p"
                PLP
            ENDIF
        ENDIF


        ; --- Process Parameter 4 ---
        IFMA    4
            IFSAME  V4,"A"
                PLA
            ENDIF
            IFSAME  V4,"a"
                PLA
            ENDIF
            IFSAME  V4,"X"
                PLX
            ENDIF
            IFSAME  V4,"x"
                PLX
            ENDIF
            IFSAME  V4,"Y"
                PLY
            ENDIF
            IFSAME  V4,"y"
                PLY
            ENDIF
            IFSAME  V4,"P"
                PLP
            ENDIF
            IFSAME  V4,"p"
                PLP
            ENDIF
        ENDIF
    ENDM

REPEAT	MACRO	V1,V2
	PUSH X
	ldx #V2
?mac1	jsr V1
	dex
	bne ?1
	PULL X
	endm

; -----------------------------------------------------------------------------
; Macro: PRT_CSTRING
; Usage: PRT_CSTRING Label
; -----------------------------------------------------------------------------
PRT_CSTRING MACRO   V1
	PUSH A
        LDA #<V1            ; Load Low Byte of address
        STA STR_PTR         ; Store in ZP $66
        LDA #>V1            ; Load High Byte of address
        STA STR_PTR+1       ; Store in ZP $67
	PULL A
        JSR PRT_C_STRING    ; Call the display routine
    ENDM
	
DUMP    MACRO   V1, V2
        LDA     #<V1
        STA     PTR_DUMP_CUR
        LDA     #>V1
        STA     PTR_DUMP_CUR+1
        LDA     #<V2
        STA     PTR_TEMP
        LDA     #>V2
        STA     PTR_TEMP+1
        JSR     MEM_DUMP
    ENDM

; -----------------------------------------------------------------------------
; Macro: FILL
; Usage: FILL StartAddr, EndAddr, B0 [,B1] [,B2] [,B3] [,B4]
; Notes:
;   - EndAddr is inclusive.
;   - Pattern repeats across the range.
;   - Calls MEM_FILL_PATTERN.
; -----------------------------------------------------------------------------
FILL    MACRO   V1,V2,V3,V4,V5,V6,V7
        ; PTR_DUMP_CUR = start
        LDA     #<V1
        STA     PTR_DUMP_CUR
        LDA     #>V1
        STA     PTR_DUMP_CUR+1

        ; PTR_DUMP_END = end + 1 (exclusive)
        LDA     #<(V2+1)
        STA     PTR_DUMP_END
        LDA     #>(V2+1)
        STA     PTR_DUMP_END+1

        ; Build 1..5 byte pattern in F_PATTERN
        LDY     #$00
        LDA     #V3
        STA     F_PATTERN,Y
        INY
        IFMA    4
            LDA #V4
            STA F_PATTERN,Y
            INY
        ENDIF
        IFMA    5
            LDA #V5
            STA F_PATTERN,Y
            INY
        ENDIF
        IFMA    6
            LDA #V6
            STA F_PATTERN,Y
            INY
        ENDIF
        IFMA    7
            LDA #V7
            STA F_PATTERN,Y
            INY
        ENDIF
        STY     F_COUNT
        JSR     MEM_FILL_PATTERN
    ENDM

; -----------------------------------------------------------------------------
; Macro: COPY
; Usage: COPY SrcStart, SrcEnd, DstStart
; Notes:
;   - SrcEnd is inclusive.
;   - Overlap-safe direction is selected automatically.
;   - Calls MEM_COPY_RANGE.
; -----------------------------------------------------------------------------
COPY    MACRO   V1,V2,V3
        ; PTR_LEG = source start (inclusive)
        LDA     #<V1
        STA     PTR_LEG
        LDA     #>V1
        STA     PTR_LEG+1

        ; PTR_DUMP_END = source end + 1 (exclusive)
        LDA     #<(V2+1)
        STA     PTR_DUMP_END
        LDA     #>(V2+1)
        STA     PTR_DUMP_END+1

        ; PTR_TEMP = destination start
        LDA     #<V3
        STA     PTR_TEMP
        LDA     #>V3
        STA     PTR_TEMP+1
        JSR     MEM_COPY_RANGE
    ENDM

; -----------------------------------------------------------------------------
; Macro: CMP_CSTRING
; Usage: CMP_CSTRING AddrA, AddrB
; Result: ACC = STRCMP_RES (0=EQUAL, 1=NOT EQUAL)
; -----------------------------------------------------------------------------
CMP_CSTRING MACRO V1, V2
        LDA     #<V1
        STA     STRCMP_PTR_A
        LDA     #>V1
        STA     STRCMP_PTR_A+1
        LDA     #<V2
        STA     STRCMP_PTR_B
        LDA     #>V2
        STA     STRCMP_PTR_B+1
        JSR     STR_COMPARE
        LDA     STRCMP_RES
    ENDM

; -----------------------------------------------------------------------------
; Macro: COPY_BLOCK
; Usage: COPY_BLOCK SrcAddr, Length, DstAddr
; -----------------------------------------------------------------------------
COPY_BLOCK MACRO V1, V2, V3
        COPY    V1,((V1)+(V2)-1),V3
    ENDM
