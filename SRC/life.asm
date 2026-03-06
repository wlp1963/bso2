; SPDX-License-Identifier: MIT
; Copyright (c) 2026 95west.us
; See LICENSE for full license text.
;
; LIFE.ASM
; Standalone Conway's Game of Life for W65C02EDU userland.
; 16x16 toroidal board, text UI over serial.
;
; Provenance:
; - Project-original implementation for this repository.
; - Conway's Life rules are implemented directly in code.
                        CHIP       65C02
                        PL         60
                        PW         132
                        TITLE      LIFE
                        MACLIST    OFF

                        INCLUDE    equates.inc

        XREF PRT_C_STRING
        XREF PRT_CRLF
        XREF PRT_HEX
        XREF WRITE_BYTE
        XREF UTIL_TO_UPPER
        XREF RNG_SEED_RAM_0_7EFF
        XREF RNG8_NEXT
        XREF LED_DATA
        XREF WDC_WRITE_BYTE

        XDEF STR_PTR
        XDEF TMP_IDX
        XDEF TMP_X
        XDEF TMP_Y
        XDEF TMP_COUNT
        XDEF GEN_COUNT
        XDEF NX_L
        XDEF NX_R
        XDEF NY_U
        XDEF NY_D

STR_PTR                 EQU        $0036
TMP_IDX                 EQU        $0090
TMP_X                   EQU        $0091
TMP_Y                   EQU        $0092
TMP_COUNT               EQU        $0093
GEN_COUNT               EQU        $0094
NX_L                    EQU        $0095
NX_R                    EQU        $0096
NY_U                    EQU        $0097
NY_D                    EQU        $0098
RAND_STIR               EQU        $0099
TMP_OFF                 EQU        $009A

START:
        LDA RAND_STIR
        JSR RNG_SEED_RAM_0_7EFF
        STA RAND_STIR
        JSR CLEAR_BOARDS
        JSR SEED_GLIDER
        STZ GEN_COUNT
        PRT_CSTRING MSG_BANNER

MAIN_LOOP:
        JSR DRAW_BOARD
        PRT_CSTRING MSG_PROMPT
        JSR READ_KEY
        CMP #$0D
        BEQ DO_STEP
        CMP #$0A
        BEQ DO_STEP
        CMP #'N'
        BEQ DO_STEP
        CMP #'C'
        BEQ DO_CLEAR
        CMP #'G'
        BEQ DO_GLIDER
        CMP #'B'
        BEQ DO_BLINKER
        CMP #'R'
        BEQ DO_RANDOM
        CMP #'P'
        BEQ DO_PATTERN
        CMP #'Q'
        BEQ DO_QUIT
        PRT_CSTRING MSG_HELP_SHORT
        JMP MAIN_LOOP

DO_STEP:
        JSR STEP_LIFE
        INC GEN_COUNT
        JMP MAIN_LOOP

DO_CLEAR:
        JSR CLEAR_BOARDS
        STZ GEN_COUNT
        JMP MAIN_LOOP

DO_GLIDER:
        JSR CLEAR_BOARDS
        JSR SEED_GLIDER
        STZ GEN_COUNT
        JMP MAIN_LOOP

DO_BLINKER:
        JSR CLEAR_BOARDS
        JSR SEED_BLINKER
        STZ GEN_COUNT
        JMP MAIN_LOOP

DO_RANDOM:
        JSR RANDOMIZE_BOARD
        STZ GEN_COUNT
        JMP MAIN_LOOP

DO_PATTERN:
        JSR INPUT_PATTERN
        JMP MAIN_LOOP

DO_QUIT:
        PRT_CSTRING MSG_QUIT
        BRK $00
        WAI
        JMP START

READ_KEY:
        JSR WDC_READ_BYTE
        JSR WRITE_BYTE
        JSR UTIL_TO_UPPER
        RTS

INPUT_PATTERN:
        JSR CLEAR_BOARDS
        STZ GEN_COUNT
        STZ TMP_IDX
        PRT_CSTRING MSG_PATTERN_MODE
        LDY #$00
?IP_ROW:
        CPY #$10
        BNE ?IP_ROW_CONT
        JMP ?IP_DONE
?IP_ROW_CONT:
        PRT_CSTRING MSG_PATTERN_ROW
        TYA
        JSR PRT_HEX
        PRT_CSTRING MSG_PATTERN_PROMPT
        LDX #$00
?IP_ROW_CHAR:
        PHX
        PHY
        JSR READ_KEY
        PLY
        PLX
        CPX #$00
        BNE ?IP_NOT_LEAD_LF
        CMP #$0A
        BEQ ?IP_ROW_CHAR
?IP_NOT_LEAD_LF:
        CMP #'Q'
        BEQ ?IP_ABORT
        CMP #$0D
        BEQ ?IP_PAD_ROW
        CMP #$0A
        BEQ ?IP_PAD_ROW
        CMP #'.'
        BEQ ?IP_STORE_DEAD
        CMP #'#'
        BEQ ?IP_STORE_LIVE
        BRA ?IP_ROW_CHAR
?IP_STORE_DEAD:
        LDA #$00
        BRA ?IP_STORE
?IP_STORE_LIVE:
        LDA #$01
?IP_STORE:
        STX TMP_X
        PHA
        LDX TMP_IDX
        PLA
        STA BOARD_CUR,X
        INX
        STX TMP_IDX
        LDX TMP_X
        INX
        CPX #$10
        BCC ?IP_ROW_CHAR
?IP_WAIT_EOL:
        PHX
        PHY
        JSR READ_KEY
        PLY
        PLX
        CMP #'Q'
        BEQ ?IP_ABORT
        CMP #$0D
        BEQ ?IP_ROW_DONE
        CMP #$0A
        BNE ?IP_WAIT_EOL
?IP_ROW_DONE:
        INY
        BRA ?IP_ROW
?IP_PAD_ROW:
        CPX #$10
        BCS ?IP_ROW_DONE
?IP_PAD_LOOP:
        STX TMP_X
        LDX TMP_IDX
        STZ BOARD_CUR,X
        INX
        STX TMP_IDX
        LDX TMP_X
        INX
        CPX #$10
        BCC ?IP_PAD_LOOP
        BRA ?IP_ROW_DONE
?IP_ABORT:
        PRT_CSTRING MSG_PATTERN_ABORT
?IP_DONE:
        RTS

DRAW_BOARD:
        PRT_CSTRING MSG_GEN
        LDA GEN_COUNT
        JSR PRT_HEX
        JSR PRT_CRLF

        LDX #$00
?DB_LOOP:
        LDA BOARD_CUR,X
        BEQ ?DB_DEAD
        LDA #'#'
        BRA ?DB_PRINT
?DB_DEAD:
        LDA #'.'
?DB_PRINT:
        PHX
        JSR WRITE_BYTE
        PLX
        INX
        TXA
        AND #$0F
        BNE ?DB_NOLF
        PHX
        JSR PRT_CRLF
        PLX
?DB_NOLF:
        BNE ?DB_LOOP
        RTS

STEP_LIFE:
        LDX #$00
?SL_CELL:
        STX TMP_IDX

        TXA
        AND #$0F
        STA TMP_X

        TXA
        LSR A
        LSR A
        LSR A
        LSR A
        STA TMP_Y

        LDX TMP_X
        LDY TMP_Y
        JSR COUNT_NEIGHBORS
        STA TMP_COUNT

        LDX TMP_IDX
        LDA BOARD_CUR,X
        BEQ ?SL_DEAD

        LDA TMP_COUNT
        CMP #$02
        BEQ ?SL_ALIVE
        CMP #$03
        BEQ ?SL_ALIVE
        LDA #$00
        BRA ?SL_STORE

?SL_DEAD:
        LDA TMP_COUNT
        CMP #$03
        BEQ ?SL_ALIVE
        LDA #$00
        BRA ?SL_STORE

?SL_ALIVE:
        LDA #$01

?SL_STORE:
        LDX TMP_IDX
        STA BOARD_NEXT,X
        INX
        BNE ?SL_CELL

        ; Commit next generation.
        LDX #$00
?SL_COPY:
        LDA BOARD_NEXT,X
        STA BOARD_CUR,X
        INX
        BNE ?SL_COPY
        RTS

COUNT_NEIGHBORS:
        STX TMP_X
        STY TMP_Y
        STZ TMP_COUNT

        LDY TMP_X
        LDA X_LEFT_TAB,Y
        STA NX_L
        LDA X_RIGHT_TAB,Y
        STA NX_R

        LDY TMP_Y
        LDA Y_UP_TAB,Y
        STA NY_U
        LDA Y_DOWN_TAB,Y
        STA NY_D

        LDX NX_L
        LDY NY_U
        JSR ADD_NEIGHBOR

        LDX TMP_X
        LDY NY_U
        JSR ADD_NEIGHBOR

        LDX NX_R
        LDY NY_U
        JSR ADD_NEIGHBOR

        LDX NX_L
        LDY TMP_Y
        JSR ADD_NEIGHBOR

        LDX NX_R
        LDY TMP_Y
        JSR ADD_NEIGHBOR

        LDX NX_L
        LDY NY_D
        JSR ADD_NEIGHBOR

        LDX TMP_X
        LDY NY_D
        JSR ADD_NEIGHBOR

        LDX NX_R
        LDY NY_D
        JSR ADD_NEIGHBOR

        LDA TMP_COUNT
        RTS

ADD_NEIGHBOR:
        TXA
        STA TMP_OFF
        TYA
        ASL A
        ASL A
        ASL A
        ASL A
        CLC
        ADC TMP_OFF
        TAY
        LDA BOARD_CUR,Y
        BEQ ?AN_DONE
        INC TMP_COUNT
?AN_DONE:
        RTS

CLEAR_BOARDS:
        LDX #$00
        LDA #$00
?CB_LOOP:
        STA BOARD_CUR,X
        STA BOARD_NEXT,X
        INX
        BNE ?CB_LOOP
        RTS

RANDOMIZE_BOARD:
        LDX #$00
?RB_LOOP:
        LDA RAND_STIR
        JSR RNG8_NEXT
        EOR VIA_T1L
        EOR VIA_T1H
        JSR RNG8_NEXT
        STA RAND_STIR
        AND #$01
        STA BOARD_CUR,X
        INX
        BNE ?RB_LOOP
        RTS

SEED_GLIDER:
        LDA #$01
        STA BOARD_CUR+1
        STA BOARD_CUR+18
        STA BOARD_CUR+32
        STA BOARD_CUR+33
        STA BOARD_CUR+34
        RTS

SEED_BLINKER:
        LDA #$01
        STA BOARD_CUR+122
        STA BOARD_CUR+123
        STA BOARD_CUR+124
        RTS

MSG_BANNER:             DB $0D,$0A,"=== LIFE 16x16 ===",$0D,$0A,"N/Enter=step  C=clear  G=glider  B=blinker  R=random  P=pattern  Q=break",$0D,$0A,0
MSG_GEN:                DB $0D,$0A,"GEN $",0
MSG_PROMPT:             DB $0D,$0A,"> ",0
MSG_HELP_SHORT:         DB $0D,$0A,"Keys: N C G B R P Q",0
MSG_PATTERN_MODE:       DB $0D,$0A,"PATTERN: ENTER 16 ROWS USING . AND #",$0D,$0A,"PRESS ENTER AFTER EACH ROW (SHORT ROWS PAD WITH .)",0
MSG_PATTERN_ROW:        DB $0D,$0A,"ROW ",0
MSG_PATTERN_PROMPT:     DB " > ",0
MSG_PATTERN_ABORT:      DB $0D,$0A,"PATTERN ABORTED",0
MSG_QUIT:               DB $0D,$0A,"BREAK",0

X_LEFT_TAB:             DB 15,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14
X_RIGHT_TAB:            DB 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0
Y_UP_TAB:               DB 15,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14
Y_DOWN_TAB:             DB 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0

BOARD_CUR:              DS 256
BOARD_NEXT:             DS 256
