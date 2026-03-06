; SPDX-License-Identifier: MIT
; Copyright (c) 2026 95west.us
; See LICENSE for full license text.
;
; MASTERMIND.ASM
; Userland text game for W65C02EDU.
; Guess a 4-hex-digit code; feedback is Exact and Near counts.
;
; Provenance:
; - Project-original implementation for this repository.
; - No third-party gameplay source text was copied into this file.
                        CHIP       65C02
                        PL         60
                        PW         132
                        TITLE      MASTERMIND
                        MACLIST    OFF

                        INCLUDE    equates.inc

        XREF PRT_C_STRING
        XREF PRT_CRLF
        XREF WRITE_BYTE
        XREF UTIL_TO_UPPER
        XREF HEX_TO_NIBBLE
        XREF CVT_NIBBLE
        XREF LED_DATA
        XREF WDC_WRITE_BYTE
        XDEF STR_PTR
        XDEF SECRET0
        XDEF GUESS0
        XDEF GFLAG0
        XDEF SFLAG0
        XDEF TRIES_LEFT
        XDEF EXACT_CNT
        XDEF NEAR_CNT

STR_PTR                 EQU        $0036
SECRET0                 EQU        $0090 ; 4 bytes
GUESS0                  EQU        $0094 ; 4 bytes
GFLAG0                  EQU        $0098 ; 4 bytes
SFLAG0                  EQU        $009C ; 4 bytes
TRIES_LEFT              EQU        $00A0
EXACT_CNT               EQU        $00A1
NEAR_CNT                EQU        $00A2

START:
        JSR NEW_GAME

MAIN_LOOP:
        PRT_CSTRING MSG_TRIES
        LDA TRIES_LEFT
        JSR PRINT_DIGIT
        PRT_CSTRING MSG_PROMPT

        JSR READ_GUESS

        JSR SCORE_GUESS
        LDA EXACT_CNT
        CMP #$04
        BEQ PLAYER_WIN

        DEC TRIES_LEFT
        JSR SHOW_SCORE
        LDA TRIES_LEFT
        BNE MAIN_LOOP

        JSR SHOW_LOSE
        JMP NEW_GAME

PLAYER_WIN:
        PRT_CSTRING MSG_WIN
        JMP NEW_GAME

NEW_GAME:
        JSR ROLL_SECRET
        LDA #$08
        STA TRIES_LEFT
        PRT_CSTRING MSG_BANNER
        RTS

READ_GUESS:
        LDX #$00
?RG_LOOP:
        JSR READ_BYTE
        AND #$7F
        JSR UTIL_TO_UPPER
        CMP #$0D
        BEQ ?RG_EOL
        CMP #$0A
        BEQ ?RG_EOL

        ; Echo printable keypresses.
        PHA
        CMP #$20
        BCC ?RG_NO_ECHO
        CMP #$7F
        BCS ?RG_NO_ECHO
        PHX
        JSR WRITE_BYTE
        PLX
?RG_NO_ECHO:
        PLA

        JSR HEX_TO_NIBBLE
        BCC ?RG_EOL            ; Any non-hex byte acts as a separator.
        CPX #$04
        BCS ?RG_LOOP           ; Ignore extra digits until separator/enter.
        STA GUESS0,X
        INX
        CPX #$04
        BEQ ?RG_OK
        BRA ?RG_LOOP
?RG_EOL:
        CPX #$00
        BEQ ?RG_LOOP
        CPX #$04
        BEQ ?RG_OK
        JSR PRT_CRLF
        PRT_CSTRING MSG_NEED4
        LDX #$00
        BRA ?RG_LOOP
?RG_OK:
        JSR PRT_CRLF
        SEC
        RTS

SCORE_GUESS:
        STZ EXACT_CNT
        STZ NEAR_CNT

        LDX #$00
?CLR_FLAGS:
        STZ GFLAG0,X
        STZ SFLAG0,X
        INX
        CPX #$04
        BNE ?CLR_FLAGS

        ; Pass 1: exact matches (right value, right position).
        LDX #$00
?EXACT_LOOP:
        LDA GUESS0,X
        CMP SECRET0,X
        BNE ?EXACT_NEXT
        INC EXACT_CNT
        LDA #$01
        STA GFLAG0,X
        STA SFLAG0,X
?EXACT_NEXT:
        INX
        CPX #$04
        BNE ?EXACT_LOOP

        ; Pass 2: near matches (right value, wrong position).
        LDX #$00
?NEAR_OUTER:
        LDA GFLAG0,X
        BNE ?NEAR_NEXT_G
        LDY #$00
?NEAR_INNER:
        LDA SFLAG0,Y
        BNE ?NEAR_NEXT_S
        LDA GUESS0,X
        CMP SECRET0,Y
        BNE ?NEAR_NEXT_S
        INC NEAR_CNT
        LDA #$01
        STA GFLAG0,X
        STA SFLAG0,Y
        BRA ?NEAR_NEXT_G
?NEAR_NEXT_S:
        INY
        CPY #$04
        BNE ?NEAR_INNER
?NEAR_NEXT_G:
        INX
        CPX #$04
        BNE ?NEAR_OUTER
        RTS

SHOW_SCORE:
        PRT_CSTRING MSG_SCORE_A
        LDA EXACT_CNT
        JSR PRINT_DIGIT
        PRT_CSTRING MSG_SCORE_B
        LDA NEAR_CNT
        JSR PRINT_DIGIT
        JSR PRT_CRLF
        RTS

SHOW_LOSE:
        PRT_CSTRING MSG_LOSE_A
        LDX #$00
?SL_LOOP:
        LDA SECRET0,X
        JSR CVT_NIBBLE
        PHX
        JSR WRITE_BYTE
        PLX
        INX
        CPX #$04
        BNE ?SL_LOOP
        PRT_CSTRING MSG_LOSE_B
        RTS

ROLL_SECRET:
        LDX #$00
?RS_LOOP:
        TXA
        EOR VIA_T1L
        ROL A
        EOR VIA_T1H
        AND #$0F
        STA SECRET0,X
        INX
        CPX #$04
        BNE ?RS_LOOP
        RTS

PRINT_DIGIT:
        CLC
        ADC #'0'
        JSR WRITE_BYTE
        RTS

READ_BYTE:
        JSR WDC_READ_BYTE
        RTS

MSG_BANNER:             DB $0D,$0A,"=== MASTERMIND V5 (HEX) ===",$0D,$0A,"Type 4 hex digits (Enter optional).",$0D,$0A,0
MSG_TRIES:              DB $0D,$0A,"Tries left: ",0
MSG_PROMPT:             DB "  Enter 4 hex keys: ",0
MSG_NEED4:              DB "Need exactly 4 hex keys.",$0D,$0A,"Enter 4 hex keys: ",0
MSG_SCORE_A:            DB "Exact=",0
MSG_SCORE_B:            DB " Near=",0
MSG_WIN:                DB "Solved. New game!",0
MSG_LOSE_A:             DB "No tries left. Code was ",0
MSG_LOSE_B:             DB $0D,$0A,"New game.",0

