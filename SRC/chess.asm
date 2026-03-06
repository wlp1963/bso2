; SPDX-License-Identifier: MIT
; Copyright (c) 2026 95west.us
; See LICENSE for full license text.
;
; CHESS.ASM
; Standalone userland chess scaffold for W65C02EDU.
; Supports legal moves for pawns and knights (other pieces static/unsupported).
                        CHIP       65C02
                        PL         60
                        PW         132
                        TITLE      CHESS
                        MACLIST    OFF

                        INCLUDE    equates.inc

        XREF PRT_C_STRING
        XREF PRT_CRLF
        XREF WRITE_BYTE
        XREF UTIL_TO_UPPER
        XREF LED_DATA
        XREF WDC_WRITE_BYTE

        XDEF STR_PTR

STR_PTR                 EQU        $0036
MOVE_LEN                EQU        $0090
MOVE_BUF0               EQU        $0091 ; up to 8 chars
SRC_FILE                EQU        $0099
SRC_RANK                EQU        $009A
DST_FILE                EQU        $009B
DST_RANK                EQU        $009C
SRC_IDX                 EQU        $009D
DST_IDX                 EQU        $009E
PIECE                   EQU        $009F
SIDE                    EQU        $00A0 ; 0=white, 1=black
TMP0                    EQU        $00A1
TMP1                    EQU        $00A2
TMP2                    EQU        $00A3
TMP_DX                  EQU        $00A4
TMP_DY                  EQU        $00A5

START:
        JSR RESET_GAME

MAIN_LOOP:
        JSR DRAW_BOARD
        PRT_CSTRING MSG_PROMPT
        JSR READ_LINE
        LDA MOVE_LEN
        BEQ MAIN_LOOP
        LDA MOVE_BUF0
        CMP #'Q'
        BEQ DO_QUIT
        JSR PARSE_MOVE
        BCS BAD_MOVE
        JSR TRY_MOVE
        BCS BAD_MOVE
        PRT_CSTRING MSG_OK
        JMP MAIN_LOOP

BAD_MOVE:
        PRT_CSTRING MSG_BAD
        JMP MAIN_LOOP

DO_QUIT:
        PRT_CSTRING MSG_QUIT
        BRK $00
        WAI
        JMP START

RESET_GAME:
        LDX #$00
?RG_COPY:
        LDA INIT_BOARD,X
        STA BOARD,X
        INX
        CPX #$40
        BNE ?RG_COPY
        STZ SIDE
        PRT_CSTRING MSG_BANNER
        RTS

READ_LINE:
        LDX #$00
?RL_LOOP:
        JSR READ_BYTE
        AND #$7F
        CMP #$0D
        BEQ ?RL_DONE
        CMP #$0A
        BEQ ?RL_DONE
        JSR UTIL_TO_UPPER
        PHA
        CMP #$20
        BCC ?RL_NO_ECHO
        CMP #$7F
        BCS ?RL_NO_ECHO
        PHX
        JSR WRITE_BYTE
        PLX
?RL_NO_ECHO:
        PLA
        CPX #$08
        BCS ?RL_LOOP
        STA MOVE_BUF0,X
        INX
        BRA ?RL_LOOP
?RL_DONE:
        STX MOVE_LEN
        JSR PRT_CRLF
        RTS

DRAW_BOARD:
        PRT_CSTRING MSG_SIDE
        LDA SIDE
        BEQ ?DB_WHITE
        LDA #'B'
        BRA ?DB_SIDE_OUT
?DB_WHITE:
        LDA #'W'
?DB_SIDE_OUT:
        JSR WRITE_BYTE
        JSR PRT_CRLF

        LDX #$07 ; rank 8..1
?DB_RANK_LOOP:
        TXA
        CLC
        ADC #'1'
        PHX
        JSR WRITE_BYTE
        LDA #' '
        JSR WRITE_BYTE
        PLX

        TXA
        ASL A
        ASL A
        ASL A
        STA TMP0 ; row base index

        LDY #$00 ; file 0..7
?DB_FILE_LOOP:
        TYA
        STA TMP1
        CLC
        ADC TMP0
        TAY
        LDA BOARD,Y
        PHX
        JSR PIECE_TO_CHAR
        JSR WRITE_BYTE
        LDA #' '
        JSR WRITE_BYTE
        PLX
        LDY TMP1
        INY
        CPY #$08
        BNE ?DB_FILE_LOOP

        PHX
        JSR PRT_CRLF
        PLX
        DEX
        BPL ?DB_RANK_LOOP
        PRT_CSTRING MSG_FILES
        RTS

PIECE_TO_CHAR:
        CMP #$0F
        BCC ?PTC_IN_RANGE
        LDA #'?'
        RTS
?PTC_IN_RANGE:
        TAX
        LDA PIECE_CHARS,X
        RTS

PARSE_MOVE:
        LDA MOVE_LEN
        CMP #$04
        BCC ?PM_ERR

        LDA MOVE_BUF0
        JSR PARSE_FILE_CHAR
        BCS ?PM_ERR
        STA SRC_FILE

        LDA MOVE_BUF0+1
        JSR PARSE_RANK_CHAR
        BCS ?PM_ERR
        STA SRC_RANK

        LDA MOVE_BUF0+2
        JSR PARSE_FILE_CHAR
        BCS ?PM_ERR
        STA DST_FILE

        LDA MOVE_BUF0+3
        JSR PARSE_RANK_CHAR
        BCS ?PM_ERR
        STA DST_RANK

        LDA SRC_RANK
        ASL A
        ASL A
        ASL A
        CLC
        ADC SRC_FILE
        STA SRC_IDX

        LDA DST_RANK
        ASL A
        ASL A
        ASL A
        CLC
        ADC DST_FILE
        STA DST_IDX

        LDA SRC_IDX
        CMP DST_IDX
        BEQ ?PM_ERR
        CLC
        RTS
?PM_ERR:
        SEC
        RTS

PARSE_FILE_CHAR:
        CMP #'A'
        BCC ?PFC_ERR
        CMP #'H'+1
        BCS ?PFC_ERR
        SEC
        SBC #'A'
        CLC
        RTS
?PFC_ERR:
        SEC
        RTS

PARSE_RANK_CHAR:
        CMP #'1'
        BCC ?PRC_ERR
        CMP #'8'+1
        BCS ?PRC_ERR
        SEC
        SBC #'1'
        CLC
        RTS
?PRC_ERR:
        SEC
        RTS

TRY_MOVE:
        JSR VALIDATE_MOVE
        BCS ?TM_ERR
        LDX SRC_IDX
        LDA BOARD,X
        STA PIECE
        LDA #$00
        STA BOARD,X
        LDX DST_IDX
        LDA PIECE
        STA BOARD,X
        LDA SIDE
        EOR #$01
        STA SIDE
        CLC
        RTS
?TM_ERR:
        SEC
        RTS

VALIDATE_MOVE:
        LDX SRC_IDX
        LDA BOARD,X
        STA PIECE
        BEQ ?VM_ERR
        JSR PIECE_COLOR
        BCS ?VM_ERR
        CMP SIDE
        BNE ?VM_ERR

        LDX DST_IDX
        LDA BOARD,X
        BEQ ?VM_DISPATCH
        JSR PIECE_COLOR
        BCS ?VM_ERR
        CMP SIDE
        BEQ ?VM_ERR

?VM_DISPATCH:
        LDA PIECE
        CMP #$01
        BNE ?VM_CHK_BP
        JMP VALID_PAWN_WHITE
?VM_CHK_BP:
        CMP #$09
        BNE ?VM_CHK_WN
        JMP VALID_PAWN_BLACK
?VM_CHK_WN:
        CMP #$02
        BNE ?VM_CHK_BN
        JMP VALID_KNIGHT
?VM_CHK_BN:
        CMP #$0A
        BNE ?VM_UNSUP
        JMP VALID_KNIGHT
?VM_UNSUP:
        SEC             ; unsupported piece for now
        RTS
?VM_ERR:
        SEC
        RTS

PIECE_COLOR:
        BEQ ?PC_ERR
        CMP #$07
        BCC ?PC_WHITE
        CMP #$09
        BCC ?PC_ERR
        CMP #$0F
        BCC ?PC_BLACK
?PC_ERR:
        SEC
        RTS
?PC_WHITE:
        LDA #$00
        CLC
        RTS
?PC_BLACK:
        LDA #$01
        CLC
        RTS

VALID_PAWN_WHITE:
        LDA DST_RANK
        SEC
        SBC SRC_RANK
        STA TMP0 ; dy
        LDA DST_FILE
        SEC
        SBC SRC_FILE
        STA TMP1 ; dx

        LDA TMP1
        BEQ ?VPW_FWD
        CMP #$01
        BEQ ?VPW_CAP
        CMP #$FF
        BEQ ?VPW_CAP
        SEC
        RTS

?VPW_FWD:
        LDA TMP0
        CMP #$01
        BEQ ?VPW_FWD1
        CMP #$02
        BNE ?VPW_ERR
        LDA SRC_RANK
        CMP #$01
        BNE ?VPW_ERR
        LDX DST_IDX
        LDA BOARD,X
        BNE ?VPW_ERR
        LDA SRC_IDX
        CLC
        ADC #$08
        TAX
        LDA BOARD,X
        BNE ?VPW_ERR
        CLC
        RTS
?VPW_FWD1:
        LDX DST_IDX
        LDA BOARD,X
        BEQ ?VPW_OK
?VPW_ERR:
        SEC
        RTS
?VPW_OK:
        CLC
        RTS

?VPW_CAP:
        LDA TMP0
        CMP #$01
        BNE ?VPW_ERR
        LDX DST_IDX
        LDA BOARD,X
        BEQ ?VPW_ERR
        JSR PIECE_COLOR
        BCS ?VPW_ERR
        CMP #$01
        BNE ?VPW_ERR
        CLC
        RTS

VALID_PAWN_BLACK:
        LDA DST_RANK
        SEC
        SBC SRC_RANK
        STA TMP0 ; dy
        LDA DST_FILE
        SEC
        SBC SRC_FILE
        STA TMP1 ; dx

        LDA TMP1
        BEQ ?VPB_FWD
        CMP #$01
        BEQ ?VPB_CAP
        CMP #$FF
        BEQ ?VPB_CAP
        SEC
        RTS

?VPB_FWD:
        LDA TMP0
        CMP #$FF
        BEQ ?VPB_FWD1
        CMP #$FE
        BNE ?VPB_ERR
        LDA SRC_RANK
        CMP #$06
        BNE ?VPB_ERR
        LDX DST_IDX
        LDA BOARD,X
        BNE ?VPB_ERR
        LDA SRC_IDX
        SEC
        SBC #$08
        TAX
        LDA BOARD,X
        BNE ?VPB_ERR
        CLC
        RTS
?VPB_FWD1:
        LDX DST_IDX
        LDA BOARD,X
        BEQ ?VPB_OK
?VPB_ERR:
        SEC
        RTS
?VPB_OK:
        CLC
        RTS

?VPB_CAP:
        LDA TMP0
        CMP #$FF
        BNE ?VPB_ERR
        LDX DST_IDX
        LDA BOARD,X
        BEQ ?VPB_ERR
        JSR PIECE_COLOR
        BCS ?VPB_ERR
        CMP #$00
        BNE ?VPB_ERR
        CLC
        RTS

VALID_KNIGHT:
        LDA DST_RANK
        SEC
        SBC SRC_RANK
        JSR ABS8
        STA TMP_DY
        LDA DST_FILE
        SEC
        SBC SRC_FILE
        JSR ABS8
        STA TMP_DX

        LDA TMP_DX
        CMP #$01
        BNE ?VK_CHK2
        LDA TMP_DY
        CMP #$02
        BEQ ?VK_OK
?VK_CHK2:
        LDA TMP_DX
        CMP #$02
        BNE ?VK_ERR
        LDA TMP_DY
        CMP #$01
        BEQ ?VK_OK
?VK_ERR:
        SEC
        RTS
?VK_OK:
        CLC
        RTS

ABS8:
        BPL ?ABS_DONE
        EOR #$FF
        CLC
        ADC #$01
?ABS_DONE:
        RTS

READ_BYTE:
        JSR WDC_READ_BYTE
        RTS

MSG_BANNER:             DB $0D,$0A,"=== CHESS (PAWN+KNIGHT) @ $6000 ===",$0D,$0A
                        DB "Enter moves like E2E4. Q to quit.",$0D,$0A,0
MSG_SIDE:               DB $0D,$0A,"Side: ",0
MSG_FILES:              DB "  A B C D E F G H",$0D,$0A,0
MSG_PROMPT:             DB $0D,$0A,"move> ",0
MSG_BAD:                DB "Illegal/unsupported move.",0
MSG_OK:                 DB "OK",0
MSG_QUIT:               DB $0D,$0A,"BREAK",0

PIECE_CHARS:            DB '.', 'P', 'N', 'B', 'R', 'Q', 'K', '?', '?', 'p', 'n', 'b', 'r', 'q', 'k'

INIT_BOARD:
                        DB 4,2,3,5,6,3,2,4
                        DB 1,1,1,1,1,1,1,1
                        DB 0,0,0,0,0,0,0,0
                        DB 0,0,0,0,0,0,0,0
                        DB 0,0,0,0,0,0,0,0
                        DB 0,0,0,0,0,0,0,0
                        DB 9,9,9,9,9,9,9,9
                        DB 12,10,11,13,14,11,10,12

BOARD:                  DS 64
