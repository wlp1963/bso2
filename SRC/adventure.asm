; SPDX-License-Identifier: MIT
; Copyright (c) 2026 95west.us
; See LICENSE for full license text.
;
; ADVENTURE.ASM
; Small standalone text adventure for W65C02EDU userland.
; One-letter commands: N S E W L T O I H C Q
                        CHIP       65C02
                        PL         60
                        PW         132
                        TITLE      ADVENTURE
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
ROOM                    EQU        $0090 ; 0=cell,1=hall,2=closet,3=vault
HAS_KEY                 EQU        $0091
DOOR_OPEN               EQU        $0092
HAS_TREASURE            EQU        $0093
CMD_LEN                 EQU        $0094
CMD_BUF0                EQU        $0095 ; 16-byte line buffer

START:
        JSR NEW_GAME

MAIN_LOOP:
        JSR SHOW_ROOM
        PRT_CSTRING MSG_PROMPT
        JSR READ_LINE
        LDX #$00
        JSR SKIP_SPACES
        LDA CMD_BUF0,X
        BEQ MAIN_LOOP

        CMP #'N'
        BNE ?ML_CHK_S
        JMP CMD_N
?ML_CHK_S:
        CMP #'S'
        BNE ?ML_CHK_E
        JMP CMD_S
?ML_CHK_E:
        CMP #'E'
        BNE ?ML_CHK_W
        JMP CMD_E
?ML_CHK_W:
        CMP #'W'
        BNE ?ML_CHK_L
        JMP CMD_W
?ML_CHK_L:
        CMP #'L'
        BNE ?ML_CHK_T
        JMP CMD_LOOK
?ML_CHK_T:
        CMP #'T'
        BNE ?ML_CHK_O
        JMP CMD_TAKE
?ML_CHK_O:
        CMP #'O'
        BNE ?ML_CHK_I
        JMP CMD_OPEN
?ML_CHK_I:
        CMP #'I'
        BNE ?ML_CHK_H
        JMP CMD_INV
?ML_CHK_H:
        CMP #'H'
        BNE ?ML_CHK_QM
        JMP CMD_HELP
?ML_CHK_QM:
        CMP #'?'
        BNE ?ML_CHK_C
        JMP CMD_HELP
?ML_CHK_C:
        CMP #'C'
        BNE ?ML_CHK_Q
        JMP CMD_RESET
?ML_CHK_Q:
        CMP #'Q'
        BNE ?ML_BAD
        JMP CMD_QUIT
?ML_BAD:
        PRT_CSTRING MSG_BAD
        JMP MAIN_LOOP

NEW_GAME:
        STZ ROOM
        STZ HAS_KEY
        STZ DOOR_OPEN
        STZ HAS_TREASURE
        PRT_CSTRING MSG_BANNER
        RTS

CMD_RESET:
        PRT_CSTRING MSG_RESET
        JSR NEW_GAME
        JMP MAIN_LOOP

CMD_HELP:
        PRT_CSTRING MSG_HELP
        JMP MAIN_LOOP

CMD_LOOK:
        ; MAIN LOOP WILL REDRAW ROOM.
        JMP MAIN_LOOP

CMD_INV:
        PRT_CSTRING MSG_INV
        LDA HAS_KEY
        BEQ ?CI_NO_KEY
        PRT_CSTRING MSG_YES
        BRA ?CI_TREASURE
?CI_NO_KEY:
        PRT_CSTRING MSG_NO
?CI_TREASURE:
        PRT_CSTRING MSG_INV2
        LDA HAS_TREASURE
        BEQ ?CI_NO_TR
        PRT_CSTRING MSG_YES
        JMP MAIN_LOOP
?CI_NO_TR:
        PRT_CSTRING MSG_NO
        JMP MAIN_LOOP

CMD_OPEN:
        LDA ROOM
        CMP #$01
        BNE ?CO_NO_DOOR
        LDA DOOR_OPEN
        BNE ?CO_ALREADY
        LDA HAS_KEY
        BEQ ?CO_LOCKED
        LDA #$01
        STA DOOR_OPEN
        PRT_CSTRING MSG_DOOR_OPEN
        JMP MAIN_LOOP
?CO_LOCKED:
        PRT_CSTRING MSG_DOOR_LOCKED
        JMP MAIN_LOOP
?CO_ALREADY:
        PRT_CSTRING MSG_DOOR_ALREADY
        JMP MAIN_LOOP
?CO_NO_DOOR:
        PRT_CSTRING MSG_NO_DOOR
        JMP MAIN_LOOP

CMD_TAKE:
        LDA ROOM
        CMP #$02
        BEQ ?CT_CLOSET
        CMP #$03
        BEQ ?CT_VAULT
        PRT_CSTRING MSG_TAKE_NOTHING
        JMP MAIN_LOOP
?CT_CLOSET:
        LDA HAS_KEY
        BNE ?CT_HAVE_KEY
        LDA #$01
        STA HAS_KEY
        PRT_CSTRING MSG_TAKE_KEY
        JMP MAIN_LOOP
?CT_HAVE_KEY:
        PRT_CSTRING MSG_TAKE_NOTHING
        JMP MAIN_LOOP
?CT_VAULT:
        LDA HAS_TREASURE
        BNE ?CT_HAVE_TREASURE
        LDA #$01
        STA HAS_TREASURE
        PRT_CSTRING MSG_WIN
        JMP MAIN_LOOP
?CT_HAVE_TREASURE:
        PRT_CSTRING MSG_TAKE_NOTHING
        JMP MAIN_LOOP

CMD_N:
        LDA ROOM
        CMP #$00
        BNE ?CN_BLOCK
        LDA #$01
        STA ROOM
        JMP MAIN_LOOP
?CN_BLOCK:
        PRT_CSTRING MSG_BLOCKED
        JMP MAIN_LOOP

CMD_S:
        LDA ROOM
        CMP #$01
        BNE ?CS_BLOCK
        LDA #$00
        STA ROOM
        JMP MAIN_LOOP
?CS_BLOCK:
        PRT_CSTRING MSG_BLOCKED
        JMP MAIN_LOOP

CMD_E:
        LDA ROOM
        CMP #$02
        BEQ ?CE_CLOSET_TO_HALL
        CMP #$01
        BEQ ?CE_HALL_TO_VAULT
        PRT_CSTRING MSG_BLOCKED
        JMP MAIN_LOOP
?CE_CLOSET_TO_HALL:
        LDA #$01
        STA ROOM
        JMP MAIN_LOOP
?CE_HALL_TO_VAULT:
        LDA DOOR_OPEN
        BEQ ?CE_LOCKED
        LDA #$03
        STA ROOM
        JMP MAIN_LOOP
?CE_LOCKED:
        PRT_CSTRING MSG_DOOR_LOCKED
        JMP MAIN_LOOP

CMD_W:
        LDA ROOM
        CMP #$01
        BEQ ?CW_HALL_TO_CLOSET
        CMP #$03
        BEQ ?CW_VAULT_TO_HALL
        PRT_CSTRING MSG_BLOCKED
        JMP MAIN_LOOP
?CW_HALL_TO_CLOSET:
        LDA #$02
        STA ROOM
        JMP MAIN_LOOP
?CW_VAULT_TO_HALL:
        LDA #$01
        STA ROOM
        JMP MAIN_LOOP

CMD_QUIT:
        PRT_CSTRING MSG_QUIT
        BRK $00
        WAI
        JMP START

SHOW_ROOM:
        LDA ROOM
        CMP #$00
        BEQ ?SR_CELL
        CMP #$01
        BEQ ?SR_HALL
        CMP #$02
        BEQ ?SR_CLOSET
        CMP #$03
        BEQ ?SR_VAULT
        PRT_CSTRING MSG_BAD_ROOM
        RTS
?SR_CELL:
        PRT_CSTRING MSG_ROOM_CELL
        RTS
?SR_HALL:
        PRT_CSTRING MSG_ROOM_HALL
        LDA DOOR_OPEN
        BEQ ?SR_HALL_LOCKED
        PRT_CSTRING MSG_HALL_DOOR_OPEN
        RTS
?SR_HALL_LOCKED:
        PRT_CSTRING MSG_HALL_DOOR_LOCKED
        RTS
?SR_CLOSET:
        PRT_CSTRING MSG_ROOM_CLOSET
        LDA HAS_KEY
        BNE ?SR_CLOSET_EMPTY
        PRT_CSTRING MSG_KEY_HERE
        RTS
?SR_CLOSET_EMPTY:
        PRT_CSTRING MSG_CLOSET_EMPTY
        RTS
?SR_VAULT:
        PRT_CSTRING MSG_ROOM_VAULT
        LDA HAS_TREASURE
        BNE ?SR_VAULT_EMPTY
        PRT_CSTRING MSG_TREASURE_HERE
        RTS
?SR_VAULT_EMPTY:
        PRT_CSTRING MSG_VAULT_EMPTY
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
        CPX #$10
        BCS ?RL_LOOP
        STA CMD_BUF0,X
        INX
        BRA ?RL_LOOP
?RL_DONE:
        STX CMD_LEN
        TXA
        BEQ ?RL_ZERO
        DEX
        LDA #$00
        STA CMD_BUF0+1,X ; NULL-TERMINATE IF NONEMPTY
        BRA ?RL_CRLF
?RL_ZERO:
        LDA #$00
        STA CMD_BUF0
?RL_CRLF:
        JSR PRT_CRLF
        RTS

SKIP_SPACES:
?SS_LOOP:
        LDA CMD_BUF0,X
        BEQ ?SS_DONE
        CMP #' '
        BNE ?SS_DONE
        INX
        BRA ?SS_LOOP
?SS_DONE:
        RTS

READ_BYTE:
        JSR WDC_READ_BYTE
        RTS

MSG_BANNER:             DB $0D,$0A,"=== MINI ADVENTURE @ $7000 ===",$0D,$0A
                        DB "Goal: get KEY, open DOOR, take TREASURE.",$0D,$0A,0
MSG_PROMPT:             DB $0D,$0A,"> ",0
MSG_HELP:               DB "Commands: N S E W L T O I H C Q",0
MSG_BAD:                DB "I do not understand.",0
MSG_BLOCKED:            DB "You cannot go that way.",0
MSG_NO_DOOR:            DB "There is nothing to open here.",0
MSG_DOOR_LOCKED:        DB "The east door is locked.",0
MSG_DOOR_OPEN:          DB "You unlock and open the east door.",0
MSG_DOOR_ALREADY:       DB "The east door is already open.",0
MSG_TAKE_KEY:           DB "You take the brass key.",0
MSG_TAKE_NOTHING:       DB "There is nothing useful to take.",0
MSG_WIN:                DB "You take the treasure. You win!",0
MSG_RESET:              DB "Resetting adventure...",0
MSG_QUIT:               DB $0D,$0A,"BREAK",0
MSG_BAD_ROOM:           DB "You are nowhere. (State error)",0
MSG_INV:                DB "KEY: ",0
MSG_INV2:               DB "  TREASURE: ",0
MSG_YES:                DB "YES",0
MSG_NO:                 DB "NO",0

MSG_ROOM_CELL:          DB $0D,$0A,"[CELL] Stone walls. Exit: N",0
MSG_ROOM_HALL:          DB $0D,$0A,"[HALL] Narrow hall. Exits: S, W, E",0
MSG_HALL_DOOR_LOCKED:   DB "  (E door locked)",0
MSG_HALL_DOOR_OPEN:     DB "  (E door open)",0
MSG_ROOM_CLOSET:        DB $0D,$0A,"[CLOSET] Dusty closet. Exit: E",0
MSG_KEY_HERE:           DB "  A brass key glints here.",0
MSG_CLOSET_EMPTY:       DB "  Empty shelves.",0
MSG_ROOM_VAULT:         DB $0D,$0A,"[VAULT] Cold vault. Exit: W",0
MSG_TREASURE_HERE:      DB "  A small treasure chest sits here.",0
MSG_VAULT_EMPTY:        DB "  Empty pedestal.",0
