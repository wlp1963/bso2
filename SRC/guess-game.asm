; GUESS-GAME.ASM
; Short text game for W65C02EDU serial monitor environment.
                        CHIP       65C02
                        PL         60
                        PW         132
                        TITLE      GUESS-GAME
                        MACLIST    OFF

                        INCLUDE    equates.inc

        XREF PRT_C_STRING
        XREF PRT_CRLF
        XREF PRT_HEX
        XREF WRITE_BYTE
        XREF UTIL_TO_UPPER
        XREF HEX_TO_NIBBLE

        XDEF STR_PTR
        XDEF SECRET
        XDEF TRIES_LEFT

STR_PTR                 EQU        $0036
SECRET                  EQU        $0090
TRIES_LEFT              EQU        $0091

START:
        JSR NEW_ROUND

GAME_LOOP:
        PRT_CSTRING PROMPT
        JSR READ_BYTE
        JSR WRITE_BYTE
        JSR UTIL_TO_UPPER
        JSR HEX_TO_NIBBLE
        BCC BAD_INPUT
        JSR PRT_CRLF
        CMP SECRET
        BEQ WIN
        BCC TOO_LOW

TOO_HIGH:
        DEC TRIES_LEFT
        BEQ LOSE
        PRT_CSTRING MSG_HIGH
        JMP GAME_LOOP

TOO_LOW:
        DEC TRIES_LEFT
        BEQ LOSE
        PRT_CSTRING MSG_LOW
        JMP GAME_LOOP

BAD_INPUT:
        JSR PRT_CRLF
        PRT_CSTRING MSG_BAD
        JMP GAME_LOOP

WIN:
        PRT_CSTRING MSG_WIN
        JMP NEW_ROUND

LOSE:
        PRT_CSTRING MSG_LOSE_A
        LDA SECRET
        JSR PRT_HEX
        PRT_CSTRING MSG_LOSE_B
        JMP NEW_ROUND

NEW_ROUND:
        ; Use low timer bits as a quick pseudo-random seed (0..F).
        LDA VIA_T1L
        AND #$0F
        STA SECRET
        LDA #$05
        STA TRIES_LEFT
        PRT_CSTRING BANNER
        RTS

READ_BYTE:
        JSR WDC_READ_BYTE
        RTS

BANNER:                 DB $0D,$0A,"=== HEX GUESS ===",0
PROMPT:                 DB $0D,$0A,"Guess [0-9/A-F]: ",0
MSG_BAD:                DB "Invalid key. Use 0-9 or A-F.",0
MSG_HIGH:               DB "Too high.",0
MSG_LOW:                DB "Too low.",0
MSG_WIN:                DB "You win. New round!",0
MSG_LOSE_A:             DB "Out of tries. Secret was $",0
MSG_LOSE_B:             DB ". New round!",0
