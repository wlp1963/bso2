                PW 132
                
                INCLUDE ../INCLUDES/equates.inc
                
; ----------------------------------------------------------------------------
; PROVENANCE NOTE:
; The hex-to-ASCII adjustment pattern below follows the method described in:
;   - "6502 Assembly Language Programming" (Lance A. Leventhal), p. 7-3
; Leventhal cites (superscript 1; bibliographic entry listed on p. 7-15):
;   - D. R. Allison, "A Design Philosophy for Microcomputer Architectures."
;     Computer, February 1977, pp. 35-41.
; ----------------------------------------------------------------------------

                MODULE CVT_NIBBLE
                XDEF CVT_NIBBLE

; ----------------------------------------------------------------------------
; SUBROUTINE: CVT_NIBBLE
; DESCRIPTION: CONVERTS LOW NIBBLE IN ACC TO ASCII HEX
; INPUT: ACC = VALUE (0-F)
; OUTPUT: ACC = ASCII ('0'-'9', 'A'-'F')
; FLAGS: DECIMAL MODIFIED THEN CLEARED
; ZP USED: NONE
; ----------------------------------------------------------------------------
CVT_NIBBLE:
                        SED             ; SET DECIMAL
                        CLC             ; CLEAR CARRY
                        ADC         #$90 ; MAGIC HEX ADC
                        ADC         #$40 ; ADJUST FOR ASCII
                        CLD             ; CLEAR DECIMAL
                        RTS             ; RETURN CHAR

                        ENDMOD

                        MODULE CVT_PRT_NIBBLE
                        XDEF CVT_PRT_NIBBLE

                        XREF CVT_NIBBLE
                        XREF WRITE_BYTE

; ----------------------------------------------------------------------------
; SUBROUTINE: CVT_PRT_NIBBLE
; DESCRIPTION: CONVERTS AND PRINTS NIBBLE
; INPUT: ACC = VALUE
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: NONE
; ----------------------------------------------------------------------------
CVT_PRT_NIBBLE:
                        JSR         CVT_NIBBLE ; CONVERT TO ASCII
                        JSR         WRITE_BYTE ; PRINT CHAR
                        RTS             ; DONE

                        ENDMOD

                        MODULE PRT_HEX
                        XDEF PRT_HEX

                        XREF CVT_PRT_NIBBLE
; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_HEX
; DESCRIPTION: PRINTS 8-BIT VALUE IN HEX
; INPUT: ACC = BYTE
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: NONE
; ----------------------------------------------------------------------------
PRT_HEX:
                        PHA
                        LSR         A   ; SHIFT HI NIBBLE
                        LSR         A   ; DOWN
                        LSR         A   ; TO
                        LSR         A   ; LO
                        JSR         CVT_PRT_NIBBLE ; PRINT HI NIBBLE
                        PLA
                        AND         #%00001111 ; MASK LO NIBBLE
                        JSR         CVT_PRT_NIBBLE ; PRINT LO NIBBLE
                        RTS             ; DONE

                        ENDMOD

                        MODULE PRT_HEX_WORD_AX
                        XDEF PRT_HEX_WORD_AX

                        XREF PRT_HEX
; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_HEX_WORD_AX
; DESCRIPTION: PRINTS 16-BIT WORD IN HEX (HIGH BYTE THEN LOW BYTE)
; INPUT: A = HIGH BYTE, X = LOW BYTE
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ----------------------------------------------------------------------------
PRT_HEX_WORD_AX:
                        PHX
                        JSR         PRT_HEX
                        PLX
                        TXA
                        JSR         PRT_HEX
                        RTS

                        ENDMOD

                        MODULE PRT_WORD_FROM_PARSE
                        XDEF PRT_WORD_FROM_PARSE

                        XREF CMD_PARSE_VAL
                        XREF PRT_HEX_WORD_AX

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_WORD_FROM_PARSE
; DESCRIPTION: PRINTS CMD_PARSE_VAL AS 16-BIT HEX (HI THEN LO)
; INPUT: CMD_PARSE_VAL/CMD_PARSE_VAL+1
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ----------------------------------------------------------------------------
PRT_WORD_FROM_PARSE:    PHA 
                        PHX 
                        LDA         CMD_PARSE_VAL+1
                        LDX         CMD_PARSE_VAL
                        JSR         PRT_HEX_WORD_AX
                        PLX 
                        PLA 
                        RTS

                        ENDMOD

                        MODULE PRT_C_STRING
                        XDEF PRT_C_STRING

                        XREF STR_PTR
                        XREF WRITE_BYTE
; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_C_STRING
; DESCRIPTION: PRINTS NULL-TERMINATED STRING
; INPUT: STR_PTR (ZP) = ADDR OF STRING
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: STR_PTR
; ----------------------------------------------------------------------------
PRT_C_STRING:           PHA
                        PHY 
                        LDY         #$00 ; RESET INDEX
?STRING_LOOP:           LDA         (STR_PTR),Y ; GET CHAR
                        BEQ         ?STRING_DONE ; EXIT IF NULL
                        JSR         WRITE_BYTE ; SEND CHAR
                        INY             ; NEXT INDEX
                        BNE         ?STRING_LOOP ; LOOP IF NOT WRAP
                        INC         STR_PTR+1 ; CROSS PAGE
                        BRA         ?STRING_LOOP ; REPEAT
?STRING_DONE:           PLY 
                        PLA
                        RTS             ; DONE

                        ENDMOD

                        MODULE HEX_TO_NIBBLE
                        XDEF HEX_TO_NIBBLE

; ----------------------------------------------------------------------------
; SUBROUTINE: HEX_TO_NIBBLE
; DESCRIPTION: CONVERTS ASCII HEX [0-9A-F] IN ACC TO BINARY NIBBLE
; INPUT: ACC = ASCII CHAR
; OUTPUT: ACC = NIBBLE, C=1 IF VALID, C=0 IF INVALID
; ----------------------------------------------------------------------------
HEX_TO_NIBBLE:
                        CMP         #'0'
                        BCC         ?HTN_BAD
                        CMP         #':'
                        BCC         ?HTN_DEC
                        CMP         #'A'
                        BCC         ?HTN_BAD
                        CMP         #'G'
                        BCS         ?HTN_BAD
                        SEC
                        SBC         #'A'-10
                        RTS
?HTN_DEC:
                        SEC
                        SBC         #'0'
                        RTS
?HTN_BAD:
                        CLC
                        RTS

                        ENDMOD

                        MODULE PRT_CRLF
                        XDEF PRT_CRLF

                        XREF WRITE_BYTE

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_CRLF
; DESCRIPTION: PRINTS CARRIAGE RETURN + LINE FEED
; ----------------------------------------------------------------------------
PRT_CRLF:               PHA   ; SAVE ACC
                        LDA         #$0D ; CR
                        JSR         WRITE_BYTE ; SEND
                        LDA         #$0A ; LF
                        JSR         WRITE_BYTE ; SEND
                        PLA
                        RTS             ; DONE

                        ENDMOD 

                        MODULE READ_BYTE_ECHO
                        XDEF READ_BYTE_ECHO

                        XREF READ_BYTE
                        XREF WRITE_BYTE

; ----------------------------------------------------------------------------
; SUBROUTINE: READ_BYTE_ECHO
; DESCRIPTION: READS ONE BYTE (BLOCKING) AND ECHOES IT TO UART
; INPUT: NONE
; OUTPUT: A = BYTE RECEIVED
; ----------------------------------------------------------------------------
READ_BYTE_ECHO:
                        JSR         READ_BYTE
                        PHA
                        JSR         WRITE_BYTE
                        PLA
                        RTS

                        ENDMOD

                        MODULE  UTIL_TO_UPPER
                        XDEF  UTIL_TO_UPPER

; ----------------------------------------------------------------------------
; SUBROUTINE: UTIL_TO_UPPER
; DESCRIPTION: CONVERTS ASCII a-z TO A-Z
; INPUT: ACC = BYTE
; OUTPUT: ACC = UPPERCASE IF a-z, OTHERWISE UNCHANGED
; ----------------------------------------------------------------------------
UTIL_TO_UPPER:
                        CMP         #'a'
                        BCC         ?TOUP_DONE
                        CMP         #'{'
                        BCS         ?TOUP_DONE
                        AND         #%11011111 ; CLEAR ASCII LOWERCASE BIT
?TOUP_DONE:
                        RTS

                        ENDMOD

                    MODULE WRITE_BYTE
                    XDEF WRITE_BYTE

                    XREF WDC_WRITE_BYTE
                    XREF PUT_LED

; ----------------------------------------------------------------------------
; SUBROUTINE: WRITE_BYTE
; DESCRIPTION: SENDS CHAR TO UART, UPDATES LEDS, ADDS DELAY
; INPUT: ACC = CHAR TO SEND
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: NONE
; ----------------------------------------------------------------------------
WRITE_BYTE:
                        JSR         WDC_WRITE_BYTE ; CALL ROM SEND
                        JSR         PUT_LED ; SHOW ON LEDS
                        RTS

                        ENDMOD

                        MODULE PUT_LED
                        XDEF PUT_LED

                        XREF LED_DATA

                        
PUT_LED:                STA         LED_DATA ; WRITE TO LEDS
                        RTS
