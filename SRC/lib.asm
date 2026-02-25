                PW 132
                
                INCLUDE ../INCLUDES/equates.inc

                MODULE LIB_COUNTERS
                XDEF READ_BYTE_COUNT
                XDEF WRITE_BYTE_COUNT

                UDATA
                ORG         $0400
READ_BYTE_COUNT:        DS          4   ; READ_BYTE CALL COUNT (32-BIT, LO..HI)
WRITE_BYTE_COUNT:       DS          4   ; WRITE_BYTE CALL COUNT (32-BIT, LO..HI)

                CODE

; ----------------------------------------------------------------------------
; PROVENANCE NOTE:
; The hex-to-ASCII adjustment pattern below follows the method described in:
;   - "6502 Assembly Language Programming" (Lance A. Leventhal), p. 7-3
; Leventhal cites (superscript 1; bibliographic entry listed on p. 7-15):
;   - D. R. Allison, "A Design Philosophy for Microcomputer Architectures."
;     Computer, February 1977, pp. 35-41.
; ----------------------------------------------------------------------------
                ENDMOD

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
                        PHA 
                        AND         #%00001111 ; MASK LO NIBBLE
                        JSR         CVT_PRT_NIBBLE ; PRINT LO NIBBLE
                        PLA
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

                        MODULE RNG_SEED_RAM_0_7EFF
                        XDEF  RNG_SEED_RAM_0_7EFF

                        XREF STR_PTR

; ----------------------------------------------------------------------------
; SUBROUTINE: RNG_SEED_RAM_0_7EFF
; DESCRIPTION: FOLDS RAM BYTES $0000..$7EFF INTO AN 8-BIT SEED
; INPUT: A = CURRENT/PRIOR SEED BYTE
; OUTPUT: A = SEEDED BYTE (NONZERO)
; CLOBBERS: X, Y, STR_PTR
; ----------------------------------------------------------------------------
RNG_SEED_RAM_0_7EFF:
                        EOR         #$A5 ; BIAS + MIX INPUT SEED
                        BNE         ?RSR_INIT_OK
                        LDA         #$5A ; NEVER START FROM ZERO
?RSR_INIT_OK:
                        TAX             ; X HOLDS RUNNING SEED
                        STZ         STR_PTR
                        STZ         STR_PTR+1
?RSR_PAGE_LOOP:
                        LDY         #$00
?RSR_BYTE_LOOP:
                        TXA
                        EOR         (STR_PTR),Y
                        CLC
                        ADC         STR_PTR+1
                        ROL         A
                        EOR         #$1D
                        TAX
                        INY
                        BNE         ?RSR_BYTE_LOOP
                        INC         STR_PTR+1
                        LDA         STR_PTR+1
                        CMP         #$7F ; STOP AFTER PAGE $7E
                        BNE         ?RSR_PAGE_LOOP
                        TXA
                        BNE         ?RSR_DONE
                        LDA         #$5A
?RSR_DONE:
                        RTS

                        ENDMOD

                        MODULE RNG8_NEXT
                        XDEF  RNG8_NEXT

; ----------------------------------------------------------------------------
; SUBROUTINE: RNG8_NEXT
; DESCRIPTION: ADVANCES AN 8-BIT LFSR (TAPS MASK $B8)
; INPUT: A = CURRENT STATE
; OUTPUT: A = NEXT STATE (NONZERO)
; CLOBBERS: A
; ----------------------------------------------------------------------------
RNG8_NEXT:
                        BNE         ?R8N_HAVE_SEED
                        LDA         #$5A
?R8N_HAVE_SEED:
                        LSR         A
                        BCC         ?R8N_NO_FB
                        EOR         #$B8
?R8N_NO_FB:
                        BNE         ?R8N_DONE
                        LDA         #$5A
?R8N_DONE:
                        RTS

                        ENDMOD

                        MODULE ADD_LE_BYTES
                        XDEF  ADD_LE_BYTES

                        XREF PTR_LEG
                        XREF PTR_TEMP

; ----------------------------------------------------------------------------
; SUBROUTINE: ADD_LE_BYTES
; DESCRIPTION: ADDS LITTLE-ENDIAN SRC INTO LITTLE-ENDIAN DST IN PLACE
; INPUT: PTR_LEG  = DST ADDRESS (RESULT WRITTEN IN PLACE)
;        PTR_TEMP = SRC ADDRESS
;        X        = DST LENGTH IN BYTES
;        Y        = SRC LENGTH IN BYTES (MUST BE <= X)
; OUTPUT: DST UPDATED, C=1 IF OVERFLOW PAST DST WIDTH, C=0 OTHERWISE
; CLOBBERS: A, X, Y, PTR_LEG, PTR_TEMP
; ----------------------------------------------------------------------------
ADD_LE_BYTES:
                        CLC
?ALB_SRC_LOOP:
                        CPY         #$00
                        BEQ         ?ALB_PROP_CARRY
                        LDA         (PTR_LEG)
                        ADC         (PTR_TEMP)
                        STA         (PTR_LEG)

                        INC         PTR_LEG
                        BNE         ?ALB_DST_PTR_OK
                        INC         PTR_LEG+1
?ALB_DST_PTR_OK:
                        INC         PTR_TEMP
                        BNE         ?ALB_SRC_PTR_OK
                        INC         PTR_TEMP+1
?ALB_SRC_PTR_OK:
                        DEY
                        DEX
                        BRA         ?ALB_SRC_LOOP

?ALB_PROP_CARRY:
                        BCC         ?ALB_DONE
?ALB_CARRY_LOOP:
                        CPX         #$00
                        BEQ         ?ALB_DONE
                        LDA         (PTR_LEG)
                        ADC         #$00
                        STA         (PTR_LEG)

                        INC         PTR_LEG
                        BNE         ?ALB_DST2_PTR_OK
                        INC         PTR_LEG+1
?ALB_DST2_PTR_OK:
                        DEX
                        BCS         ?ALB_CARRY_LOOP
?ALB_DONE:
                        RTS

                        ENDMOD

                        MODULE ADD_LE_BYTES_SAFE
                        XDEF  ADD_LE_BYTES_SAFE

                        XREF ADD_LE_BYTES
                        XREF PTR_LEG
                        XREF PTR_TEMP

; ----------------------------------------------------------------------------
; SUBROUTINE: ADD_LE_BYTES_SAFE
; DESCRIPTION: SAFE WRAPPER FOR ADD_LE_BYTES
; INPUT: PTR_LEG  = DST ADDRESS (RESULT WRITTEN IN PLACE)
;        PTR_TEMP = SRC ADDRESS
;        X        = DST LENGTH IN BYTES
;        Y        = SRC LENGTH IN BYTES (MUST BE <= X)
; OUTPUT: DST UPDATED, C=1 IF OVERFLOW PAST DST WIDTH, C=0 OTHERWISE
; FLAGS: C PRESERVED FROM ADD_LE_BYTES RESULT
; CLOBBERS: NONE (A, X, Y, PTR_LEG, PTR_TEMP PRESERVED)
; ----------------------------------------------------------------------------
ADD_LE_BYTES_SAFE:
                        PHA
                        PHX
                        PHY

                        LDA         PTR_LEG
                        PHA
                        LDA         PTR_LEG+1
                        PHA
                        LDA         PTR_TEMP
                        PHA
                        LDA         PTR_TEMP+1
                        PHA

                        JSR         ADD_LE_BYTES

                        PLA
                        STA         PTR_TEMP+1
                        PLA
                        STA         PTR_TEMP
                        PLA
                        STA         PTR_LEG+1
                        PLA
                        STA         PTR_LEG

                        PLY
                        PLX
                        PLA
                        RTS

                        ENDMOD

                    MODULE WRITE_BYTE
                    XDEF WRITE_BYTE

                    XREF WDC_WRITE_BYTE
                    XREF PUT_LED
                    XREF WRITE_BYTE_COUNT

; ----------------------------------------------------------------------------
; SUBROUTINE: WRITE_BYTE
; DESCRIPTION: SENDS CHAR TO UART, UPDATES LEDS, ADDS DELAY
; INPUT: ACC = CHAR TO SEND
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: NONE
; ----------------------------------------------------------------------------
WRITE_BYTE:
                        PHA
                        PHP
                        SEI
                        CLC
                        LDA         WRITE_BYTE_COUNT
                        ADC         #$01
                        STA         WRITE_BYTE_COUNT
                        LDA         WRITE_BYTE_COUNT+1
                        ADC         #$00
                        STA         WRITE_BYTE_COUNT+1
                        LDA         WRITE_BYTE_COUNT+2
                        ADC         #$00
                        STA         WRITE_BYTE_COUNT+2
                        LDA         WRITE_BYTE_COUNT+3
                        ADC         #$00
                        STA         WRITE_BYTE_COUNT+3
                        PLP
                        PLA
                        PHA
                        CMP         #$0D ; CR RESETS COLUMN TRACKING
                        BEQ         ?WB_CR
                        CMP         #$0A ; LF RESETS COLUMN TRACKING
                        BEQ         ?WB_LF
                        CMP         #$08 ; BS MOVES COLUMN BACK WHEN POSSIBLE
                        BEQ         ?WB_BS
                        CMP         #$20 ; TRACK ONLY PRINTABLE ASCII
                        BCC         ?WB_RAW
                        CMP         #$7F
                        BCS         ?WB_RAW

                        LDA         $7A ; TERM_COLS
                        CMP         #$14 ; 20 COLUMNS
                        BEQ         ?WB_CHK_WRAP
                        CMP         #$28 ; 40 COLUMNS
                        BEQ         ?WB_CHK_WRAP
                        CMP         #$50 ; 80 COLUMNS
                        BEQ         ?WB_CHK_WRAP
                        CMP         #$84 ; 132 COLUMNS
                        BEQ         ?WB_CHK_WRAP
                        LDA         #$50 ; DEFAULT TO 80 COLUMNS
                        STA         $7A ; TERM_COLS
?WB_CHK_WRAP:
                        LDX         $7E ; TERM_CUR_COL
                        CPX         $7A ; TERM_COLS
                        BCC         ?WB_SEND_PRINTABLE
                        LDA         #$0D
                        JSR         WDC_WRITE_BYTE
;                       JSR         PUT_LED
                        LDA         #$0A
                        JSR         WDC_WRITE_BYTE
;                       JSR         PUT_LED
                        STZ         $7E ; TERM_CUR_COL
?WB_SEND_PRINTABLE:
                        PLA
                        JSR         WDC_WRITE_BYTE
;                       JSR         PUT_LED
                        INC         $7E ; TERM_CUR_COL
                        RTS

?WB_CR:
                        PLA
                        JSR         WDC_WRITE_BYTE
;                       JSR         PUT_LED
                        STZ         $7E ; TERM_CUR_COL
                        RTS

?WB_LF:
                        PLA
                        JSR         WDC_WRITE_BYTE
;                       JSR         PUT_LED
                        STZ         $7E ; TERM_CUR_COL
                        RTS

?WB_BS:
                        PLA
                        JSR         WDC_WRITE_BYTE
;                       JSR         PUT_LED
                        LDA         $7E ; TERM_CUR_COL
                        BEQ         ?WB_DONE
                        DEC         $7E ; TERM_CUR_COL
?WB_DONE:
                        RTS

?WB_RAW:
                        PLA
                        JSR         WDC_WRITE_BYTE
;                       JSR         PUT_LED
                        RTS

                        ENDMOD

                        MODULE PUT_LED
                        XDEF PUT_LED

                        XREF LED_DATA

                        
PUT_LED:                STA         LED_DATA ; WRITE TO LEDS
                        RTS
