; Provenance and licensing:
; This file includes material derived/adapted from WDC EDU sample sources:
;   - 02EDU_GettingStarted.zip/EDU_GettingStarted/ASM/EDUGS_FLASH.asm
;   - 02EDU_GettingStarted.zip/EDU_GettingStarted/ASM/02EDU.inc
; Upstream-derived portions remain subject to the original WDC notices/terms.
; Project-original additions/modifications in this file are
; Copyright (c) 2026 95west.us and licensed under MIT.
; See LICENSE and THIRD_PARTY_NOTICES.md for details.

; ****************************************************************************
; * *
; * BSO2 MONITOR FOR W65C02EDU                                               *
; * REV: 2.0 (VECTOR INIT ON RESET)                                          *
; * *
; ****************************************************************************

                        MACLIST     OFF
                        CHIP        65C02
                        LONGI       OFF
                        LONGA       OFF
                        PL          60
                        PW          132
                        TITLE       'BSO2'

                        INCLUDE     MACROS.INC

; --- EXTERNAL WDC ROM ENTRY POINTS ---
WDC_INIT_SERIAL         EQU         $F800 ; ROM SERIAL INIT
WDC_WRITE_BYTE          EQU         $F803 ; ROM SERIAL OUT
WDC_READ_BYTE           EQU         $F806 ; ROM SERIAL IN
WDC_CHECK_BYTE          EQU         $F809 ; ROM SERIAL STATUS

; --- W65C21 PIA REGISTERS ---
PIA                     EQU         $7FA0 ; PIA BASE ADDR
PIA_PA                  EQU         PIA ; PORT A DATA
PIA_DDRA                EQU         PIA ; PORT A DIR
PIA_CRA                 EQU         PIA+1 ; PORT A CTRL
PIA_PB                  EQU         PIA+2 ; PORT B DATA
PIA_DDRB                EQU         PIA+2 ; PORT B DIR
PIA_CRB                 EQU         PIA+3 ; PORT B CTRL

; --- LED CONSTANTS ---
LED0                    EQU         %00000001 ; LED BIT 0
LED1                    EQU         %00000010 ; LED BIT 1
LED2                    EQU         %00000100 ; LED BIT 2
LED3                    EQU         %00001000 ; LED BIT 3
LED4                    EQU         %00010000 ; LED BIT 4
LED5                    EQU         %00100000 ; LED BIT 5
LED6                    EQU         %01000000 ; LED BIT 6
LED7                    EQU         %10000000 ; LED BIT 7
LED_ALL                 EQU         %11111111 ; ALL LEDS ON
LED_NONE                EQU         %00000000 ; ALL LEDS OFF

LED_DATA                EQU         PIA_PA ; MAP LEDS TO PORT A
LED_DDR                 EQU         PIA_DDRA ; DIR REG PORT A

; --- INPUT BUFFER/PARSER CONFIG ---
RBUF_SIZE               EQU         32  ; CONFIGURABLE INPUT RING BUFFER SIZE
CMD_MAX_LEN             EQU         31  ; MAX COMMAND LENGTH (EXCLUDING NULL)
SREC_MODE_SKIP          EQU         $00
SREC_MODE_DATA          EQU         $01
SREC_MODE_TERM          EQU         $02
DBG_MODE_JSR            EQU         0
DBG_MODE_NMI            EQU         1
DBG_MODE_IRQ            EQU         2
DBG_MODE_BRK            EQU         3
MOD_MAX_BYTES           EQU         16  ; MAX INLINE BYTE DEPOSITS FOR M
F_MAX_BYTES             EQU         16  ; MAX BYTE PATTERN LENGTH FOR F
PROTECT_HI_LIMIT        EQU         $04
                                        ; ADDR < $0400 IS PROTECTED (UNLESS
                                        ; FORCED)

; --- FIXED ADDRESS CONTRACTS (LOW/HIGH) ---
ZP_BASE_ADDR            EQU         $30
ZP_GAME_ASK_ADDR        EQU         $78
ZP_RST_HOOK_ADDR        EQU         $80
ZP_NMI_HOOK_ADDR        EQU         $83
ZP_IRQ_HOOK_ADDR        EQU         $86
ZP_BRK_HOOK_ADDR        EQU         $89
ZP_HW_HOOK_ADDR         EQU         $8C
ZP_BRK_FLAG_ADDR        EQU         $79
ZP_TERM_COLS_ADDR       EQU         $7A
ZP_TERM_TIMEOUT_ADDR    EQU         $7B
ZP_TERM_WAIT_LED_ADDR   EQU         $7E
ZP_TERM_WAIT_SECS_ADDR  EQU         $7F
ZP_GUARD_END_EXCL       EQU         $0090 ; KEEP USAGE <= $008F
USER_ZP_BASE_ADDR       EQU         $90
USER_ZP_END_ADDR        EQU         $FF
HW_VEC_NMI_ADDR         EQU         $FFFA
HW_VEC_RST_ADDR         EQU         $FFFC
HW_VEC_IRQ_ADDR         EQU         $FFFE
TERM_COLS_40            EQU         $28
TERM_COLS_80            EQU         $50
TERM_COLS_132           EQU         $84
TERM_WIDTH_TIMEOUT_DFLT EQU         $08 ; SECONDS (0=WAIT FOREVER)
RPN_STACK_DEPTH         EQU         8   ; I C CALCULATOR STACK DEPTH (16-BIT)

; --- ZERO PAGE MEMORY ALLOCATION ---
                        PAGE0
                        ORG         ZP_BASE_ADDR


PTR_TEMP:               DS          3   ; GEN PURPOSE SCRATCH
PTR_LEG:                DS          2   ; ADDR SCRATCH
PSR_TEMP:               DS          1   ; FLAGS SCRATCH
STR_PTR:                DS          2   ; STRING POINTER
PTR_DUMP_CUR:           DS          2   ; MEM DUMP CURSOR
PTR_DUMP_END:           DS          2   ; MEM DUMP LIMIT
PAGE_NUMBER             DS          2   ; LOW BYTE/HIGH BYTE
RBUF_HEAD:              DS          1   ; RING WRITE INDEX
RBUF_TAIL:              DS          1   ; RING READ INDEX
RBUF_COUNT:             DS          1   ; STORED BYTE COUNT
BUF_HEAD_PTR:           DS          2   ; ACTIVE BUFFER HEAD POINTER
BUF_TAIL_PTR:           DS          2   ; ACTIVE BUFFER TAIL POINTER
BUF_COUNT_PTR:          DS          2   ; ACTIVE BUFFER COUNT POINTER
BUF_DATA_PTR:           DS          2   ; ACTIVE BUFFER DATA BASE POINTER
BUF_SIZE:               DS          1   ; ACTIVE BUFFER SIZE
BUF_TMP:                DS          1   ; ACTIVE BUFFER TEMP BYTE
BUF_IDX:                DS          1   ; ACTIVE BUFFER INDEX TEMP
CMD_DISPATCH_CH:        DS          1   ; COMMAND LETTER FOR TABLE DISPATCH
CMD_POST_ACTION:        DS          1
                                        ; POST-DISPATCH ACTION (0=NONE,1=JMP
                                        ; MONITOR)
SYS_FLAGS:              DS          1   ; PACKED SYSTEM FLAGS (SEE BITS BELOW)
CMD_LEN:                DS          1   ; CURRENT COMMAND LENGTH
CMD_READY:              DS          1   ; 0=NOT READY, 1=READY
CMD_ESC_STATE:          DS          1   ; ESC/CSI PARSER STATE (0/1/2)
CMD_LAST_LEN:           DS          1   ; LAST COMMAND LENGTH FOR UP-ARROW REPEAT
CMD_PARSE_VAL:          DS          2   ; PARSED 16-BIT TOKEN VALUE
CMD_PARSE_NIB:          DS          1   ; PARSED HEX NIBBLE
DUMP_NEXT:              DS          2   ; NEXT START ADDR FOR "D"
DUMP_SPAN:              DS          2   ; BYTE COUNT FOR REPEATED "D"
DUMP_VALID:             DS          1   ; 1 IF DUMP_NEXT/DUMP_SPAN VALID
MEM_DUMP_CNT:           DS          1   ; BYTES TO PRINT ON CURRENT DUMP LINE
SEARCH_FOUND:           DS          1   ; 1 IF CURRENT S COMMAND FOUND A MATCH
DIS_OPCODE:             DS          1   ; CURRENT OPCODE FOR U DISASSEMBLER
DIS_MODE:               DS          1   ; CURRENT ADDRESSING MODE FOR U
DIS_LEN:                DS          1   ; CURRENT INSTRUCTION LENGTH FOR U
ASM_MODE_ALT:           DS          1   ; ALTERNATE MODE CANDIDATE FOR A
ASM_BIT_ID:             DS          1   ; BIT INDEX FOR RMB/SMB/BBR/BBS
ASM_MN0:                DS          1   ; ASSEMBLER MNEMONIC CHAR 0
ASM_MN1:                DS          1   ; ASSEMBLER MNEMONIC CHAR 1
ASM_MN2:                DS          1   ; ASSEMBLER MNEMONIC CHAR 2
ASM_OP0:                DS          1   ; ENCODED OPERAND BYTE 0
ASM_OP1:                DS          1   ; ENCODED OPERAND BYTE 1
DBG_A:                  DS          1   ; DEBUG SNAPSHOT A
DBG_X:                  DS          1   ; DEBUG SNAPSHOT X
DBG_Y:                  DS          1   ; DEBUG SNAPSHOT Y
DBG_P:                  DS          1   ; DEBUG SNAPSHOT P
DBG_PC_LO:              DS          1   ; DEBUG SNAPSHOT PC LO
DBG_PC_HI:              DS          1   ; DEBUG SNAPSHOT PC HI
DBG_SP:                 DS          1   ; DEBUG SNAPSHOT SP
DBG_MODE:               DS          1   ; DEBUG SOURCE MODE
DBG_BRK_SIG:            DS          1   ; BRK SIGNATURE BYTE (xx)
DBG_CONTINUE:           DS          1   ; NMI DEBUG LOOP EXIT FLAG
STEP_ADDR:              DS          2   ; TEMP BRK ADDRESS FOR N COMMAND
STEP_ORIG:              DS          1   ; ORIGINAL BYTE REPLACED BY TEMP BRK
STEP_ACTIVE:            DS          1   ; 1 IF TEMP BRK IS CURRENTLY ARMED
GAME_TARGET:            DS          1   ; TARGET VALUE FOR G NUMBER GAME (1..10)
GAME_TRIES:             DS          1   ; REMAINING TRIES FOR G NUMBER GAME
GAME_GUESS:             DS          1   ; LAST PARSED GUESS FOR G NUMBER GAME
RNG_STATE:              DS          1   ; LIVE RNG STIR STATE
                        DS          ZP_GAME_ASK_ADDR-* ; PIN ABI BYTE
GAME_ASK_PENDING:       DS          1   ; FIXED @ $0078 (EXTERNAL/USER-FACING)

                        DS          ZP_BRK_FLAG_ADDR-* ; PIN DEBUG CONTEXT FLAG
BRK_FLAG                DS          1
                        DS          ZP_TERM_COLS_ADDR-* ; PIN TERMINAL WIDTH BYTE
TERM_COLS:              DS          1   ; 40/80/132 COLUMN PREFERENCE
                        DS          ZP_TERM_TIMEOUT_ADDR-*
                                        ; PIN TERM WIDTH PROMPT TIMEOUT BYTE
TERM_WIDTH_TIMEOUT:     DS          1   ; 0=WAIT FOREVER, 1..255=SECONDS
                        DS          ZP_TERM_WAIT_LED_ADDR-*
TERM_WAIT_LED:          DS          1   ; LED BLINK PATTERN SCRATCH
                        DS          ZP_TERM_WAIT_SECS_ADDR-*
TERM_WAIT_SECS:         DS          1   ; WIDTH-PROMPT SECONDS SCRATCH

                        DS          ZP_RST_HOOK_ADDR-* ; PIN RST TRAMPOLINE
RST_HOOK:               DS          3   ; RST VECTOR JUMP
                        DS          ZP_NMI_HOOK_ADDR-* ; PIN NMI TRAMPOLINE
NMI_HOOK:               DS          3   ; NMI VECTOR JUMP
                        DS          ZP_IRQ_HOOK_ADDR-* ; PIN IRQ TRAMPOLINE
IRQ_HOOK:               DS          3   ; IRQ VECTOR JUMP
                        DS          ZP_BRK_HOOK_ADDR-* ; PIN BRK SUB-HOOK
BRK_HOOK:               DS          3   ; BRK DISPATCH TRAMPOLINE
                        DS          ZP_HW_HOOK_ADDR-* ; PIN HW IRQ SUB-HOOK
HW_HOOK:                DS          3   ; HW IRQ DISPATCH TRAMPOLINE

; PAGE0 GUARD: RESERVE UP TO $008F; NEGATIVE DS FAILS IF WE EVER CROSS IT
                        DS          ZP_GUARD_END_EXCL-*

SYSF_FORCE_MODE_M       EQU         %00000001 ; 1 IF COMMAND PREFIXED WITH '!'
SYSF_NMI_FLAG_M         EQU         %00000010 ; 1 WHEN NMI SIGNAL IS PENDING
SYSF_RESET_FLAG_M       EQU         %00000100 ; 1 IF RESET COOKIE PATH WAS TAKEN
SYSF_GO_FLAG_M          EQU         %00001000 ; 1 WHILE X-LAUNCHED CODE IS RUNNING
SYSF_H_AUTO_EN_M        EQU         %00010000 ; 1 IF STARTUP AUTO-HELP IS ENABLED
SYSF_H_AUTO_PEND_M      EQU         %00100000 ; 1 IF STARTUP AUTO-HELP IS PENDING

MOD_NEXT                EQU         PAGE_NUMBER ; LAST/NEXT MODIFY ADDRESS
MOD_VALID               EQU         DBG_CONTINUE ; 1 IF MOD_NEXT IS VALID
MOD_BYTE                EQU         BUF_TMP
                                        ; SCRATCH BYTE FOR INTERACTIVE M
MOD_COUNT               EQU         MEM_DUMP_CNT ; SCRATCH COUNT FOR INLINE M
F_COUNT                 EQU         MOD_COUNT
                                        ; SCRATCH COUNT FOR F BYTE PATTERN
F_PAT_IDX               EQU         CMD_PARSE_NIB ; CURRENT F PATTERN INDEX
SREC_ADDR_LEN           EQU         F_PAT_IDX ; S-RECORD ADDRESS BYTE COUNT
SREC_COUNT              EQU         CMD_PARSE_VAL ; S-RECORD BYTE COUNT
SREC_MODE               EQU         CMD_PARSE_VAL+1 ; S-RECORD RECORD MODE
SREC_REMAIN             EQU         MOD_COUNT ; S-RECORD DATA+CHECKSUM BYTES LEFT
SREC_SUM                EQU         BUF_IDX ; S-RECORD CHECKSUM ACCUMULATOR

; --- DISASSEMBLER ADDRESSING MODE IDS ---
DISM_IMP                EQU         $00 ; IMPLIED (NO OPERAND)
DISM_ACC                EQU         $01 ; ACCUMULATOR
DISM_IMM                EQU         $02 ; IMMEDIATE
DISM_ZP                 EQU         $03 ; ZERO PAGE
DISM_ZPX                EQU         $04 ; ZERO PAGE,X
DISM_ZPY                EQU         $05 ; ZERO PAGE,Y
DISM_ABS                EQU         $06 ; ABSOLUTE
DISM_ABSX               EQU         $07 ; ABSOLUTE,X
DISM_ABSY               EQU         $08 ; ABSOLUTE,Y
DISM_IND                EQU         $09 ; (ABSOLUTE)
DISM_INDX               EQU         $0A ; (ZERO PAGE,X)
DISM_INDY               EQU         $0B ; (ZERO PAGE),Y
DISM_REL                EQU         $0C ; RELATIVE BRANCH
DISM_ZPIND              EQU         $0D ; (ZERO PAGE)
DISM_ABSINDX            EQU         $0E ; (ABSOLUTE,X)
DISM_ZPREL              EQU         $0F ; ZERO PAGE,RELATIVE (BBR/BBS)

                        UDATA
                        ORG         $0200
RBUF_DATA:              DS          RBUF_SIZE ; INPUT RING BUFFER STORAGE
CMD_LINE:               DS          CMD_MAX_LEN+1 ; PARSED COMMAND + NULL
CMD_LAST_LINE:          DS          CMD_MAX_LEN+1 ; LAST COMMAND + NULL
RESET_COOKIE:           DS          4   ; RESET-PERSISTENT COOKIE
UNASM_NEXT:             DS          2   ; NEXT START ADDR FOR "U" (UDATA)
UNASM_SPAN:             DS          2   ; BYTE COUNT FOR REPEATED "U" (UDATA)
UNASM_VALID:            DS          1   ; 1 IF UNASM_NEXT/UNASM_SPAN VALID
F_PATTERN:              DS          F_MAX_BYTES
                                        ; FILL BYTE PATTERN (UP TO 16 BYTES)
RPN_SP:                 DS          1   ; I C STACK DEPTH (0..RPN_STACK_DEPTH)
RPN_TMP_L:              DS          2   ; I C LEFT OPERAND (16-BIT)
RPN_TMP_R:              DS          2   ; I C RIGHT OPERAND (16-BIT)
RPN_REM:                DS          2   ; I C DIVIDE REMAINDER (16-BIT)
RPN_LAST_REM:           DS          2   ; I C LAST DIV REMAINDER (16-BIT)
RPN_REM_VALID:          DS          1   ; 1 IF LAST TOKENIZED RESULT CAME FROM DIV
RPN_STACK:              DS          RPN_STACK_DEPTH*2 ; I C VALUE STACK (LO/HI)
DBG_TAG_BUF:            DS          6   ; MUTABLE TAG BUFFER "[   ]",0
SREC_FIRST_ADDR:        DS          2   ; FIRST S1/S2/S3 ADDRESS FOR LGS FALLBACK
SREC_FIRST_VALID:       DS          1   ; 1 IF SREC_FIRST_ADDR IS VALID

                        CODE

WDC_SIG:                DB          'WDC', $00 ; ROM SIGNATURE

WDC_RST:                JMP         INIT_RST ; SYSTEM COLD START
WDC_NMI:                JMP         NMI_HOOK ; NMI TRAMPOLINE
WDC_IRQ:                JMP         IRQ_HOOK ; IRQ TRAMPOLINE

; ----------------------------------------------------------------------------
; SUBROUTINE: SYS_RST
; DESCRIPTION: COLD BOOT HARDWARE INITIALIZATION
; INPUT: NONE
; OUTPUT: INITIALIZED SYSTEM
; FLAGS: INTERRUPTS DISABLED
; ZP USED: NONE
; ----------------------------------------------------------------------------

SYS_RST:
                        JSR         BUZZER_OFF ; SILENCE PIEZO
                        JSR         INIT_SERIAL
                                        ; SERIAL READY FOR BOOT MESSAGES
                        JSR         INIT_LED ; LED PORT READY FOR WRITE_BYTE
                        STZ         BRK_FLAG ; NO RESUME CONTEXT AFTER RESET
                        STZ         SYS_FLAGS ; RESET PACKED SYSTEM FLAGS
                        STZ         CMD_ESC_STATE
                        STZ         CMD_LAST_LEN
                        STZ         STEP_ACTIVE
                        LDA         TERM_COLS ; SAVE PRIOR WIDTH ACROSS RESET
                        STA         PSR_TEMP
                        LDA         #TERM_COLS_80 ; DEFAULT TERMINAL WIDTH
                        STA         TERM_COLS
                        LDA         #TERM_WIDTH_TIMEOUT_DFLT
                        STA         TERM_WIDTH_TIMEOUT
                        LDA         #$01 ; ASK ONCE AFTER RESET
                        STA         GAME_ASK_PENDING
                        LDA         #SYSF_H_AUTO_EN_M+SYSF_H_AUTO_PEND_M
                        TSB         SYS_FLAGS
                        LDA         #SYSF_RESET_FLAG_M
                        TRB         SYS_FLAGS
                                        ; 0 = POWER-ON PATH, 1 = RESET PATH

        ; --- CHECK RESET COOKIE ("WDC",0) ---
                        LDA         RESET_COOKIE
                        CMP         #'W'
                        BNE         ?TO_POWER_ON_CLR
                        LDA         RESET_COOKIE+1
                        CMP         #'D'
                        BNE         ?TO_POWER_ON_CLR
                        LDA         RESET_COOKIE+2
                        CMP         #'C'
                        BNE         ?TO_POWER_ON_CLR
                        LDA         RESET_COOKIE+3
                        BNE         ?TO_POWER_ON_CLR
                        BRA         ?COOKIE_OK
?TO_POWER_ON_CLR:
                        JMP         POWER_ON_CLR
?COOKIE_OK:

        ; --- MAGIC VALID: ASK USER ---
        ; C/W/M DECISION TRUTH TABLE:
        ;   KEY=C -> ASK "Y/N" CONFIRM:
        ;              Y -> C PATH (MEMCLR)
        ;              N -> W PATH (WARM NO VECT)
        ;   KEY=W -> W PATH (WARM NO VECT)
        ;   KEY=M -> M PATH (MONITOR)
        ;   INVALID KEY -> RE-PROMPT/COUNTDOWN CONTINUES
        ;   TIMEOUT -> DEFAULT M PATH
        ; POWER-ON (NO VALID COOKIE) USES C/M TREE (TIMEOUT DEFAULTS TO C).
        ; NOTE: THIS IS DEFAULT BOOT BEHAVIOR FOR THE CURRENTLY INSTALLED
        ; VECTOR TRAMPOLINES/HANDLERS; FUTURE TRAMPOLINES MAY BEHAVE DIFFERENTLY.
                        LDA         #SYSF_RESET_FLAG_M
                        TSB         SYS_FLAGS
                        PRT_CSTRING MSG_RESET_TRIGGERED ; RESET BUTTON PATH
                        PRT_CSTRING OSI ; PRINT "C/W/M"
?ASK_BOOT:
                        JSR         BOOT_WAIT_CWM_KEY
                        JSR         KEY_IS_C ; C/c = COLD / CLEAR
                        BEQ         ?ASK_CLR_CONFIRM

                        JSR         KEY_IS_W ; W/w = WARM
                        BNE         ?NOT_W_KEY
                        JMP         ?BOOT_GO_WARM_NO_VECT

?NOT_W_KEY:
                        JSR         KEY_IS_M ; M/m = MONITOR
                        BNE         ?ASK_BOOT ; Invalid key? Ask again
                        JMP         ?BOOT_GO_MONITOR

?ASK_CLR_CONFIRM:
                        PRT_CSTRING MSG_CLR_CONFIRM
?WAIT_CLR_CONFIRM:
                        JSR         READ_BYTE_ECHO_UPPER
                        JSR         KEY_IS_Y
                        BEQ         ?BOOT_GO_MEMCLR
                        JSR         KEY_IS_N
                        BEQ         ?CLR_ABORTED
                        BRA         ?WAIT_CLR_CONFIRM

?CLR_ABORTED:
                        PRT_CSTRING MSG_RAM_NOT_CLEARED
                        JMP         ?BOOT_GO_WARM_NO_VECT

?BOOT_GO_MEMCLR:
                        JSR         PROMPT_TERM_WIDTH
                        JMP         MEMCLR
?BOOT_GO_WARM_NO_VECT:
                        JSR         TERM_RESTORE_SAVED
                        JSR         PROMPT_TERM_WIDTH
                        JMP         WARM_NO_VECT
?BOOT_GO_MONITOR:
                        JSR         TERM_RESTORE_SAVED
                        JSR         PROMPT_TERM_WIDTH
                        JMP         MONITOR_CLEAN

POWER_ON_CLR:
                        PRT_CSTRING MSG_POWER_ON
                        PRT_CSTRING OSI_CM ; PRINT "C/M"
?POW_ASK_BOOT:
                        JSR         BOOT_WAIT_CM_KEY
                        JSR         KEY_IS_C
                        BEQ         ?POW_GO_MEMCLR
                        JSR         KEY_IS_M
                        BNE         ?POW_ASK_BOOT
                        JSR         RESET_COOKIE_SET
                        JSR         PROMPT_TERM_WIDTH
                        JMP         MONITOR_CLEAN
?POW_GO_MEMCLR:
                        JSR         PROMPT_TERM_WIDTH
                        JMP         MEMCLR

MEMCLR:                 PRT_CSTRING MSG_RAM_CLEARED
                        JSR         MEMCLR_CORE
                        LDA         #SYSF_RESET_FLAG_M
                        BIT         SYS_FLAGS
                        BNE         WARM_NO_VECT
WARM:                   PRT_CSTRING BSO2_INIT ; PRINT SIGN-ON
                        JSR         SHOW_VECTORS ; SHOW INTERRUPT CHAINS
                        BRA         MONITOR
WARM_NO_VECT:           PRT_CSTRING BSO2_INIT ; PRINT SIGN-ON (NO VECTOR DUMP)
                        PRT_CSTRING MSG_WARMSTART

; ----------------------------------------------------------------------------
; ENTRY: MONITOR_CLEAN
; DESCRIPTION: CLEAN OPERATOR ENTRY (USED BY M PATH)
; NOTES:
;   - CLEARS LAST-COMMAND REPEAT + INTERACTIVE CONTINUATION STATE.
;   - THEN FALLS THROUGH TO QUICK MONITOR ENTRY.
; ----------------------------------------------------------------------------
MONITOR_CLEAN:
                        STZ         CMD_LAST_LEN ; DROP UP-ARROW REPEAT CONTEXT
                        STZ         DUMP_VALID ; RESET D REPEAT CONTEXT
                        STZ         UNASM_VALID ; RESET U REPEAT CONTEXT
                        STZ         MOD_VALID ; RESET M REPEAT CONTEXT

MONITOR:                JSR         PRT_CRLF ; NEW LINE
                        JSR         PRT_UNDER ; PRINT CURSOR
                        JSR         SHOW_STARTUP_HELP_ONCE
;       JSR     DEBUG                   ; TEST DUMP
                        JSR         RBUF_INIT ; RESET INPUT RING BUFFER
                        JSR         CMD_PARSER_INIT ; RESET COMMAND PARSER
?MONITOR_LOOP:
                        INC         RNG_STATE ; LIVE STIR FROM LOOP CADENCE
                        JSR         INPUT_POLL_RING
                                        ; MOVE UART BYTES INTO RING
                        JSR         CMD_PARSE_RING ; BUILD COMMANDS FROM RING
                        JSR         CMD_PROCESS_IF_READY
                                        ; EXECUTE COMPLETED COMMAND
                        BRA         ?MONITOR_LOOP

; ----------------------------------------------------------------------------
; SUBROUTINE: SHOW_STARTUP_HELP_ONCE
; DESCRIPTION: ON FIRST MONITOR ENTRY, EMIT "-?" THEN SHORT HELP
; INPUT: SYS_FLAGS
; OUTPUT: STARTUP AUTO-HELP DISPLAYED AT MOST ONCE (UNTIL RE-ARMED)
; ----------------------------------------------------------------------------
SHOW_STARTUP_HELP_ONCE:
                        LDA         #SYSF_H_AUTO_EN_M
                        BIT         SYS_FLAGS
                        BEQ         ?SSHO_EXIT
                        LDA         #SYSF_H_AUTO_PEND_M
                        BIT         SYS_FLAGS
                        BEQ         ?SSHO_EXIT
                        LDA         #SYSF_H_AUTO_PEND_M
                        TRB         SYS_FLAGS
                        LDA         #'?'
                        JSR         WRITE_BYTE
                        PRT_CSTRING MSG_HELP_BOOT_SHORT
                        JSR         PRT_CRLF
                        JSR         PRT_UNDER
?SSHO_EXIT:
                        RTS

MEMCLR_CMD:
                        PRT_CSTRING MSG_RAM_CLEARED
                        JSR         MEMCLR_CORE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: MEM_CLEAR_RAM_LOOP
; DESCRIPTION: CLEARS RAM FROM $7E00 DOWN TO $0200
;              SKIPS STACK ($01xx) AND ZP ($00xx) FOR SAFETY
; INPUT: NONE
; ZP USED: PTR_TEMP
; ----------------------------------------------------------------------------
MEM_CLEAR_RAM_LOOP:
                        LDA         #$7E ; Start at Page $7E (User RAM Top)
                        STA         PTR_TEMP+1
                        STZ         PTR_TEMP ; Start at Offset $00
?MCR_PAGE:
                        LDY         #$00 ; Index
                        LDA         #$00 ; Value to write
?MCR_LOOP:
                        STA         (PTR_TEMP),Y ; Clear Byte
                        INY             ; Next Byte
                        BNE         ?MCR_LOOP ; Loop until Y wraps to 0

                        DEC         PTR_TEMP+1 ; Decrement Page
                        LDA         PTR_TEMP+1 ; Check new page
                        CMP         #$01 ; HIT PAGE 1 (STACK)?
                        BNE         ?MCR_PAGE ; IF NOT, CONTINUE
                        RTS             ; IF YES, STOP (Protect Stack)

MEMCLR_CORE:
                        JSR         MEM_CLEAR_RAM_LOOP
                        JSR         RESET_COOKIE_SET
                        jsr         MAIN_INIT
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RESET_COOKIE_SET
; DESCRIPTION: WRITES RESET COOKIE "WDC",0 TO RESET_COOKIE
; ----------------------------------------------------------------------------
RESET_COOKIE_SET:
                        LDA         #'W'
                        STA         RESET_COOKIE
                        LDA         #'D'
                        STA         RESET_COOKIE+1
                        LDA         #'C'
                        STA         RESET_COOKIE+2
                        STZ         RESET_COOKIE+3
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: MEM_CLEAR
; DESCRIPTION: CLEARS RAM FROM $7E00 DOWN TO $0200
;              SKIPS STACK ($01xx) AND ZP ($00xx) FOR SAFETY
; INPUT: NONE
; ZP USED: PTR_TEMP
; ----------------------------------------------------------------------------
MEM_CLEAR:
                        JMP         MEM_CLEAR_RAM_LOOP

; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
; SUBROUTINE: INIT_SERIAL
; DESCRIPTION: CALLS WDC ROM SERIAL INITIALIZATION
; INPUT: NONE
; OUTPUT: SERIAL PORT READY
; FLAGS: UNCHANGED
; ZP USED: NONE
; ----------------------------------------------------------------------------
INIT_SERIAL:
                        JSR         WDC_INIT_SERIAL ; CALL ROM
                        RTS             ; DONE

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
;       JSR     DELAY                   ; STABILIZE UART
                        RTS             ; DONE

; ----------------------------------------------------------------------------
; SUBROUTINE: READ_BYTE
; DESCRIPTION: READS CHAR FROM UART (BLOCKING)
; INPUT: NONE
; OUTPUT: ACC = CHAR RECEIVED
; FLAGS: CARRY SET IF NO DATA
; ZP USED: NONE
; ----------------------------------------------------------------------------
READ_BYTE:
                        JSR         WDC_READ_BYTE ; CALL ROM READ
                        RTS             ; RETURN TO CALLER

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

; ----------------------------------------------------------------------------
; SUBROUTINE: READ_BYTE_ECHO_UPPER
; DESCRIPTION: READS ONE BYTE (BLOCKING), UPPERCASES, ECHOES, RETURNS UPPERCASE
; INPUT: NONE
; OUTPUT: A = UPPERCASED BYTE
; ----------------------------------------------------------------------------
READ_BYTE_ECHO_UPPER:
                        JSR         READ_BYTE
                        JSR         UTIL_TO_UPPER
                        PHA
                        JSR         WRITE_BYTE
                        PLA
                        RTS

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

; ----------------------------------------------------------------------------
; SUBROUTINES: KEY_IS_Z/C/W/M/Y/N
; DESCRIPTION: CASE-INSENSITIVE KEY MATCHERS FOR INPUT BYTES
; INPUT: ACC = BYTE
; OUTPUT: Z=1 IF MATCH, Z=0 IF NO MATCH
; ----------------------------------------------------------------------------
KEY_IS_Z:
                        JSR         UTIL_TO_UPPER
                        CMP         #'Z'
                        RTS

KEY_IS_C:
                        JSR         UTIL_TO_UPPER
                        CMP         #'C'
                        RTS

KEY_IS_W:
                        JSR         UTIL_TO_UPPER
                        CMP         #'W'
                        RTS

KEY_IS_M:
                        JSR         UTIL_TO_UPPER
                        CMP         #'M'
                        RTS

KEY_IS_Y:
                        JSR         UTIL_TO_UPPER
                        CMP         #'Y'
                        RTS

KEY_IS_N:
                        JSR         UTIL_TO_UPPER
                        CMP         #'N'
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: BOOT_WAIT_CWM_KEY
; DESCRIPTION: WAITS UP TO 6 SECONDS FOR C/W/M (RESET PATH)
; INPUT: NONE
; OUTPUT: A=KEY BYTE; DEFAULTS TO 'M' ON TIMEOUT
; NOTES:
;   - VISUAL MARKER: PRINTS " <" ON EACH SECOND TICK.
;   - BLINKS LED EVERY ~333MS WHILE WAITING.
;   - TIME BASE: 3 x DELAY_333MS ~= 1 SECOND PER COUNT.
; ----------------------------------------------------------------------------
BOOT_WAIT_CWM_KEY:
                        LDA         #$06
                        STA         TERM_WAIT_SECS
                        LDA         #$FF
                        STA         TERM_WAIT_LED
?BWCK_RST_TICK:
                        LDA         #' '
                        JSR         WRITE_BYTE
                        LDA         #'<'
                        JSR         WRITE_BYTE
                        LDY         #$03
?BWCK_RST_WAIT_1S:
                        JSR         CHECK_BYTE
                        BCC         ?BWCK_RST_HAVE
                        LDA         TERM_WAIT_LED
                        JSR         PUT_LED
                        LDA         TERM_WAIT_LED
                        EOR         #$FF
                        STA         TERM_WAIT_LED
                        JSR         DELAY_333MS
                        DEY
                        BNE         ?BWCK_RST_WAIT_1S
                        DEC         TERM_WAIT_SECS
                        BNE         ?BWCK_RST_TICK
                        LDA         #' '
                        JSR         WRITE_BYTE
                        LDA         #'M' ; TIMEOUT DEFAULT: MONITOR PATH
                        JSR         WRITE_BYTE
                        LDA         #'M'
                        RTS
?BWCK_RST_HAVE:
                        JSR         READ_BYTE_ECHO_UPPER
                        JSR         KEY_IS_C
                        BEQ         ?BWCK_RST_DONE
                        JSR         KEY_IS_W
                        BEQ         ?BWCK_RST_DONE
                        JSR         KEY_IS_M
                        BEQ         ?BWCK_RST_DONE
                        BRA         ?BWCK_RST_WAIT_1S
?BWCK_RST_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: BOOT_WAIT_CM_KEY
; DESCRIPTION: WAITS UP TO 6 SECONDS FOR C/M (POWER-ON PATH)
; INPUT: NONE
; OUTPUT: A=KEY BYTE; DEFAULTS TO 'C' ON TIMEOUT
; NOTES:
;   - VISUAL MARKER: PRINTS " >" ON EACH SECOND TICK.
;   - TIME BASE: 3 x DELAY_333MS ~= 1 SECOND PER COUNT.
; ----------------------------------------------------------------------------
BOOT_WAIT_CM_KEY:
                        LDA         #$06
                        STA         TERM_WAIT_SECS
?BWCM_TICK:
                        LDA         #' '
                        JSR         WRITE_BYTE
                        LDA         #'>'
                        JSR         WRITE_BYTE
                        LDY         #$03
?BWCM_WAIT_1S:
                        JSR         CHECK_BYTE
                        BCC         ?BWCM_HAVE
                        JSR         DELAY_333MS
                        DEY
                        BNE         ?BWCM_WAIT_1S
                        DEC         TERM_WAIT_SECS
                        BNE         ?BWCM_TICK
                        LDA         #' '
                        JSR         WRITE_BYTE
                        LDA         #'C' ; TIMEOUT DEFAULT: COLD/CLEAR PATH
                        JSR         WRITE_BYTE
                        LDA         #'C'
                        RTS
?BWCM_HAVE:
                        JSR         READ_BYTE_ECHO_UPPER
                        JSR         KEY_IS_C
                        BEQ         ?BWCM_DONE
                        JSR         KEY_IS_M
                        BEQ         ?BWCM_DONE
                        BRA         ?BWCM_WAIT_1S
?BWCM_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: PROMPT_TERM_WIDTH
; DESCRIPTION: PROMPTS FOR TERMINAL WIDTH USING SINGLE-KEY SELECTION
; INPUT: TERM_WIDTH_TIMEOUT (0=WAIT FOREVER, 1..255=SECONDS)
; OUTPUT: TERM_COLS = 40/80/132 (DEFAULT REMAINS 80 ON OTHER INPUT)
; ----------------------------------------------------------------------------
PROMPT_TERM_WIDTH:
                        PRT_CSTRING MSG_TERM_WIDTH_PROMPT
                        JSR         TERM_WAIT_WIDTH_KEY
                        BCS         ?PTW_DONE ; TIMEOUT: KEEP CURRENT WIDTH
                        CMP         #'4'
                        BEQ         ?PTW_SET_40
                        CMP         #'8'
                        BEQ         ?PTW_SET_80
                        CMP         #'1'
                        BEQ         ?PTW_SET_132
                        JSR         TERM_FLUSH_LINE
                        RTS
?PTW_SET_40:
                        LDA         #TERM_COLS_40
                        STA         TERM_COLS
                        JSR         TERM_FLUSH_LINE
                        RTS
?PTW_SET_80:
                        LDA         #TERM_COLS_80
                        STA         TERM_COLS
                        JSR         TERM_FLUSH_LINE
                        RTS
?PTW_SET_132:
                        LDA         #TERM_COLS_132
                        STA         TERM_COLS
                        JSR         TERM_FLUSH_LINE
                        RTS
?PTW_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: TERM_WAIT_WIDTH_KEY
; DESCRIPTION: WAITS FOR WIDTH KEY WITH BLINKING LED + TIMEOUT
; INPUT: TERM_WIDTH_TIMEOUT (0=WAIT FOREVER, 1..255=SECONDS)
; OUTPUT: C=0 + A=KEY IF RECEIVED, C=1 ON TIMEOUT
; NOTES:
;   - Uses DELAY_333MS as half-step for a ~0.333s ON / ~0.333s OFF cadence.
;   - Blink pattern/counter are stored in fixed ZP scratch bytes.
; ----------------------------------------------------------------------------
TERM_WAIT_WIDTH_KEY:
                        LDA         #$FF ; START BLINK PATTERN
                        STA         TERM_WAIT_LED
                        LDA         TERM_WIDTH_TIMEOUT
                        BEQ         ?TWWK_FOREVER
                        STA         TERM_WAIT_SECS
?TWWK_TIMED_LOOP:
                        JSR         TERM_WAIT_POLL_KEY
                        BCC         ?TWWK_GOT_KEY
                        LDA         TERM_WAIT_LED
                        JSR         PUT_LED
                        JSR         TERM_WAIT_HALF_POLL_KEY
                        BCC         ?TWWK_GOT_KEY
                        LDA         TERM_WAIT_LED
                        EOR         #$FF
                        STA         TERM_WAIT_LED
                        LDA         TERM_WAIT_LED
                        JSR         PUT_LED
                        JSR         TERM_WAIT_HALF_POLL_KEY
                        BCC         ?TWWK_GOT_KEY
                        LDA         TERM_WAIT_LED
                        EOR         #$FF
                        STA         TERM_WAIT_LED
                        DEC         TERM_WAIT_SECS
                        BNE         ?TWWK_TIMED_LOOP
                        SEC
                        RTS

?TWWK_FOREVER:
?TWWK_FOREVER_LOOP:
                        JSR         TERM_WAIT_POLL_KEY
                        BCC         ?TWWK_GOT_KEY
                        LDA         TERM_WAIT_LED
                        JSR         PUT_LED
                        JSR         TERM_WAIT_HALF_POLL_KEY
                        BCC         ?TWWK_GOT_KEY
                        LDA         TERM_WAIT_LED
                        EOR         #$FF
                        STA         TERM_WAIT_LED
                        LDA         TERM_WAIT_LED
                        JSR         PUT_LED
                        JSR         TERM_WAIT_HALF_POLL_KEY
                        BCC         ?TWWK_GOT_KEY
                        LDA         TERM_WAIT_LED
                        EOR         #$FF
                        STA         TERM_WAIT_LED
                        BRA         ?TWWK_FOREVER_LOOP

?TWWK_GOT_KEY:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: TERM_WAIT_POLL_KEY
; DESCRIPTION: NON-BLOCKING KEY POLL FOR TERM_WAIT_WIDTH_KEY
; OUTPUT: C=0 + A=KEY IF READY, C=1 IF NO DATA
; ----------------------------------------------------------------------------
TERM_WAIT_POLL_KEY:
                        JSR         CHECK_BYTE
                        BCS         ?TWPK_EMPTY
                        JSR         READ_BYTE
                        CLC
                        RTS
?TWPK_EMPTY:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: TERM_WAIT_HALF_POLL_KEY
; DESCRIPTION: WAITS ONE ~333ms HALF-STEP, THEN POLLS FOR KEY
; OUTPUT: C=0 + A=KEY IF READY, C=1 IF NO DATA
; ----------------------------------------------------------------------------
TERM_WAIT_HALF_POLL_KEY:
                        JSR         DELAY_333MS
                        JMP         TERM_WAIT_POLL_KEY

; ----------------------------------------------------------------------------
; SUBROUTINE: TERM_FLUSH_LINE
; DESCRIPTION: CONSUMES PENDING SERIAL BYTES THROUGH CR/LF (NON-BLOCKING)
; INPUT: NONE
; OUTPUT: PENDING TAIL OF WIDTH PROMPT INPUT IS DISCARDED
; ----------------------------------------------------------------------------
TERM_FLUSH_LINE:
?TFL_LOOP:
                        JSR         CHECK_BYTE ; C=1 IF EMPTY
                        BCS         ?TFL_DONE
                        JSR         READ_BYTE
                        CMP         #$0D
                        BEQ         ?TFL_DONE
                        CMP         #$0A
                        BEQ         ?TFL_DONE
                        BRA         ?TFL_LOOP
?TFL_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: TERM_RESTORE_SAVED
; DESCRIPTION: RESTORES SAVED TERM WIDTH ONLY IF IT MATCHES 40/80/132
; INPUT: PSR_TEMP = SAVED WIDTH CANDIDATE
; OUTPUT: TERM_COLS UPDATED WHEN CANDIDATE IS VALID
; ----------------------------------------------------------------------------
TERM_RESTORE_SAVED:
                        LDA         PSR_TEMP
                        CMP         #TERM_COLS_40
                        BEQ         ?TRS_SET
                        CMP         #TERM_COLS_80
                        BEQ         ?TRS_SET
                        CMP         #TERM_COLS_132
                        BNE         ?TRS_DONE
?TRS_SET:
                        STA         TERM_COLS
?TRS_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RBUF_INIT
; DESCRIPTION: INITIALIZES INPUT RING BUFFER STATE (GENERIC DESCRIPTOR CORE)
; ----------------------------------------------------------------------------
RBUF_INIT:
                        JSR         BUF_BIND_RBUF
                        JMP         BUF_INIT

; ----------------------------------------------------------------------------
; SUBROUTINE: RBUF_PUT_A
; DESCRIPTION: STORES ACC BYTE INTO INPUT RING BUFFER
; INPUT: ACC = BYTE
; OUTPUT: C=0 IF STORED, C=1 IF BUFFER FULL
; ----------------------------------------------------------------------------
RBUF_PUT_A:
                        PHA
                        JSR         BUF_BIND_RBUF
                        PLA
                        JMP         BUF_PUT_A

; ----------------------------------------------------------------------------
; SUBROUTINE: RBUF_GET_A
; DESCRIPTION: FETCHES ONE BYTE FROM INPUT RING BUFFER
; OUTPUT: ACC = BYTE (IF AVAILABLE), C=0 IF BYTE RETURNED, C=1 IF EMPTY
; ----------------------------------------------------------------------------
RBUF_GET_A:
                        JSR         BUF_BIND_RBUF
                        JMP         BUF_GET_A

; ----------------------------------------------------------------------------
; SUBROUTINE: BUF_BIND_RBUF
; DESCRIPTION: BINDS GENERIC BUFFER DESCRIPTOR TO THE MONITOR INPUT RING
; ----------------------------------------------------------------------------
BUF_BIND_RBUF:
                        LDA         #<RBUF_HEAD
                        STA         BUF_HEAD_PTR
                        LDA         #>RBUF_HEAD
                        STA         BUF_HEAD_PTR+1
                        LDA         #<RBUF_TAIL
                        STA         BUF_TAIL_PTR
                        LDA         #>RBUF_TAIL
                        STA         BUF_TAIL_PTR+1
                        LDA         #<RBUF_COUNT
                        STA         BUF_COUNT_PTR
                        LDA         #>RBUF_COUNT
                        STA         BUF_COUNT_PTR+1
                        LDA         #<RBUF_DATA
                        STA         BUF_DATA_PTR
                        LDA         #>RBUF_DATA
                        STA         BUF_DATA_PTR+1
                        LDA         #RBUF_SIZE
                        STA         BUF_SIZE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: BUF_INIT
; DESCRIPTION: CLEARS ACTIVE BUFFER HEAD/TAIL/COUNT
; ----------------------------------------------------------------------------
BUF_INIT:
                        LDY         #$00
                        LDA         #$00
                        STA         (BUF_HEAD_PTR),Y
                        STA         (BUF_TAIL_PTR),Y
                        STA         (BUF_COUNT_PTR),Y
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: BUF_PUT_A
; DESCRIPTION: STORES ACC INTO ACTIVE BUFFER
; INPUT: ACC = BYTE
; OUTPUT: C=0 STORED, C=1 FULL
; ----------------------------------------------------------------------------
BUF_PUT_A:
                        STA         BUF_TMP
                        LDY         #$00
                        LDA         (BUF_COUNT_PTR),Y
                        CMP         BUF_SIZE
                        BCC         ?BPUT_SPACE
                        SEC
                        RTS
?BPUT_SPACE:
                        LDA         (BUF_HEAD_PTR),Y
                        STA         BUF_IDX
                        CLC
                        ADC         BUF_DATA_PTR
                        STA         PTR_TEMP
                        LDA         BUF_DATA_PTR+1
                        ADC         #$00
                        STA         PTR_TEMP+1

                        LDY         #$00
                        LDA         BUF_TMP
                        STA         (PTR_TEMP),Y

                        LDA         BUF_IDX
                        CLC
                        ADC         #$01
                        CMP         BUF_SIZE
                        BCC         ?BPUT_HEAD_OK
                        LDA         #$00
?BPUT_HEAD_OK:
                        LDY         #$00
                        STA         (BUF_HEAD_PTR),Y

                        LDA         (BUF_COUNT_PTR),Y
                        CLC
                        ADC         #$01
                        STA         (BUF_COUNT_PTR),Y
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: BUF_GET_A
; DESCRIPTION: FETCHES ONE BYTE FROM ACTIVE BUFFER
; OUTPUT: ACC = BYTE (IF AVAILABLE), C=0 IF BYTE RETURNED, C=1 IF EMPTY
; ----------------------------------------------------------------------------
BUF_GET_A:
                        LDY         #$00
                        LDA         (BUF_COUNT_PTR),Y
                        BNE         ?BGET_HAVE
                        SEC
                        RTS
?BGET_HAVE:
                        LDA         (BUF_TAIL_PTR),Y
                        STA         BUF_IDX
                        CLC
                        ADC         BUF_DATA_PTR
                        STA         PTR_TEMP
                        LDA         BUF_DATA_PTR+1
                        ADC         #$00
                        STA         PTR_TEMP+1

                        LDY         #$00
                        LDA         (PTR_TEMP),Y
                        PHA

                        LDA         BUF_IDX
                        CLC
                        ADC         #$01
                        CMP         BUF_SIZE
                        BCC         ?BGET_TAIL_OK
                        LDA         #$00
?BGET_TAIL_OK:
                        LDY         #$00
                        STA         (BUF_TAIL_PTR),Y

                        LDA         (BUF_COUNT_PTR),Y
                        SEC
                        SBC         #$01
                        STA         (BUF_COUNT_PTR),Y

                        PLA
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: INPUT_POLL_RING
; DESCRIPTION: POLLS UART AND QUEUES ALL AVAILABLE BYTES INTO INPUT RING
; ----------------------------------------------------------------------------
INPUT_POLL_RING:
?IPOLL_LOOP:
                        JSR         CHECK_BYTE
                        BCS         ?IPOLL_DONE ; C=1 => NO BYTE AVAILABLE
                        JSR         READ_BYTE
                        PHA
                        EOR         RNG_STATE
                        ROL         A
                        ADC         #$17
                        STA         RNG_STATE
                        PLA
                        JSR         RBUF_PUT_A ; DROP BYTE IF BUFFER FULL
                        BRA         ?IPOLL_LOOP
?IPOLL_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PARSER_INIT
; DESCRIPTION: RESETS COMMAND PARSER STATE
; ----------------------------------------------------------------------------
CMD_PARSER_INIT:
                        STZ         CMD_LEN
                        STZ         CMD_READY
                        STZ         CMD_ESC_STATE
                        STZ         CMD_POST_ACTION
                        STZ         MOD_COUNT ; RESET CR/LF COALESCER SCRATCH
                        LDA         #SYSF_FORCE_MODE_M
                        TRB         SYS_FLAGS
                        LDA         #SYSF_GO_FLAG_M
                        TRB         SYS_FLAGS
                        STZ         DUMP_VALID
                        STZ         UNASM_VALID
                        STZ         MOD_VALID
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PARSE_RING
; DESCRIPTION: DRAINS RING BUFFER AND BUILDS A NULL-TERMINATED COMMAND
; ----------------------------------------------------------------------------
CMD_PARSE_RING:
                        LDA         CMD_READY
                        BNE         ?CPARSE_DONE
                                        ; WAIT UNTIL CURRENT CMD IS PROCESSED
?CPARSE_LOOP:
                        JSR         RBUF_GET_A
                        BCS         ?CPARSE_DONE
                        JSR         CMD_PARSER_FEED_A
                        LDA         CMD_READY
                        BEQ         ?CPARSE_LOOP
?CPARSE_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PARSER_FEED_A
; DESCRIPTION: FEEDS ONE BYTE INTO COMMAND PARSER (RAW BYTE CAPTURE + SAFE
; ECHO, ESC [ A REPEAT)
; INPUT: ACC = INPUT BYTE
; ----------------------------------------------------------------------------
CMD_PARSER_FEED_A:
                        LDX         CMD_ESC_STATE
                        BEQ         ?CFEED_ESC_IDLE
                        CPX         #$01 ; ESC SEEN, EXPECT '['
                        BNE         ?CFEED_ESC_CSI ; CSI BODY
                        CMP         #$5B ; '['
                        BNE         ?CFEED_ESC_CANCEL
                        LDA         #$02
                        STA         CMD_ESC_STATE
                        RTS
?CFEED_ESC_CSI:
                        CMP         #'A' ; UP ARROW
                        BEQ         ?CFEED_ESC_UP
                        CMP         #$40 ; CSI FINAL BYTE RANGE [$40..$7E]
                        BCC         ?CFEED_ESC_WAIT
                        CMP         #$7F
                        BCC         ?CFEED_ESC_CLEAR
?CFEED_ESC_WAIT:
                        RTS
?CFEED_ESC_CLEAR:
                        STZ         CMD_ESC_STATE
                        RTS
?CFEED_ESC_UP:
                        STZ         CMD_ESC_STATE
                        JSR         CMD_REPEAT_LAST
                        RTS
?CFEED_ESC_CANCEL:
                        STZ         CMD_ESC_STATE ; FALL THROUGH, PROCESS BYTE
?CFEED_ESC_IDLE:
                        CMP         #$1B ; ESC STARTS CSI SEQUENCE
                        BNE         ?CFEED_NOT_ESC
                        LDA         #$01
                        STA         CMD_ESC_STATE
                        RTS
?CFEED_NOT_ESC:
                        CMP         #$0D ; CR TERMINATES COMMAND
                        BEQ         ?CFEED_TERMINATE
                        CMP         #$0A ; LF TERMINATES COMMAND
                        BEQ         ?CFEED_TERMINATE
                        CMP         #$08 ; BACKSPACE
                        BEQ         ?CFEED_BACKSPACE
                        CMP         #$7F ; DEL
                        BEQ         ?CFEED_BACKSPACE
        ; Keep all remaining bytes so future key-consumers (e.g. ESC
        ; sequences)
        ; can parse function keys from the command stream.
                        JSR         UTIL_TO_UPPER
                        LDX         CMD_LEN
                        CPX         #CMD_MAX_LEN
                        BCS         ?CFEED_DONE
                        STA         CMD_LINE,X
                        INC         CMD_LEN
                        CMP         #$20 ; ECHO PRINTABLE ONLY
                        BCC         ?CFEED_DONE
                        CMP         #$7F
                        BCS         ?CFEED_DONE
                        JSR         WRITE_BYTE
?CFEED_DONE:
                        RTS

?CFEED_BACKSPACE:
                        LDA         CMD_LEN
                        BEQ         ?CFEED_DONE
                        DEC         CMD_LEN
                        LDA         #$08 ; MOVE CURSOR LEFT
                        JSR         WRITE_BYTE
                        LDA         #' ' ; ERASE PREVIOUS CHAR
                        JSR         WRITE_BYTE
                        LDA         #$08 ; MOVE CURSOR LEFT AGAIN
                        JSR         WRITE_BYTE
                        RTS

?CFEED_TERMINATE:
                        LDA         CMD_LEN
                        BEQ         ?CFEED_DONE
                                        ; IGNORE EMPTY CR/LF (e.g. CRLF SECOND
                                        ; BYTE)
                        LDX         CMD_LEN
                        LDA         #$00
                        STA         CMD_LINE,X
                        LDA         #$01
                        STA         CMD_READY
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_REPEAT_LAST
; DESCRIPTION: REPEATS LAST COMMAND ON UP-ARROW (ONLY WHEN LINE IS EMPTY)
; ----------------------------------------------------------------------------
CMD_REPEAT_LAST:
                        LDA         CMD_LEN
                        BNE         ?CRL_DONE
                        LDA         CMD_LAST_LEN
                        BEQ         ?CRL_DONE
                        LDA         CMD_LAST_LINE
                        CMP         #'D'
                        BEQ         ?CRL_REPEAT_D
                        CMP         #'U'
                        BEQ         ?CRL_REPEAT_U
                        LDX         #$00
?CRL_COPY:
                        LDA         CMD_LAST_LINE,X
                        STA         CMD_LINE,X
                        BEQ         ?CRL_READY
                        JSR         WRITE_BYTE
                        INX
                        BRA         ?CRL_COPY
?CRL_READY:
                        STX         CMD_LEN
                        JSR         PRT_CRLF
                        LDA         #$01
                        STA         CMD_READY
                        RTS

?CRL_REPEAT_D:
        ; D has built-in cursor/span repeat state, so UP uses bare "D"
        ; even when last command was "D <START>" or "D <START> <END>".
                        LDA         #'D'
                        STA         CMD_LINE
                        STZ         CMD_LINE+1
                        LDA         #'D'
                        JSR         WRITE_BYTE
                        LDA         #$01
                        STA         CMD_LEN
                        JSR         PRT_CRLF
                        LDA         #$01
                        STA         CMD_READY
                        RTS

?CRL_REPEAT_U:
        ; U has built-in cursor/span repeat state, so UP uses bare "U"
        ; even when last command was "U <START> <END>".
                        LDA         #'U'
                        STA         CMD_LINE
                        STZ         CMD_LINE+1
                        LDA         #'U'
                        JSR         WRITE_BYTE
                        LDA         #$01
                        STA         CMD_LEN
                        JSR         PRT_CRLF
                        LDA         #$01
                        STA         CMD_READY
?CRL_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_SAVE_LAST
; DESCRIPTION: SAVES CURRENT CMD_LINE AS LAST COMMAND FOR UP-ARROW REPEAT
; ----------------------------------------------------------------------------
CMD_SAVE_LAST:
                        LDX         #$00
?CSL_LOOP:
                        LDA         CMD_LINE,X
                        STA         CMD_LAST_LINE,X
                        BEQ         ?CSL_DONE
                        INX
                        BRA         ?CSL_LOOP
?CSL_DONE:
                        STX         CMD_LAST_LEN
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PROCESS_IF_READY
; DESCRIPTION: DISPATCHES A COMPLETED COMMAND LINE
; COMMANDS: Z (CLEAR), C (COPY), W (WARM), M (MODIFY), D (DUMP), U
; (DISASSEMBLE), A (ASSEMBLE), X (EXECUTE), G (NUMBER GAME), R (RESUME),
; N (NEXT), F (FILL), S B / S C (SEARCH), L S / L B (SERIAL LOAD), I C (RPN),
; Q (WAIT), V (VECTORS), H/? (HELP)
; SEARCH TODAY:
;   - S B START END B0..B15 ; BYTE PATTERN SEARCH
;   - S C START END TEXT    ; ASCII TEXT SEARCH (REST OF LINE)
; SEARCH FUTURE:
;   - QUOTED/ESCAPED TEXT TOKENS
;   - WILDCARDS / WORD TOKENS
;   - GAME IDEAS (FUTURE): MASTERMIND, CONWAY'S LIFE, TIC-TAC-TOE.
; PREFIX: ! FORCES LOW-RAM ACCESS FOR PROTECTED COMMANDS (F/M/C/A/N/L)
; ----------------------------------------------------------------------------
CMD_PROCESS_IF_READY:
                        LDA         CMD_READY
                        BNE         ?CPROC_HAVE_READY
                        RTS
?CPROC_HAVE_READY:
                        STZ         CMD_READY

                        LDA         CMD_LINE
                        BEQ         ?CPROC_PROMPT
                        JSR         CMD_SAVE_LAST

                        STZ         CMD_POST_ACTION
                        LDA         #SYSF_FORCE_MODE_M
                        TRB         SYS_FLAGS

                        LDA         CMD_LINE
                        CMP         #'!'
                        BNE         ?CPROC_DISPATCH
                        LDA         #SYSF_FORCE_MODE_M
                        TSB         SYS_FLAGS
                        JSR         CMD_SHIFT_BANG
?CPROC_BANG_SKIP_SPACE:
                        LDA         CMD_LINE
                        CMP         #' '
                        BNE         ?CPROC_DISPATCH
                        JSR         CMD_SHIFT_BANG
                        BRA         ?CPROC_BANG_SKIP_SPACE

?CPROC_DISPATCH:
                        LDA         CMD_LINE
                        BEQ         ?CPROC_PROMPT
                        JSR         CMD_PRE_HOOK
                        LDA         CMD_LINE
                        BEQ         ?CPROC_PROMPT
                        JSR         CMD_DISPATCH
                        BCC         ?CPROC_PROMPT
                        PRT_CSTRING MSG_UNKNOWN_CMD
                        BRA         ?CPROC_PROMPT

?CPROC_PROMPT:
                        LDA         CMD_POST_ACTION
                        BEQ         ?CPROC_REAL_PROMPT
                        LDA         #SYSF_FORCE_MODE_M
                        TRB         SYS_FLAGS
                        JMP         MONITOR
?CPROC_REAL_PROMPT:
                        STZ         CMD_LEN
                        LDA         #SYSF_FORCE_MODE_M
                        TRB         SYS_FLAGS
                        LDA         #SYSF_GO_FLAG_M
                        TRB         SYS_FLAGS
                        LDA         CMD_LINE
                        BEQ         ?CPROC_PRINT_PROMPT
                        JSR         CMD_ASK_GAME_IF_PENDING
?CPROC_PRINT_PROMPT:
                        JSR         PRT_CRLF
                        JSR         PRT_UNDER
?CPROC_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_ASK_GAME_IF_PENDING
; DESCRIPTION: PRINTS "WANT TO PLAY A GAME?" ONCE WHEN PENDING FLAG IS SET
; INPUT: GAME_ASK_PENDING (0/1)
; OUTPUT: MAY PRINT PROMPT LINE
; ----------------------------------------------------------------------------
CMD_ASK_GAME_IF_PENDING:
                        LDA         GAME_ASK_PENDING
                        BEQ         ?CMGA_DONE
                        STZ         GAME_ASK_PENDING
                        PRT_CSTRING MSG_GAME_ASK
?CMGA_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PRE_HOOK
; DESCRIPTION: PRE-DISPATCH COMMAND HOOK (NO-OP PLACEHOLDER)
; INPUT: CMD_LINE
; OUTPUT: NONE
; ----------------------------------------------------------------------------
CMD_PRE_HOOK:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_SHIFT_BANG
; DESCRIPTION: SHIFTS CMD_LINE LEFT BY 1 BYTE (USED FOR FORCE PREFIX STRIP)
; ----------------------------------------------------------------------------
CMD_SHIFT_BANG:
                        LDX         #$00
?CSHIFT_LOOP:
                        LDA         CMD_LINE+1,X
                        STA         CMD_LINE,X
                        BEQ         ?CSHIFT_DONE
                        INX
                        BRA         ?CSHIFT_LOOP
?CSHIFT_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DISPATCH
; DESCRIPTION: TABLE-DRIVEN COMMAND DISPATCH
; INPUT:  A = COMMAND LETTER
; OUTPUT: C=0 HANDLED, C=1 UNKNOWN COMMAND
; ----------------------------------------------------------------------------
CMD_DISPATCH:
                        STA         CMD_DISPATCH_CH
                        LDX         #$00
?CDIS_LOOP:
                        LDA         CMD_TABLE,X
                        BEQ         ?CDIS_UNKNOWN
                        CMP         CMD_DISPATCH_CH
                        BEQ         ?CDIS_FOUND
                        TXA
                        CLC
                        ADC         #$03
                                        ; ENTRY SIZE: 1 BYTE KEY + 2 BYTE
                                        ; HANDLER
                        TAX
                        BRA         ?CDIS_LOOP
?CDIS_FOUND:
                        INX
                        LDA         CMD_TABLE,X
                        STA         PTR_TEMP
                        INX
                        LDA         CMD_TABLE,X
                        STA         PTR_TEMP+1

        ; Build stack frame so RTS enters handler, and handler RTS returns
        ; here.
                        LDA         PTR_TEMP
                        SEC
                        SBC         #$01
                        STA         PTR_TEMP
                        LDA         PTR_TEMP+1
                        SBC         #$00
                        STA         PTR_TEMP+1

                        LDA         #>(?CDIS_AFTER_CALL-1)
                        PHA
                        LDA         #<(?CDIS_AFTER_CALL-1)
                        PHA
                        LDA         PTR_TEMP+1
                        PHA
                        LDA         PTR_TEMP
                        PHA
                        RTS

?CDIS_AFTER_CALL:
                        CLC
                        RTS
?CDIS_UNKNOWN:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: COMMAND WRAPPERS FOR TABLE DISPATCH
; ----------------------------------------------------------------------------
CMD_DO_WARM:
                        LDA         #$01
                        STA         CMD_POST_ACTION
                        RTS

CMD_DO_VECTORS:
                        JSR         PRT_CRLF
                        JSR         SHOW_VECTORS
                        RTS

CMD_DO_HELP_SHORT:
                        PRT_CSTRING MSG_HELP_SHORT
                        RTS

CMD_DO_HELP_FULL:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?H_INDEX
                        CMP         #'A'
                        BEQ         ?H_ALL
                        CMP         #'P'
                        BEQ         ?H_PROT
                        CMP         #'M'
                        BEQ         ?H_MEM
                        CMP         #'S'
                        BEQ         ?H_STEER
                        CMP         #'-'
                        BEQ         ?H_AUTO_OFF
                        CMP         #'+'
                        BEQ         ?H_AUTO_ON
                        BRA         ?H_USAGE
?H_ALL:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?H_USAGE
                        JSR         CMD_PRINT_HELP_FULL
                        RTS
?H_PROT:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?H_USAGE
                        JSR         CMD_PRINT_HELP_PROTECTION
                        RTS
?H_MEM:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?H_USAGE
                        JSR         CMD_PRINT_HELP_MEMORY
                        RTS
?H_STEER:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?H_USAGE
                        JSR         CMD_PRINT_HELP_STEERING
                        RTS
?H_AUTO_OFF:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?H_USAGE
                        JSR         CMD_SET_AUTOHELP_OFF
                        RTS
?H_AUTO_ON:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?H_USAGE
                        JSR         CMD_SET_AUTOHELP_ON
                        RTS
?H_INDEX:
                        PRT_CSTRING MSG_HELP_SHORT
                        PRT_CSTRING MSG_HELP_SECTIONS
                        RTS
?H_USAGE:
                        PRT_CSTRING MSG_H_USAGE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_SET_AUTOHELP_OFF
; DESCRIPTION: DISABLES STARTUP "-H" AUTO-HELP
; ----------------------------------------------------------------------------
CMD_SET_AUTOHELP_OFF:
                        LDA         #SYSF_H_AUTO_EN_M+SYSF_H_AUTO_PEND_M
                        TRB         SYS_FLAGS
                        PRT_CSTRING MSG_H_AUTO_OFF
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_SET_AUTOHELP_ON
; DESCRIPTION: ENABLES STARTUP "-H" AUTO-HELP (RE-ARMED FOR NEXT PROMPT)
; ----------------------------------------------------------------------------
CMD_SET_AUTOHELP_ON:
                        LDA         #SYSF_H_AUTO_EN_M+SYSF_H_AUTO_PEND_M
                        TSB         SYS_FLAGS
                        PRT_CSTRING MSG_H_AUTO_ON
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_AUTOHELP_OFF_ALIAS
; DESCRIPTION: ALIAS COMMAND "-H" TO DISABLE STARTUP AUTO-HELP
; ----------------------------------------------------------------------------
CMD_DO_AUTOHELP_OFF_ALIAS:
                        LDX         #$01 ; PARSE AFTER '-'
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'H'
                        BNE         ?CDAHO_USAGE
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CDAHO_USAGE
                        JSR         CMD_SET_AUTOHELP_OFF
                        RTS
?CDAHO_USAGE:
                        PRT_CSTRING MSG_AUTOHELP_ALIAS_USAGE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_AUTOHELP_ON_ALIAS
; DESCRIPTION: ALIAS COMMAND "+H" TO ENABLE STARTUP AUTO-HELP
; ----------------------------------------------------------------------------
CMD_DO_AUTOHELP_ON_ALIAS:
                        LDX         #$01 ; PARSE AFTER '+'
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'H'
                        BNE         ?CDAHO2_USAGE
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CDAHO2_USAGE
                        JSR         CMD_SET_AUTOHELP_ON
                        RTS
?CDAHO2_USAGE:
                        PRT_CSTRING MSG_AUTOHELP_ALIAS_USAGE
                        RTS

CMD_DO_QUIT_MONITOR:
                        JSR         CMD_DO_QUIT
                        LDA         #$01
                        STA         CMD_POST_ACTION
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_GO
; DESCRIPTION: EXECUTE CODE AT ADDRESS
; USAGE: X <START>
; ----------------------------------------------------------------------------
CMD_DO_GO:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BNE         ?CG_USAGE
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CG_USAGE
                        LDA         CMD_PARSE_VAL
                        STA         PTR_LEG
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_LEG+1
                        LDA         #SYSF_GO_FLAG_M
                        TSB         SYS_FLAGS
                        LDA         PTR_LEG
                        SEC
                        SBC         #$01
                        STA         PTR_LEG
                        LDA         PTR_LEG+1
                        SBC         #$00
                        STA         PTR_LEG+1
                        LDA         PTR_LEG+1
                        PHA
                        LDA         PTR_LEG
                        PHA
                        RTS
?CG_USAGE:
                        PRT_CSTRING MSG_G_USAGE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_RESUME
; DESCRIPTION: RESUMES LAST DEBUG CONTEXT (A/X/Y/P/SP/PC)
; USAGE: R [A=HH] [X=HH] [Y=HH]
; ----------------------------------------------------------------------------
CMD_DO_RESUME:
                        LDA         BRK_FLAG ; REUSED AS "DEBUG CONTEXT VALID"
                        BNE         ?CR_HAVE_CTX
                        PRT_CSTRING MSG_R_NO_CTX
                        RTS
?CR_HAVE_CTX:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        LDA         DBG_A
                        STA         PTR_LEG
                        LDA         DBG_X
                        STA         PTR_LEG+1
                        LDA         DBG_Y
                        STA         PTR_TEMP
?CR_PARSE_LOOP:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?CR_DO_RESUME
                        CMP         #'A'
                        BEQ         ?CR_SET_A
                        CMP         #'X'
                        BEQ         ?CR_SET_X
                        CMP         #'Y'
                        BEQ         ?CR_SET_Y
                        BRA         ?CR_USAGE
?CR_SET_A:
                        INX
                        JSR         R_PARSE_ASSIGN_BYTE
                        BCS         ?CR_USAGE
                        STA         PTR_LEG
                        BRA         ?CR_PARSE_LOOP
?CR_SET_X:
                        INX
                        JSR         R_PARSE_ASSIGN_BYTE
                        BCS         ?CR_USAGE
                        STA         PTR_LEG+1
                        BRA         ?CR_PARSE_LOOP
?CR_SET_Y:
                        INX
                        JSR         R_PARSE_ASSIGN_BYTE
                        BCS         ?CR_USAGE
                        STA         PTR_TEMP
                        BRA         ?CR_PARSE_LOOP
?CR_DO_RESUME:
                        LDA         PTR_LEG
                        STA         DBG_A
                        LDA         PTR_LEG+1
                        STA         DBG_X
                        LDA         PTR_TEMP
                        STA         DBG_Y
                        JMP         DBG_RESUME_CONTEXT
?CR_USAGE:
                        PRT_CSTRING MSG_R_USAGE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_NEXT
; DESCRIPTION: RUNS UNTIL NEXT SEQUENTIAL INSTRUCTION USING TEMP BRK PATCH
; USAGE: N
; NOTES:
;   - RAM TARGET ONLY (ROM/I/O NOT PATCHED)
;   - CURRENTLY STEPS LINEAR FLOW (SEQUENTIAL NEXT ADDRESS)
; ----------------------------------------------------------------------------
CMD_DO_NEXT:
                        LDA         BRK_FLAG
                        BNE         ?CN_HAVE_CTX
                        PRT_CSTRING MSG_R_NO_CTX
                        RTS
?CN_HAVE_CTX:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?CN_ARG_OK
                        PRT_CSTRING MSG_N_USAGE
                        RTS
?CN_ARG_OK:
                        LDA         STEP_ACTIVE
                        BEQ         ?CN_ARM
        ; DISARM ANY PREVIOUS PENDING STEP BREAKPOINT BEFORE RE-ARMING.
                        LDA         STEP_ADDR
                        STA         PTR_LEG
                        LDA         STEP_ADDR+1
                        STA         PTR_LEG+1
                        LDY         #$00
                        LDA         STEP_ORIG
                        STA         (PTR_LEG),Y
                        STZ         STEP_ACTIVE

?CN_ARM:
        ; STEP_ADDR = DBG_PC + U_OP_LEN_TAB[OPCODE@DBG_PC]
                        LDA         DBG_PC_LO
                        STA         PTR_LEG
                        LDA         DBG_PC_HI
                        STA         PTR_LEG+1
                        LDY         #$00
                        LDA         (PTR_LEG),Y
                        TAX
                        LDA         U_OP_LEN_TAB,X
                        CLC
                        ADC         DBG_PC_LO
                        STA         STEP_ADDR
                        LDA         DBG_PC_HI
                        ADC         #$00
                        STA         STEP_ADDR+1

        ; PATCH TARGET MUST BE RAM (NOT $7Fxx I/O OR $80xx+ ROM)
                        LDA         STEP_ADDR+1
                        CMP         #$7F
                        BCS         ?CN_ROM

        ; ENFORCE LOW-RAM PROTECTION RULES (FORCE WITH !N IF NEEDED)
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCS         ?CN_DONE

        ; SAVE ORIGINAL BYTE, PATCH BRK ($00), VERIFY PATCH TOOK
                        LDA         STEP_ADDR
                        STA         PTR_LEG
                        LDA         STEP_ADDR+1
                        STA         PTR_LEG+1
                        LDY         #$00
                        LDA         (PTR_LEG),Y
                        STA         STEP_ORIG
                        LDA         #$00
                        STA         (PTR_LEG),Y
                        LDA         (PTR_LEG),Y
                        BEQ         ?CN_PATCH_OK
                        LDA         STEP_ORIG
                        STA         (PTR_LEG),Y
                        BRA         ?CN_ROM
?CN_PATCH_OK:
                        LDA         #$01
                        STA         STEP_ACTIVE
                        JMP         DBG_RESUME_CONTEXT

?CN_ROM:
                        PRT_CSTRING MSG_N_ROM
?CN_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_GAME
; DESCRIPTION: PLAYS A 3-TRY NUMBER GUESS GAME
; USAGE: G
; RULES:
;   - TARGET IS 1..10.
;   - PLAYER HAS 3 CHANCES.
; ----------------------------------------------------------------------------
CMD_DO_GAME:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?GD_START
                        PRT_CSTRING MSG_GAME_USAGE
                        RTS
?GD_START:
                        JSR         GAME_PICK_TARGET
                        LDA         #$03
                        STA         GAME_TRIES
                        PRT_CSTRING MSG_GAME_INTRO
?GD_ROUND:
                        PRT_CSTRING MSG_GAME_PROMPT
                        JSR         ASM_READ_LINE
                        LDX         #$00
                        JSR         GAME_PARSE_GUESS
                        BCS         ?GD_BAD_INPUT
                        STA         GAME_GUESS
                        CMP         GAME_TARGET
                        BEQ         ?GD_WIN
                        BCC         ?GD_LOW
                        PRT_CSTRING MSG_GAME_HIGH
                        BRA         ?GD_MISS
?GD_LOW:
                        PRT_CSTRING MSG_GAME_LOW
?GD_MISS:
                        DEC         GAME_TRIES
                        BNE         ?GD_ROUND
                        PRT_CSTRING MSG_GAME_LOSE
                        LDA         GAME_TARGET
                        JSR         GAME_PRINT_1_10
                        RTS
?GD_BAD_INPUT:
                        PRT_CSTRING MSG_GAME_BAD_INPUT
                        BRA         ?GD_ROUND
?GD_WIN:
                        PRT_CSTRING MSG_GAME_WIN
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_INFO
; DESCRIPTION: INFO NAMESPACE ROOT
; USAGE: I C <RPN TOKENS>   OR   IC <RPN TOKENS>
; ----------------------------------------------------------------------------
CMD_DO_INFO:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'C'
                        BEQ         ?CID_CALC
                        PRT_CSTRING MSG_I_USAGE
                        RTS
?CID_CALC:
                        INX             ; PARSE AFTER SUBCOMMAND LETTER
                        JSR         CMD_DO_INFO_CALC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_INFO_CALC
; DESCRIPTION: 16-BIT HEX RPN CALCULATOR
; TOKENS:
;   - VALUES: 1..4 HEX DIGITS (OPTIONAL '$' PREFIX)
;   - OPS: + - * / & | ^ ~
; ----------------------------------------------------------------------------
CMD_DO_INFO_CALC:
                        STZ         RPN_SP
                        STZ         RPN_REM_VALID
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CIC_LOOP
                        PRT_CSTRING MSG_IC_USAGE
                        RTS
?CIC_LOOP:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CIC_HAVE_TOK
                        JMP         ?CIC_DONE
?CIC_HAVE_TOK:
                        CMP         #'+'
                        BNE         ?CIC_CHK_SUB
                        INX
                        JSR         RPN_OP_ADD
                        BCC         ?CIC_ADD_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_ADD_OK:
                        JMP         ?CIC_LOOP
?CIC_CHK_SUB:
                        CMP         #'-'
                        BNE         ?CIC_CHK_MUL
                        INX
                        JSR         RPN_OP_SUB
                        BCC         ?CIC_SUB_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_SUB_OK:
                        JMP         ?CIC_LOOP
?CIC_CHK_MUL:
                        CMP         #'*'
                        BNE         ?CIC_CHK_DIV
                        INX
                        JSR         RPN_OP_MUL
                        BCC         ?CIC_MUL_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_MUL_OK:
                        JMP         ?CIC_LOOP
?CIC_CHK_DIV:
                        CMP         #'/'
                        BNE         ?CIC_CHK_AND
                        INX
                        JSR         RPN_OP_DIV
                        BCC         ?CIC_DIV_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_DIV_OK:
                        JMP         ?CIC_LOOP
?CIC_CHK_AND:
                        CMP         #'&'
                        BNE         ?CIC_CHK_OR
                        INX
                        JSR         RPN_OP_AND
                        BCC         ?CIC_AND_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_AND_OK:
                        JMP         ?CIC_LOOP
?CIC_CHK_OR:
                        CMP         #'|'
                        BNE         ?CIC_CHK_XOR
                        INX
                        JSR         RPN_OP_OR
                        BCC         ?CIC_OR_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_OR_OK:
                        JMP         ?CIC_LOOP
?CIC_CHK_XOR:
                        CMP         #'^'
                        BNE         ?CIC_CHK_NOT
                        INX
                        JSR         RPN_OP_XOR
                        BCC         ?CIC_XOR_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_XOR_OK:
                        JMP         ?CIC_LOOP
?CIC_CHK_NOT:
                        CMP         #'~'
                        BNE         ?CIC_NUM_CHK
                        INX
                        JSR         RPN_OP_NOT
                        BCC         ?CIC_NOT_OK
                        JMP         ?CIC_ERR_FROM_A
?CIC_NOT_OK:
                        JMP         ?CIC_LOOP

        ; NUMBER TOKEN: "$" PREFIX OR LEADING HEX DIGIT
?CIC_NUM_CHK:
                        CMP         #'$'
                        BEQ         ?CIC_NUM
                        JSR         HEX_TO_NIBBLE
                        BCC         ?CIC_BADTOK
?CIC_NUM:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BNE         ?CIC_BADTOK
                        STZ         RPN_REM_VALID ; NEW VALUE INVALIDATES LAST REM
                        JSR         RPN_PUSH_PARSE
                        BCS         ?CIC_OVERFLOW
                        JMP         ?CIC_LOOP

?CIC_DONE:
                        JSR         RPN_PEEK_TOP_TO_PARSE
                        BCS         ?CIC_EMPTY
                        PRT_CSTRING MSG_IC_RESULT
                        JSR         PRT_WORD_FROM_PARSE
                        LDA         RPN_REM_VALID
                        BEQ         ?CIC_DONE_RTS
                        PRT_CSTRING MSG_IC_REM
                        LDA         RPN_LAST_REM+1
                        LDX         RPN_LAST_REM
                        JSR         PRT_HEX_WORD_AX
?CIC_DONE_RTS:
                        RTS

?CIC_ERR_FROM_A:
                        CMP         #$01
                        BEQ         ?CIC_UNDERFLOW
                        CMP         #$02
                        BEQ         ?CIC_OVERFLOW
                        CMP         #$03
                        BEQ         ?CIC_DIV0
                        BRA         ?CIC_BADTOK
?CIC_BADTOK:
                        PRT_CSTRING MSG_IC_BADTOK
                        RTS
?CIC_UNDERFLOW:
                        PRT_CSTRING MSG_IC_UNDERFLOW
                        RTS
?CIC_OVERFLOW:
                        PRT_CSTRING MSG_IC_OVERFLOW
                        RTS
?CIC_DIV0:
                        PRT_CSTRING MSG_IC_DIV0
                        RTS
?CIC_EMPTY:
                        PRT_CSTRING MSG_IC_EMPTY
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RPN_OP_ADD/SUB/MUL/DIV/AND/OR/XOR/NOT
; DESCRIPTION: EXECUTES ONE RPN OPERATION
; OUTPUT: C=0 OK
;         C=1 ERROR, A=1 UNDERFLOW, A=2 OVERFLOW, A=3 DIV0
; ----------------------------------------------------------------------------
RPN_OP_ADD:
                        JSR         RPN_POP2_TO_TMP
                        BCC         ?ROPA_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPA_POP_OK:
                        CLC
                        LDA         RPN_TMP_L
                        ADC         RPN_TMP_R
                        STA         CMD_PARSE_VAL
                        LDA         RPN_TMP_L+1
                        ADC         RPN_TMP_R+1
                        STA         CMD_PARSE_VAL+1
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPA_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPA_PUSH_OK:
                        STZ         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_SUB:
                        JSR         RPN_POP2_TO_TMP
                        BCC         ?ROPS_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPS_POP_OK:
                        SEC
                        LDA         RPN_TMP_L
                        SBC         RPN_TMP_R
                        STA         CMD_PARSE_VAL
                        LDA         RPN_TMP_L+1
                        SBC         RPN_TMP_R+1
                        STA         CMD_PARSE_VAL+1
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPS_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPS_PUSH_OK:
                        STZ         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_MUL:
                        JSR         RPN_POP2_TO_TMP
                        BCC         ?ROPM_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPM_POP_OK:
                        PHX             ; PRESERVE CMD_LINE TOKEN INDEX
                        JSR         RPN_MUL_16
                        PLX
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPM_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPM_PUSH_OK:
                        STZ         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_DIV:
                        JSR         RPN_POP2_TO_TMP
                        BCC         ?ROPD_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPD_POP_OK:
                        LDA         RPN_TMP_R
                        ORA         RPN_TMP_R+1
                        BNE         ?ROPD_DIVNZ
                        JMP         RPN_OP_ERR_DIV0
?ROPD_DIVNZ:
                        PHX             ; PRESERVE CMD_LINE TOKEN INDEX
                        JSR         RPN_DIV_16
                        PLX
                        LDA         RPN_REM
                        STA         RPN_LAST_REM
                        LDA         RPN_REM+1
                        STA         RPN_LAST_REM+1
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPD_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPD_PUSH_OK:
                        LDA         #$01
                        STA         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_AND:
                        JSR         RPN_POP2_TO_TMP
                        BCC         ?ROPA2_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPA2_POP_OK:
                        LDA         RPN_TMP_L
                        AND         RPN_TMP_R
                        STA         CMD_PARSE_VAL
                        LDA         RPN_TMP_L+1
                        AND         RPN_TMP_R+1
                        STA         CMD_PARSE_VAL+1
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPA2_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPA2_PUSH_OK:
                        STZ         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_OR:
                        JSR         RPN_POP2_TO_TMP
                        BCC         ?ROPO_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPO_POP_OK:
                        LDA         RPN_TMP_L
                        ORA         RPN_TMP_R
                        STA         CMD_PARSE_VAL
                        LDA         RPN_TMP_L+1
                        ORA         RPN_TMP_R+1
                        STA         CMD_PARSE_VAL+1
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPO_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPO_PUSH_OK:
                        STZ         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_XOR:
                        JSR         RPN_POP2_TO_TMP
                        BCC         ?ROPX_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPX_POP_OK:
                        LDA         RPN_TMP_L
                        EOR         RPN_TMP_R
                        STA         CMD_PARSE_VAL
                        LDA         RPN_TMP_L+1
                        EOR         RPN_TMP_R+1
                        STA         CMD_PARSE_VAL+1
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPX_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPX_PUSH_OK:
                        STZ         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_NOT:
                        JSR         RPN_POP_TO_PARSE
                        BCC         ?ROPN_POP_OK
                        JMP         RPN_OP_ERR_UNDER
?ROPN_POP_OK:
                        LDA         CMD_PARSE_VAL
                        EOR         #$FF
                        STA         CMD_PARSE_VAL
                        LDA         CMD_PARSE_VAL+1
                        EOR         #$FF
                        STA         CMD_PARSE_VAL+1
                        JSR         RPN_PUSH_PARSE
                        BCC         ?ROPN_PUSH_OK
                        JMP         RPN_OP_ERR_OVER
?ROPN_PUSH_OK:
                        STZ         RPN_REM_VALID
                        CLC
                        RTS

RPN_OP_ERR_UNDER:
                        LDA         #$01
                        SEC
                        RTS
RPN_OP_ERR_OVER:
                        LDA         #$02
                        SEC
                        RTS
RPN_OP_ERR_DIV0:
                        LDA         #$03
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RPN_PUSH_PARSE
; DESCRIPTION: PUSHES CMD_PARSE_VAL (16-BIT) ONTO RPN STACK
; OUTPUT: C=0 OK, C=1 OVERFLOW
; ----------------------------------------------------------------------------
RPN_PUSH_PARSE:
                        LDA         RPN_SP
                        CMP         #RPN_STACK_DEPTH
                        BCS         ?RPS_OVER
                        ASL         A
                        TAY
                        LDA         CMD_PARSE_VAL
                        STA         RPN_STACK,Y
                        LDA         CMD_PARSE_VAL+1
                        STA         RPN_STACK+1,Y
                        INC         RPN_SP
                        CLC
                        RTS
?RPS_OVER:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RPN_POP_TO_PARSE
; DESCRIPTION: POPS TOP RPN VALUE INTO CMD_PARSE_VAL (16-BIT)
; OUTPUT: C=0 OK, C=1 UNDERFLOW
; ----------------------------------------------------------------------------
RPN_POP_TO_PARSE:
                        LDA         RPN_SP
                        BEQ         ?RP2_UNDER
                        DEC         RPN_SP
                        LDA         RPN_SP
                        ASL         A
                        TAY
                        LDA         RPN_STACK,Y
                        STA         CMD_PARSE_VAL
                        LDA         RPN_STACK+1,Y
                        STA         CMD_PARSE_VAL+1
                        CLC
                        RTS
?RP2_UNDER:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RPN_POP2_TO_TMP
; DESCRIPTION: POPS RIGHT THEN LEFT OPERAND INTO RPN_TMP_R / RPN_TMP_L
; OUTPUT: C=0 OK, C=1 UNDERFLOW
; ----------------------------------------------------------------------------
RPN_POP2_TO_TMP:
                        JSR         RPN_POP_TO_PARSE
                        BCS         ?RP22_UNDER
                        LDA         CMD_PARSE_VAL
                        STA         RPN_TMP_R
                        LDA         CMD_PARSE_VAL+1
                        STA         RPN_TMP_R+1
                        JSR         RPN_POP_TO_PARSE
                        BCS         ?RP22_UNDER
                        LDA         CMD_PARSE_VAL
                        STA         RPN_TMP_L
                        LDA         CMD_PARSE_VAL+1
                        STA         RPN_TMP_L+1
                        CLC
                        RTS
?RP22_UNDER:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RPN_PEEK_TOP_TO_PARSE
; DESCRIPTION: COPIES TOP RPN VALUE TO CMD_PARSE_VAL WITHOUT POPPING
; OUTPUT: C=0 OK, C=1 EMPTY STACK
; ----------------------------------------------------------------------------
RPN_PEEK_TOP_TO_PARSE:
                        LDA         RPN_SP
                        BEQ         ?RPK_EMPTY
                        SEC
                        SBC         #$01
                        ASL         A
                        TAY
                        LDA         RPN_STACK,Y
                        STA         CMD_PARSE_VAL
                        LDA         RPN_STACK+1,Y
                        STA         CMD_PARSE_VAL+1
                        CLC
                        RTS
?RPK_EMPTY:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RPN_MUL_16
; DESCRIPTION: 16-BIT UNSIGNED MULTIPLY (LOW 16-BIT PRODUCT)
; INPUT: RPN_TMP_L * RPN_TMP_R
; OUTPUT: CMD_PARSE_VAL = PRODUCT (MOD 65536)
; ----------------------------------------------------------------------------
RPN_MUL_16:
                        STZ         CMD_PARSE_VAL
                        STZ         CMD_PARSE_VAL+1
                        LDX         #$10
?RM16_LOOP:
                        LDA         RPN_TMP_R+1
                        LSR         A
                        STA         RPN_TMP_R+1
                        LDA         RPN_TMP_R
                        ROR         A
                        STA         RPN_TMP_R
                        BCC         ?RM16_NOADD
                        CLC
                        LDA         CMD_PARSE_VAL
                        ADC         RPN_TMP_L
                        STA         CMD_PARSE_VAL
                        LDA         CMD_PARSE_VAL+1
                        ADC         RPN_TMP_L+1
                        STA         CMD_PARSE_VAL+1
?RM16_NOADD:
                        ASL         RPN_TMP_L
                        ROL         RPN_TMP_L+1
                        DEX
                        BNE         ?RM16_LOOP
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RPN_DIV_16
; DESCRIPTION: 16-BIT UNSIGNED DIVIDE
; INPUT: RPN_TMP_L / RPN_TMP_R (DIVISOR MUST BE NONZERO)
; OUTPUT: CMD_PARSE_VAL = QUOTIENT, RPN_REM = REMAINDER
; ----------------------------------------------------------------------------
RPN_DIV_16:
                        LDA         RPN_TMP_L
                        STA         CMD_PARSE_VAL
                        LDA         RPN_TMP_L+1
                        STA         CMD_PARSE_VAL+1
                        STZ         RPN_REM
                        STZ         RPN_REM+1
                        LDX         #$10
?RD16_LOOP:
                        ASL         CMD_PARSE_VAL
                        ROL         CMD_PARSE_VAL+1
                        ROL         RPN_REM
                        ROL         RPN_REM+1
                        SEC
                        LDA         RPN_REM
                        SBC         RPN_TMP_R
                        STA         RPN_REM
                        LDA         RPN_REM+1
                        SBC         RPN_TMP_R+1
                        STA         RPN_REM+1
                        BCC         ?RD16_RESTORE
                        LDA         CMD_PARSE_VAL
                        ORA         #$01
                        STA         CMD_PARSE_VAL
                        BRA         ?RD16_NEXT
?RD16_RESTORE:
                        CLC
                        LDA         RPN_REM
                        ADC         RPN_TMP_R
                        STA         RPN_REM
                        LDA         RPN_REM+1
                        ADC         RPN_TMP_R+1
                        STA         RPN_REM+1
?RD16_NEXT:
                        DEX
                        BNE         ?RD16_LOOP
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: GAME_PICK_TARGET
; DESCRIPTION: SETS GAME_TARGET IN RANGE 1..10
; OUTPUT: GAME_TARGET = 1..10
; ----------------------------------------------------------------------------
GAME_PICK_TARGET:
                        JSR         RNG_MIX_NCAXR ; A = RAW RNG BYTE
                        LDX         #$0A ; RANGE SIZE = 10
                        JSR         RNG_MOD_N ; A = 0..9
                        CLC
                        ADC         #$01 ; A = 1..10
                        STA         GAME_TARGET
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RNG_MIX_NCAXR
; DESCRIPTION: RETURNS A NON-CRYPTOGRAPHIC RNG BYTE
;              SOURCE WINDOW: $0040..$02FF
; OUTPUT: A = RNG BYTE (0..255)
; CLOBBERS: A,Y, PTR_LEG, GAME_TARGET
; ----------------------------------------------------------------------------
RNG_MIX_NCAXR:
                        ; INITIALIZE RNG STATE.
                        LDA         RBUF_HEAD
                        EOR         CMD_LAST_LEN
                        EOR         RNG_STATE
                        STA         GAME_TARGET

                        ; SCAN STATE WINDOW.
                        LDA         #<PTR_TEMP
                        STA         PTR_LEG
                        LDA         #>PTR_TEMP
                        STA         PTR_LEG+1
?RNG_MIX_SCAN:
                        LDY         #$00
                        LDA         (PTR_LEG),Y
                        CLC
                        ADC         GAME_TARGET
                        ROL         A
                        EOR         PTR_LEG
                        LSR         A
                        CLC
                        ADC         GAME_TARGET
                        STA         GAME_TARGET

                        INC         PTR_LEG
                        BNE         ?RNG_MIX_SCAN_CHECK
                        INC         PTR_LEG+1
?RNG_MIX_SCAN_CHECK:
                        LDA         PTR_LEG+1
                        CMP         #$03 ; STOP WHEN POINTER REACHES $0300
                        BCC         ?RNG_MIX_SCAN

                        LDA         GAME_TARGET
                        EOR         RNG_STATE
                        ROL         A
                        ADC         #$3D
                        STA         RNG_STATE
                        LDA         RNG_STATE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: RNG_MOD_N
; DESCRIPTION: MAPS A RANDOM BYTE TO 0..N-1
; INPUT: A = RANDOM BYTE (0..255), X = N (0..255)
; OUTPUT: A = A MOD N (IF N=0, A IS RETURNED UNCHANGED)
; CLOBBERS: A, CMD_PARSE_NIB, MOD_COUNT
; ----------------------------------------------------------------------------
RNG_MOD_N:
                        CPX         #$00
                        BEQ         ?RMN_DONE ; N=0 => RAW BYTE PATH
                        STA         CMD_PARSE_NIB ; REMAINDER
                        TXA
                        STA         MOD_COUNT ; DIVISOR
?RMN_LOOP:
                        LDA         CMD_PARSE_NIB
                        CMP         MOD_COUNT
                        BCC         ?RMN_OUT
                        SEC
                        SBC         MOD_COUNT
                        STA         CMD_PARSE_NIB
                        BRA         ?RMN_LOOP
?RMN_OUT:
                        LDA         CMD_PARSE_NIB
?RMN_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: GAME_PARSE_GUESS
; DESCRIPTION: PARSES CMD_LINE AS DECIMAL 1..10 (OPTIONAL LEADING/TRAILING SPACES)
; INPUT:  X = START INDEX
; OUTPUT: A = GUESS (1..10), C=0 OK / C=1 ERROR
; ----------------------------------------------------------------------------
GAME_PARSE_GUESS:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'1'
                        BEQ         ?GPG_ONE_OR_TEN
                        CMP         #'2'
                        BCC         ?GPG_ERR
                        CMP         #'9'+1
                        BCS         ?GPG_ERR
                        SEC
                        SBC         #'0'
                        PHA
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?GPG_POP_ERR
                        PLA
                        CLC
                        RTS
?GPG_ONE_OR_TEN:
                        INX
                        LDA         CMD_LINE,X
                        BEQ         ?GPG_ONE_OK
                        CMP         #' '
                        BEQ         ?GPG_ONE_TAIL
                        CMP         #'0'
                        BNE         ?GPG_ERR
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?GPG_ERR
                        LDA         #$0A
                        CLC
                        RTS
?GPG_ONE_OK:
                        LDA         #$01
                        CLC
                        RTS
?GPG_ONE_TAIL:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?GPG_ERR
                        LDA         #$01
                        CLC
                        RTS
?GPG_POP_ERR:
                        PLA
?GPG_ERR:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: GAME_PRINT_1_10
; DESCRIPTION: PRINTS DECIMAL VALUE IN A (EXPECTED 1..10)
; ----------------------------------------------------------------------------
GAME_PRINT_1_10:
                        CMP         #$0A
                        BNE         ?GP10_ONE
                        LDA         #'1'
                        JSR         WRITE_BYTE
                        LDA         #'0'
                        JSR         WRITE_BYTE
                        RTS
?GP10_ONE:
                        CLC
                        ADC         #'0'
                        JSR         WRITE_BYTE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_LOAD
; DESCRIPTION: SERIAL LOADER COMMAND DISPATCH
; USAGE: L S | L G S | L B <ADDR> <LEN>
; ----------------------------------------------------------------------------
CMD_DO_LOAD:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'S'
                        BEQ         ?CL_PARSE_S
                        CMP         #'G'
                        BEQ         ?CL_PARSE_G
                        CMP         #'B'
                        BEQ         ?CL_PARSE_B
                        BRA         ?CL_USAGE
?CL_PARSE_S:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CL_USAGE
                        JMP         CMD_DO_LOAD_SREC
?CL_PARSE_G:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'S'
                        BNE         ?CL_USAGE
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CL_USAGE
                        JMP         CMD_DO_LOAD_SREC_GO
?CL_PARSE_B:
                        INX
                        JMP         CMD_DO_LOAD_BIN
?CL_USAGE:
                        PRT_CSTRING MSG_L_USAGE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_LOAD_SREC_GO
; DESCRIPTION: LOADS S-RECORDS (L S) AND AUTO-JUMPS TO TERM RECORD START ADDR
; USAGE: L G S | LGS | LG S
; NOTES:
;   - AUTO-GO RUNS ONLY AFTER SUCCESSFUL S-RECORD TERMINATION/CHECKSUM.
;   - TARGET ENTRY ADDRESS IS THE PARSED S7/S8/S9 ADDRESS (LOW 16-BIT).
; ----------------------------------------------------------------------------
CMD_DO_LOAD_SREC_GO:
                        JSR         CMD_DO_LOAD_SREC
                        BCS         ?LGS_DONE ; LOAD ALREADY PRINTED ERROR/ABORT
                        ; If S7/S8/S9 entry is zero, fall back to first S1/S2/S3
                        ; data record address captured during load.
                        LDA         PTR_TEMP
                        ORA         PTR_TEMP+1
                        BNE         ?LGS_USE_TERM
                        LDA         SREC_FIRST_VALID
                        BNE         ?LGS_USE_FIRST
                        PRT_CSTRING MSG_LGS_NO_ENTRY
                        BRA         ?LGS_DONE
?LGS_USE_FIRST:
                        LDA         SREC_FIRST_ADDR
                        STA         PTR_LEG
                        LDA         SREC_FIRST_ADDR+1
                        STA         PTR_LEG+1
                        BRA         ?LGS_GO
?LGS_USE_TERM:
                        LDA         PTR_TEMP
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        STA         PTR_LEG+1
?LGS_GO:
                        LDA         #SYSF_GO_FLAG_M
                        TSB         SYS_FLAGS
                        LDA         PTR_LEG
                        SEC
                        SBC         #$01
                        STA         PTR_LEG
                        LDA         PTR_LEG+1
                        SBC         #$00
                        STA         PTR_LEG+1
                        LDA         PTR_LEG+1
                        PHA
                        LDA         PTR_LEG
                        PHA
?LGS_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_LOAD_BIN
; DESCRIPTION: RECEIVES RAW BINARY BYTES OVER SERIAL INTO MEMORY
; USAGE: L B <ADDR> <LEN>
; NOTES:
;   - ADDR/LEN ARE 1..4 HEX DIGITS (OPTIONAL '$' PREFIX).
;   - LEN MUST BE 1..$FFFF.
;   - ADDR+LEN MAY END AT EXACTLY $10000, BUT MUST NOT EXCEED IT.
;   - NO CHECKSUM/CRC IS USED.
; ----------------------------------------------------------------------------
CMD_DO_LOAD_BIN:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?LB_ADDR_OK
                        JMP         ?LB_USAGE
?LB_ADDR_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_TEMP ; DEST ADDR
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_TEMP+1

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?LB_LEN_PARSE_OK
                        JMP         ?LB_USAGE
?LB_LEN_PARSE_OK:
                        LDA         CMD_PARSE_VAL
                        STA         SREC_COUNT ; BYTES LEFT LO
                        LDA         CMD_PARSE_VAL+1
                        STA         SREC_COUNT+1 ; BYTES LEFT HI

                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?LB_ARGS_DONE
                        JMP         ?LB_USAGE
?LB_ARGS_DONE:

                        LDA         SREC_COUNT
                        ORA         SREC_COUNT+1
                        BNE         ?LB_LEN_OK
                        PRT_CSTRING MSG_LB_LEN_ERR
                        RTS
?LB_LEN_OK:
        ; EARLY PROTECT CHECK SO WE FAIL BEFORE CONSUMING BINARY STREAM BYTES.
                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCC         ?LB_PROTECT_OK
                        RTS
?LB_PROTECT_OK:
        ; RANGE CHECK: ADDR+LEN MAY BE <= $10000 ONLY.
                        CLC
                        LDA         PTR_TEMP
                        ADC         SREC_COUNT
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        ADC         SREC_COUNT+1
                        STA         PTR_LEG+1
                        BCC         ?LB_RANGE_OK
                        LDA         PTR_LEG
                        ORA         PTR_LEG+1
                        BEQ         ?LB_RANGE_OK ; EXACTLY $10000
                        PRT_CSTRING MSG_LB_RANGE_ERR
                        RTS

?LB_RANGE_OK:
                        PRT_CSTRING MSG_LB_READY
?LB_RECV_LOOP:
                        LDA         SREC_COUNT
                        ORA         SREC_COUNT+1
                        BEQ         ?LB_DONE
                        JSR         READ_BYTE
                        JSR         SREC_WRITE_BYTE
                        BCC         ?LB_DEC
                        PRT_CSTRING MSG_LB_ABORT
                        RTS
?LB_DEC:
                        LDA         SREC_COUNT
                        BNE         ?LB_DEC_LO
                        DEC         SREC_COUNT+1
?LB_DEC_LO:
                        DEC         SREC_COUNT
                        BRA         ?LB_RECV_LOOP
?LB_DONE:
                        PRT_CSTRING MSG_LB_DONE
                        RTS

?LB_USAGE:
                        PRT_CSTRING MSG_LB_USAGE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_LOAD_SREC
; DESCRIPTION: RECEIVES MOTOROLA S-RECORDS OVER SERIAL UNTIL S7/S8/S9
; NOTES:
;   - S1/S2/S3 DATA RECORDS ARE WRITTEN TO MEMORY.
;   - S0/S5/S6 ARE PARSED+CHECKED BUT DATA IS IGNORED.
;   - S7/S8/S9 TERMINATES LOAD AFTER CHECKSUM VALIDATION.
;   - DATA/TERM ADDRESSES ABOVE $FFFF ARE REJECTED.
; OUTPUT:
;   - C=0 ON SUCCESSFUL TERM RECORD + CHECKSUM.
;   - C=1 ON ABORT/PARSE/CHECKSUM/ADDRESS ERROR.
; ----------------------------------------------------------------------------
CMD_DO_LOAD_SREC:
                        STZ         SREC_FIRST_VALID
                        STZ         SREC_FIRST_ADDR
                        STZ         SREC_FIRST_ADDR+1
                        PRT_CSTRING MSG_LS_READY

?LS_REC_LOOP:
                        JSR         SREC_WAIT_START
                        JSR         READ_BYTE
                        JSR         UTIL_TO_UPPER
                        CMP         #'0'
                        BEQ         ?LS_T0
                        CMP         #'1'
                        BEQ         ?LS_T1
                        CMP         #'2'
                        BEQ         ?LS_T2
                        CMP         #'3'
                        BEQ         ?LS_T3
                        CMP         #'5'
                        BEQ         ?LS_T5
                        CMP         #'6'
                        BEQ         ?LS_T6
                        CMP         #'7'
                        BEQ         ?LS_T7
                        CMP         #'8'
                        BEQ         ?LS_T8
                        CMP         #'9'
                        BEQ         ?LS_T9
                        CMP         #'X'
                        BEQ         ?LS_USER_ABORT
                        PRT_CSTRING MSG_LS_TYPE_ERR
                        PRT_CSTRING MSG_LS_ABORT
                        SEC
                        RTS
?LS_USER_ABORT:
                        PRT_CSTRING MSG_LS_ABORT
                        SEC
                        RTS
?LS_T0:
                        LDA         #$02
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_SKIP
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T1:
                        LDA         #$02
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_DATA
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T2:
                        LDA         #$03
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_DATA
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T3:
                        LDA         #$04
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_DATA
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T5:
                        LDA         #$02
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_SKIP
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T6:
                        LDA         #$03
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_SKIP
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T7:
                        LDA         #$04
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_TERM
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T8:
                        LDA         #$03
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_TERM
                        STA         SREC_MODE
                        BRA         ?LS_HAVE_TYPE
?LS_T9:
                        LDA         #$02
                        STA         SREC_ADDR_LEN
                        LDA         #SREC_MODE_TERM
                        STA         SREC_MODE

?LS_HAVE_TYPE:
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_COUNT_OK
                        JMP         ?LS_PARSE_ERR
?LS_COUNT_OK:
                        STA         SREC_COUNT
                        STA         SREC_SUM

        ; REMAIN = COUNT - ADDR_LEN ; (DATA + CHECKSUM)
                        SEC
                        SBC         SREC_ADDR_LEN
                        BCS         ?LS_REMAIN_OK
                        JMP         ?LS_PARSE_ERR
?LS_REMAIN_OK:
                        STA         SREC_REMAIN
                        BNE         ?LS_REMAIN_NONZERO
                        JMP         ?LS_PARSE_ERR
?LS_REMAIN_NONZERO:

        ; PARSE ADDRESS FIELD.
                        STZ         PTR_LEG
                        STZ         PTR_TEMP
                        STZ         PTR_TEMP+1
                        LDA         SREC_ADDR_LEN
                        CMP         #$02
                        BEQ         ?LS_ADDR16
                        CMP         #$03
                        BEQ         ?LS_ADDR24

        ; 32-BIT ADDRESS
?LS_ADDR32:
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A32_B3_OK
                        JMP         ?LS_PARSE_ERR
?LS_A32_B3_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_LEG
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A32_B2_OK
                        JMP         ?LS_PARSE_ERR
?LS_A32_B2_OK:
                        JSR         SREC_SUM_ADD_A
                        ORA         PTR_LEG
                        STA         PTR_LEG
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A32_B1_OK
                        JMP         ?LS_PARSE_ERR
?LS_A32_B1_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_TEMP+1
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A32_B0_OK
                        JMP         ?LS_PARSE_ERR
?LS_A32_B0_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_TEMP
                        BRA         ?LS_ADDR_DONE

?LS_ADDR24:
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A24_B2_OK
                        JMP         ?LS_PARSE_ERR
?LS_A24_B2_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_LEG
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A24_B1_OK
                        JMP         ?LS_PARSE_ERR
?LS_A24_B1_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_TEMP+1
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A24_B0_OK
                        JMP         ?LS_PARSE_ERR
?LS_A24_B0_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_TEMP
                        BRA         ?LS_ADDR_DONE

?LS_ADDR16:
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A16_B1_OK
                        JMP         ?LS_PARSE_ERR
?LS_A16_B1_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_TEMP+1
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_A16_B0_OK
                        JMP         ?LS_PARSE_ERR
?LS_A16_B0_OK:
                        JSR         SREC_SUM_ADD_A
                        STA         PTR_TEMP

?LS_ADDR_DONE:
        ; DATA/TERM ADDRESSES MUST FIT 16-BIT ADDR SPACE.
                        LDA         SREC_MODE
                        CMP         #SREC_MODE_SKIP
                        BEQ         ?LS_AFTER_ADDR_CHECK
                        LDA         PTR_LEG
                        BEQ         ?LS_AFTER_ADDR_CHECK
                        PRT_CSTRING MSG_LS_ADDR_ERR
                        PRT_CSTRING MSG_LS_ABORT
                        SEC
                        RTS
?LS_AFTER_ADDR_CHECK:
        ; Capture first S1/S2/S3 address for LGS fallback when S7/S8/S9 is 0000.
                        LDA         SREC_MODE
                        CMP         #SREC_MODE_DATA
                        BNE         ?LS_AFTER_FIRST_CAPTURE
                        LDA         SREC_FIRST_VALID
                        BNE         ?LS_AFTER_FIRST_CAPTURE
                        LDA         PTR_TEMP
                        STA         SREC_FIRST_ADDR
                        LDA         PTR_TEMP+1
                        STA         SREC_FIRST_ADDR+1
                        LDA         #$01
                        STA         SREC_FIRST_VALID
?LS_AFTER_FIRST_CAPTURE:

        ; TERMINATION RECORDS SHOULD HAVE NO DATA BYTES.
                        LDA         SREC_MODE
                        CMP         #SREC_MODE_TERM
                        BNE         ?LS_DATA_LOOP
                        LDA         SREC_REMAIN
                        CMP         #$01
                        BEQ         ?LS_READ_CKSUM
                        JMP         ?LS_PARSE_ERR

?LS_DATA_LOOP:
                        LDA         SREC_REMAIN
                        CMP         #$01
                        BEQ         ?LS_READ_CKSUM
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_DATA_HEX_OK
                        JMP         ?LS_PARSE_ERR
?LS_DATA_HEX_OK:
                        JSR         SREC_SUM_ADD_A
                        LDX         SREC_MODE
                        CPX         #SREC_MODE_DATA
                        BNE         ?LS_DATA_NEXT
                        JSR         SREC_WRITE_BYTE
                        BCC         ?LS_DATA_NEXT
                        PRT_CSTRING MSG_LS_ABORT
                        SEC
                        RTS
?LS_DATA_NEXT:
                        DEC         SREC_REMAIN
                        BRA         ?LS_DATA_LOOP

?LS_READ_CKSUM:
                        JSR         SREC_READ_HEXBYTE
                        BCC         ?LS_CKSUM_HEX_OK
                        JMP         ?LS_PARSE_ERR
?LS_CKSUM_HEX_OK:
                        JSR         SREC_SUM_ADD_A
                        LDA         SREC_SUM
                        CMP         #$FF
                        BEQ         ?LS_CKSUM_OK
                        PRT_CSTRING MSG_LS_CHKSUM_ERR
                        PRT_CSTRING MSG_LS_ABORT
                        SEC
                        RTS
?LS_CKSUM_OK:
                        LDA         SREC_MODE
                        CMP         #SREC_MODE_TERM
                        BEQ         ?LS_DONE
                        JMP         ?LS_REC_LOOP
?LS_DONE:
                        PRT_CSTRING MSG_LS_DONE
                        CLC
                        RTS

?LS_PARSE_ERR:
                        PRT_CSTRING MSG_LS_PARSE_ERR
                        PRT_CSTRING MSG_LS_ABORT
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SREC_WAIT_START
; DESCRIPTION: BLOCKS UNTIL AN 'S' CHARACTER IS RECEIVED
; ----------------------------------------------------------------------------
SREC_WAIT_START:
?SWS_LOOP:
                        JSR         READ_BYTE
                        JSR         UTIL_TO_UPPER
                        CMP         #'S'
                        BNE         ?SWS_LOOP
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SREC_READ_HEXBYTE
; DESCRIPTION: READS TWO ASCII HEX CHARS FROM SERIAL AND RETURNS BYTE
; OUTPUT: A=BYTE, C=0 OK / C=1 INVALID
; ----------------------------------------------------------------------------
SREC_READ_HEXBYTE:
                        JSR         READ_BYTE
                        JSR         UTIL_TO_UPPER
                        JSR         HEX_TO_NIBBLE
                        BCC         ?SRHB_BAD
                        ASL         A
                        ASL         A
                        ASL         A
                        ASL         A
                        STA         MOD_BYTE
                        JSR         READ_BYTE
                        JSR         UTIL_TO_UPPER
                        JSR         HEX_TO_NIBBLE
                        BCC         ?SRHB_BAD
                        CLC
                        ADC         MOD_BYTE
                        CLC
                        RTS
?SRHB_BAD:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SREC_SUM_ADD_A
; DESCRIPTION: ADDS A BYTE INTO THE 8-BIT S-RECORD CHECKSUM SUM
; INPUT: A=BYTE
; OUTPUT: SREC_SUM UPDATED, A PRESERVED
; ----------------------------------------------------------------------------
SREC_SUM_ADD_A:
                        PHA
                        CLC
                        ADC         SREC_SUM
                        STA         SREC_SUM
                        PLA
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SREC_WRITE_BYTE
; DESCRIPTION: WRITES A BYTE TO PTR_TEMP WITH PROTECT CHECK + VERIFY
; INPUT: A=DATA BYTE, PTR_TEMP=DESTINATION
; OUTPUT: C=0 OK / C=1 FAIL
; ----------------------------------------------------------------------------
SREC_WRITE_BYTE:
                        STA         MOD_BYTE
                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCC         ?SWB_ALLOWED
                        SEC
                        RTS
?SWB_ALLOWED:
                        LDY         #$00
                        LDA         MOD_BYTE
                        STA         (PTR_TEMP),Y
                        LDA         (PTR_TEMP),Y
                        CMP         MOD_BYTE
                        BEQ         ?SWB_OK
                        LDA         PTR_TEMP
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        STA         PTR_LEG+1
                        LDA         #'L'
                        JSR         PRT_VERIFY_ERR
                        SEC
                        RTS
?SWB_OK:
                        INC         PTR_TEMP
                        BNE         ?SWB_DONE
                        INC         PTR_TEMP+1
?SWB_DONE:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: R_PARSE_ASSIGN_BYTE
; DESCRIPTION: PARSES "=HH" (HEX BYTE) FOR R REGISTER OVERRIDES
; INPUT: X = FIRST CHAR AFTER REGISTER LETTER
; OUTPUT: A = PARSED BYTE, X = DELIMITER/NUL, C=0 OK / C=1 ERROR
; ----------------------------------------------------------------------------
R_PARSE_ASSIGN_BYTE:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'='
                        BNE         ?RPAB_ERR
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        JSR         HEX_TO_NIBBLE
                        BCC         ?RPAB_ERR
                        ASL         A
                        ASL         A
                        ASL         A
                        ASL         A
                        STA         CMD_PARSE_NIB
                        INX
                        LDA         CMD_LINE,X
                        JSR         HEX_TO_NIBBLE
                        BCC         ?RPAB_ERR
                        CLC
                        ADC         CMD_PARSE_NIB
                        STA         CMD_PARSE_VAL
                        INX
                        LDA         CMD_LINE,X
                        BEQ         ?RPAB_OK
                        CMP         #' '
                        BNE         ?RPAB_ERR
?RPAB_OK:
                        LDA         CMD_PARSE_VAL
                        CLC
                        RTS
?RPAB_ERR:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DBG_RESUME_CONTEXT
; DESCRIPTION: RESUMES USING DBG_A/X/Y/P/SP/PC VIA RTI
; ----------------------------------------------------------------------------
DBG_RESUME_CONTEXT:
                        STZ         BRK_FLAG ; CONTEXT CONSUMED BY RESUME/NEXT
                        LDA         #SYSF_GO_FLAG_M
                        TSB         SYS_FLAGS
                        LDA         DBG_SP
                        TAX
                        TXS
                        LDA         DBG_PC_HI
                        PHA
                        LDA         DBG_PC_LO
                        PHA
                        LDA         DBG_P
                        PHA
                        LDA         DBG_A
                        LDX         DBG_X
                        LDY         DBG_Y
                        RTI

; ----------------------------------------------------------------------------
; TABLE: CMD_TABLE
; FORMAT: DB <COMMAND_CHAR>, DW <HANDLER_ADDR>
; TERMINATOR: DB $00
; ----------------------------------------------------------------------------
CMD_TABLE:
                        DB          'Z'
                        DW          CMD_CONFIRM_CLEAR
                        DB          'C'
                        DW          CMD_DO_COPY
                        DB          'W'
                        DW          CMD_DO_WARM
                        DB          'M'
                        DW          CMD_DO_MODIFY
                        DB          'D'
                        DW          CMD_DO_DUMP
                        DB          'U'
                        DW          CMD_DO_UNASM
                        DB          'A'
                        DW          CMD_DO_ASM
                        DB          'X'
                        DW          CMD_DO_GO
                        DB          'G'
                        DW          CMD_DO_GAME
                        DB          'I'
                        DW          CMD_DO_INFO
                        DB          'R'
                        DW          CMD_DO_RESUME
                        DB          'N'
                        DW          CMD_DO_NEXT
                        DB          'F'
                        DW          CMD_DO_FILL
                        DB          'L'
                        DW          CMD_DO_LOAD
                        DB          'S'
                        DW          CMD_DO_SEARCH
                        DB          'Q'
                        DW          CMD_DO_QUIT_MONITOR
                        DB          'V'
                        DW          CMD_DO_VECTORS
                        DB          'H'
                        DW          CMD_DO_HELP_FULL
                        DB          '?'
                        DW          CMD_DO_HELP_SHORT
                        DB          '-'
                        DW          CMD_DO_AUTOHELP_OFF_ALIAS
                        DB          '+'
                        DW          CMD_DO_AUTOHELP_ON_ALIAS
                        DB          $00

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_CONFIRM_CLEAR
; DESCRIPTION: COMMAND-MODE CLEAR CONFIRMATION (Y/N)
; ----------------------------------------------------------------------------
CMD_CONFIRM_CLEAR:
                        PRT_CSTRING MSG_CLR_CONFIRM
?CCC_WAIT_KEY:
                        JSR         READ_BYTE_ECHO_UPPER
                        JSR         KEY_IS_Y
                        BEQ         ?CCC_DO_CLEAR
                        JSR         KEY_IS_N
                        BEQ         ?CCC_CANCEL
                        BRA         ?CCC_WAIT_KEY
?CCC_DO_CLEAR:
                        JSR         MEMCLR_CMD
                        RTS
?CCC_CANCEL:
                        PRT_CSTRING MSG_RAM_NOT_CLEARED
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_QUIT
; DESCRIPTION: PRINTS HALT MESSAGE AND WAITS UNTIL NMI SIGNALS RESUME
; NOTES: IRQ IS MASKED HERE; NMI SETS SYSF_NMI_FLAG_M TO RELEASE THE WAIT LOOP.
; ----------------------------------------------------------------------------
CMD_DO_QUIT:
                        PRT_CSTRING MSG_Q_WAIT
                        LDA         #SYSF_NMI_FLAG_M
                        TRB         SYS_FLAGS
                        SEI
?CQ_WAIT:
                        WAI
                        LDA         #SYSF_NMI_FLAG_M
                        BIT         SYS_FLAGS
                        BEQ         ?CQ_WAIT
                        LDA         #SYSF_NMI_FLAG_M
                        TRB         SYS_FLAGS
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PRINT_HELP_FULL
; DESCRIPTION: PRINTS DETAILED MONITOR HELP
; ----------------------------------------------------------------------------
CMD_PRINT_HELP_FULL:
                        PRT_CSTRING MSG_HELP_FULL_0
                        PRT_CSTRING MSG_HELP_FULL_1
                        PRT_CSTRING MSG_HELP_FULL_2
                        PRT_CSTRING MSG_HELP_FULL_3
                        PRT_CSTRING MSG_HELP_FULL_4
                        PRT_CSTRING MSG_HELP_FULL_5
                        PRT_CSTRING MSG_HELP_FULL_6
                        PRT_CSTRING MSG_HELP_FULL_7
                        PRT_CSTRING MSG_HELP_FULL_8
                        PRT_CSTRING MSG_HELP_FULL_9
                        PRT_CSTRING MSG_HELP_FULL_10
                        PRT_CSTRING MSG_HELP_FULL_11
                        PRT_CSTRING MSG_HELP_FULL_28
                        PRT_CSTRING MSG_HELP_FULL_37
                        PRT_CSTRING MSG_HELP_FULL_12
                        PRT_CSTRING MSG_HELP_FULL_13
                        PRT_CSTRING MSG_HELP_FULL_14
                        PRT_CSTRING MSG_HELP_FULL_15
                        PRT_CSTRING MSG_HELP_FULL_16
                        PRT_CSTRING MSG_HELP_FULL_17
                        PRT_CSTRING MSG_HELP_FULL_18
                        PRT_CSTRING MSG_HELP_FULL_26
                        PRT_CSTRING MSG_HELP_FULL_27
                        PRT_CSTRING MSG_HELP_FULL_29
                        PRT_CSTRING MSG_HELP_FULL_21
                        PRT_CSTRING MSG_HELP_FULL_22
                        PRT_CSTRING MSG_HELP_FULL_23
                        PRT_CSTRING MSG_HELP_FULL_24
                        PRT_CSTRING MSG_HELP_FULL_25
                        PRT_CSTRING MSG_HELP_FULL_30
                        PRT_CSTRING MSG_HELP_FULL_31
                        PRT_CSTRING MSG_HELP_FULL_32
                        PRT_CSTRING MSG_HELP_FULL_33
                        PRT_CSTRING MSG_HELP_FULL_34
                        PRT_CSTRING MSG_HELP_FULL_35
                        PRT_CSTRING MSG_HELP_FULL_36
                        JSR         CMD_PRINT_HELP_FIXED_ADDRS
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PRINT_HELP_PROTECTION
; DESCRIPTION: PRINTS PROTECTION + FIXED-ADDRESS HELP SUBSET
; ----------------------------------------------------------------------------
CMD_PRINT_HELP_PROTECTION:
                        PRT_CSTRING MSG_HELP_PROT_HDR
                        PRT_CSTRING MSG_HELP_FULL_23
                        PRT_CSTRING MSG_HELP_FULL_24
                        PRT_CSTRING MSG_HELP_FULL_25
                        JSR         CMD_PRINT_HELP_FIXED_ADDRS
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PRINT_HELP_MEMORY
; DESCRIPTION: PRINTS MEMORY/LOAD TOOL HELP SUBSET
; ----------------------------------------------------------------------------
CMD_PRINT_HELP_MEMORY:
                        PRT_CSTRING MSG_HELP_MEM_HDR
                        PRT_CSTRING MSG_HELP_FULL_12
                        PRT_CSTRING MSG_HELP_FULL_13
                        PRT_CSTRING MSG_HELP_FULL_14
                        PRT_CSTRING MSG_HELP_FULL_15
                        PRT_CSTRING MSG_HELP_FULL_16
                        PRT_CSTRING MSG_HELP_FULL_17
                        PRT_CSTRING MSG_HELP_FULL_18
                        PRT_CSTRING MSG_HELP_FULL_26
                        PRT_CSTRING MSG_HELP_FULL_27
                        PRT_CSTRING MSG_HELP_FULL_21
                        PRT_CSTRING MSG_HELP_FULL_22
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PRINT_HELP_STEERING
; DESCRIPTION: PRINTS STEERING/TRANSITION HELP SUBSET
; ----------------------------------------------------------------------------
CMD_PRINT_HELP_STEERING:
                        PRT_CSTRING MSG_HELP_STEER_HDR
                        PRT_CSTRING MSG_HELP_FULL_30
                        PRT_CSTRING MSG_HELP_FULL_31
                        PRT_CSTRING MSG_HELP_FULL_32
                        PRT_CSTRING MSG_HELP_FULL_33
                        PRT_CSTRING MSG_HELP_FULL_34
                        PRT_CSTRING MSG_HELP_FULL_35
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PRINT_HELP_FIXED_ADDRS
; DESCRIPTION: PRINTS FIXED HW/ZP VECTOR + PINNED BYTE HELP LINES USING
;              CURRENT SYMBOL ADDRESSES.
; ----------------------------------------------------------------------------
CMD_PRINT_HELP_FIXED_ADDRS:
                        PRT_CSTRING MSG_HELP_ADDRS_HW
                        LDA         #>HW_VEC_NMI_ADDR
                        JSR         PRT_HEX
                        LDA         #<HW_VEC_NMI_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_ADDR_R
                        LDA         #>HW_VEC_RST_ADDR
                        JSR         PRT_HEX
                        LDA         #<HW_VEC_RST_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_ADDR_I
                        LDA         #>HW_VEC_IRQ_ADDR
                        JSR         PRT_HEX
                        LDA         #<HW_VEC_IRQ_ADDR
                        JSR         PRT_HEX

                        PRT_CSTRING MSG_HELP_ADDRS_ZP
                        LDA         #>ZP_NMI_HOOK_ADDR
                        JSR         PRT_HEX
                        LDA         #<ZP_NMI_HOOK_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_ADDR_R
                        LDA         #>ZP_RST_HOOK_ADDR
                        JSR         PRT_HEX
                        LDA         #<ZP_RST_HOOK_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_ADDR_I
                        LDA         #>ZP_IRQ_HOOK_ADDR
                        JSR         PRT_HEX
                        LDA         #<ZP_IRQ_HOOK_ADDR
                        JSR         PRT_HEX

                        PRT_CSTRING MSG_HELP_GAME_ADDR
                        LDA         #<ZP_GAME_ASK_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_GAME_SET
                        LDA         #<ZP_GAME_ASK_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_GAME_CLR
                        LDA         #<ZP_GAME_ASK_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_GAME_END

                        PRT_CSTRING MSG_HELP_TERM_ADDR
                        LDA         #<ZP_TERM_COLS_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_TERM_SET
                        LDA         #<ZP_TERM_COLS_ADDR
                        JSR         PRT_HEX
                        PRT_CSTRING MSG_HELP_TERM_END
                        PRT_CSTRING MSG_HELP_USER_ZP
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_DUMP
; DESCRIPTION: HANDLES DUMP COMMAND FORMS:
;              D <START> <END>  (INCLUSIVE RANGE)
;              D <START>        (256 BYTES)
;              D                (REPEAT LAST SPAN FROM NEXT ADDRESS)
; NOTES:
;   - User END is inclusive; internal END is converted to exclusive.
;   - If END+1 wraps to $0000, it is treated as exclusive $10000.
; ----------------------------------------------------------------------------
CMD_DO_DUMP:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?DD_HAS_START

        ; --- D (repeat) ---
                        LDA         DUMP_VALID
                        BNE         ?DD_REPEAT_OK
                        PRT_CSTRING MSG_D_USAGE
                        RTS
?DD_REPEAT_OK:
                        LDA         DUMP_NEXT
                        STA         PTR_DUMP_CUR
                        LDA         DUMP_NEXT+1
                        STA         PTR_DUMP_CUR+1
                        CLC
                        LDA         PTR_DUMP_CUR
                        ADC         DUMP_SPAN
                        STA         PTR_TEMP
                        LDA         PTR_DUMP_CUR+1
                        ADC         DUMP_SPAN+1
                        STA         PTR_TEMP+1
                        BCS         ?DD_REPEAT_WRAP
                        JMP         DD_RUN
?DD_REPEAT_WRAP:
                        STZ         PTR_TEMP ; WRAP => TREAT AS END-OF-MEMORY
                        STZ         PTR_TEMP+1
                        JMP         DD_RUN

?DD_HAS_START:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?DD_START_OK
                        PRT_CSTRING MSG_D_USAGE
                        RTS
?DD_START_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_DUMP_CUR
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_DUMP_CUR+1

                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?DD_HAS_END

        ; --- D <START> => 256 BYTES ---
                        LDA         PTR_DUMP_CUR
                        STA         PTR_TEMP
                        LDA         PTR_DUMP_CUR+1
                        CLC
                        ADC         #$01
                        STA         PTR_TEMP+1
                        BCC         ?DD_SPAN_256
                        STZ         PTR_TEMP ; WRAP => END-OF-MEMORY
                        STZ         PTR_TEMP+1
?DD_SPAN_256:
                        STZ         DUMP_SPAN
                        LDA         #$01
                        STA         DUMP_SPAN+1
                        JMP         DD_RUN

?DD_HAS_END:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?DD_END_OK
                        PRT_CSTRING MSG_D_USAGE
                        RTS
?DD_END_OK:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?DD_END_PARSE_DONE
                        PRT_CSTRING MSG_D_USAGE
                        RTS

?DD_END_PARSE_DONE:
        ; END MUST BE >= START
                        LDA         CMD_PARSE_VAL+1
                        CMP         PTR_DUMP_CUR+1
                        BCC         ?DD_RANGE_ERR
                        BNE         ?DD_END_GE_START
                        LDA         CMD_PARSE_VAL
                        CMP         PTR_DUMP_CUR
                        BCC         ?DD_RANGE_ERR
?DD_END_GE_START:
        ; CONVERT INCLUSIVE END TO EXCLUSIVE END
                        LDA         CMD_PARSE_VAL
                        CLC
                        ADC         #$01
                        STA         PTR_TEMP
                        LDA         CMD_PARSE_VAL+1
                        ADC         #$00
                        STA         PTR_TEMP+1

        ; SPAN = END_EXCLUSIVE - START
                        LDA         PTR_TEMP
                        SEC
                        SBC         PTR_DUMP_CUR
                        STA         DUMP_SPAN
                        LDA         PTR_TEMP+1
                        SBC         PTR_DUMP_CUR+1
                        STA         DUMP_SPAN+1
                        JMP         DD_RUN

?DD_RANGE_ERR:
                        PRT_CSTRING MSG_D_RANGE_ERR
                        RTS

DD_RUN:
                        JSR         MEM_DUMP
                        LDA         PTR_TEMP ; REMEMBER NEXT START
                        STA         DUMP_NEXT
                        LDA         PTR_TEMP+1
                        STA         DUMP_NEXT+1
                        LDA         #$01
                        STA         DUMP_VALID
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_UNASM
; DESCRIPTION: DISASSEMBLES A 65C02 MEMORY RANGE
; USAGE: U [<START> <END>]
; NOTES:
;   - END IS INCLUSIVE.
;   - OUTPUT FORMAT: "ADDR: MNM OPERAND"
; ----------------------------------------------------------------------------
CMD_DO_UNASM:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?UD_HAS_START

        ; --- U (repeat from saved next/span) ---
                        LDA         UNASM_VALID
                        BNE         ?UD_REPEAT_OK
                        PRT_CSTRING MSG_U_USAGE
                        RTS
?UD_REPEAT_OK:
                        LDA         UNASM_NEXT
                        STA         PTR_DUMP_CUR
                        LDA         UNASM_NEXT+1
                        STA         PTR_DUMP_CUR+1
        ; Repeat mode steps exactly one disassembled instruction.
                        LDA         PTR_DUMP_CUR
                        STA         PTR_TEMP
                        LDA         PTR_DUMP_CUR+1
                        STA         PTR_TEMP+1
                        JMP         UD_RUN

?UD_HAS_START:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?UD_START_OK
                        PRT_CSTRING MSG_U_USAGE
                        RTS
?UD_START_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_DUMP_CUR
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_DUMP_CUR+1

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?UD_END_OK
                        PRT_CSTRING MSG_U_USAGE
                        RTS
?UD_END_OK:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?UD_END_PARSE_DONE
                        PRT_CSTRING MSG_U_USAGE
                        RTS

?UD_END_PARSE_DONE:
        ; END MUST BE >= START
                        LDA         CMD_PARSE_VAL+1
                        CMP         PTR_DUMP_CUR+1
                        BCC         ?UD_RANGE_ERR
                        BNE         ?UD_END_GE_START
                        LDA         CMD_PARSE_VAL
                        CMP         PTR_DUMP_CUR
                        BCC         ?UD_RANGE_ERR
?UD_END_GE_START:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_TEMP
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_TEMP+1
        ; NEXT START = END + 1 (KEEP IN PTR_LEG UNTIL UD_RUN COMMITS)
                        CLC
                        LDA         PTR_TEMP
                        ADC         #$01
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        ADC         #$00
                        STA         PTR_LEG+1
        ; SPAN = END_EXCLUSIVE - START
                        LDA         PTR_LEG
                        SEC
                        SBC         PTR_DUMP_CUR
                        STA         UNASM_SPAN
                        LDA         PTR_LEG+1
                        SBC         PTR_DUMP_CUR+1
                        STA         UNASM_SPAN+1
                        JMP         UD_RUN
?UD_RANGE_ERR:
                        PRT_CSTRING MSG_U_RANGE_ERR
                        RTS

UD_RUN:
                        JSR         MEM_DISASM_65C02
        ; Use actual disassembler post-run cursor so repeats continue
        ; at the next instruction boundary (not just END+1).
                        LDA         PTR_DUMP_CUR
                        STA         UNASM_NEXT
                        LDA         PTR_DUMP_CUR+1
                        STA         UNASM_NEXT+1
                        LDA         #$01
                        STA         UNASM_VALID
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_ASM
; DESCRIPTION: TINY 65C02 ASSEMBLER
; USAGE: A <START> [MNEMONIC OPERANDS]
; INTERACTIVE: PROMPTS "A <ADDR>:", ACCEPTS "."
; INTERNAL NOTE (NOT FOR PUBLISH):
; IF/THEN/ELSE IN COLUMN 1 ARE RESERVED LABELS FOR FUTURE ASM DIALECT WORK.
; ----------------------------------------------------------------------------
CMD_DO_ASM:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?CA_START_OK
                        PRT_CSTRING MSG_A_USAGE
                        RTS
?CA_START_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_TEMP
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_TEMP+1
                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCS         ?CA_DONE

                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ASM_INTERACTIVE

        ; --- Assemble inline tail once, then continue interactively ---
                        LDY         #$00
?CA_SHIFT_LOOP:
                        LDA         CMD_LINE,X
                        STA         CMD_LINE,Y
                        BEQ         ?CA_SHIFT_DONE
                        INX
                        INY
                        BRA         ?CA_SHIFT_LOOP
?CA_SHIFT_DONE:
                        LDX         #$00
                        JSR         ASM_ASSEMBLE_LINE
                        BCS         ?CA_DONE
                        JMP         ASM_INTERACTIVE
?CA_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_INTERACTIVE
; DESCRIPTION: PROMPTS AND ASSEMBLES ONE SOURCE LINE AT A TIME
; ----------------------------------------------------------------------------
ASM_INTERACTIVE:
?AI_LOOP:
                        JSR         ASM_PRINT_PROMPT
                        JSR         ASM_READ_LINE
                        LDX         #$00
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?AI_LOOP ; EMPTY LINE => REPROMPT
                        CMP         #'.'
                        BNE         ?AI_ASM
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?AI_DONE
                        LDX         #$00 ; "." ONLY IF NOTHING FOLLOWS
?AI_ASM:
                        JSR         ASM_ASSEMBLE_LINE
                        BRA         ?AI_LOOP
?AI_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_PRINT_PROMPT
; DESCRIPTION: PRINTS "A <ADDR>: "
; ----------------------------------------------------------------------------
ASM_PRINT_PROMPT:
                        JSR         PRT_CRLF
                        LDA         #'A'
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE
                        LDA         PTR_TEMP+1
                        JSR         PRT_HEX
                        LDA         PTR_TEMP
                        JSR         PRT_HEX
                        LDA         #':'
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_READ_LINE
; DESCRIPTION: READS ONE INTERACTIVE LINE INTO CMD_LINE (UPPERCASE, BS EDIT)
; OUTPUT: CMD_LINE = NUL-TERMINATED
; ----------------------------------------------------------------------------
ASM_READ_LINE:
                        STZ         CMD_LEN
?ARL_LOOP:
                        JSR         READ_BYTE
                        LDX         MOD_COUNT ; CRLF COALESCER (REUSE SCRATCH)
                        BEQ         ?ARL_NO_PENDING
                        STZ         MOD_COUNT
                        CMP         #$0A
                        BEQ         ?ARL_LOOP
?ARL_NO_PENDING:
                        CMP         #$0D
                        BEQ         ?ARL_TERM_CR
                        CMP         #$0A
                        BEQ         ?ARL_TERM_LF
                        CMP         #$08
                        BEQ         ?ARL_BS
                        CMP         #$7F
                        BEQ         ?ARL_BS
                        JSR         UTIL_TO_UPPER
                        CMP         #$20
                        BCC         ?ARL_LOOP
                        CMP         #$7F
                        BCS         ?ARL_LOOP
                        LDX         CMD_LEN
                        CPX         #CMD_MAX_LEN
                        BCS         ?ARL_LOOP
                        STA         CMD_LINE,X
                        INC         CMD_LEN
                        JSR         WRITE_BYTE
                        LDX         CMD_LEN
                        CPX         #$01
                        BNE         ?ARL_LOOP
                        LDA         CMD_LINE
                        CMP         #'.'
                        BNE         ?ARL_LOOP
                        BRA         ?ARL_TERM_LF ; SINGLE '.' ENDS IMMEDIATELY

?ARL_BS:
                        LDA         CMD_LEN
                        BEQ         ?ARL_LOOP
                        DEC         CMD_LEN
                        LDA         #$08
                        JSR         WRITE_BYTE
                        LDA         #' '
                        JSR         WRITE_BYTE
                        LDA         #$08
                        JSR         WRITE_BYTE
                        BRA         ?ARL_LOOP

?ARL_TERM_CR:
                        LDA         #$01
                        STA         MOD_COUNT
                        BRA         ?ARL_TERM
?ARL_TERM_LF:
                        STZ         MOD_COUNT
?ARL_TERM:
                        LDX         CMD_LEN
                        LDA         #$00
                        STA         CMD_LINE,X
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_ASSEMBLE_LINE
; DESCRIPTION: ASSEMBLES ONE LINE FROM CMD_LINE[X] AT PTR_TEMP
; OUTPUT: C=0 SUCCESS, C=1 ERROR (MESSAGE PRINTED)
; ----------------------------------------------------------------------------
ASM_ASSEMBLE_LINE:
                        JSR         CMD_SKIP_SPACES
                        LDA         #$FF
                        STA         ASM_MODE_ALT
                        STA         ASM_BIT_ID

        ; --- Parse 3-char mnemonic ---
                        LDA         CMD_LINE,X
                        BEQ         ?AAL_ERR
                        CMP         #'A'
                        BCC         ?AAL_ERR
                        CMP         #'Z'+1
                        BCS         ?AAL_ERR
                        STA         ASM_MN0
                        INX
                        LDA         CMD_LINE,X
                        CMP         #'A'
                        BCC         ?AAL_ERR
                        CMP         #'Z'+1
                        BCS         ?AAL_ERR
                        STA         ASM_MN1
                        INX
                        LDA         CMD_LINE,X
                        CMP         #'A'
                        BCC         ?AAL_ERR
                        CMP         #'Z'+1
                        BCS         ?AAL_ERR
                        STA         ASM_MN2
                        INX

        ; --- Optional bit digit for RMB/SMB/BBR/BBS ---
                        LDA         CMD_LINE,X
                        CMP         #'0'
                        BCC         ?AAL_BIT_DONE
                        CMP         #'8'
                        BCS         ?AAL_BIT_DONE
                        JSR         ASM_IS_BIT_MNEM
                        BCC         ?AAL_BIT_DONE
                        LDA         CMD_LINE,X
                        SEC
                        SBC         #'0'
                        STA         ASM_BIT_ID
                        INX
?AAL_BIT_DONE:
                        JSR         ASM_PARSE_OPERANDS
                        BCS         ?AAL_ERR
                        JSR         ASM_FIND_OPCODE
                        BCS         ?AAL_ERR
                        JSR         ASM_STORE_INSN
                        RTS

?AAL_ERR:
                        PRT_CSTRING MSG_A_USAGE
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_PARSE_OPERANDS
; DESCRIPTION: DETERMINES MODE + OPERAND BYTES FROM CMD_LINE[X]
; OUTPUT: DIS_MODE (PRIMARY), ASM_MODE_ALT (OPTIONAL), ASM_OP0/ASM_OP1
;         C=0 OK, C=1 ERROR
; ----------------------------------------------------------------------------
ASM_PARSE_OPERANDS:
                        STZ         ASM_OP0
                        STZ         ASM_OP1
                        LDA         #$FF
                        STA         ASM_MODE_ALT
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?APO_HAS
                        JSR         ASM_IS_BRK
                        BCC         ?APO_NOOP_DEFAULT
                        STZ         ASM_OP0 ; DEFAULT BRK SIGNATURE BYTE = $00
                        LDA         #DISM_IMM
                        STA         DIS_MODE
                        CLC
                        RTS
?APO_NOOP_DEFAULT:
                        LDA         #DISM_IMP
                        STA         DIS_MODE
                        LDA         #DISM_ACC ; FALLBACK FOR ASL/LSR/ROL/ROR/INC/DEC A
                        STA         ASM_MODE_ALT
                        CLC
                        RTS
?APO_HAS:
                        CMP         #'#'
                        BNE         ?APO_NOT_IMM
                        INX
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?APO_IMM_OK
                        JMP         ?APO_ERR
?APO_IMM_OK:
                        LDA         CMD_PARSE_VAL+1
                        BEQ         ?APO_IMM_BYTE_OK
                        JMP         ?APO_ERR
?APO_IMM_BYTE_OK:
                        LDA         CMD_PARSE_VAL
                        STA         ASM_OP0
                        LDA         #DISM_IMM
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_NOT_IMM:
                        CMP         #'('
                        BNE         ?APO_NOT_PAREN
                        JMP         ASM_PARSE_PAREN
?APO_NOT_PAREN:
        ; Treat explicit "A" as accumulator operand for accumulator-capable
        ; mnemonics (ASL/LSR/ROL/ROR/INC/DEC), not as hex $0A.
                        LDA         CMD_LINE,X
                        CMP         #'A'
                        BNE         ?APO_PARSE_TOK1
                        TXA
                        PHA
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?APO_NOT_ACC_A
                        JSR         ASM_IS_ACC_MNEM
                        BCC         ?APO_NOT_ACC_A
                        PLA             ; DISCARD SAVED X
                        LDA         #DISM_ACC
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_NOT_ACC_A:
                        PLA
                        TAX
?APO_PARSE_TOK1:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?APO_TOK1_OK
                        JMP         ?APO_ERR
?APO_TOK1_OK:
                        LDA         CMD_PARSE_VAL
                        STA         ASM_OP0
                        LDA         CMD_PARSE_VAL+1
                        STA         ASM_OP1
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?APO_PLAIN_DONE
                        CMP         #','
                        BEQ         ?APO_COMMA_OK
                        JMP         ?APO_ERR
?APO_COMMA_OK:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'X'
                        BEQ         ?APO_COMMA_X
                        CMP         #'Y'
                        BEQ         ?APO_COMMA_Y
                        JSR         ASM_IS_BB_BRANCH
                        BCS         ?APO_IS_BB
                        JMP         ?APO_ERR
?APO_IS_BB:
                        LDA         ASM_OP1 ; ZP OPERAND REQUIRED FOR BBR/BBS
                        BEQ         ?APO_BB_ZP_OK
                        JMP         ?APO_ERR
?APO_BB_ZP_OK:
                        JSR         CMD_PARSE_ADDR16_TOKEN ; SECOND OPERAND (TARGET)
                        CMP         #$00
                        BEQ         ?APO_BB_TARGET_OK
                        JMP         ?APO_ERR
?APO_BB_TARGET_OK:
                        LDA         PTR_TEMP
                        CLC
                        ADC         #$03
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        ADC         #$00
                        STA         PTR_LEG+1
                        JSR         ASM_MAKE_REL_FROM_CMD_PARSE
                        BCC         ?APO_BB_REL_OK
                        JMP         ?APO_RANGE_ERR
?APO_BB_REL_OK:
                        STA         ASM_OP1
                        LDA         #DISM_ZPREL
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_COMMA_X:
                        INX
                        LDA         ASM_OP1
                        BEQ         ?APO_ZPX
                        LDA         #DISM_ABSX
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_ZPX:
                        LDA         #DISM_ZPX
                        STA         DIS_MODE
                        LDA         #DISM_ABSX
                        STA         ASM_MODE_ALT
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_COMMA_Y:
                        INX
                        LDA         ASM_OP1
                        BEQ         ?APO_ZPY
                        LDA         #DISM_ABSY
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_ZPY:
                        LDA         #DISM_ZPY
                        STA         DIS_MODE
                        LDA         #DISM_ABSY
                        STA         ASM_MODE_ALT
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_PLAIN_DONE:
                        JSR         ASM_IS_BRK
                        BCC         ?APO_NOT_BRK_PLAIN
                        LDA         ASM_OP1 ; BRK SIGNATURE MUST BE 8-BIT
                        BNE         ?APO_ERR
                        LDA         #DISM_IMM ; ACCEPT "BRK 00" AS "BRK #$00"
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APO_NOT_BRK_PLAIN:
                        JSR         ASM_IS_REL_BRANCH
                        BCC         ?APO_NOT_REL
                        LDA         PTR_TEMP
                        CLC
                        ADC         #$02
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        ADC         #$00
                        STA         PTR_LEG+1
                        LDA         ASM_OP0
                        STA         CMD_PARSE_VAL
                        LDA         ASM_OP1
                        STA         CMD_PARSE_VAL+1
                        JSR         ASM_MAKE_REL_FROM_CMD_PARSE
                        BCC         ?APO_REL_OK
                        JMP         ?APO_RANGE_ERR
?APO_REL_OK:
                        STA         ASM_OP0
                        LDA         #DISM_REL
                        STA         DIS_MODE
                        CLC
                        RTS
?APO_NOT_REL:
                        LDA         ASM_OP1
                        BEQ         ?APO_ZP
                        LDA         #DISM_ABS
                        STA         DIS_MODE
                        CLC
                        RTS
?APO_ZP:
                        LDA         #DISM_ZP
                        STA         DIS_MODE
                        LDA         #DISM_ABS
                        STA         ASM_MODE_ALT
                        CLC
                        RTS
?APO_RANGE_ERR:
                        PRT_CSTRING MSG_A_RANGE_ERR
                        SEC
                        RTS
?APO_ERR:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_PARSE_PAREN
; DESCRIPTION: PARSES PARENTHESIZED OPERAND FOR A
; INPUT: X POINTS TO '('
; OUTPUT: MODE/OPERANDS SET
; ----------------------------------------------------------------------------
ASM_PARSE_PAREN:
                        INX
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BNE         ?APP_ERR
                        LDA         CMD_PARSE_VAL
                        STA         ASM_OP0
                        LDA         CMD_PARSE_VAL+1
                        STA         ASM_OP1
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #','
                        BEQ         ?APP_COMMA
                        CMP         #')'
                        BNE         ?APP_ERR
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #','
                        BEQ         ?APP_AFTER_CLOSE_COMMA
                        LDA         ASM_OP1
                        BEQ         ?APP_ZPIND
                        LDA         #DISM_IND
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APP_ZPIND:
                        LDA         #DISM_ZPIND
                        STA         DIS_MODE
                        LDA         #DISM_IND
                        STA         ASM_MODE_ALT
                        JMP         ASM_PARSE_REQUIRE_EOS
?APP_AFTER_CLOSE_COMMA:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'Y'
                        BNE         ?APP_ERR
                        INX
                        LDA         ASM_OP1
                        BNE         ?APP_ERR
                        LDA         #DISM_INDY
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APP_COMMA:
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'X'
                        BNE         ?APP_ERR
                        INX
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #')'
                        BNE         ?APP_ERR
                        INX
                        LDA         ASM_OP1
                        BEQ         ?APP_INDX
                        LDA         #DISM_ABSINDX
                        STA         DIS_MODE
                        JMP         ASM_PARSE_REQUIRE_EOS
?APP_INDX:
                        LDA         #DISM_INDX
                        STA         DIS_MODE
                        LDA         #DISM_ABSINDX
                        STA         ASM_MODE_ALT
                        JMP         ASM_PARSE_REQUIRE_EOS
?APP_ERR:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_PARSE_REQUIRE_EOS
; DESCRIPTION: REQUIRES ONLY OPTIONAL SPACES UNTIL END OF LINE
; ----------------------------------------------------------------------------
ASM_PARSE_REQUIRE_EOS:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?APRE_OK
                        SEC
                        RTS
?APRE_OK:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_MAKE_REL_FROM_CMD_PARSE
; DESCRIPTION: CALCULATES SIGNED 8-BIT OFFSET = TARGET - BASE
; INPUT: CMD_PARSE_VAL = TARGET, PTR_LEG = BASE
; OUTPUT: A = OFFSET BYTE, C=0 IF IN RANGE, C=1 IF OUT OF RANGE
; CLOBBERS: CMD_PARSE_VAL (USED AS 16-BIT DIFF SCRATCH)
; ----------------------------------------------------------------------------
ASM_MAKE_REL_FROM_CMD_PARSE:
                        LDA         CMD_PARSE_VAL
                        SEC
                        SBC         PTR_LEG
                        STA         CMD_PARSE_VAL
                        LDA         CMD_PARSE_VAL+1
                        SBC         PTR_LEG+1
                        STA         CMD_PARSE_VAL+1
                        LDA         CMD_PARSE_VAL+1
                        BEQ         ?AMR_POS
                        CMP         #$FF
                        BNE         ?AMR_BAD
                        LDA         CMD_PARSE_VAL
                        BMI         ?AMR_OK
                        BRA         ?AMR_BAD
?AMR_POS:
                        LDA         CMD_PARSE_VAL
                        BMI         ?AMR_BAD
?AMR_OK:
                        LDA         CMD_PARSE_VAL
                        CLC
                        RTS
?AMR_BAD:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_FIND_OPCODE
; DESCRIPTION: FINDS OPCODE MATCHING MNEMONIC + MODE (WITH OPTIONAL ALT MODE)
; OUTPUT: DIS_OPCODE, DIS_LEN, C=0 FOUND / C=1 NOT FOUND
; ----------------------------------------------------------------------------
ASM_FIND_OPCODE:
                        LDA         DIS_MODE
                        JSR         ASM_FIND_WITH_MODE
                        BCC         ?AFO_DONE
                        LDA         ASM_MODE_ALT
                        CMP         #$FF
                        BEQ         ?AFO_FAIL
                        JSR         ASM_FIND_WITH_MODE
                        BCC         ?AFO_DONE
?AFO_FAIL:
                        SEC
                        RTS
?AFO_DONE:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_FIND_WITH_MODE
; INPUT: A = MODE
; OUTPUT: DIS_OPCODE, DIS_LEN, C=0 FOUND / C=1 NOT FOUND
; ----------------------------------------------------------------------------
ASM_FIND_WITH_MODE:
                        STA         DIS_MODE
                        LDX         #$00
?AFM_LOOP:
                        LDA         U_OP_MODE_TAB,X
                        CMP         DIS_MODE
                        BNE         ?AFM_NEXT

                        STX         DIS_OPCODE
                        TXA
                        ASL         A
                        STA         PTR_LEG
                        LDA         #$00
                        ROL         A
                        STA         PTR_LEG+1
                        CLC
                        LDA         PTR_LEG
                        ADC         DIS_OPCODE
                        STA         PTR_LEG
                        LDA         PTR_LEG+1
                        ADC         #$00
                        STA         PTR_LEG+1
                        CLC
                        LDA         PTR_LEG
                        ADC         #<U_OP_MNEM_TAB
                        STA         PTR_LEG
                        LDA         PTR_LEG+1
                        ADC         #>U_OP_MNEM_TAB
                        STA         PTR_LEG+1

                        LDY         #$00
                        LDA         (PTR_LEG),Y
                        CMP         ASM_MN0
                        BNE         ?AFM_NEXT
                        INY
                        LDA         (PTR_LEG),Y
                        CMP         ASM_MN1
                        BNE         ?AFM_NEXT
                        INY
                        LDA         (PTR_LEG),Y
                        CMP         ASM_MN2
                        BNE         ?AFM_NEXT

                        LDA         ASM_BIT_ID
                        CMP         #$FF
                        BEQ         ?AFM_FOUND
                        LDA         DIS_OPCODE
                        AND         #$0F
                        CMP         #$07
                        BEQ         ?AFM_CHK_BIT
                        CMP         #$0F
                        BNE         ?AFM_NEXT
?AFM_CHK_BIT:
                        LDA         DIS_OPCODE
                        LSR         A
                        LSR         A
                        LSR         A
                        LSR         A
                        AND         #$07
                        CMP         ASM_BIT_ID
                        BNE         ?AFM_NEXT
?AFM_FOUND:
                        LDX         DIS_OPCODE
                        LDA         U_OP_LEN_TAB,X
                        STA         DIS_LEN
                        CLC
                        RTS
?AFM_NEXT:
                        INX
                        BNE         ?AFM_LOOP
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_STORE_INSN
; DESCRIPTION: WRITES OPCODE + OPERANDS AT PTR_TEMP AND ADVANCES PTR_TEMP
; OUTPUT: C=0 SUCCESS, C=1 FAIL
; ----------------------------------------------------------------------------
ASM_STORE_INSN:
                        LDA         PTR_TEMP
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        STA         PTR_LEG+1

        ; --- Pre-check protected address range for every byte ---
                        LDY         DIS_LEN
?ASI_CHK_LOOP:
                        LDA         PTR_LEG+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCS         ?ASI_FAIL
                        DEY
                        BEQ         ?ASI_DO_WRITE
                        INC         PTR_LEG
                        BNE         ?ASI_CHK_LOOP
                        INC         PTR_LEG+1
                        BRA         ?ASI_CHK_LOOP

?ASI_DO_WRITE:
                        LDY         #$00
                        LDA         DIS_OPCODE
                        STA         (PTR_TEMP),Y
                        INC         PTR_TEMP
                        BNE         ?ASI_W2
                        INC         PTR_TEMP+1
?ASI_W2:
                        LDA         DIS_LEN
                        CMP         #$02
                        BCC         ?ASI_OK
                        LDY         #$00
                        LDA         ASM_OP0
                        STA         (PTR_TEMP),Y
                        INC         PTR_TEMP
                        BNE         ?ASI_W3
                        INC         PTR_TEMP+1
?ASI_W3:
                        LDA         DIS_LEN
                        CMP         #$03
                        BCC         ?ASI_OK
                        LDY         #$00
                        LDA         ASM_OP1
                        STA         (PTR_TEMP),Y
                        INC         PTR_TEMP
                        BNE         ?ASI_OK
                        INC         PTR_TEMP+1
?ASI_OK:
                        CLC
                        RTS
?ASI_FAIL:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_IS_REL_BRANCH
; DESCRIPTION: C=1 IF MNEMONIC IS BRA/BCC/BCS/BEQ/BMI/BNE/BPL/BVC/BVS
; ----------------------------------------------------------------------------
ASM_IS_REL_BRANCH:
                        LDA         ASM_MN0
                        CMP         #'B'
                        BNE         ?AIR_NO
                        LDA         ASM_MN1
                        CMP         #'C'
                        BEQ         ?AIR_CC_CS
                        CMP         #'E'
                        BEQ         ?AIR_EQ
                        CMP         #'M'
                        BEQ         ?AIR_MI
                        CMP         #'N'
                        BEQ         ?AIR_NE
                        CMP         #'P'
                        BEQ         ?AIR_PL
                        CMP         #'R'
                        BEQ         ?AIR_RA
                        CMP         #'V'
                        BEQ         ?AIR_VC_VS
                        BRA         ?AIR_NO
?AIR_CC_CS:
                        LDA         ASM_MN2
                        CMP         #'C'
                        BEQ         ?AIR_YES
                        CMP         #'S'
                        BEQ         ?AIR_YES
                        BRA         ?AIR_NO
?AIR_EQ:
                        LDA         ASM_MN2
                        CMP         #'Q'
                        BEQ         ?AIR_YES
                        BRA         ?AIR_NO
?AIR_MI:
                        LDA         ASM_MN2
                        CMP         #'I'
                        BEQ         ?AIR_YES
                        BRA         ?AIR_NO
?AIR_NE:
                        LDA         ASM_MN2
                        CMP         #'E'
                        BEQ         ?AIR_YES
                        BRA         ?AIR_NO
?AIR_PL:
                        LDA         ASM_MN2
                        CMP         #'L'
                        BEQ         ?AIR_YES
                        BRA         ?AIR_NO
?AIR_RA:
                        LDA         ASM_MN2
                        CMP         #'A'
                        BEQ         ?AIR_YES
                        BRA         ?AIR_NO
?AIR_VC_VS:
                        LDA         ASM_MN2
                        CMP         #'C'
                        BEQ         ?AIR_YES
                        CMP         #'S'
                        BEQ         ?AIR_YES
                        BRA         ?AIR_NO
?AIR_YES:
                        SEC
                        RTS
?AIR_NO:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_IS_BRK
; DESCRIPTION: C=1 IF MNEMONIC IS BRK
; ----------------------------------------------------------------------------
ASM_IS_BRK:
                        LDA         ASM_MN0
                        CMP         #'B'
                        BNE         ?AIBRK_NO
                        LDA         ASM_MN1
                        CMP         #'R'
                        BNE         ?AIBRK_NO
                        LDA         ASM_MN2
                        CMP         #'K'
                        BEQ         ?AIBRK_YES
?AIBRK_NO:
                        CLC
                        RTS
?AIBRK_YES:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_IS_ACC_MNEM
; DESCRIPTION: C=1 IF MNEMONIC SUPPORTS EXPLICIT ACCUMULATOR OPERAND "A"
;              (ASL, LSR, ROL, ROR, INC, DEC)
; ----------------------------------------------------------------------------
ASM_IS_ACC_MNEM:
                        LDA         ASM_MN0
                        CMP         #'A'
                        BEQ         ?AIAM_ASL
                        CMP         #'L'
                        BEQ         ?AIAM_LSR
                        CMP         #'R'
                        BEQ         ?AIAM_ROX
                        CMP         #'I'
                        BEQ         ?AIAM_INC
                        CMP         #'D'
                        BEQ         ?AIAM_DEC
                        BRA         ?AIAM_NO
?AIAM_ASL:
                        LDA         ASM_MN1
                        CMP         #'S'
                        BNE         ?AIAM_NO
                        LDA         ASM_MN2
                        CMP         #'L'
                        BEQ         ?AIAM_YES
                        BRA         ?AIAM_NO
?AIAM_LSR:
                        LDA         ASM_MN1
                        CMP         #'S'
                        BNE         ?AIAM_NO
                        LDA         ASM_MN2
                        CMP         #'R'
                        BEQ         ?AIAM_YES
                        BRA         ?AIAM_NO
?AIAM_ROX:
                        LDA         ASM_MN1
                        CMP         #'O'
                        BNE         ?AIAM_NO
                        LDA         ASM_MN2
                        CMP         #'L'
                        BEQ         ?AIAM_YES
                        CMP         #'R'
                        BEQ         ?AIAM_YES
                        BRA         ?AIAM_NO
?AIAM_INC:
                        LDA         ASM_MN1
                        CMP         #'N'
                        BNE         ?AIAM_NO
                        LDA         ASM_MN2
                        CMP         #'C'
                        BEQ         ?AIAM_YES
                        BRA         ?AIAM_NO
?AIAM_DEC:
                        LDA         ASM_MN1
                        CMP         #'E'
                        BNE         ?AIAM_NO
                        LDA         ASM_MN2
                        CMP         #'C'
                        BEQ         ?AIAM_YES
                        BRA         ?AIAM_NO
?AIAM_NO:
                        CLC
                        RTS
?AIAM_YES:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_IS_BIT_MNEM
; DESCRIPTION: C=1 IF MNEMONIC BASE IS RMB/SMB/BBR/BBS
; ----------------------------------------------------------------------------
ASM_IS_BIT_MNEM:
                        LDA         ASM_MN0
                        CMP         #'R'
                        BEQ         ?AIBM_RMB
                        CMP         #'S'
                        BEQ         ?AIBM_SMB
                        CMP         #'B'
                        BEQ         ?AIBM_BBRBBS
                        CLC
                        RTS
?AIBM_RMB:
                        LDA         ASM_MN1
                        CMP         #'M'
                        BNE         ?AIBM_NO
                        LDA         ASM_MN2
                        CMP         #'B'
                        BEQ         ?AIBM_YES
                        BRA         ?AIBM_NO
?AIBM_SMB:
                        LDA         ASM_MN1
                        CMP         #'M'
                        BNE         ?AIBM_NO
                        LDA         ASM_MN2
                        CMP         #'B'
                        BEQ         ?AIBM_YES
                        BRA         ?AIBM_NO
?AIBM_BBRBBS:
                        LDA         ASM_MN1
                        CMP         #'B'
                        BNE         ?AIBM_NO
                        LDA         ASM_MN2
                        CMP         #'R'
                        BEQ         ?AIBM_YES
                        CMP         #'S'
                        BEQ         ?AIBM_YES
?AIBM_NO:
                        CLC
                        RTS
?AIBM_YES:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: ASM_IS_BB_BRANCH
; DESCRIPTION: C=1 IF MNEMONIC BASE IS BBR OR BBS
; ----------------------------------------------------------------------------
ASM_IS_BB_BRANCH:
                        LDA         ASM_MN0
                        CMP         #'B'
                        BNE         ?AIBB_NO
                        LDA         ASM_MN1
                        CMP         #'B'
                        BNE         ?AIBB_NO
                        LDA         ASM_MN2
                        CMP         #'R'
                        BEQ         ?AIBB_YES
                        CMP         #'S'
                        BEQ         ?AIBB_YES
?AIBB_NO:
                        CLC
                        RTS
?AIBB_YES:
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_FILL
; DESCRIPTION: FILLS RANGE WITH 1..16 BYTE PATTERN
; USAGE: F <START> <END> <B0..B15>
; NOTES:
;   - END IS INCLUSIVE.
;   - PATTERN REPEATS UNTIL END IS REACHED.
;   - NO INTERACTIVE MODE FOR F.
; ----------------------------------------------------------------------------
CMD_DO_FILL:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?FD_START_OK
                        PRT_CSTRING MSG_F_USAGE
                        RTS
?FD_START_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_DUMP_CUR
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_DUMP_CUR+1

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?FD_END_OK
                        PRT_CSTRING MSG_F_USAGE
                        RTS
?FD_END_OK:
        ; END MUST BE >= START
                        LDA         CMD_PARSE_VAL+1
                        CMP         PTR_DUMP_CUR+1
                        BCC         ?FD_RANGE_ERR
                        BNE         ?FD_END_GE_START
                        LDA         CMD_PARSE_VAL
                        CMP         PTR_DUMP_CUR
                        BCC         ?FD_RANGE_ERR
                        BRA         ?FD_END_GE_START
?FD_RANGE_ERR:
                        JMP         ?FD_USAGE
?FD_END_GE_START:
        ; CONVERT INCLUSIVE END TO EXCLUSIVE END
                        LDA         CMD_PARSE_VAL
                        CLC
                        ADC         #$01
                        STA         PTR_DUMP_END
                        LDA         CMD_PARSE_VAL+1
                        ADC         #$00
                        STA         PTR_DUMP_END+1

                        STZ         F_COUNT
?FD_PARSE_LOOP:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?FD_PARSE_DONE

                        LDA         F_COUNT
                        CMP         #F_MAX_BYTES
                        BCC         ?FD_PARSE_TOKEN
                        PRT_CSTRING MSG_F_USAGE
                        RTS

?FD_PARSE_TOKEN:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BNE         ?FD_USAGE
                        LDA         CMD_PARSE_VAL+1 ; BYTE TOKEN ONLY
                        BNE         ?FD_USAGE

                        LDY         F_COUNT
                        LDA         CMD_PARSE_VAL
                        STA         F_PATTERN,Y
                        INC         F_COUNT
                        BRA         ?FD_PARSE_LOOP

?FD_PARSE_DONE:
                        LDA         F_COUNT
                        BNE         ?FD_FILL_START
                        PRT_CSTRING MSG_F_USAGE
                        RTS

?FD_FILL_START:
                        JSR         MEM_FILL_PATTERN
                        RTS

?FD_USAGE:
                        PRT_CSTRING MSG_F_USAGE
                        RTS

?FD_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_SEARCH
; DESCRIPTION: SEARCHES MEMORY BY BYTE PATTERN OR ASCII TEXT
; USAGE:
;   S B <START> <END> <B0..B15>
;   S C <START> <END> <TEXT>
; NOTES:
;   - END IS INCLUSIVE.
;   - S C TREATS REST-OF-LINE AS LITERAL TEXT (NO QUOTE ESCAPING YET).
; ----------------------------------------------------------------------------
CMD_DO_SEARCH:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        CMP         #'B'
                        BEQ         ?SD_PARSE_B
                        CMP         #'C'
                        BEQ         ?SD_PARSE_C
                        BRA         ?SD_USAGE

?SD_PARSE_B:
                        INX
                        JSR         SEARCH_PARSE_RANGE
                        BCS         ?SD_USAGE
                        STZ         F_COUNT
?SD_B_LOOP:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?SD_B_DONE
                        LDA         F_COUNT
                        CMP         #F_MAX_BYTES
                        BCS         ?SD_USAGE
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BNE         ?SD_USAGE
                        LDA         CMD_PARSE_VAL+1 ; BYTE TOKENS ONLY
                        BNE         ?SD_USAGE
                        LDY         F_COUNT
                        LDA         CMD_PARSE_VAL
                        STA         F_PATTERN,Y
                        INC         F_COUNT
                        BRA         ?SD_B_LOOP
?SD_B_DONE:
                        LDA         F_COUNT
                        BNE         ?SD_RUN
                        BRA         ?SD_USAGE

?SD_PARSE_C:
                        INX
                        JSR         SEARCH_PARSE_RANGE
                        BCS         ?SD_USAGE
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?SD_USAGE
                        STZ         F_COUNT
?SD_C_LOOP:
                        LDA         CMD_LINE,X
                        BEQ         ?SD_C_DONE
                        LDY         F_COUNT
                        CPY         #F_MAX_BYTES
                        BCS         ?SD_USAGE
                        STA         F_PATTERN,Y
                        INC         F_COUNT
                        INX
                        BRA         ?SD_C_LOOP
?SD_C_DONE:

?SD_RUN:
                        JSR         SEARCH_RUN
                        RTS

?SD_USAGE:
                        PRT_CSTRING MSG_S_USAGE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SEARCH_PARSE_RANGE
; DESCRIPTION: PARSES "START END" INTO SEARCH CURSOR/LIMIT POINTERS
; INPUT: X = INDEX AFTER S <MODE>
; OUTPUT: PTR_DUMP_CUR = START (INCLUSIVE), PTR_DUMP_END = END (EXCLUSIVE)
;         C=0 OK, C=1 ERROR
; ----------------------------------------------------------------------------
SEARCH_PARSE_RANGE:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?SPR_START_OK
                        SEC
                        RTS
?SPR_START_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_DUMP_CUR
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_DUMP_CUR+1

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?SPR_END_OK
                        SEC
                        RTS
?SPR_END_OK:
        ; END MUST BE >= START
                        LDA         CMD_PARSE_VAL+1
                        CMP         PTR_DUMP_CUR+1
                        BCC         ?SPR_RANGE_ERR
                        BNE         ?SPR_END_GE_START
                        LDA         CMD_PARSE_VAL
                        CMP         PTR_DUMP_CUR
                        BCC         ?SPR_RANGE_ERR
?SPR_END_GE_START:
        ; CONVERT INCLUSIVE END TO EXCLUSIVE END
                        LDA         CMD_PARSE_VAL
                        CLC
                        ADC         #$01
                        STA         PTR_DUMP_END
                        LDA         CMD_PARSE_VAL+1
                        ADC         #$00
                        STA         PTR_DUMP_END+1
                        CLC
                        RTS

?SPR_RANGE_ERR:
                        PRT_CSTRING MSG_S_RANGE_ERR
                        SEC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SEARCH_RUN
; DESCRIPTION: RUNS PATTERN SEARCH OVER [PTR_DUMP_CUR .. PTR_DUMP_END)
; INPUT: PTR_DUMP_CUR, PTR_DUMP_END, F_COUNT, F_PATTERN
; OUTPUT: PRINTS EACH HIT ADDRESS; PRINTS "S NO MATCH" IF NONE
; ----------------------------------------------------------------------------
SEARCH_RUN:
                        STZ         SEARCH_FOUND
?SR_LOOP:
                        JSR         SEARCH_HAS_ROOM
                        BCS         ?SR_DONE_SCAN
                        JSR         SEARCH_MATCH_AT_CUR
                        BCC         ?SR_NEXT
                        JSR         SEARCH_PRINT_HIT
                        LDA         #$01
                        STA         SEARCH_FOUND
?SR_NEXT:
                        INC         PTR_DUMP_CUR
                        BNE         ?SR_LOOP
                        INC         PTR_DUMP_CUR+1
                        BRA         ?SR_LOOP
?SR_DONE_SCAN:
                        LDA         SEARCH_FOUND
                        BNE         ?SR_DONE
                        PRT_CSTRING MSG_S_NOT_FOUND
?SR_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SEARCH_HAS_ROOM
; DESCRIPTION: C=0 IF PATTERN FITS AT CURRENT CURSOR, C=1 IF SCAN COMPLETE
; INPUT: PTR_DUMP_CUR, PTR_DUMP_END, F_COUNT
; ----------------------------------------------------------------------------
SEARCH_HAS_ROOM:
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BEQ         ?SHR_SENTINEL_END

                        CLC
                        LDA         PTR_DUMP_CUR
                        ADC         F_COUNT
                        STA         PTR_TEMP
                        LDA         PTR_DUMP_CUR+1
                        ADC         #$00
                        STA         PTR_TEMP+1
                        BCS         ?SHR_NO_ROOM

                        LDA         PTR_TEMP+1
                        CMP         PTR_DUMP_END+1
                        BCC         ?SHR_ROOM
                        BNE         ?SHR_NO_ROOM
                        LDA         PTR_TEMP
                        CMP         PTR_DUMP_END
                        BCC         ?SHR_ROOM
                        BEQ         ?SHR_ROOM
                        BRA         ?SHR_NO_ROOM

?SHR_SENTINEL_END:
        ; END=$0000 MEANS EXCLUSIVE $10000; CARRY=1 IS ONLY VALID IF SUM==0000.
                        CLC
                        LDA         PTR_DUMP_CUR
                        ADC         F_COUNT
                        STA         PTR_TEMP
                        LDA         PTR_DUMP_CUR+1
                        ADC         #$00
                        STA         PTR_TEMP+1
                        BCC         ?SHR_ROOM
                        LDA         PTR_TEMP
                        ORA         PTR_TEMP+1
                        BEQ         ?SHR_ROOM

?SHR_NO_ROOM:
                        SEC
                        RTS
?SHR_ROOM:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SEARCH_MATCH_AT_CUR
; DESCRIPTION: COMPARES F_PATTERN TO MEMORY AT PTR_DUMP_CUR
; OUTPUT: C=1 MATCH, C=0 MISMATCH
; ----------------------------------------------------------------------------
SEARCH_MATCH_AT_CUR:
                        LDA         PTR_DUMP_CUR
                        STA         PTR_TEMP
                        LDA         PTR_DUMP_CUR+1
                        STA         PTR_TEMP+1
                        LDY         #$00
?SMC_LOOP:
                        CPY         F_COUNT
                        BEQ         ?SMC_MATCH
                        LDA         (PTR_TEMP),Y
                        CMP         F_PATTERN,Y
                        BNE         ?SMC_MISS
                        INY
                        BRA         ?SMC_LOOP
?SMC_MATCH:
                        SEC
                        RTS
?SMC_MISS:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: SEARCH_PRINT_HIT
; DESCRIPTION: PRINTS MATCH ADDRESS + ALIGNED 16-BYTE CONTEXT ROW
; FORMAT: "HHLL HHH0: <16 HEX BYTES> |ASCII|"
;         - FIRST WORD IS EXACT HIT ADDRESS
;         - SECOND WORD IS HIT ADDRESS ALIGNED TO $xxx0
; ----------------------------------------------------------------------------
SEARCH_PRINT_HIT:
                        JSR         PRT_CRLF
                        LDA         PTR_DUMP_CUR+1
                        JSR         PRT_HEX
                        LDA         PTR_DUMP_CUR
                        JSR         PRT_HEX

        ; PRINT MATCH-TO-ROW SEPARATOR:
        ;   ' ' = MATCH FULLY WITHIN ROW
        ;   '*' = MATCH CONTINUES INTO NEXT 16-BYTE ROW
                        LDA         PTR_DUMP_CUR
                        AND         #$0F
                        CLC
                        ADC         F_COUNT
                        CMP         #$11
                        BCC         ?SPH_SEP_SPACE
                        LDA         #'*'
                        BRA         ?SPH_SEP_OUT
?SPH_SEP_SPACE:
                        LDA         #' '
?SPH_SEP_OUT:
                        JSR         WRITE_BYTE

        ; ROW BASE = HIT & $FFF0
                        LDA         PTR_DUMP_CUR
                        AND         #$F0
                        STA         PTR_TEMP
                        LDA         PTR_DUMP_CUR+1
                        STA         PTR_TEMP+1

                        LDA         PTR_TEMP+1
                        JSR         PRT_HEX
                        LDA         PTR_TEMP
                        JSR         PRT_HEX
                        LDA         #':'
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE

        ; HEX COLUMN (16 BYTES, WITH 8|8 SPLIT MARKER)
                        LDY         #$00
?SPH_HEX_LOOP:
                        LDA         (PTR_TEMP),Y
                        JSR         PRT_HEX
                        JSR         PRT_SPACE
                        INY
                        CPY         #$08
                        BNE         ?SPH_HEX_NEXT
                        LDA         #'|'
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE
?SPH_HEX_NEXT:
                        CPY         #$10
                        BNE         ?SPH_HEX_LOOP

        ; ASCII COLUMN
                        JSR         PRT_SPACE
                        LDA         #'|'
                        JSR         WRITE_BYTE
                        LDY         #$00
?SPH_ASCII_LOOP:
                        LDA         (PTR_TEMP),Y
                        CMP         #$20
                        BCC         ?SPH_ASCII_DOT
                        CMP         #$7F
                        BCS         ?SPH_ASCII_DOT
                        BRA         ?SPH_ASCII_OUT
?SPH_ASCII_DOT:
                        LDA         #'.'
?SPH_ASCII_OUT:
                        JSR         WRITE_BYTE
                        INY
                        CPY         #$08
                        BNE         ?SPH_ASCII_NEXT
                        LDA         #'|'
                        JSR         WRITE_BYTE
?SPH_ASCII_NEXT:
                        CPY         #$10
                        BNE         ?SPH_ASCII_LOOP
                        LDA         #'|'
                        JSR         WRITE_BYTE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: MEM_FILL_PATTERN
; DESCRIPTION: FILLS [PTR_DUMP_CUR .. PTR_DUMP_END) WITH REPEATING F_PATTERN
; INPUT:
;   PTR_DUMP_CUR = START (INCLUSIVE)
;   PTR_DUMP_END = END (EXCLUSIVE)
;   F_COUNT      = PATTERN LENGTH (1..16)
;   F_PATTERN    = PATTERN BYTES
; OUTPUT: C=0 COMPLETE, C=1 ABORTED (PROTECT/VERIFY ERROR)
; ----------------------------------------------------------------------------
MEM_FILL_PATTERN:
                        STZ         F_PAT_IDX

?MFP_LOOP:
        ; DONE CHECK (NORMAL EXCLUSIVE-END MODE)
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BEQ         ?MFP_DO_WRITE

?MFP_CHK_EQUAL:
                        LDA         PTR_DUMP_CUR
                        CMP         PTR_DUMP_END
                        BNE         ?MFP_DO_WRITE
                        LDA         PTR_DUMP_CUR+1
                        CMP         PTR_DUMP_END+1
                        BEQ         ?MFP_DONE

?MFP_DO_WRITE:
                        LDA         PTR_DUMP_CUR+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCS         ?MFP_FAIL
                        LDY         F_PAT_IDX
                        LDA         F_PATTERN,Y
                        LDY         #$00
                        STA         (PTR_DUMP_CUR),Y
                        JSR         F_VERIFY_WRITE
                        BCS         ?MFP_FAIL

                        INC         PTR_DUMP_CUR
                        BNE         ?MFP_ADV_PAT
                        INC         PTR_DUMP_CUR+1

?MFP_ADV_PAT:
                        INC         F_PAT_IDX
                        LDA         F_PAT_IDX
                        CMP         F_COUNT
                        BCC         ?MFP_POST_ADV
                        STZ         F_PAT_IDX

?MFP_POST_ADV:
        ; SENTINEL END ($0000) MEANS EXCLUSIVE $10000: STOP ONLY AFTER WRAP.
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BNE         ?MFP_LOOP
                        LDA         PTR_DUMP_CUR
                        ORA         PTR_DUMP_CUR+1
                        BEQ         ?MFP_DONE
                        BRA         ?MFP_LOOP

?MFP_FAIL:
                        SEC
                        RTS
?MFP_DONE:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: F_VERIFY_WRITE
; DESCRIPTION: VERIFY LAST F WRITE AT PTR_DUMP_CUR USING A AS EXPECTED BYTE
; OUTPUT: C=0 OK, C=1 VERIFY FAILED (PRINTS ERROR + ADDRESS)
; ----------------------------------------------------------------------------
F_VERIFY_WRITE:
                        STA         MOD_BYTE
                        LDY         #$00
                        LDA         (PTR_DUMP_CUR),Y
                        CMP         MOD_BYTE
                        BEQ         ?FV_OK

                        LDA         PTR_DUMP_CUR
                        STA         PTR_LEG
                        LDA         PTR_DUMP_CUR+1
                        STA         PTR_LEG+1
                        LDA         #'F'
                        JSR         PRT_VERIFY_ERR
                        SEC
                        RTS

?FV_OK:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_COPY
; DESCRIPTION: COPIES SOURCE RANGE TO DESTINATION (OVERLAP-SAFE)
; USAGE: C <SRC_START> <SRC_END> <DST_START>
; NOTES:
;   - SRC_END IS INCLUSIVE.
;   - CHOOSES FORWARD/BACKWARD COPY AUTOMATICALLY FOR OVERLAP SAFETY.
; ----------------------------------------------------------------------------
CMD_DO_COPY:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?CD_SRC_OK
                        PRT_CSTRING MSG_C_USAGE
                        RTS
?CD_SRC_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_DUMP_CUR ; SRC CURSOR
                        STA         PTR_LEG ; SRC START (PRESERVED)
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_DUMP_CUR+1
                        STA         PTR_LEG+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCC         ?CD_SRC_ALLOWED
                        RTS
?CD_SRC_ALLOWED:

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?CD_END_OK
                        PRT_CSTRING MSG_C_USAGE
                        RTS
?CD_END_OK:
        ; SRC_END MUST BE >= SRC_START
                        LDA         CMD_PARSE_VAL+1
                        CMP         PTR_LEG+1
                        BCC         ?CD_USAGE_NEAR
                        BNE         ?CD_END_GE_START
                        LDA         CMD_PARSE_VAL
                        CMP         PTR_LEG
                        BCC         ?CD_USAGE_NEAR
?CD_END_GE_START:
        ; CONVERT INCLUSIVE END TO EXCLUSIVE END
                        LDA         CMD_PARSE_VAL
                        CLC
                        ADC         #$01
                        STA         PTR_DUMP_END
                        LDA         CMD_PARSE_VAL+1
                        ADC         #$00
                        STA         PTR_DUMP_END+1

                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BNE         ?CD_USAGE_NEAR
                        LDA         CMD_PARSE_VAL
                        STA         PTR_TEMP ; DST CURSOR
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_TEMP+1

                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?CD_USAGE_NEAR
                        BRA         ?CD_SELECT_DIR

?CD_USAGE_NEAR:
                        JMP         ?CD_USAGE

?CD_SELECT_DIR:
                        JSR         MEM_COPY_RANGE
                        RTS

?CD_USAGE:
                        PRT_CSTRING MSG_C_USAGE
?CD_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: MEM_COPY_RANGE
; DESCRIPTION: COPIES SRC RANGE TO DST (OVERLAP-SAFE)
; INPUT:
;   PTR_LEG      = SRC START (INCLUSIVE)
;   PTR_DUMP_END = SRC END (EXCLUSIVE)
;   PTR_TEMP     = DST START
; OUTPUT: C=0 COMPLETE, C=1 ABORTED (PROTECT/VERIFY ERROR)
; ----------------------------------------------------------------------------
MEM_COPY_RANGE:
        ; EMPTY EXCLUSIVE RANGE: NOTHING TO COPY.
                        LDA         PTR_LEG
                        CMP         PTR_DUMP_END
                        BNE         ?MCR_NONEMPTY
                        LDA         PTR_LEG+1
                        CMP         PTR_DUMP_END+1
                        BNE         ?MCR_NONEMPTY
                        CLC
                        RTS
?MCR_NONEMPTY:
                        LDA         PTR_LEG+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCC         ?MCR_SRC_OK
                        JMP         ?MCR_FAIL
?MCR_SRC_OK:
                        LDA         PTR_LEG
                        STA         PTR_DUMP_CUR
                        LDA         PTR_LEG+1
                        STA         PTR_DUMP_CUR+1

        ; IF DST <= SRC_START, FORWARD COPY IS SAFE.
                        LDA         PTR_TEMP+1
                        CMP         PTR_LEG+1
                        BCC         ?MCR_FWD_INIT
                        BNE         ?MCR_DST_GT_SRC
                        LDA         PTR_TEMP
                        CMP         PTR_LEG
                        BCC         ?MCR_FWD_INIT
                        BEQ         ?MCR_FWD_INIT
?MCR_DST_GT_SRC:
        ; IF SRC_END_EXCL == $0000, SRC RANGE EXTENDS TO $FFFF.
        ; WITH DST > SRC_START THIS ALWAYS OVERLAPS, SO COPY BACKWARD.
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BEQ         ?MCR_BWD_INIT

        ; IF DST < SRC_END_EXCL, RANGES OVERLAP => BACKWARD COPY.
                        LDA         PTR_TEMP+1
                        CMP         PTR_DUMP_END+1
                        BCC         ?MCR_BWD_INIT
                        BNE         ?MCR_FWD_INIT
                        LDA         PTR_TEMP
                        CMP         PTR_DUMP_END
                        BCC         ?MCR_BWD_INIT
                        BRA         ?MCR_FWD_INIT

?MCR_FWD_INIT:
?MCR_FWD_LOOP:
                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCC         ?MCR_FWD_DST_OK
                        JMP         ?MCR_FAIL
?MCR_FWD_DST_OK:
                        LDY         #$00
                        LDA         (PTR_DUMP_CUR),Y
                        STA         (PTR_TEMP),Y
                        JSR         C_VERIFY_WRITE
                        BCC         ?MCR_FWD_VFY_OK
                        JMP         ?MCR_FAIL
?MCR_FWD_VFY_OK:

                        INC         PTR_DUMP_CUR
                        BNE         ?MCR_FWD_DST_INC
                        INC         PTR_DUMP_CUR+1
?MCR_FWD_DST_INC:
                        INC         PTR_TEMP
                        BNE         ?MCR_FWD_CHK_DONE
                        INC         PTR_TEMP+1

?MCR_FWD_CHK_DONE:
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BNE         ?MCR_FWD_EQ_CHECK
                        LDA         PTR_DUMP_CUR
                        ORA         PTR_DUMP_CUR+1
                        BNE         ?MCR_FWD_LOOP
                        JMP         ?MCR_DONE
?MCR_FWD_EQ_CHECK:
                        LDA         PTR_DUMP_CUR
                        CMP         PTR_DUMP_END
                        BNE         ?MCR_FWD_LOOP
                        LDA         PTR_DUMP_CUR+1
                        CMP         PTR_DUMP_END+1
                        BNE         ?MCR_FWD_LOOP
                        BRA         ?MCR_DONE

?MCR_BWD_INIT:
        ; SRC CURSOR = SRC_END_INCLUSIVE
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BNE         ?MCR_BWD_SRC_FROM_END
                        LDA         #$FF
                        STA         PTR_DUMP_CUR
                        STA         PTR_DUMP_CUR+1
                        BRA         ?MCR_BWD_DST_TAIL
?MCR_BWD_SRC_FROM_END:
                        LDA         PTR_DUMP_END
                        SEC
                        SBC         #$01
                        STA         PTR_DUMP_CUR
                        LDA         PTR_DUMP_END+1
                        SBC         #$00
                        STA         PTR_DUMP_CUR+1

?MCR_BWD_DST_TAIL:
        ; OFFSET = SRC_END_INCLUSIVE - SRC_START
                        LDA         PTR_DUMP_END
                        SEC
                        SBC         PTR_LEG
                        STA         CMD_PARSE_VAL
                        LDA         PTR_DUMP_END+1
                        SBC         PTR_LEG+1
                        STA         CMD_PARSE_VAL+1
                        LDA         CMD_PARSE_VAL
                        SEC
                        SBC         #$01
                        STA         CMD_PARSE_VAL
                        LDA         CMD_PARSE_VAL+1
                        SBC         #$00
                        STA         CMD_PARSE_VAL+1

        ; DST CURSOR = DST_START + OFFSET
                        CLC
                        LDA         PTR_TEMP
                        ADC         CMD_PARSE_VAL
                        STA         PTR_TEMP
                        LDA         PTR_TEMP+1
                        ADC         CMD_PARSE_VAL+1
                        STA         PTR_TEMP+1

?MCR_BWD_LOOP:
                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCS         ?MCR_FAIL
                        LDY         #$00
                        LDA         (PTR_DUMP_CUR),Y
                        STA         (PTR_TEMP),Y
                        JSR         C_VERIFY_WRITE
                        BCS         ?MCR_FAIL

        ; FINISHED AFTER COPYING SRC_START BYTE.
                        LDA         PTR_DUMP_CUR
                        CMP         PTR_LEG
                        BNE         ?MCR_BWD_DEC
                        LDA         PTR_DUMP_CUR+1
                        CMP         PTR_LEG+1
                        BEQ         ?MCR_DONE

?MCR_BWD_DEC:
                        LDA         PTR_DUMP_CUR
                        BNE         ?MCR_BWD_DEC_SRC_LO
                        DEC         PTR_DUMP_CUR+1
?MCR_BWD_DEC_SRC_LO:
                        DEC         PTR_DUMP_CUR

                        LDA         PTR_TEMP
                        BNE         ?MCR_BWD_DEC_DST_LO
                        DEC         PTR_TEMP+1
?MCR_BWD_DEC_DST_LO:
                        DEC         PTR_TEMP
                        BRA         ?MCR_BWD_LOOP

?MCR_FAIL:
                        SEC
                        RTS
?MCR_DONE:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: C_VERIFY_WRITE
; DESCRIPTION: READS BACK BYTE AT PTR_TEMP AND COMPARES TO ACC
; INPUT: A = EXPECTED BYTE, PTR_TEMP = TARGET ADDRESS
; OUTPUT: C=0 OK, C=1 VERIFY FAILED (AND PRINTS ERROR + ADDRESS)
; ----------------------------------------------------------------------------
C_VERIFY_WRITE:
                        STA         MOD_BYTE
                        LDY         #$00
                        LDA         (PTR_TEMP),Y
                        CMP         MOD_BYTE
                        BEQ         ?CV_OK
                        LDA         PTR_TEMP
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        STA         PTR_LEG+1
                        LDA         #'C'
                        JSR         PRT_VERIFY_ERR
                        SEC
                        RTS
?CV_OK:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_DO_MODIFY
; DESCRIPTION: HANDLES MODIFY COMMAND FORMS:
;              M <START> <B0..B15>   (INLINE DEPOSIT, UP TO 16 BYTES)
;              M <START>             (INTERACTIVE MODIFY)
;              M                     (CONTINUE FROM LAST MODIFY ADDRESS)
; INTERACTIVE: CR/LF = SKIP/NEXT, '.' = END, 2 HEX DIGITS = STORE BYTE
; ----------------------------------------------------------------------------
CMD_DO_MODIFY:
                        LDX         #$01 ; PARSE AFTER COMMAND LETTER
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?MD_HAS_START

        ; --- M (continue from last) ---
                        LDA         MOD_VALID
                        BNE         ?MD_USE_LAST
                        PRT_CSTRING MSG_M_USAGE
                        RTS

?MD_USE_LAST:
                        LDA         MOD_NEXT
                        STA         PTR_TEMP
                        LDA         MOD_NEXT+1
                        STA         PTR_TEMP+1
                        STZ         MOD_COUNT ; CLEAR CR/LF PAIR STATE
                        JMP         CMD_MODIFY_INTERACTIVE

?MD_HAS_START:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$00
                        BEQ         ?MD_START_OK
                        PRT_CSTRING MSG_M_USAGE
                        RTS

?MD_START_OK:
                        LDA         CMD_PARSE_VAL
                        STA         PTR_TEMP
                        LDA         CMD_PARSE_VAL+1
                        STA         PTR_TEMP+1

                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BNE         ?MD_HAS_BYTES
                        STZ         MOD_COUNT ; CLEAR CR/LF PAIR STATE
                        JMP         CMD_MODIFY_INTERACTIVE

?MD_HAS_BYTES:
                        STZ         MOD_COUNT

?MD_DEPOSIT_LOOP:
                        LDA         MOD_COUNT
                        CMP         #MOD_MAX_BYTES
                        BCC         ?MD_PARSE_BYTE
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?MD_DONE
                        PRT_CSTRING MSG_M_USAGE
                        RTS

?MD_PARSE_BYTE:
                        JSR         CMD_PARSE_ADDR16_TOKEN
                        CMP         #$01 ; NO MORE TOKENS
                        BEQ         ?MD_DONE
                        CMP         #$00
                        BNE         ?MD_USAGE
                        LDA         CMD_PARSE_VAL+1 ; MUST FIT BYTE
                        BNE         ?MD_USAGE

                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCS         ?MD_DONE
                        LDY         #$00
                        LDA         CMD_PARSE_VAL
                        STA         (PTR_TEMP),Y
                        JSR         M_VERIFY_WRITE
                        BCS         ?MD_DONE
                        INC         PTR_TEMP
                        BNE         ?MD_STORE_OK
                        INC         PTR_TEMP+1
?MD_STORE_OK:
                        INC         MOD_COUNT
                        BRA         ?MD_DEPOSIT_LOOP

?MD_USAGE:
                        PRT_CSTRING MSG_M_USAGE
                        RTS

?MD_DONE:
                        LDA         PTR_TEMP
                        STA         MOD_NEXT
                        LDA         PTR_TEMP+1
                        STA         MOD_NEXT+1
                        LDA         #$01
                        STA         MOD_VALID
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_MODIFY_INTERACTIVE
; DESCRIPTION: INTERACTIVE MEMORY MODIFY AT PTR_TEMP
; CONTROLS: CR/LF=SKIP/NEXT, '.'=END, 2 HEX DIGITS=STORE BYTE THEN NEXT
; ----------------------------------------------------------------------------
CMD_MODIFY_INTERACTIVE:
?MI_LINE:
                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCC         ?MI_ADDR_OK
                        JMP         ?MI_DONE
?MI_ADDR_OK:
                        JSR         PRT_CRLF
                        LDA         PTR_TEMP+1
                        JSR         PRT_HEX
                        LDA         PTR_TEMP
                        JSR         PRT_HEX
                        LDA         #':'
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE
                        LDY         #$00
                        LDA         (PTR_TEMP),Y
                        JSR         PRT_HEX
                        JSR         PRT_SPACE

?MI_WAIT_KEY:
                        JSR         READ_BYTE
                        CMP         #'.'
                        BEQ         ?MI_DONE
                        CMP         #$0A
                        BEQ         ?MI_LF
                        CMP         #$0D
                        BEQ         ?MI_CR
                        STZ         MOD_COUNT ; NON-NEWLINE CLEARS CR/LF STATE

                        JSR         UTIL_TO_UPPER
                        PHA
                        JSR         WRITE_BYTE ; ECHO HI NIBBLE
                        PLA
                        JSR         HEX_TO_NIBBLE
                        BCC         ?MI_BAD
                        ASL         A
                        ASL         A
                        ASL         A
                        ASL         A
                        STA         MOD_BYTE

                        JSR         READ_BYTE
                        JSR         UTIL_TO_UPPER
                        PHA
                        JSR         WRITE_BYTE ; ECHO LO NIBBLE
                        PLA
                        JSR         HEX_TO_NIBBLE
                        BCC         ?MI_BAD
                        CLC
                        ADC         MOD_BYTE
                        PHA
                        LDA         PTR_TEMP+1
                        JSR         CHECK_ADDR_ALLOWED_HI
                        BCS         ?MI_DROP_AND_LINE
                        PLA
                        LDY         #$00
                        STA         (PTR_TEMP),Y
                        JSR         M_VERIFY_WRITE
                        BCS         ?MI_LINE

?MI_CR:
                        LDA         #$01
                        STA         MOD_COUNT ; MARK THAT CR WAS SEEN
                        JMP         ?MI_NEXT

?MI_LF:
                        LDA         MOD_COUNT
                        BEQ         ?MI_NEXT ; LONE LF => NEXT
                        STZ         MOD_COUNT ; CRLF PAIR => CONSUME LF ONLY
                        JMP         ?MI_WAIT_KEY

?MI_NEXT:
                        INC         PTR_TEMP
                        BEQ         ?MI_NEXT_HI
                        JMP         ?MI_LINE
?MI_NEXT_HI:
                        INC         PTR_TEMP+1
                        JMP         ?MI_LINE

?MI_BAD:
                        LDA         #'?'
                        JSR         WRITE_BYTE
                        JMP         ?MI_LINE

?MI_DROP_AND_LINE:
                        PLA
                        JMP         ?MI_LINE

?MI_DONE:
                        LDA         PTR_TEMP
                        STA         MOD_NEXT
                        LDA         PTR_TEMP+1
                        STA         MOD_NEXT+1
                        LDA         #$01
                        STA         MOD_VALID
                        RTS
; ----------------------------------------------------------------------------
; SUBROUTINE: M_VERIFY_WRITE
; DESCRIPTION: READS BACK BYTE AT PTR_TEMP AND COMPARES TO ACC
; INPUT: A = EXPECTED BYTE, PTR_TEMP = TARGET ADDRESS
; OUTPUT: C=0 OK, C=1 VERIFY FAILED (AND PRINTS ERROR + ADDRESS)
; ----------------------------------------------------------------------------
M_VERIFY_WRITE:
                        STA         MOD_BYTE
                        LDY         #$00
                        LDA         (PTR_TEMP),Y
                        CMP         MOD_BYTE
                        BEQ         ?MV_OK
                        LDA         PTR_TEMP
                        STA         PTR_LEG
                        LDA         PTR_TEMP+1
                        STA         PTR_LEG+1
                        LDA         #'M'
                        JSR         PRT_VERIFY_ERR
                        SEC
                        RTS
?MV_OK:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_VERIFY_ERR
; DESCRIPTION: PRINTS "<MODE> VERIFY FAILED AT ADDR " + 16-BIT ADDRESS
; INPUT: A = MODE CHAR ('C','F','M'), PTR_LEG = ADDRESS
; OUTPUT: NONE
; ----------------------------------------------------------------------------
PRT_VERIFY_ERR:
                        JSR         PRT_CRLF
                        JSR         WRITE_BYTE
                        PRT_CSTRING MSG_VERIFY_ERR_SUFFIX
                        LDA         PTR_LEG+1
                        JSR         PRT_HEX
                        LDA         PTR_LEG
                        JSR         PRT_HEX
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CHECK_ADDR_ALLOWED_HI
; DESCRIPTION: BLOCKS ACCESS TO $0000-$03FF UNLESS FORCE MODE IS ACTIVE
; INPUT: A = ADDRESS HIGH BYTE
; OUTPUT: C=0 ALLOWED, C=1 BLOCKED (AND PRINTS ERROR)
; ----------------------------------------------------------------------------
CHECK_ADDR_ALLOWED_HI:
                        PHA
                        LDA         #SYSF_FORCE_MODE_M
                        BIT         SYS_FLAGS
                        BNE         ?CAA_ALLOW
                        PLA
                        CMP         #PROTECT_HI_LIMIT
                        BCS         ?CAA_ALLOW_NOPOP
                        PRT_CSTRING MSG_PROTECT_ERR
                        SEC
                        RTS
?CAA_ALLOW:
                        PLA
?CAA_ALLOW_NOPOP:
                        CLC
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_SKIP_SPACES
; DESCRIPTION: ADVANCES X PAST ASCII SPACES IN CMD_LINE
; ----------------------------------------------------------------------------
CMD_SKIP_SPACES:
?CSS_LOOP:
                        LDA         CMD_LINE,X
                        CMP         #' '
                        BNE         ?CSS_DONE
                        INX
                        BRA         ?CSS_LOOP
?CSS_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: CMD_PARSE_ADDR16_TOKEN
; DESCRIPTION: PARSES 1-4 HEX DIGITS (OPTIONAL '$' PREFIX) AT CMD_LINE[X]
; INPUT: X = CURRENT INDEX
; OUTPUT: CMD_PARSE_VAL, X = FIRST NON-TOKEN CHAR
;         A=0 SUCCESS, A=1 NO TOKEN, A=2 INVALID TOKEN
; ----------------------------------------------------------------------------
CMD_PARSE_ADDR16_TOKEN:
                        JSR         CMD_SKIP_SPACES
                        LDA         CMD_LINE,X
                        BEQ         ?CPAT_NO_TOKEN
                        CMP         #'$'
                        BNE         ?CPAT_START
                        INX
?CPAT_START:
                        STZ         CMD_PARSE_VAL
                        STZ         CMD_PARSE_VAL+1
                        LDY         #$00
?CPAT_LOOP:
                        LDA         CMD_LINE,X
                        JSR         HEX_TO_NIBBLE
                        BCC         ?CPAT_DONE_DIGITS
                        STA         CMD_PARSE_NIB

                        ASL         CMD_PARSE_VAL
                        ROL         CMD_PARSE_VAL+1
                        ASL         CMD_PARSE_VAL
                        ROL         CMD_PARSE_VAL+1
                        ASL         CMD_PARSE_VAL
                        ROL         CMD_PARSE_VAL+1
                        ASL         CMD_PARSE_VAL
                        ROL         CMD_PARSE_VAL+1

                        LDA         CMD_PARSE_VAL
                        CLC
                        ADC         CMD_PARSE_NIB
                        STA         CMD_PARSE_VAL
                        BCC         ?CPAT_ACCUM_OK
                        INC         CMD_PARSE_VAL+1
?CPAT_ACCUM_OK:
                        INY
                        CPY         #$05 ; MAX 4 HEX DIGITS
                        BCS         ?CPAT_INVALID
                        INX
                        BRA         ?CPAT_LOOP

?CPAT_DONE_DIGITS:
                        CPY         #$00
                        BEQ         ?CPAT_INVALID
                        LDA         CMD_LINE,X
                        BEQ         ?CPAT_SUCCESS
                        CMP         #' '
                        BEQ         ?CPAT_SUCCESS

?CPAT_INVALID:
                        LDA         #$02
                        RTS
?CPAT_NO_TOKEN:
                        LDA         #$01
                        RTS
?CPAT_SUCCESS:
                        LDA         #$00
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG
; DESCRIPTION: DEBUG ENTRY ALIAS FOR JSR CONTEXT
; ----------------------------------------------------------------------------
DEBUG:
                        JMP         DEBUG_JSR

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_JSR
; DESCRIPTION: DEBUG REPORT FOR NORMAL JSR ENTRY
; ----------------------------------------------------------------------------
DEBUG_JSR:
                        STA         DBG_A
                        STX         DBG_X
                        STY         DBG_Y
                        PHP
                        PLA
                        STA         DBG_P
                        TSX
                        TXA
                        CLC
                        ADC         #$02 ; RECONSTRUCT SP BEFORE JSR
                        STA         DBG_SP
                        LDA         $101,X ; JSR RETURN LO
                        CLC
                        ADC         #$01 ; SHOW NEXT INSTRUCTION
                        STA         DBG_PC_LO
                        LDA         $102,X ; JSR RETURN HI
                        ADC         #$00
                        STA         DBG_PC_HI
                        LDA         #DBG_MODE_JSR
                        STA         DBG_MODE
                        JSR         DEBUG_PRINT_CONTEXT
                        JMP         DEBUG_RESTORE_REGS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_NMI
; DESCRIPTION: DEBUG REPORT FOR NMI CONTEXT
; ----------------------------------------------------------------------------
DEBUG_NMI:
                        STA         DBG_A
                        STX         DBG_X
                        STY         DBG_Y
                        JSR         DEBUG_CAPTURE_IRQ_FRAME
                        LDA         #DBG_MODE_NMI
                        STA         DBG_MODE
                        JSR         DEBUG_PRINT_CONTEXT
                        JMP         DEBUG_RESTORE_REGS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_IRQ
; DESCRIPTION: DEBUG REPORT FOR IRQ/BRK CONTEXT
; ----------------------------------------------------------------------------
DEBUG_IRQ:
                        STA         DBG_A
                        STX         DBG_X
                        STY         DBG_Y
                        JSR         DEBUG_CAPTURE_IRQ_FRAME

                        LDA         #DBG_MODE_IRQ
                        STA         DBG_MODE
                        LDA         DBG_P
                        AND         #$10 ; B BIT SET => BRK
                        BEQ         ?DI_NOT_BRK
                        LDA         #DBG_MODE_BRK
                        STA         DBG_MODE
                        LDA         DBG_PC_LO ; BRK SIGNATURE AT (PC-1)
                        SEC
                        SBC         #$01
                        STA         PTR_LEG
                        LDA         DBG_PC_HI
                        SBC         #$00
                        STA         PTR_LEG+1
                        LDY         #$00
                        LDA         (PTR_LEG),Y
                        STA         DBG_BRK_SIG
?DI_NOT_BRK:
                        JSR         STEP_PREVIEW_BRK_HIT
                        JSR         DEBUG_PRINT_CONTEXT
                        JSR         STEP_HANDLE_BRK_HIT
                        JMP         DEBUG_RESTORE_REGS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_CAPTURE_IRQ_FRAME
; DESCRIPTION: CAPTURES STACKED P/PC AND RECONSTRUCTED PRE-INTERRUPT SP
; INPUT: NONE
; OUTPUT: DBG_P, DBG_PC_LO/HI, DBG_SP
; ----------------------------------------------------------------------------
DEBUG_CAPTURE_IRQ_FRAME:
                        TSX
                        ; Called via JSR from DEBUG_IRQ/DEBUG_NMI, which are
                        ; themselves entered via JSR from SYS_IRQ/SYS_NMI.
                        ; Skip both JSR return addresses (4 bytes) to reach
                        ; the hardware interrupt frame (P, PCL, PCH).
                        LDA         $105,X ; STACKED P
                        STA         DBG_P
                        LDA         $106,X ; STACKED PC LO
                        STA         DBG_PC_LO
                        LDA         $107,X ; STACKED PC HI
                        STA         DBG_PC_HI
                        TXA
                        CLC
                        ADC         #$07 ; RECONSTRUCT SP BEFORE INTERRUPT
                        STA         DBG_SP
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: STEP_PREVIEW_BRK_HIT
; DESCRIPTION: IF CURRENT BRK IS THE TEMP N-BREAKPOINT, RESTORE THE ORIGINAL
;              OPCODE BEFORE DEBUG PRINTING SO PREV DISASSEMBLY SHOWS THE
;              REAL INSTRUCTION INSTEAD OF THE PATCHED BRK.
;              DBG_BRK_SIG IS NORMALIZED:
;                - synthetic step break => $00
;                - stepped-to real BRK  => real signature byte at STEP_ADDR+1
; ----------------------------------------------------------------------------
STEP_PREVIEW_BRK_HIT:
                        LDA         STEP_ACTIVE
                        BEQ         ?SPBH_DONE
                        LDA         DBG_MODE
                        CMP         #DBG_MODE_BRK
                        BNE         ?SPBH_DONE

                        LDA         DBG_PC_LO ; TRAP ADDR = DBG_PC - 2
                        SEC
                        SBC         #$02
                        STA         PTR_TEMP
                        LDA         DBG_PC_HI
                        SBC         #$00
                        STA         PTR_TEMP+1

                        LDA         PTR_TEMP
                        CMP         STEP_ADDR
                        BNE         ?SPBH_DONE
                        LDA         PTR_TEMP+1
                        CMP         STEP_ADDR+1
                        BNE         ?SPBH_DONE

        ; Restore original opcode at STEP_ADDR before rendering PREV line.
                        LDA         STEP_ADDR
                        STA         PTR_LEG
                        LDA         STEP_ADDR+1
                        STA         PTR_LEG+1
                        LDY         #$00
                        LDA         STEP_ORIG
                        STA         (PTR_LEG),Y

        ; Signature formatting for TRAP line.
                        LDA         STEP_ORIG
                        BNE         ?SPBH_SYNTH
        ; Real BRK at stepped address: keep real signature byte.
                        INC         PTR_LEG
                        BNE         ?SPBH_REAL_SIG_RD
                        INC         PTR_LEG+1
?SPBH_REAL_SIG_RD:
                        LDA         (PTR_LEG),Y
                        STA         DBG_BRK_SIG
                        BRA         ?SPBH_DONE
?SPBH_SYNTH:
                        STZ         DBG_BRK_SIG
?SPBH_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: STEP_HANDLE_BRK_HIT
; DESCRIPTION: RESTORES TEMP N-BREAKPOINT BYTE IF THIS BRK MATCHES STEP_ADDR
;              AND NORMALIZES DBG_PC FOR NON-BRK PATCH TARGETS.
; ----------------------------------------------------------------------------
STEP_HANDLE_BRK_HIT:
                        LDA         STEP_ACTIVE
                        BEQ         ?SH_DONE
                        LDA         DBG_MODE
                        CMP         #DBG_MODE_BRK
                        BNE         ?SH_DONE

                        LDA         DBG_PC_LO ; TRAP ADDR = DBG_PC - 2
                        SEC
                        SBC         #$02
                        STA         PTR_TEMP
                        LDA         DBG_PC_HI
                        SBC         #$00
                        STA         PTR_TEMP+1

                        LDA         PTR_TEMP
                        CMP         STEP_ADDR
                        BNE         ?SH_DONE
                        LDA         PTR_TEMP+1
                        CMP         STEP_ADDR+1
                        BNE         ?SH_DONE

        ; RESTORE ORIGINAL BYTE AT STEP_ADDR
                        LDA         STEP_ADDR
                        STA         PTR_LEG
                        LDA         STEP_ADDR+1
                        STA         PTR_LEG+1
                        LDY         #$00
                        LDA         STEP_ORIG
                        STA         (PTR_LEG),Y

        ; If the original opcode was BRK ($00), the CPU genuinely executed BRK
        ; at STEP_ADDR, so keep DBG_PC as captured (post-BRK) to avoid
        ; re-stepping the same BRK forever.
                        LDA         STEP_ORIG
                        BEQ         ?SH_CLEAR_ONLY

        ; For patched non-BRK targets, present next-stop PC as patch address.
                        LDA         STEP_ADDR
                        STA         DBG_PC_LO
                        LDA         STEP_ADDR+1
                        STA         DBG_PC_HI
?SH_CLEAR_ONLY:
                        STZ         STEP_ACTIVE
?SH_DONE:
                        RTS

DEBUG_RESTORE_REGS:
                        LDA         DBG_A
                        LDX         DBG_X
                        LDY         DBG_Y
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_BUILD_TAG
; DESCRIPTION: BUILDS "[   ]" TAG IN DBG_TAG_BUF FOR JSR/NMI/IRQ/BRK
; OUTPUT: DBG_TAG_BUF = "[JSR]" / "[NMI]" / "[IRQ]" / "[BRK]"
; ----------------------------------------------------------------------------
DEBUG_BUILD_TAG:
                        LDA         #'['
                        STA         DBG_TAG_BUF
                        LDA         #' '
                        STA         DBG_TAG_BUF+1
                        STA         DBG_TAG_BUF+2
                        STA         DBG_TAG_BUF+3
                        LDA         #']'
                        STA         DBG_TAG_BUF+4
                        STZ         DBG_TAG_BUF+5

                        LDA         DBG_MODE
                        CMP         #DBG_MODE_JSR
                        BNE         ?DBT_NOT_JSR
                        LDA         #'J'
                        STA         DBG_TAG_BUF+1
                        LDA         #'S'
                        STA         DBG_TAG_BUF+2
                        LDA         #'R'
                        STA         DBG_TAG_BUF+3
                        RTS
?DBT_NOT_JSR:
                        CMP         #DBG_MODE_NMI
                        BNE         ?DBT_NOT_NMI
                        LDA         #'N'
                        STA         DBG_TAG_BUF+1
                        LDA         #'M'
                        STA         DBG_TAG_BUF+2
                        LDA         #'I'
                        STA         DBG_TAG_BUF+3
                        RTS
?DBT_NOT_NMI:
                        CMP         #DBG_MODE_IRQ
                        BNE         ?DBT_BRK
                        LDA         #'I'
                        STA         DBG_TAG_BUF+1
                        LDA         #'R'
                        STA         DBG_TAG_BUF+2
                        LDA         #'Q'
                        STA         DBG_TAG_BUF+3
                        RTS
?DBT_BRK:
                        LDA         #'B'
                        STA         DBG_TAG_BUF+1
                        LDA         #'R'
                        STA         DBG_TAG_BUF+2
                        LDA         #'K'
                        STA         DBG_TAG_BUF+3
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_PRINT_CONTEXT
; DESCRIPTION: PRINTS COMMON DEBUG LINE FOR JSR/NMI/IRQ/BRK
; ----------------------------------------------------------------------------
DEBUG_PRINT_CONTEXT:
                        LDA         #$01 ; MARK RESUME CONTEXT AS VALID
                        STA         BRK_FLAG
                        LDA         DBG_MODE
                        CMP         #DBG_MODE_BRK
                        BNE         ?DPC_TRAP_LINE
                        JSR         DEBUG_PRINT_BRK_CURR_NEXT
?DPC_TRAP_LINE:
                        JSR         PRT_CRLF
                        LDA         DBG_MODE
                        CMP         #DBG_MODE_BRK
                        BNE         ?DPC_NO_TRAP_PREFIX
                        PRT_CSTRING STR_STATE
?DPC_NO_TRAP_PREFIX:
                        JSR         DEBUG_BUILD_TAG
                        PRT_CSTRING DBG_TAG_BUF
                        LDA         DBG_MODE
                        CMP         #DBG_MODE_BRK
                        BNE         ?DPC_TAG_DONE
                        JSR         PRT_SPACE
                        LDA         DBG_BRK_SIG
                        JSR         PRT_HEX
?DPC_TAG_DONE:
                        JSR         PRT_SPACE

                        LDA         #'P'
                        JSR         WRITE_BYTE
                        LDA         #'C'
                        JSR         WRITE_BYTE
                        LDA         #':'
                        JSR         WRITE_BYTE
                        LDA         DBG_PC_HI
                        LDX         DBG_PC_LO
                        JSR         PRT_HEX_WORD_AX
                        JSR         PRT_SPACE

                        JSR         PRT_A
                        LDA         #':'
                        JSR         WRITE_BYTE
                        LDA         DBG_A
                        JSR         PRT_HEX
                        JSR         PRT_SPACE

                        JSR         PRT_X
                        LDA         #':'
                        JSR         WRITE_BYTE
                        LDA         DBG_X
                        JSR         PRT_HEX
                        JSR         PRT_SPACE

                        JSR         PRT_Y
                        LDA         #':'
                        JSR         WRITE_BYTE
                        LDA         DBG_Y
                        JSR         PRT_HEX
                        JSR         PRT_SPACE

                        JSR         PRT_P
                        LDA         #':'
                        JSR         WRITE_BYTE
                        LDA         DBG_P
                        JSR         PRT_HEX
                        JSR         PRT_SPACE
                        LDA         DBG_P
                        STA         PSR_TEMP
                        JSR         PRT_FLAG_STR
                        JSR         PRT_SPACE

                        JSR         PRT_STACK
                        LDA         #':'
                        JSR         WRITE_BYTE
                        LDA         DBG_SP
                        JSR         PRT_HEX
                        LDA         DBG_MODE
                        CMP         #DBG_MODE_BRK
                        BNE         ?DPC_STD_NEXT
                        JSR         PRT_CRLF
                        RTS
?DPC_STD_NEXT:
                        JSR         DEBUG_PRINT_NEXT_INSN
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_PRINT_NEXT_INSN
; DESCRIPTION: PRINTS NEXT INSTRUCTION AT DBG_PC
; ----------------------------------------------------------------------------
DEBUG_PRINT_NEXT_INSN:
                        PUSH        A, X, Y
                        JSR         PRT_CRLF
                        LDA         DBG_PC_LO
                        STA         PTR_DUMP_CUR
                        LDA         DBG_PC_HI
                        STA         PTR_DUMP_CUR+1
                        JSR         DEBUG_PRINT_ONE_INSN_AT_PTR
                        JSR         PRT_CRLF
                        PULL        Y, X, A
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_PRINT_BRK_CURR_NEXT
; DESCRIPTION: PRINTS "CURR: " (DBG_PC-2) + "NEXT: " (DBG_PC) ON ONE LINE
; ----------------------------------------------------------------------------
DEBUG_PRINT_BRK_CURR_NEXT:
                        PUSH        A, X, Y
                        JSR         PRT_CRLF
                        PRT_CSTRING STR_CURR
                        LDA         DBG_PC_LO
                        SEC
                        SBC         #$02
                        STA         PTR_DUMP_CUR
                        LDA         DBG_PC_HI
                        SBC         #$00
                        STA         PTR_DUMP_CUR+1
                        JSR         DEBUG_PRINT_ONE_INSN_AT_PTR
                        JSR         PRT_SPACE
                        JSR         PRT_SPACE
                        JSR         PRT_SPACE
                        JSR         PRT_SPACE
                        PRT_CSTRING STR_NEXT
                        LDA         DBG_PC_LO
                        STA         PTR_DUMP_CUR
                        LDA         DBG_PC_HI
                        STA         PTR_DUMP_CUR+1
                        JSR         DEBUG_PRINT_ONE_INSN_AT_PTR
                        PULL        Y, X, A
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_PRINT_BRK_NEXT
; DESCRIPTION: PRINTS "NEXT: " + DISASM AT RESUME ADDRESS (DBG_PC)
; ----------------------------------------------------------------------------
DEBUG_PRINT_BRK_NEXT:
                        PUSH        A, X, Y
                        JSR         PRT_CRLF
                        PRT_CSTRING STR_NEXT
                        LDA         DBG_PC_LO
                        STA         PTR_DUMP_CUR
                        LDA         DBG_PC_HI
                        STA         PTR_DUMP_CUR+1
                        JSR         DEBUG_PRINT_ONE_INSN_AT_PTR
                        JSR         PRT_CRLF
                        PULL        Y, X, A
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DEBUG_PRINT_ONE_INSN_AT_PTR
; DESCRIPTION: PRINTS ONE DISASSEMBLED INSTRUCTION AT PTR_DUMP_CUR
; FORMAT: "HHLL: MNM OPERAND"
; ----------------------------------------------------------------------------
DEBUG_PRINT_ONE_INSN_AT_PTR:
                        LDY         #$00
                        LDA         (PTR_DUMP_CUR),Y
                        STA         DIS_OPCODE
                        TAX
                        LDA         U_OP_MODE_TAB,X
                        STA         DIS_MODE
                        LDA         U_OP_LEN_TAB,X
                        STA         DIS_LEN

                        LDA         PTR_DUMP_CUR+1
                        JSR         PRT_HEX
                        LDA         PTR_DUMP_CUR
                        JSR         PRT_HEX
                        LDA         #':'
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE

                        LDA         DIS_OPCODE
                        ASL         A
                        STA         PTR_TEMP
                        LDA         #$00
                        ROL         A
                        STA         PTR_TEMP+1
                        CLC
                        LDA         PTR_TEMP
                        ADC         DIS_OPCODE
                        STA         PTR_TEMP
                        LDA         PTR_TEMP+1
                        ADC         #$00
                        STA         PTR_TEMP+1
                        CLC
                        LDA         PTR_TEMP
                        ADC         #<U_OP_MNEM_TAB
                        STA         PTR_TEMP
                        LDA         PTR_TEMP+1
                        ADC         #>U_OP_MNEM_TAB
                        STA         PTR_TEMP+1

                        LDY         #$00
                        LDA         (PTR_TEMP),Y
                        JSR         WRITE_BYTE
                        INY
                        LDA         (PTR_TEMP),Y
                        JSR         WRITE_BYTE
                        INY
                        LDA         (PTR_TEMP),Y
                        JSR         WRITE_BYTE
                        JSR         DIS_PRINT_BIT_SUFFIX

                        LDA         DIS_MODE
                        BEQ         ?DPOI_DONE
                        JSR         PRT_SPACE
                        JSR         DIS_PRINT_OPERAND
?DPOI_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_FLAG_STR
; DESCRIPTION: EXPANDS PSR_TEMP INTO [NV-BDIZC] STRING
; INPUT: PSR_TEMP = STATUS BYTE
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: PSR_TEMP
; ----------------------------------------------------------------------------
PRT_FLAG_STR:
                        LDA         #'[' ; START
                        JSR         WRITE_BYTE

                        LDA         #'N' ; NEGATIVE FLAG
                        BIT         PSR_TEMP ; TEST BIT 7
                        BMI         ?N_SET ; IF SET, HI
                        LDA         #'n' ; ELSE LO
?N_SET:                 JSR         WRITE_BYTE ; PRINT N

                        LDA         #'V' ; OVERFLOW FLAG
                        BIT         PSR_TEMP ; TEST BIT 6
                        BVS         ?V_SET ; IF SET, HI
                        LDA         #'v' ; ELSE LO
?V_SET:                 JSR         WRITE_BYTE ; PRINT V

                        LDA         #'-' ; RESERVED BIT
                        JSR         WRITE_BYTE

                        LDA         PSR_TEMP ; W65C02: LOAD FLAGS BYTE
                        BIT         #$10 ; TEST BREAK BIT (BIT 4)
                        BNE         ?B_SET ; IF SET, HI
                        LDA         #'b' ; ELSE LO
                        BRA         ?B_PRT ; PRINT IT
?B_SET:                 LDA         #'B'
?B_PRT:                 JSR         WRITE_BYTE

                        LDA         PSR_TEMP ; W65C02: LOAD FLAGS BYTE
                        BIT         #$08 ; TEST DECIMAL BIT (BIT 3)
                        BNE         ?D_SET ; IF SET, HI
                        LDA         #'d' ; ELSE LO
                        BRA         ?D_PRT
?D_SET:                 LDA         #'D'
?D_PRT:                 JSR         WRITE_BYTE

                        LDA         PSR_TEMP ; W65C02: LOAD FLAGS BYTE
                        BIT         #$04 ; TEST IRQ MASK BIT (BIT 2)
                        BNE         ?I_SET ; IF SET, HI
                        LDA         #'i' ; ELSE LO
                        BRA         ?I_PRT
?I_SET:                 LDA         #'I'
?I_PRT:                 JSR         WRITE_BYTE

                        LDA         PSR_TEMP ; W65C02: LOAD FLAGS BYTE
                        BIT         #$02 ; TEST ZERO BIT (BIT 1)
                        BNE         ?Z_SET ; IF SET, HI
                        LDA         #'z' ; ELSE LO
                        BRA         ?Z_PRT
?Z_SET:                 LDA         #'Z'
?Z_PRT:                 JSR         WRITE_BYTE

                        LDA         PSR_TEMP ; W65C02: LOAD FLAGS BYTE
                        BIT         #$01 ; TEST CARRY BIT (BIT 0)
                        BNE         ?C_SET ; IF SET, HI
                        LDA         #'c' ; ELSE LO
                        BRA         ?C_PRT
?C_SET:                 LDA         #'C'
?C_PRT:                 JSR         WRITE_BYTE

                        LDA         #']' ; END STRING
                        JSR         WRITE_BYTE
                        RTS             ; DONE

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_CRLF
; DESCRIPTION: PRINTS CARRIAGE RETURN + LINE FEED
; ----------------------------------------------------------------------------
PRT_CRLF:               PUSH        A   ; SAVE ACC
                        LDA         #$0D ; CR
                        JSR         WRITE_BYTE ; SEND
                        LDA         #$0A ; LF
                        JSR         WRITE_BYTE ; SEND
                        PULL        A   ; RESTORE ACC
                        RTS             ; DONE

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_SPACE / PRT_STACK / PRT_A / PRT_X / PRT_Y / PRT_P
; DESCRIPTION: PRINT SINGLE ASCII LABELS
; ----------------------------------------------------------------------------
PRT_SPACE:              PUSH        A
                        LDA         #' '
                        JSR         WRITE_BYTE
                        PULL        A
                        RTS

PRT_STACK:              PUSH        A
                        LDA         #'S'
                        JSR         WRITE_BYTE
                        PULL        A
                        RTS

PRT_A:                  PUSH        A
                        LDA         #'A'
                        JSR         WRITE_BYTE
                        PULL        A
                        RTS

PRT_X:                  PUSH        A
                        LDA         #'X'
                        JSR         WRITE_BYTE
                        PULL        A
                        RTS

PRT_Y:                  PUSH        A
                        LDA         #'Y'
                        JSR         WRITE_BYTE
                        PULL        A
                        RTS

PRT_P:                  PUSH        A
                        LDA         #'P'
                        JSR         WRITE_BYTE
                        PULL        A
                        RTS

PRT_UNDER:              PUSH        A
                        LDA         #'-'
                        JSR         WRITE_BYTE
                        PULL        A
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: INIT_NMI / INIT_IRQ
; DESCRIPTION: SETS HARDWARE JUMP VECTORS IN ZP RAM
; ----------------------------------------------------------------------------
INIT_NMI:               PUSH        A
                        LDA         #$4C ; JMP OPCODE
                        STA         NMI_HOOK ; STORE
                        LDA         #<SYS_NMI ; LO ADDR
                        STA         NMI_HOOK+1
                        LDA         #>SYS_NMI ; HI ADDR
                        STA         NMI_HOOK+2
                        PULL        A
                        RTS

INIT_IRQ:               PUSH        A
                        LDA         #$4C ; JMP OPCODE
                        STA         IRQ_HOOK ; STORE
                        LDA         #<SYS_IRQ ; LO ADDR
                        STA         IRQ_HOOK+1
                        LDA         #>SYS_IRQ ; HI ADDR
                        STA         IRQ_HOOK+2
                        PULL        A
                        RTS

INIT_IRQ_SUBHOOKS:      PUSH        A
                        LDA         #$4C ; JMP OPCODE
                        STA         BRK_HOOK
                        LDA         #<SYS_IRQ_BRK_DISPATCH ; LO ADDR
                        STA         BRK_HOOK+1
                        LDA         #>SYS_IRQ_BRK_DISPATCH ; HI ADDR
                        STA         BRK_HOOK+2
                        LDA         #$4C ; JMP OPCODE
                        STA         HW_HOOK
                        LDA         #<SYS_IRQ_HW_DISPATCH ; LO ADDR
                        STA         HW_HOOK+1
                        LDA         #>SYS_IRQ_HW_DISPATCH ; HI ADDR
                        STA         HW_HOOK+2
                        PULL        A
                        RTS

INIT_RST:
                        SEI             ; DISABLE INTS
                        CLD             ; CLEAR DECIMAL
                        LDX         #$FF ; INIT STACK POINTER
                        TXS             ; MOVE TO SP

        ; --- INIT ALL VECTORS ---
                        JSR         INIT_NMI ; Setup NMI_HOOK trampoline
                        JSR         INIT_IRQ ; Setup IRQ_HOOK trampoline
                        JSR         INIT_IRQ_SUBHOOKS

                        LDA         #$4C ; JMP OPCODE
                        STA         RST_HOOK ; STORE
                        LDA         #<SYS_RST ; LO ADDR
                        STA         RST_HOOK+1
                        LDA         #>SYS_RST ; HI ADDR
                        STA         RST_HOOK+2
                        jmp         RST_HOOK


; ----------------------------------------------------------------------------
; INTERRUPT HANDLERS
; ----------------------------------------------------------------------------
SYS_NMI:                SEI             ; LOCK INTS
                        CLD             ; CLEAR DEC
                        JSR         DEBUG_NMI
                        LDA         #$01 ; ASK ONCE AFTER NMI RETURNS TO MONITOR
                        STA         GAME_ASK_PENDING
                        LDA         #SYSF_NMI_FLAG_M
                        TSB         SYS_FLAGS
                        LDA         #SYSF_GO_FLAG_M
                        BIT         SYS_FLAGS
                        BEQ         ?SNMI_MONITOR_IDLE
                        LDA         #SYSF_GO_FLAG_M
                        TRB         SYS_FLAGS
                        TSX
                        LDA         $101,X ; OLD STACKED P
                        STA         $105,X ; NEW STACKED P AFTER +4 SP ADJUST
                        LDA         #<MONITOR
                        STA         $106,X ; NEW STACKED PC LO
                        LDA         #>MONITOR
                        STA         $107,X ; NEW STACKED PC HI
                        TXA
                        CLC
                        ADC         #$04 ; DROP STALE RTS TRAMPOLINE FRAME
                        TAX
                        TXS
                        RTI             ; DONE (RUN-MODE BREAK TO MONITOR)
?SNMI_MONITOR_IDLE:
                        JSR         PRT_CRLF ; RE-ISSUE PROMPT WHEN INTERRUPTING
                        JSR         PRT_UNDER ; MONITOR-IDLE CODE PATH
                        RTI             ; DONE

STR_NMI_NAME:           DB          "               **NMI_PLACEHOLDER**", 0

SYS_IRQ:                SEI             ; LOCK
                        CLD             ; CLEAR
                        JSR         DEBUG_IRQ
                        LDA         DBG_MODE
                        CMP         #DBG_MODE_BRK
                        BEQ         ?SIRQ_TO_BRK
                        JMP         HW_HOOK
?SIRQ_TO_BRK:
                        JMP         BRK_HOOK
SYS_IRQ_BRK_DISPATCH:
                        LDA         #SYSF_GO_FLAG_M
                        BIT         SYS_FLAGS
                        BEQ         SYS_IRQ_BRK_PATCH_DIRECT
                        LDA         #SYSF_GO_FLAG_M
                        TRB         SYS_FLAGS
                        TSX
                        LDA         $101,X ; OLD STACKED P
                        STA         $105,X ; NEW STACKED P AFTER +4 SP ADJUST
                        LDA         #<MONITOR
                        STA         $106,X ; NEW STACKED PC LO
                        LDA         #>MONITOR
                        STA         $107,X ; NEW STACKED PC HI
                        TXA
                        CLC
                        ADC         #$04 ; DROP STALE RTS TRAMPOLINE FRAME
                        TAX
                        TXS
                        BRA         SYS_IRQ_HW_RTI
SYS_IRQ_BRK_PATCH_DIRECT:
                        TSX
                        LDA         #<MONITOR
                        STA         $102,X ; STACKED PC LO (AFTER RTI)
                        LDA         #>MONITOR
                        STA         $103,X ; STACKED PC HI (AFTER RTI)
SYS_IRQ_HW_DISPATCH:
SYS_IRQ_HW_RTI:
                        RTI             ; DONE

STR_IRQ_NAME:           DB          " > BSO2_IRQ_BRK_HW_DISPATCH", 0
STR_IRQ_BRK:            DB          "     BRK: ", 0
STR_IRQ_HW:             DB          "     HW:  ", 0
STR_IRQ_BRK_NAME:       DB          " **BRK_PLACEHOLDER**", 0
STR_IRQ_HW_NAME:        DB          " **HW_PLACEHOLDER**", 0

; ----------------------------------------------------------------------------
; HARDWARE CONTROL
; ----------------------------------------------------------------------------
INIT_LED:               PUSH        A
                        LDA         #%11111111 ; ALL OUTPUT
                        STA         LED_DDR ; TO PORT A DDR
                        LDA         #$34 ; ENABLE DATA REG
                        STA         PIA_CRA ; BUZZER OFF
                        PULL        A   ; RESTORE
                        RTS

PUT_LED:                STA         LED_DATA ; WRITE TO LEDS
                        RTS

BUZZER_ON:              PUSH        A
                        LDA         #$3C ; CA2 HI
                        STA         PIA_CRA ; PIEZO ON
                        PULL        A
                        RTS

BUZZER_OFF:             PUSH        A
                        LDA         #$34 ; CA2 LO
                        STA         PIA_CRA ; PIEZO OFF
                        PULL        A
                        RTS

DELAY:                  PUSH        X, Y ; SAVE INDEXES
                        LDX         #$00 ; HI DELAY
                        LDY         #$00 ; LO DELAY
?DELAY_LOOP:            DEY             ; DEC LO
                        BNE         ?DELAY_LOOP ; WAIT LO
                        DEX             ; DEC HI
                        BNE         ?DELAY_LOOP ; WAIT HI
                        PULL        Y, X ; RESTORE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DELAY_500MS
; DESCRIPTION: APPROXIMATE 500MS DELAY (W65C02EDU CLOCK-CALIBRATED)
; NOTES:
;   - BUILDS ON DELAY PRIMITIVE.
;   - TUNED FOR PROMPT BLINK CADENCE; ADJUST COUNT IF BOARD CLOCK CHANGES.
; ----------------------------------------------------------------------------
DELAY_500MS:            PUSH        A, X ; SAVE REGS
                        LDX         #$16 ; 22 * DELAY ~= 500MS ON EDU
?D500_LOOP:
                        JSR         DELAY
                        DEX
                        BNE         ?D500_LOOP
                        PULL        X, A ; RESTORE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DELAY_333MS
; DESCRIPTION: APPROXIMATE 333MS DELAY (W65C02EDU CLOCK-CALIBRATED)
; NOTES:
;   - BUILDS ON DELAY PRIMITIVE.
;   - KEEPS DELAY_500MS AVAILABLE; THIS IS FOR FASTER BLINK/POLL CADENCE.
; ----------------------------------------------------------------------------
DELAY_333MS:            PUSH        A, X ; SAVE REGS
                        LDX         #$0F ; 15 * DELAY ~= 333MS ON EDU
?D333_LOOP:
                        JSR         DELAY
                        DEX
                        BNE         ?D333_LOOP
                        PULL        X, A ; RESTORE
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: MEM_DISASM_65C02
; DESCRIPTION: DISASSEMBLES MEMORY RANGE AS W65C02 ASSEMBLY
; INPUT: PTR_DUMP_CUR (ZP) = START ADDR (INCLUSIVE)
;        PTR_TEMP     (ZP) = END ADDR (INCLUSIVE)
; OUTPUT: DISASSEMBLY LINES TO UART
; FLAGS: UNCHANGED
; ZP USED: PTR_DUMP_CUR, PTR_DUMP_END, PTR_TEMP, PTR_LEG, DIS_*
; ----------------------------------------------------------------------------
MEM_DISASM_65C02:
                        LDA         PTR_TEMP ; SAVE INCLUSIVE END
                        STA         PTR_DUMP_END
                        LDA         PTR_TEMP+1
                        STA         PTR_DUMP_END+1
                        JSR         PRT_CRLF

?MDIS_NEXT:
        ; DONE IF CURSOR > END (INCLUSIVE COMPARISON)
                        LDA         PTR_DUMP_CUR+1
                        CMP         PTR_DUMP_END+1
                        BCC         ?MDIS_HAVE_DATA
                        BNE         ?MDIS_HI_GT
                        LDA         PTR_DUMP_CUR
                        CMP         PTR_DUMP_END
                        BCC         ?MDIS_HAVE_DATA
                        BEQ         ?MDIS_HAVE_DATA
                        JMP         ?MDIS_DONE
?MDIS_HI_GT:
                        JMP         ?MDIS_DONE

?MDIS_HAVE_DATA:
                        LDY         #$00
                        LDA         (PTR_DUMP_CUR),Y
                        STA         DIS_OPCODE
                        TAX
                        LDA         U_OP_MODE_TAB,X
                        STA         DIS_MODE
                        LDA         U_OP_LEN_TAB,X
                        STA         DIS_LEN

        ; --- PRINT ADDRESS PREFIX "HHLL: " ---
                        LDA         PTR_DUMP_CUR+1
                        JSR         PRT_HEX
                        LDA         PTR_DUMP_CUR
                        JSR         PRT_HEX
                        LDA         #':'
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE

        ; --- PRINT MNEMONIC (3 CHARS) ---
        ; PTR_TEMP = &U_OP_MNEM_TAB + (OPCODE * 3)
                        LDA         DIS_OPCODE
                        ASL         A
                        STA         PTR_TEMP
                        LDA         #$00
                        ROL         A
                        STA         PTR_TEMP+1
                        CLC
                        LDA         PTR_TEMP
                        ADC         DIS_OPCODE
                        STA         PTR_TEMP
                        LDA         PTR_TEMP+1
                        ADC         #$00
                        STA         PTR_TEMP+1
                        CLC
                        LDA         PTR_TEMP
                        ADC         #<U_OP_MNEM_TAB
                        STA         PTR_TEMP
                        LDA         PTR_TEMP+1
                        ADC         #>U_OP_MNEM_TAB
                        STA         PTR_TEMP+1

                        LDY         #$00
                        LDA         (PTR_TEMP),Y
                        JSR         WRITE_BYTE
                        INY
                        LDA         (PTR_TEMP),Y
                        JSR         WRITE_BYTE
                        INY
                        LDA         (PTR_TEMP),Y
                        JSR         WRITE_BYTE
                        JSR         DIS_PRINT_BIT_SUFFIX

        ; --- PRINT OPERAND ---
                        LDA         DIS_MODE
                        BEQ         ?MDIS_ADVANCE ; IMPLIED
                        JSR         PRT_SPACE
                        JSR         DIS_PRINT_OPERAND

?MDIS_ADVANCE:
                        CLC
                        LDA         PTR_DUMP_CUR
                        ADC         DIS_LEN
                        STA         PTR_DUMP_CUR
                        LDA         PTR_DUMP_CUR+1
                        ADC         #$00
                        STA         PTR_DUMP_CUR+1
                        BCS         ?MDIS_DONE ; WRAP ENDS RANGE
                        JSR         PRT_CRLF
                        JMP         ?MDIS_NEXT

?MDIS_DONE:
                        JSR         PRT_CRLF
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_PRINT_BIT_SUFFIX
; DESCRIPTION: PRINTS RMB/SMB/BBR/BBS BIT DIGIT (0-7) WHEN APPLICABLE
; INPUT: DIS_OPCODE
; OUTPUT: OPTIONAL DIGIT WRITTEN
; ----------------------------------------------------------------------------
DIS_PRINT_BIT_SUFFIX:
                        LDA         DIS_OPCODE
                        AND         #$0F
                        CMP         #$07
                        BEQ         ?DBS_DO
                        CMP         #$0F
                        BNE         ?DBS_DONE
?DBS_DO:
                        LDA         DIS_OPCODE
                        LSR         A
                        LSR         A
                        LSR         A
                        LSR         A
                        AND         #$07
                        CLC
                        ADC         #'0'
                        JSR         WRITE_BYTE
?DBS_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_PRINT_OPERAND
; DESCRIPTION: PRINTS OPERAND FOR CURRENT DISASSEMBLED OPCODE
; INPUT: DIS_MODE, PTR_DUMP_CUR
; ----------------------------------------------------------------------------
DIS_PRINT_OPERAND:
                        LDA         DIS_MODE
                        CMP         #DISM_ACC
                        BNE         ?DPO_NOT_ACC
                        LDA         #'A'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_ACC:
                        CMP         #DISM_IMM
                        BNE         ?DPO_NOT_IMM
                        LDA         #'#'
                        JSR         WRITE_BYTE
                        JSR         DIS_GET_OP1
                        JSR         DIS_PRT_DOLLAR_A
                        RTS
?DPO_NOT_IMM:
                        CMP         #DISM_ZP
                        BNE         ?DPO_NOT_ZP
                        JSR         DIS_GET_OP1
                        JSR         DIS_PRT_DOLLAR_A
                        RTS
?DPO_NOT_ZP:
                        CMP         #DISM_ZPX
                        BNE         ?DPO_NOT_ZPX
                        JSR         DIS_GET_OP1
                        JSR         DIS_PRT_DOLLAR_A
                        LDA         #','
                        JSR         WRITE_BYTE
                        LDA         #'X'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_ZPX:
                        CMP         #DISM_ZPY
                        BNE         ?DPO_NOT_ZPY
                        JSR         DIS_GET_OP1
                        JSR         DIS_PRT_DOLLAR_A
                        LDA         #','
                        JSR         WRITE_BYTE
                        LDA         #'Y'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_ZPY:
                        CMP         #DISM_ABS
                        BNE         ?DPO_NOT_ABS
                        JSR         DIS_GET_OPWORD_PTRTEMP
                        JSR         DIS_PRT_DOLLAR_PTRTEMP
                        RTS
?DPO_NOT_ABS:
                        CMP         #DISM_ABSX
                        BNE         ?DPO_NOT_ABSX
                        JSR         DIS_GET_OPWORD_PTRTEMP
                        JSR         DIS_PRT_DOLLAR_PTRTEMP
                        LDA         #','
                        JSR         WRITE_BYTE
                        LDA         #'X'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_ABSX:
                        CMP         #DISM_ABSY
                        BNE         ?DPO_NOT_ABSY
                        JSR         DIS_GET_OPWORD_PTRTEMP
                        JSR         DIS_PRT_DOLLAR_PTRTEMP
                        LDA         #','
                        JSR         WRITE_BYTE
                        LDA         #'Y'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_ABSY:
                        CMP         #DISM_IND
                        BNE         ?DPO_NOT_IND
                        LDA         #'('
                        JSR         WRITE_BYTE
                        JSR         DIS_GET_OPWORD_PTRTEMP
                        JSR         DIS_PRT_DOLLAR_PTRTEMP
                        LDA         #')'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_IND:
                        CMP         #DISM_INDX
                        BNE         ?DPO_NOT_INDX
                        LDA         #'('
                        JSR         WRITE_BYTE
                        JSR         DIS_GET_OP1
                        JSR         DIS_PRT_DOLLAR_A
                        LDA         #','
                        JSR         WRITE_BYTE
                        LDA         #'X'
                        JSR         WRITE_BYTE
                        LDA         #')'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_INDX:
                        CMP         #DISM_INDY
                        BNE         ?DPO_NOT_INDY
                        LDA         #'('
                        JSR         WRITE_BYTE
                        JSR         DIS_GET_OP1
                        JSR         DIS_PRT_DOLLAR_A
                        LDA         #')'
                        JSR         WRITE_BYTE
                        LDA         #','
                        JSR         WRITE_BYTE
                        LDA         #'Y'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_INDY:
                        CMP         #DISM_REL
                        BNE         ?DPO_NOT_REL
                        LDY         #$01
                        LDA         (PTR_DUMP_CUR),Y
                        STA         CMD_PARSE_NIB
                        LDA         PTR_DUMP_CUR
                        CLC
                        ADC         #$02
                        STA         PTR_LEG
                        LDA         PTR_DUMP_CUR+1
                        ADC         #$00
                        STA         PTR_LEG+1
                        JSR         DIS_ADD_SIGNED_OFF_TO_PTRLEG
                        JSR         DIS_PRT_DOLLAR_PTRLEG
                        RTS
?DPO_NOT_REL:
                        CMP         #DISM_ZPIND
                        BNE         ?DPO_NOT_ZPIND
                        LDA         #'('
                        JSR         WRITE_BYTE
                        JSR         DIS_GET_OP1
                        JSR         DIS_PRT_DOLLAR_A
                        LDA         #')'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_ZPIND:
                        CMP         #DISM_ABSINDX
                        BNE         ?DPO_NOT_ABSINDX
                        LDA         #'('
                        JSR         WRITE_BYTE
                        JSR         DIS_GET_OPWORD_PTRTEMP
                        JSR         DIS_PRT_DOLLAR_PTRTEMP
                        LDA         #','
                        JSR         WRITE_BYTE
                        LDA         #'X'
                        JSR         WRITE_BYTE
                        LDA         #')'
                        JSR         WRITE_BYTE
                        RTS
?DPO_NOT_ABSINDX:
                        CMP         #DISM_ZPREL
                        BNE         ?DPO_DONE
                        LDY         #$01
                        LDA         (PTR_DUMP_CUR),Y
                        JSR         DIS_PRT_DOLLAR_A
                        LDA         #','
                        JSR         WRITE_BYTE
                        INY
                        LDA         (PTR_DUMP_CUR),Y
                        STA         CMD_PARSE_NIB
                        LDA         PTR_DUMP_CUR
                        CLC
                        ADC         #$03
                        STA         PTR_LEG
                        LDA         PTR_DUMP_CUR+1
                        ADC         #$00
                        STA         PTR_LEG+1
                        JSR         DIS_ADD_SIGNED_OFF_TO_PTRLEG
                        JSR         DIS_PRT_DOLLAR_PTRLEG
                        RTS
?DPO_DONE:
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_GET_OP1
; DESCRIPTION: LOADS FIRST OPERAND BYTE (PC+1) INTO A
; ----------------------------------------------------------------------------
DIS_GET_OP1:
                        LDY         #$01
                        LDA         (PTR_DUMP_CUR),Y
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_GET_OPWORD_PTRTEMP
; DESCRIPTION: LOADS OPERAND WORD (PC+1..PC+2) INTO PTR_TEMP (LO/HI)
; ----------------------------------------------------------------------------
DIS_GET_OPWORD_PTRTEMP:
                        LDY         #$01
                        LDA         (PTR_DUMP_CUR),Y
                        STA         PTR_TEMP
                        INY
                        LDA         (PTR_DUMP_CUR),Y
                        STA         PTR_TEMP+1
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_PRT_DOLLAR_A
; DESCRIPTION: PRINTS "$" + HEX BYTE FROM A
; ----------------------------------------------------------------------------
DIS_PRT_DOLLAR_A:
                        PUSH        A
                        LDA         #'$'
                        JSR         WRITE_BYTE
                        PULL        A
                        JSR         PRT_HEX
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_PRT_DOLLAR_PTRTEMP
; DESCRIPTION: PRINTS "$" + HEX WORD FROM PTR_TEMP (HI THEN LO)
; ----------------------------------------------------------------------------
DIS_PRT_DOLLAR_PTRTEMP:
                        LDA         #'$'
                        JSR         WRITE_BYTE
                        LDA         PTR_TEMP+1
                        JSR         PRT_HEX
                        LDA         PTR_TEMP
                        JSR         PRT_HEX
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_PRT_DOLLAR_PTRLEG
; DESCRIPTION: PRINTS "$" + HEX WORD FROM PTR_LEG (HI THEN LO)
; ----------------------------------------------------------------------------
DIS_PRT_DOLLAR_PTRLEG:
                        LDA         #'$'
                        JSR         WRITE_BYTE
                        LDA         PTR_LEG+1
                        JSR         PRT_HEX
                        LDA         PTR_LEG
                        JSR         PRT_HEX
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: DIS_ADD_SIGNED_OFF_TO_PTRLEG
; DESCRIPTION: PTR_LEG += SIGNED 8-BIT OFFSET IN CMD_PARSE_NIB
; ----------------------------------------------------------------------------
DIS_ADD_SIGNED_OFF_TO_PTRLEG:
                        LDA         CMD_PARSE_NIB
                        STA         PTR_TEMP
                        LDA         #$00
                        BIT         PTR_TEMP
                        BPL         ?DAS_POS
                        LDA         #$FF
?DAS_POS:
                        STA         PTR_TEMP+1
                        CLC
                        LDA         PTR_LEG
                        ADC         PTR_TEMP
                        STA         PTR_LEG
                        LDA         PTR_LEG+1
                        ADC         PTR_TEMP+1
                        STA         PTR_LEG+1
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: MEM_DUMP
; DESCRIPTION: PRINTS HEX DUMP OF MEMORY RANGE WITH ASCII TRANSLATION
; INPUT: PTR_DUMP_CUR (ZP) = START ADDR (INCLUSIVE)
;        PTR_TEMP (ZP)     = END ADDR (EXCLUSIVE)
; OUTPUT: HEX DUMP TO UART
; FLAGS: UNCHANGED
; ZP USED: PTR_DUMP_CUR, PTR_DUMP_END, PTR_LEG
; ----------------------------------------------------------------------------
MEM_DUMP:
        ; --- EXCLUSIVE END RANGE MODE ---
        ; PTR_TEMP holds the exclusive end.
        ; END=$0000 is treated as exclusive $10000 (dump until wrap).
                        LDA         PTR_TEMP ; Move Arg2 Lo
                        STA         PTR_DUMP_END ; To End Lo
                        LDA         PTR_TEMP+1 ; Move Arg2 Hi
                        STA         PTR_DUMP_END+1 ; To End Hi

?START_DUMP:
                        JSR         PRT_CRLF ; NEW LINE
?LINE_LOOP:
        ; --- Determine bytes to print on this line ---
                        LDY         #$00

?COUNT_LOOP:
                        CPY         #16
                        BEQ         ?COUNT_DONE
                        TYA
                        CLC
                        ADC         PTR_DUMP_CUR
                        STA         PTR_LEG
                        LDA         PTR_DUMP_CUR+1
                        ADC         #$00
                        STA         PTR_LEG+1
        ; END=$0000 is a sentinel for exclusive $10000.
        ; In that mode, stop this line only if START+Y wrapped.
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BNE         ?COUNT_CHECK_END
                        BCS         ?COUNT_DONE
                        BRA         ?COUNT_INC
?COUNT_CHECK_END:
                        LDA         PTR_LEG
                        CMP         PTR_DUMP_END
                        BNE         ?COUNT_INC
                        LDA         PTR_LEG+1
                        CMP         PTR_DUMP_END+1
                        BEQ         ?COUNT_DONE
?COUNT_INC:
                        INY
                        BRA         ?COUNT_LOOP

?COUNT_DONE:
                        STY         MEM_DUMP_CNT
                        CPY         #$00
                        BNE         ?PRINT_LINE
                        JMP         ?FINISHED

?PRINT_LINE:
                        LDA         PTR_DUMP_CUR+1 ; ADDR HI
                        JSR         PRT_HEX ; PRINT
                        LDA         PTR_DUMP_CUR ; ADDR LO
                        JSR         PRT_HEX ; PRINT
                        LDA         #':' ; COLON
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE ; SPACE

                        LDY         #$00 ; RESET LINE INDEX
?BYTE_LOOP:
                        CPY         MEM_DUMP_CNT
                        BEQ         ?HEX_PAD
                        LDA         (PTR_DUMP_CUR),Y ; READ MEMORY
                        JSR         PRT_HEX ; PRINT HEX
                        JSR         PRT_SPACE ; SPACE
                        INY             ; NEXT
                        CPY         #$08
                        BNE         ?BYTE_LOOP
                        LDA         #'|' ; 8+8 VISUAL SPLIT
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE
                        BRA         ?BYTE_LOOP

?HEX_PAD:
                        CPY         #16
                        BEQ         ?ASCII_START
                        JSR         PRT_SPACE ; PAD "   " FOR MISSING BYTE
                        JSR         PRT_SPACE
                        JSR         PRT_SPACE
                        INY
                        CPY         #$08
                        BNE         ?HEX_PAD
                        LDA         #'|' ; KEEP SPLIT COLUMN ALIGNED
                        JSR         WRITE_BYTE
                        JSR         PRT_SPACE
                        BRA         ?HEX_PAD

        ; --- Print ASCII translation column ---
?ASCII_START:
                        JSR         PRT_SPACE ; SPACE
                        LDA         #'|' ; DELIMITER
                        JSR         WRITE_BYTE
                        LDY         #$00 ; RESET INDEX
?ASCII_LOOP:
                        CPY         MEM_DUMP_CNT
                        BEQ         ?ASCII_DONE
                        LDA         (PTR_DUMP_CUR),Y ; READ BYTE
                        CMP         #$20 ; TEST PRINTABLE
                        BCC         ?NON_PRINTABLE
                        CMP         #$7F ; TEST PRINTABLE
                        BCS         ?NON_PRINTABLE
                        BRA         ?DO_PRINT ; IS PRINTABLE
?NON_PRINTABLE:
                        LDA         #'.' ; USE DOT FOR NON-PRINT
?DO_PRINT:
                        JSR         WRITE_BYTE ; SEND TO UART
                        INY             ; NEXT
                        CPY         #$08
                        BNE         ?ASCII_LOOP
                        LDA         #'|' ; 8+8 VISUAL SPLIT IN ASCII FIELD
                        JSR         WRITE_BYTE
                        BRA         ?ASCII_LOOP
?ASCII_DONE:
                        LDA         #'|' ; DELIMITER
                        JSR         WRITE_BYTE

        ; --- Advance by bytes actually printed on this line ---
                        LDA         PTR_DUMP_CUR ; ADDR LO
                        CLC
                        ADC         MEM_DUMP_CNT
                        STA         PTR_LEG ; SCRATCH RESULT (NEXT ADDR LO)
                        LDA         PTR_DUMP_CUR+1 ; ADDR HI
                        ADC         #$00
                        STA         PTR_LEG+1 ; SCRATCH RESULT (NEXT ADDR HI)

        ; WRAP-AROUND ENDS DUMP
                        BCS         ?FINISHED

        ; END=$0000 MEANS EXCLUSIVE $10000: CONTINUE UNTIL WRAP
                        LDA         PTR_DUMP_END
                        ORA         PTR_DUMP_END+1
                        BEQ         ?CONTINUE_DUMP

        ; Stop when NEXT ADDR reaches END (exclusive)
                        LDA         PTR_LEG
                        CMP         PTR_DUMP_END
                        BNE         ?CONTINUE_DUMP
                        LDA         PTR_LEG+1
                        CMP         PTR_DUMP_END+1
                        BEQ         ?FINISHED

?CONTINUE_DUMP:
                        LDA         PTR_LEG ; UPDATE CURSOR
                        STA         PTR_DUMP_CUR
                        LDA         PTR_LEG+1
                        STA         PTR_DUMP_CUR+1
                        JSR         PRT_CRLF ; NEW LINE
                        JMP         ?LINE_LOOP ; NEXT LINE

?FINISHED:
                        JSR         PRT_CRLF ; DONE
                        RTS
; ----------------------------------------------------------------------------
; SUBROUTINE: SHOW_VECTORS
; DESCRIPTION: DISPLAYS INTERRUPT CHAINS (HW -> JMP -> JMP -> CODE)
; ----------------------------------------------------------------------------
SHOW_VECTORS:
                        JSR         PRT_CRLF

        ; --- SHOW RESET CHAIN ---
                        PRT_CSTRING STR_RST ; "RST: "
                        LDA         #<HW_VEC_RST_ADDR
                        STA         PTR_TEMP
                        LDA         #>HW_VEC_RST_ADDR
                        STA         PTR_TEMP+1
                        JSR         FOLLOW_CHAIN
                        PRT_CSTRING STR_RST_NAME
                        JSR         PRT_CRLF

        ; --- SHOW NMI CHAIN ---
                        PRT_CSTRING STR_NMI ; "NMI: "
                        LDA         #<HW_VEC_NMI_ADDR
                        STA         PTR_TEMP
                        LDA         #>HW_VEC_NMI_ADDR
                        STA         PTR_TEMP+1
                        JSR         FOLLOW_CHAIN
                        PRT_CSTRING STR_NMI_NAME
                        JSR         PRT_CRLF

        ; --- SHOW IRQ CHAIN ---
                        PRT_CSTRING STR_IRQ ; "IRQ: "
                        LDA         #<HW_VEC_IRQ_ADDR
                        STA         PTR_TEMP
                        LDA         #>HW_VEC_IRQ_ADDR
                        STA         PTR_TEMP+1
                        JSR         FOLLOW_CHAIN
                        PRT_CSTRING STR_IRQ_NAME
                        JSR         PRT_CRLF
                        PRT_CSTRING STR_IRQ_BRK
                        LDA         #'['
                        JSR         WRITE_BYTE
                        LDA         #>ZP_BRK_HOOK_ADDR
                        LDX         #<ZP_BRK_HOOK_ADDR
                        JSR         PRT_HEX_WORD_AX
                        LDA         #']'
                        JSR         WRITE_BYTE
                        PRT_CSTRING STR_ARROW
                        LDA         BRK_HOOK+2
                        LDX         BRK_HOOK+1
                        JSR         PRT_HEX_WORD_AX
                        PRT_CSTRING STR_IRQ_BRK_NAME
                        JSR         PRT_CRLF
                        PRT_CSTRING STR_IRQ_HW
                        LDA         #'['
                        JSR         WRITE_BYTE
                        LDA         #>ZP_HW_HOOK_ADDR
                        LDX         #<ZP_HW_HOOK_ADDR
                        JSR         PRT_HEX_WORD_AX
                        LDA         #']'
                        JSR         WRITE_BYTE
                        PRT_CSTRING STR_ARROW
                        LDA         HW_HOOK+2
                        LDX         HW_HOOK+1
                        JSR         PRT_HEX_WORD_AX
                        PRT_CSTRING STR_IRQ_HW_NAME
                        JSR         PRT_CRLF
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: FOLLOW_CHAIN
; DESCRIPTION: RECURSIVELY FOLLOWS JMP ($4C) AND JMP() ($6C)
; INPUT: PTR_TEMP = ADDRESS OF HARDWARE VECTOR (e.g., $FFFC)
; ----------------------------------------------------------------------------
FOLLOW_CHAIN:
        ; 1. PRINT INITIAL HARDWARE VECTOR ADDRESS (e.g., FFFC)
                        LDA         PTR_TEMP+1
                        JSR         PRT_HEX
                        LDA         PTR_TEMP
                        JSR         PRT_HEX

        ; 2. DEREFERENCE THE HARDWARE VECTOR TO START THE CHAIN
                        LDY         #0
                        LDA         (PTR_TEMP),Y ; Read Target Low
                        STA         PTR_LEG
                        INY
                        LDA         (PTR_TEMP),Y ; Read Target High
                        STA         PTR_LEG+1

?CHAIN_LOOP:
                        PRT_CSTRING STR_ARROW ; " > "

        ; --- TRAMPOLINE CHECK: IF HI BYTE IS 00, WRAP IN BRACKETS ---
                        LDA         PTR_LEG+1
                        BNE         ?PRINT_LINK
                        LDA         #'['
                        JSR         WRITE_BYTE
?PRINT_LINK:
                        LDA         PTR_LEG+1
                        JSR         PRT_HEX
                        LDA         PTR_LEG
                        JSR         PRT_HEX
                        LDA         PTR_LEG+1
                        BNE         ?BRIDGE_CHECK
                        LDA         #']'
                        JSR         WRITE_BYTE

?BRIDGE_CHECK:
        ; --- BRIDGE 1: WDC ROM -> User RAM (F818 -> 8004) ---
                        LDA         PTR_LEG+1
                        CMP         #$F8
                        BNE         ?CHECK_BRIDGE_2
                        LDA         PTR_LEG
                        CMP         #$18
                        BNE         ?CHECK_BRIDGE_2
                        LDA         #$04 ; Target 8004
                        STA         PTR_LEG
                        LDA         #$80
                        STA         PTR_LEG+1
                        BRA         ?CHAIN_LOOP

?CHECK_BRIDGE_2:
        ; --- BRIDGE 2: INIT_RST -> RST_HOOK ($0080) ---
                        LDA         PTR_LEG+1
                        CMP         #>INIT_RST
                        BNE         ?NORMAL_TRACE
                        LDA         PTR_LEG
                        CMP         #<INIT_RST
                        BNE         ?NORMAL_TRACE
                        LDA         #<RST_HOOK ; Force to ZP hook $80
                        STA         PTR_LEG
                        LDA         #>RST_HOOK
                        STA         PTR_LEG+1
                        BRA         ?CHAIN_LOOP

?NORMAL_TRACE:
        ; Prepare to read the instruction at the current address
                        LDA         PTR_LEG
                        STA         PTR_TEMP
                        LDA         PTR_LEG+1
                        STA         PTR_TEMP+1

                        LDY         #0
                        LDA         (PTR_TEMP),Y ; Read Opcode
                        CMP         #$4C ; JMP Absolute?
                        BEQ         ?FOUND_JMP
                        CMP         #$6C ; JMP Indirect?
                        BEQ         ?FOUND_IND
                        BRA         ?END_CHAIN ; Not a jump, we found code

?FOUND_JMP:
                        INY             ; Y=1
                        LDA         (PTR_TEMP),Y ; Read New Target Low
                        STA         PTR_LEG
                        INY             ; Y=2
                        LDA         (PTR_TEMP),Y ; Read New Target High
                        STA         PTR_LEG+1
                        BRA         ?CHAIN_LOOP

?FOUND_IND:
                        INY             ; Y=1
                        LDA         (PTR_TEMP),Y ; Read Pointer Low
                        STA         PTR_LEG
                        INY             ; Y=2
                        LDA         (PTR_TEMP),Y ; Read Pointer High
                        STA         PTR_LEG+1

        ; Dereference the indirect pointer
                        LDA         PTR_LEG
                        STA         PTR_TEMP
                        LDA         PTR_LEG+1
                        STA         PTR_TEMP+1

                        LDY         #0
                        LDA         (PTR_TEMP),Y ; Destination Low
                        STA         PTR_LEG
                        INY
                        LDA         (PTR_TEMP),Y ; Destination High
                        STA         PTR_LEG+1
                        JMP         ?CHAIN_LOOP

?END_CHAIN:
                        RTS

; ----------------------------------------------------------------------------
; STABLE UTILITY ROUTINES (LOW CHURN)
; ----------------------------------------------------------------------------

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

; ----------------------------------------------------------------------------
; SUBROUTINE: CHECK_BYTE
; DESCRIPTION: CHECKS UART STATUS
; INPUT: NONE
; OUTPUT: ACC = STATUS
; FLAGS: CARRY SET IF BUFFER EMPTY
; ZP USED: NONE
; ----------------------------------------------------------------------------
CHECK_BYTE:
                        JSR         WDC_CHECK_BYTE ; CALL ROM STATUS
                        RTS             ; DONE

; ----------------------------------------------------------------------------
; PROVENANCE NOTE:
; The hex-to-ASCII adjustment pattern below follows the method described in:
;   - "6502 Assembly Language Programming" (Lance A. Leventhal), p. 7-3
; Leventhal cites (superscript 1; bibliographic entry listed on p. 7-15):
;   - D. R. Allison, "A Design Philosophy for Microcomputer Architectures."
;     Computer, February 1977, pp. 35-41.
; ----------------------------------------------------------------------------
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

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_HEX
; DESCRIPTION: PRINTS 8-BIT VALUE IN HEX
; INPUT: ACC = BYTE
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: NONE
; ----------------------------------------------------------------------------
PRT_HEX:
                        PUSH        A   ; SAVE ACC
                        LSR         A   ; SHIFT HI NIBBLE
                        LSR         A   ; DOWN
                        LSR         A   ; TO
                        LSR         A   ; LO
                        JSR         CVT_PRT_NIBBLE ; PRINT HI NIBBLE
                        PULL        A   ; RESTORE ACC
                        AND         #%00001111 ; MASK LO NIBBLE
                        JSR         CVT_PRT_NIBBLE ; PRINT LO NIBBLE
                        RTS             ; DONE

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_HEX_WORD_AX
; DESCRIPTION: PRINTS 16-BIT WORD IN HEX (HIGH BYTE THEN LOW BYTE)
; INPUT: A = HIGH BYTE, X = LOW BYTE
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ----------------------------------------------------------------------------
PRT_HEX_WORD_AX:
                        PUSH        X
                        JSR         PRT_HEX
                        PULL        X
                        TXA
                        JSR         PRT_HEX
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_WORD_FROM_PARSE
; DESCRIPTION: PRINTS CMD_PARSE_VAL AS 16-BIT HEX (HI THEN LO)
; INPUT: CMD_PARSE_VAL/CMD_PARSE_VAL+1
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ----------------------------------------------------------------------------
PRT_WORD_FROM_PARSE:
                        LDA         CMD_PARSE_VAL+1
                        LDX         CMD_PARSE_VAL
                        JSR         PRT_HEX_WORD_AX
                        RTS

; ----------------------------------------------------------------------------
; SUBROUTINE: PRT_C_STRING
; DESCRIPTION: PRINTS NULL-TERMINATED STRING
; INPUT: STR_PTR (ZP) = ADDR OF STRING
; OUTPUT: NONE
; FLAGS: UNCHANGED
; ZP USED: STR_PTR
; ----------------------------------------------------------------------------
PRT_C_STRING:           PUSH        A, Y ; SAVE REGS
                        LDY         #$00 ; RESET INDEX
?STRING_LOOP:           LDA         (STR_PTR),Y ; GET CHAR
                        BEQ         ?STRING_DONE ; EXIT IF NULL
                        JSR         WRITE_BYTE ; SEND CHAR
                        INY             ; NEXT INDEX
                        BNE         ?STRING_LOOP ; LOOP IF NOT WRAP
                        INC         STR_PTR+1 ; CROSS PAGE
                        BRA         ?STRING_LOOP ; REPEAT
?STRING_DONE:           PULL        Y, A ; RESTORE
                        RTS             ; DONE

MAIN_INIT:
                        JSR         INIT_IRQ ; SETUP IRQ JUMP
                        JSR         INIT_NMI ; SETUP NMI JUMP

                        JSR         INIT_SERIAL ; SETUP UART
                        JSR         INIT_LED ; SETUP LED PORT

                        stz         BRK_FLAG
                        stz         STEP_ACTIVE
                        LDA         #SYSF_NMI_FLAG_M
                        TRB         SYS_FLAGS
                        RTS
; ----------------------------------------------------------------------------
; DATA SEGMENT
; ----------------------------------------------------------------------------
                        KDATA
; ----------------------------------------------------------------------------
; TABLE: U_OP_MODE_TAB
; DESCRIPTION: ADDRESSING MODE ID PER OPCODE (00-FF)
; ----------------------------------------------------------------------------
U_OP_MODE_TAB:
                        DB          $02, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $01, $00, $06, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $03, $04, $04, $03, $00, $08, $01, $00, $06, $07, $07, $0F
                        DB          $06, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $01, $00, $06, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $04, $04, $04, $03, $00, $08, $01, $00, $07, $07, $07, $0F
                        DB          $00, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $01, $00, $06, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $04, $04, $04, $03, $00, $08, $00, $00, $06, $07, $07, $0F
                        DB          $00, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $01, $00, $09, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $04, $04, $04, $03, $00, $08, $00, $00, $0E, $07, $07, $0F
                        DB          $0C, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $00, $00, $06, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $04, $04, $05, $03, $00, $08, $00, $00, $06, $07, $07, $0F
                        DB          $02, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $00, $00, $06, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $04, $04, $05, $03, $00, $08, $00, $00, $07, $07, $08, $0F
                        DB          $02, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $00, $00, $06, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $04, $04, $04, $03, $00, $08, $00, $00, $07, $07, $07, $0F
                        DB          $02, $0A, $02, $00, $03, $03, $03, $03, $00, $02, $00, $00, $06, $06, $06, $0F
                        DB          $0C, $0B, $0D, $00, $04, $04, $04, $03, $00, $08, $00, $00, $07, $07, $07, $0F

; ----------------------------------------------------------------------------
; TABLE: U_OP_LEN_TAB
; DESCRIPTION: INSTRUCTION LENGTH PER OPCODE (00-FF)
; ----------------------------------------------------------------------------
U_OP_LEN_TAB:
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03
                        DB          $03, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03
                        DB          $01, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03
                        DB          $01, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $02, $01, $01, $03, $03, $03, $03
                        DB          $02, $02, $02, $01, $02, $02, $02, $02, $01, $03, $01, $01, $03, $03, $03, $03

; ----------------------------------------------------------------------------
; TABLE: U_OP_MNEM_TAB
; DESCRIPTION: 3-CHAR MNEMONIC PER OPCODE (00-FF), TIGHT-PACKED
; ----------------------------------------------------------------------------
U_OP_MNEM_TAB:
                        DB          "BRKORANOPNOPTSBORAASLRMB"
                        DB          "PHPORAASLNOPTSBORAASLBBR"
                        DB          "BPLORAORANOPTRBORAASLRMB"
                        DB          "CLCORAINCNOPTRBORAASLBBR"
                        DB          "JSRANDNOPNOPBITANDROLRMB"
                        DB          "PLPANDROLNOPBITANDROLBBR"
                        DB          "BMIANDANDNOPBITANDROLRMB"
                        DB          "SECANDDECNOPBITANDROLBBR"
                        DB          "RTIEORNOPNOPNOPEORLSRRMB"
                        DB          "PHAEORLSRNOPJMPEORLSRBBR"
                        DB          "BVCEOREORNOPNOPEORLSRRMB"
                        DB          "CLIEORPHYNOPNOPEORLSRBBR"
                        DB          "RTSADCNOPNOPSTZADCRORRMB"
                        DB          "PLAADCRORNOPJMPADCRORBBR"
                        DB          "BVSADCADCNOPSTZADCRORRMB"
                        DB          "SEIADCPLYNOPJMPADCRORBBR"
                        DB          "BRASTANOPNOPSTYSTASTXSMB"
                        DB          "DEYBITTXANOPSTYSTASTXBBS"
                        DB          "BCCSTASTANOPSTYSTASTXSMB"
                        DB          "TYASTATXSNOPSTZSTASTZBBS"
                        DB          "LDYLDALDXNOPLDYLDALDXSMB"
                        DB          "TAYLDATAXNOPLDYLDALDXBBS"
                        DB          "BCSLDALDANOPLDYLDALDXSMB"
                        DB          "CLVLDATSXNOPLDYLDALDXBBS"
                        DB          "CPYCMPNOPNOPCPYCMPDECSMB"
                        DB          "INYCMPDEXWAICPYCMPDECBBS"
                        DB          "BNECMPCMPNOPNOPCMPDECSMB"
                        DB          "CLDCMPPHXSTPNOPCMPDECBBS"
                        DB          "CPXSBCNOPNOPCPXSBCINCSMB"
                        DB          "INXSBCNOPNOPCPXSBCINCBBS"
                        DB          "BEQSBCSBCNOPNOPSBCINCSMB"
                        DB          "SEDSBCPLXNOPNOPSBCINCBBS"

BSO2_INIT:              DB          $0D, $0A, $0D, $0A
                        DB          "     **** basic system operations/2 ****"
                        DB          $0D, $0A
                        DB          "     ****       b s o / 2  v0 . 9   ****"
                        DB          $0D, $0A
                        DB          "     ****         6 5 0 2           ****"
                        DB          $0D, $0A, $0D, $0A, 0
OSI:                    DB          $0D, $0A, "C/W/M", 0
OSI_CM:                 DB          $0D, $0A, "C/M", 0
MSG_RESET_TRIGGERED:    DB          $0D, $0A, "RESET TRIGGERED", 0
MSG_CLR_CONFIRM:        DB          $0D, $0A, "CLEAR MEMORY? (Y/N)", 0
MSG_POWER_ON:           DB          $0D, $0A, "POWER ON", 0
MSG_RAM_CLEARED:        DB          $0D, $0A, "RAM CLEARED", 0
MSG_WARMSTART:          DB          "WARMSTART", 0
MSG_RAM_NOT_CLEARED:    DB          $0D, $0A, "RAM NOT CLEARED", 0
MSG_TERM_WIDTH_PROMPT:  DB          $0D, $0A
                        DB          "TERM WIDTH 4=40 8=80 1=132 [8]?", 0
MSG_HELP_BOOT_SHORT:    DB          $0D, $0A
                        DB          "HELP:? H  CTRL:Q W Z  EXEC:G N R X  MEM:A C "
                        DB          "D F L M S U V", 0
MSG_HELP_SHORT:         DB          $0D, $0A
                        DB          "HELP:? H  CTRL:Q W Z  EXEC:G N R X  MEM:A C "
                        DB          "D F L M S U V", 0
                        DB          $0D, $0A
                        DB          "PROT: ! FOR F/M/C/A/N/L  H A/P/M/S/-/+  "
                        DB          "AUTO:-H/+H"
                        DB          0
MSG_HELP_SECTIONS:      DB          $0D, $0A
                        DB          "H A=ALL  H P=PROTECTION  H M=MEMORY  H S=ST"
                        DB          "EERING  H -=AUTOHELP OFF  H +=AUTOHELP ON", 0
MSG_H_USAGE:            DB          $0D, $0A
                        DB          "USAGE: H [A|P|M|S|-|+]  OR  -H / +H", 0
MSG_AUTOHELP_ALIAS_USAGE:
                        DB          $0D, $0A, "USAGE: -H (OFF)  +H (ON)", 0
MSG_H_AUTO_OFF:         DB          $0D, $0A, "AUTOHELP: OFF", 0
MSG_H_AUTO_ON:          DB          $0D, $0A, "AUTOHELP: ON (NEXT PROMPT)", 0
MSG_HELP_PROT_HDR:      DB          $0D, $0A, "HELP: PROTECTION", 0
MSG_HELP_MEM_HDR:       DB          $0D, $0A, "HELP: MEMORY/TOOLS", 0
MSG_HELP_STEER_HDR:     DB          $0D, $0A, "HELP: STEERING", 0
MSG_HELP_FULL_0:        DB          $0D, $0A, "MONITOR HELP", 0
MSG_HELP_FULL_1:        DB          $0D, $0A, "  [HELP]"
                        DB          0
MSG_HELP_FULL_2:        DB          $0D, $0A, "  ?                SHORT HELP"
                        DB          0
MSG_HELP_FULL_3:        DB          $0D, $0A
                        DB          "  H [A|P|M|S|-|+]  INDEX / ALL / PROT / MEM"
                        DB          " / STEER  (ALSO -H / +H)", 0
MSG_HELP_FULL_4:        DB          $0D, $0A, $0D, $0A
                        DB          "  [CONTROL]"
                        DB          0
MSG_HELP_FULL_5:        DB          $0D, $0A
                        DB          "  Q                WAI HALT; RESUME VIA "
                        DB          "NMI/RESET", 0
MSG_HELP_FULL_6:        DB          $0D, $0A
                        DB          "  W                WARM START (MONITOR)"
                        DB          0
MSG_HELP_FULL_7:        DB          $0D, $0A
                        DB          "  Z                CLEAR RAM (CONFIRM Y/"
                        DB          "N)", 0
MSG_HELP_FULL_8:        DB          $0D, $0A, $0D, $0A
                        DB          "  [EXECUTION/DEBUG]"
                        DB          0
MSG_HELP_FULL_9:        DB          $0D, $0A
                        DB          "  N                NEXT (RAM PATCH; NO R"
                        DB          "OM/I/O)", 0
MSG_HELP_FULL_10:       DB          $0D, $0A
                        DB          "  R [A/X/Y=HH]     RESUME LAST DEBUG CONT"
                        DB          "EXT", 0
MSG_HELP_FULL_11:       DB          $0D, $0A
                        DB          "  X S              EXECUTE; NMI BREAKS TO"
                        DB          " MONITOR", 0
MSG_HELP_FULL_28:       DB          $0D, $0A
                        DB          "  G                GUESS NUMBER (1-10, 3 "
                        DB          "TRIES)", 0
MSG_HELP_FULL_37:       DB          $0D, $0A
                        DB          "  I C EXPR         RPN CALC (16-BIT HEX) "
                        DB          "TOKENS", 0
MSG_HELP_FULL_12:       DB          $0D, $0A, $0D, $0A
                        DB          "  [MEMORY]          ***E IS INCLUSIVE***  *"
                        DB          "**ENTER HEX PAIR FOR BYTE***"
                        DB          0
MSG_HELP_FULL_13:       DB          $0D, $0A
                        DB          "  A S [INSN]       TINY ASM; '.' EXITS"
                        DB          0
MSG_HELP_FULL_14:       DB          $0D, $0A
                        DB          "  C S E DST        COPY (OVERLAP-SAFE)"
                        DB          0
MSG_HELP_FULL_15:       DB          $0D, $0A
                        DB          "  D [S [E]]        DUMP"
                        DB          0
MSG_HELP_FULL_16:       DB          $0D, $0A
                        DB          "  F S E B0..B15    FILL (NON-INTERACTIVE)"
                        DB          0
MSG_HELP_FULL_17:       DB          $0D, $0A
                        DB          "  M [S [B0..B15]]  MODIFY / DEPOSIT", 0
MSG_HELP_FULL_18:       DB          $0D, $0A
                        DB          "    INTERACTIVE:   CR/LF=NEXT, '.'=END"
                        DB          0
MSG_HELP_FULL_19:       DB          $0D, $0A
                        DB          "    ENTER HEX PAIRS (00..FF) TO STORE BY"
                        DB          "TES", 0
MSG_HELP_FULL_20:       DB          $0D, $0A
                        DB          "    NOTE: CRLF PAIR COUNTS AS ONE NEXT"
                        DB          0
MSG_HELP_FULL_26:       DB          $0D, $0A
                        DB          "  L S / L G S      LOAD S-RECORDS (L G S A"
                        DB          "UTO-RUNS ENTRY)", 0
MSG_HELP_FULL_27:       DB          $0D, $0A
                        DB          "  L B A L          LOAD RAW BYTES TO ADDR/"
                        DB          "LEN (NO CRC)", 0
MSG_HELP_FULL_29:       DB          $0D, $0A
                        DB          "  S B/C S E ...    SEARCH (BYTE/TEXT)", 0
MSG_HELP_FULL_21:       DB          $0D, $0A
                        DB          "  U [S E]          DISASSEMBLE 65C02 RANGE"
                        DB          0
MSG_HELP_FULL_22:       DB          $0D, $0A
                        DB          "  V                SHOW VECTOR CHAINS (DEP"
                        DB          "RECATING)", 0
MSG_HELP_FULL_23:       DB          $0D, $0A, $0D, $0A
                        DB          "  [PROTECTION]"
                        DB          0
MSG_HELP_FULL_24:       DB          $0D, $0A
                        DB          "  F/M/C/A/N/L      PROTECT $0000-$03FF BY"
                        DB          " DEFAULT", 0
MSG_HELP_FULL_25:       DB          $0D, $0A
                        DB          "  !<CMD> ...       FORCE-ENABLE LOW-RAM A"
                        DB          "CCESS", 0
MSG_HELP_FULL_30:       DB          $0D, $0A, $0D, $0A
                        DB          "  [STEERING] (PLANNED/PROVISIONAL)"
                        DB          0
MSG_HELP_FULL_31:       DB          $0D, $0A
                        DB          "  STYLE            NOUN VERB (M D, X S, I"
                        DB          " O V)", 0
MSG_HELP_FULL_32:       DB          $0D, $0A
                        DB          "  WILL CHANGE      X->J EXEC, TIME->I T, T"
                        DB          "->TERMINAL", 0
MSG_HELP_FULL_33:       DB          $0D, $0A
                        DB          "  DEPRECATED       TOP-LEVEL P/V; USE I O "
                        DB          "P / I O V", 0
MSG_HELP_FULL_34:       DB          $0D, $0A
                        DB          "  PROVISO          CHANGE IS CONSTANT", 0
MSG_HELP_FULL_35:       DB          $0D, $0A
                        DB          "  TERM NOTE        MINITERM: UP ARROW HISTO"
                        DB          "RY NOT SUPPORTED", 0
MSG_HELP_FULL_36:       DB          $0D, $0A
                        DB          "  HOST NOTE        NO PYTHON REQUIRED", 0
MSG_HELP_ADDRS_HW:      DB          $0D, $0A, $0D, $0A
                        DB          "  HW VECTORS @     N:", 0
MSG_HELP_ADDRS_ZP:      DB          $0D, $0A
                        DB          "  ZP VECTORS @     N:", 0
MSG_HELP_ADDR_R:        DB          " R:", 0
MSG_HELP_ADDR_I:        DB          " I:", 0
MSG_HELP_GAME_ADDR:     DB          $0D, $0A
                        DB          "  GAME PRMPT @ $", 0
MSG_HELP_GAME_SET:      DB          " !M ", 0
MSG_HELP_GAME_CLR:      DB          " 01=SET  !M ", 0
MSG_HELP_GAME_END:      DB          " 00=CLEAR", 0
MSG_HELP_TERM_ADDR:     DB          $0D, $0A
                        DB          "  TERM COL   @ $", 0
MSG_HELP_TERM_SET:      DB          " !M ", 0
MSG_HELP_TERM_END:      DB          " 28/50/84 (40/80/132)", 0
MSG_HELP_USER_ZP:       DB          $0D, $0A
                        DB          "  USER ZP    @ $90-$FF", 0
MSG_UNKNOWN_CMD:        DB          $0D, $0A, "UNKNOWN CMD", 0
MSG_D_USAGE:            DB          $0D, $0A, "USAGE: D [START [END]]", 0
MSG_D_RANGE_ERR:        DB          $0D, $0A, "D RANGE ERROR", 0
MSG_U_USAGE:            DB          $0D, $0A, "USAGE: U [START END]", 0
MSG_U_RANGE_ERR:        DB          $0D, $0A, "U RANGE ERROR", 0
MSG_A_USAGE:            DB          $0D, $0A
                        DB          "USAGE: A START [MNEMONIC OPERANDS]", 0
MSG_A_RANGE_ERR:        DB          $0D, $0A
                        DB          "A BRANCH RANGE ERROR", 0
MSG_G_USAGE:            DB          $0D, $0A, "USAGE: X START", 0
MSG_I_USAGE:            DB          $0D, $0A, "USAGE: I C <RPN>", 0
MSG_IC_USAGE:           DB          $0D, $0A
                        DB          "USAGE: I C T0 [T1 ...]  TOKENS: HEX,+,-,*,/"
                        DB          ",&,|,^,~", 0
MSG_IC_BADTOK:          DB          $0D, $0A, "I C BAD TOKEN", 0
MSG_IC_UNDERFLOW:       DB          $0D, $0A, "I C STACK UNDERFLOW", 0
MSG_IC_OVERFLOW:        DB          $0D, $0A, "I C STACK OVERFLOW", 0
MSG_IC_DIV0:            DB          $0D, $0A, "I C DIVIDE BY ZERO", 0
MSG_IC_EMPTY:           DB          $0D, $0A, "I C EMPTY", 0
MSG_IC_RESULT:          DB          $0D, $0A, "I C = $", 0
MSG_IC_REM:             DB          "  REM = $", 0
MSG_R_USAGE:            DB          $0D, $0A
                        DB          "USAGE: R [A=HH] [X=HH] [Y=HH]", 0
MSG_R_NO_CTX:           DB          $0D, $0A, "NO DEBUG CONTEXT", 0
MSG_N_USAGE:            DB          $0D, $0A, "USAGE: N", 0
MSG_GAME_USAGE:         DB          $0D, $0A, "USAGE: G", 0
MSG_L_USAGE:            DB          $0D, $0A
                        DB          "USAGE: L S | L G S | L B ADDR LEN", 0
MSG_LB_USAGE:           DB          $0D, $0A, "USAGE: L B ADDR LEN", 0
MSG_LB_LEN_ERR:         DB          $0D, $0A, "L B LEN MUST BE 1..FFFF", 0
MSG_LB_RANGE_ERR:       DB          $0D, $0A, "L B RANGE ERROR", 0
MSG_LB_READY:           DB          $0D, $0A, "L B READY - SEND RAW BYTES", 0
MSG_LB_DONE:            DB          $0D, $0A, "L B LOAD COMPLETE", 0
MSG_LB_ABORT:           DB          $0D, $0A, "L B ABORTED", 0
MSG_N_ROM:              DB          $0D, $0A, "N UNSUPPORTED IN ROM/I/O", 0
MSG_GAME_ASK:           DB          $0D, $0A, "WANT TO PLAY A GAME?", 0
MSG_GAME_INTRO:         DB          $0D, $0A, "I AM THINKING OF A NUMBER (1-10)", 0
MSG_GAME_PROMPT:        DB          $0D, $0A, "? ", 0
MSG_GAME_BAD_INPUT:     DB          $0D, $0A, "ENTER 1..10", 0
MSG_GAME_LOW:           DB          $0D, $0A, "TOO LOW", 0
MSG_GAME_HIGH:          DB          $0D, $0A, "TOO HIGH", 0
MSG_GAME_WIN:           DB          $0D, $0A, "CORRECT", 0
MSG_GAME_LOSE:          DB          $0D, $0A, "OUT OF CHANCES. NUMBER WAS ", 0
MSG_F_USAGE:            DB          $0D, $0A, "USAGE: F START END B0..B15", 0
MSG_S_USAGE:            DB          $0D, $0A
                        DB          "USAGE: S B START END B0..B15 | S C START "
                        DB          "END TEXT", 0
MSG_S_RANGE_ERR:        DB          $0D, $0A, "S RANGE ERROR", 0
MSG_S_NOT_FOUND:        DB          $0D, $0A, "S NO MATCH", 0
MSG_S_HIT:              DB          $0D, $0A, "S HIT @ $", 0
MSG_C_USAGE:            DB          $0D, $0A
                        DB          "USAGE: C SRC_START SRC_END DST_START", 0
MSG_LS_READY:           DB          $0D, $0A
                        DB          "L S READY - SEND S-RECORDS (ABORT WITH SX)"
                        DB          0
MSG_LS_DONE:            DB          $0D, $0A, "L S LOAD COMPLETE", 0
MSG_LS_ABORT:           DB          $0D, $0A, "L S ABORTED", 0
MSG_LS_PARSE_ERR:       DB          $0D, $0A, "L S RECORD FORMAT ERROR", 0
MSG_LS_CHKSUM_ERR:      DB          $0D, $0A, "L S CHECKSUM ERROR", 0
MSG_LS_TYPE_ERR:        DB          $0D, $0A, "L S UNSUPPORTED RECORD TYPE", 0
MSG_LS_ADDR_ERR:        DB          $0D, $0A, "L S ADDRESS OUT OF 16-BIT RANGE", 0
MSG_LGS_NO_ENTRY:       DB          $0D, $0A
                        DB          "L G S NO ENTRY ADDRESS; USE X START", 0
MSG_VERIFY_ERR_SUFFIX:  DB          " VERIFY FAILED AT ADDR ", 0
MSG_Q_WAIT:             DB          $0D, $0A, "Q HALT - RESET/NMI TO RESUME"
                        DB          0
MSG_M_USAGE:            DB          $0D, $0A, "USAGE: M [START [B0..B15]]", 0
MSG_PROTECT_ERR:        DB          $0D, $0A
                        DB          "PROTECTED RANGE ($0000-$03FF). USE ! TO "
                        DB          "FORCE", 0
STR_RST:                DB          "RST: ", 0
STR_RST_NAME:           DB          " **RST_PLACEHOLDER**", 0
STR_NMI:                DB          "NMI: ", 0
STR_IRQ:                DB          "IRQ: ", 0
STR_ARROW:              DB          " > ", 0
STR_CURR:               DB          "CURR: ", 0
STR_STATE:              DB          "STATE:", 0
STR_NEXT:               DB          "NEXT: ", 0
                        END
