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
; * REV: 2.0                                                                 *
; * *
; ****************************************************************************

                        MACLIST     OFF
                        CHIP        65C02
                        LONGI       OFF
                        LONGA       OFF
                        PL          60
                        PW          132

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

; ---W65c22 VIA REGISTERS ---
VIA                     EQU         $7FC0 ; VIA BASE ADDR
VIA_ORB                 EQU         VIA+0 ; OUTPUT REG B
VIA_ORA_H               EQU         VIA+1 ; OUTPUT REG  A W/HNDSHK SEE COMMENTS
VIA_DDRB                EQU         VIA+2 ; DATA DIR REG B
VIA_DDRA                EQU         VIA+3 ; DATA DIR REG A
VIA_T1L                 EQU         VIA+4 ; RD T1 LOW BYTE - WR T1 LATCH LOW BYTE
VIA_T1H                 EQU         VIA+5 ; RD T1 HI BYTE - WR T1 LATCH HI BYTE AND RUN
VIA_TIMER1L             EQU         VIA+6 ; ACCESS T1 LATCH LOW BYTE
VIA_TIMER1H             EQU         VIA+7 ; ACCESS T1 LATCH HI BYTE
VIA_T2L_RST             EQU         VIA+8 ; RD T2 LOW BYTE AND RST CNTR INTR, WR T2 LOW BYTE DO NOT RST 
VIA_TIMER2H_RST_WR      EQU         VIA+9 ; ACCESS T2 HI BYTE - WR RST CNTR INTR 
VIA_SSR                 EQU         VIA+$A ; SERIAL I/O SHIFT REG 
VIA_ACR                 EQU         VIA+$B ; AUXILIARY CTRL REG
VIA_PCR                 EQU         VIA+$C ; PERIPHERAL CTRL REG 
VIA_IFR                 EQU         VIA+$D ; INTR FLAG REG 
VIA_IER                 EQU         VIA+$E ; INTR ENABLE REG 
VIA_ORA                 EQU         VIA+$F ; OUTPUT REG A WO/HNDSHK
;
; Handshake mode: the VIA uses CA2/CB2 (and usually CA1/CB1) to do automatic
; ready/ack signaling during port transfers
; No-handshake mode: port I/O is just plain read/write; CA2/CB2 are not doing 
; automatic transfer acknowledge (they may be simple control lines or normal
; inputs).

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
ZP_HEARTBEAT_ADDR       EQU         $7C
ZP_HEARTBEAT_DIV_ADDR   EQU         $7D
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
VIA_T1_RELOAD_DFLT      EQU         $FFFF
                                        ; FREE-RUN TIMER1 RELOAD (8MHZ => ~122.07HZ)
