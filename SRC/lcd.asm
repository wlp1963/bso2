; LCD.ASM
; Standalone userland LCD demo + routines for W65C02EDU (PIA Port B).
                        CHIP        65C02
                        PL          60
                        PW          132
                        TITLE       LCD
                        MACLIST     OFF

                        INCLUDE     equates.inc

                        XDEF        START
                        XDEF        LCD_INIT
                        XDEF        LCD_CMD
                        XDEF        LCD_PUTC
                        XDEF        LCD_PUTS

; Shared monitor string pointer convention.
STR_PTR                 EQU         $0036

; Use userland ZP scratch.
LCD_PTR                 EQU         USER_ZP_BASE_ADDR

; HD44780 on PIA Port B:
; PB7..PB4 = D7..D4
; PB3       = RS
; PB2       = R/W (kept low here)
; PB1       = E
LCD_DATA                EQU         PIA_PB
LCD_DDR                 EQU         PIA_DDRB
LCD_CR                  EQU         PIA_CRB
LCD_RS_M                EQU         %00001000
LCD_E_M                 EQU         %00000010
LCD_CR_DDR_SEL          EQU         $30 ; bit2=0 => DDR at PIA+2
LCD_CR_DATA_SEL         EQU         $34 ; bit2=1 => data at PIA+2
LCD_DEBUG_EN            EQU         1   ; 1=stage BRK traps enabled, 0=normal run

START:
                        JSR         LCD_INIT
                        JSR         LCD_DBG_STAGE_1
                        LDA         #$80 ; line 1, col 0
                        JSR         LCD_CMD
                        JSR         LCD_DBG_STAGE_2
                        LDA         #<MSG_LINE1
                        STA         STR_PTR
                        LDA         #>MSG_LINE1
                        STA         STR_PTR+1
                        JSR         LCD_PUTS
                        JSR         LCD_DBG_STAGE_3

                        LDA         #$C0 ; line 2, col 0
                        JSR         LCD_CMD
                        JSR         LCD_DBG_STAGE_4
                        LDA         #<MSG_LINE2
                        STA         STR_PTR
                        LDA         #>MSG_LINE2
                        STA         STR_PTR+1
                        JSR         LCD_PUTS
                        JSR         LCD_DBG_STAGE_5

?IDLE:
                        ; Return to monitor instead of parking forever in WAI.
                        BRK         $00
                        WAI
                        BRA         ?IDLE

; ----------------------------------------------------------------------------
; LCD_INIT: initialize HD44780 in 4-bit mode.
; ----------------------------------------------------------------------------
LCD_INIT:
                        ; Select DDR register on PIA+2 and make PB0..PB7 output.
                        LDA         #LCD_CR_DDR_SEL
                        STA         LCD_CR
                        LDA         #$FF
                        STA         LCD_DDR

                        ; Select data register on PIA+2 and drive everything low.
                        LDA         #LCD_CR_DATA_SEL
                        STA         LCD_CR
                        STZ         LCD_DATA

                        ; HD44780 power-up synchronization sequence.
                        JSR         LCD_DELAY_POWERUP
                        LDA         #$30
                        JSR         LCD_WRITE_NIBBLE_CMD
                        JSR         LCD_DELAY_5MS
                        LDA         #$30
                        JSR         LCD_WRITE_NIBBLE_CMD
                        JSR         LCD_DELAY_INIT_STEP
                        LDA         #$30
                        JSR         LCD_WRITE_NIBBLE_CMD
                        JSR         LCD_DELAY_INIT_STEP
                        LDA         #$20 ; switch to 4-bit transfers
                        JSR         LCD_WRITE_NIBBLE_CMD
                        JSR         LCD_DELAY_INIT_STEP

                        ; Function set / display setup.
                        LDA         #$28 ; 4-bit, 2-line, 5x8
                        JSR         LCD_CMD
                        LDA         #$08 ; display off
                        JSR         LCD_CMD
                        LDA         #$01 ; clear display
                        JSR         LCD_CMD
                        LDA         #$06 ; entry mode increment
                        JSR         LCD_CMD
                        LDA         #$0C ; display on, cursor off
                        JSR         LCD_CMD
                        RTS

; ----------------------------------------------------------------------------
; LCD_CMD: write one instruction byte.
; Input: A = command byte.
; ----------------------------------------------------------------------------
LCD_CMD:
                        PHA
                        JSR         LCD_WRITE_CMD_BYTE
                        PLA
                        CMP         #$01 ; clear display
                        BEQ         ?LCD_CMD_LONG
                        CMP         #$02 ; return home
                        BEQ         ?LCD_CMD_LONG
                        JSR         LCD_DELAY_SHORT
                        RTS
?LCD_CMD_LONG:
                        JSR         LCD_DELAY_CLEAR
                        RTS

; ----------------------------------------------------------------------------
; LCD_PUTC: write one data byte.
; Input: A = ASCII byte.
; ----------------------------------------------------------------------------
LCD_PUTC:
                        JSR         LCD_WRITE_DATA_BYTE
                        JSR         LCD_DELAY_SHORT
                        RTS

; ----------------------------------------------------------------------------
; LCD_PUTS: write zero-terminated string pointed to by STR_PTR.
; Preserves STR_PTR.
; ----------------------------------------------------------------------------
LCD_PUTS:
                        PHY
                        LDA         STR_PTR
                        STA         LCD_PTR
                        LDA         STR_PTR+1
                        STA         LCD_PTR+1
?LCD_PUTS_NEXT:
                        LDY         #$00
                        LDA         (LCD_PTR),Y
                        BEQ         ?LCD_PUTS_DONE
                        JSR         LCD_PUTC
                        INC         LCD_PTR
                        BNE         ?LCD_PUTS_NEXT
                        INC         LCD_PTR+1
                        BRA         ?LCD_PUTS_NEXT
?LCD_PUTS_DONE:
                        PLY
                        RTS

; ----------------------------------------------------------------------------
; Low-level write helpers
; ----------------------------------------------------------------------------
LCD_WRITE_CMD_BYTE:
                        PHA
                        AND         #$F0
                        JSR         LCD_WRITE_NIBBLE_CMD
                        PLA
                        ASL         A
                        ASL         A
                        ASL         A
                        ASL         A
                        AND         #$F0
                        JSR         LCD_WRITE_NIBBLE_CMD
                        RTS

LCD_WRITE_DATA_BYTE:
                        PHA
                        AND         #$F0
                        JSR         LCD_WRITE_NIBBLE_DATA
                        PLA
                        ASL         A
                        ASL         A
                        ASL         A
                        ASL         A
                        AND         #$F0
                        JSR         LCD_WRITE_NIBBLE_DATA
                        RTS

LCD_WRITE_NIBBLE_CMD:
                        AND         #$F0
                        STA         LCD_DATA ; E=0, RS=0
                        ORA         #LCD_E_M
                        STA         LCD_DATA ; E=1
                        NOP
                        NOP
                        EOR         #LCD_E_M
                        STA         LCD_DATA ; E=0
                        RTS

LCD_WRITE_NIBBLE_DATA:
                        AND         #$F0
                        ORA         #LCD_RS_M
                        STA         LCD_DATA ; E=0, RS=1
                        ORA         #LCD_E_M
                        STA         LCD_DATA ; E=1
                        NOP
                        NOP
                        EOR         #LCD_E_M
                        STA         LCD_DATA ; E=0
                        RTS

; ----------------------------------------------------------------------------
; Conservative fixed delays for LCD timing (8MHz-class CPU).
; ----------------------------------------------------------------------------
LCD_DELAY_SHORT:
                        PHX
                        ; ~39us at 8MHz (normal LCD command/data >=37us).
                        LDX         #$60
?LCD_DLY_SHORT_LOOP:
                        DEX
                        BNE         ?LCD_DLY_SHORT_LOOP
                        PLX
                        RTS

LCD_DELAY_INIT_STEP:
                        PHX
                        ; ~180us class delay for init handoff steps (>=100us).
                        LDX         #$03
?LCD_DLY_INIT_LOOP:
                        JSR         LCD_DELAY_SHORT
                        DEX
                        BNE         ?LCD_DLY_INIT_LOOP
                        PLX
                        RTS

LCD_DELAY_CLEAR:
                        PHX
                        ; ~1.6ms at 8MHz for CLEAR/HOME (>=1.52ms worst-case).
                        LDX         #$28
?LCD_DLY_CLEAR_LOOP:
                        JSR         LCD_DELAY_SHORT
                        DEX
                        BNE         ?LCD_DLY_CLEAR_LOOP
                        PLX
                        RTS

LCD_DELAY_5MS:
                        PHX
                        ; ~5ms at 8MHz for early power-up wake sequence.
                        LDX         #$80
?LCD_DLY_5MS_LOOP:
                        JSR         LCD_DELAY_SHORT
                        DEX
                        BNE         ?LCD_DLY_5MS_LOOP
                        PLX
                        RTS

LCD_DELAY_POWERUP:
                        PHX
                        ; HD44780U init requirement is >=15ms before first 0x30.
                        ; Keep this conservative at ~35ms for module/clone variance.
                        ; 22 * ~1.6ms ~= ~35.2ms at 8MHz.
                        LDX         #$16
?LCD_DLY_PWR_LOOP:
                        JSR         LCD_DELAY_CLEAR
                        DEX
                        BNE         ?LCD_DLY_PWR_LOOP
                        PLX
                        RTS

; ----------------------------------------------------------------------------
; Optional staged debug traps for bring-up.
; BRK tags:
;   $11 after LCD_INIT
;   $12 after first LCD_CMD ($80)
;   $13 after first LCD_PUTS
;   $14 after second LCD_CMD ($C0)
;   $15 after second LCD_PUTS
; ----------------------------------------------------------------------------
LCD_DBG_STAGE_1:
                        LDA         #LCD_DEBUG_EN
                        BEQ         ?LCD_DBG_1_DONE
                        BRK         $11
?LCD_DBG_1_DONE:
                        RTS

LCD_DBG_STAGE_2:
                        LDA         #LCD_DEBUG_EN
                        BEQ         ?LCD_DBG_2_DONE
                        BRK         $12
?LCD_DBG_2_DONE:
                        RTS

LCD_DBG_STAGE_3:
                        LDA         #LCD_DEBUG_EN
                        BEQ         ?LCD_DBG_3_DONE
                        BRK         $13
?LCD_DBG_3_DONE:
                        RTS

LCD_DBG_STAGE_4:
                        LDA         #LCD_DEBUG_EN
                        BEQ         ?LCD_DBG_4_DONE
                        BRK         $14
?LCD_DBG_4_DONE:
                        RTS

LCD_DBG_STAGE_5:
                        LDA         #LCD_DEBUG_EN
                        BEQ         ?LCD_DBG_5_DONE
                        BRK         $15
?LCD_DBG_5_DONE:
                        RTS

MSG_LINE1:              DB          "BSO2 LCD READY", 0
MSG_LINE2:              DB          "4-BIT USERLAND", 0
