; HELLO-WORLD.ASM
;  hand written
                        CHIP       65C02
                        PL         60
                        PW         132
                        TITLE      HELLO-WORLD
			MACLIST	   OFF

			INCLUDE	   equates.inc

        XREF PRT_C_STRING
        XREF PRT_HEX
        XDEF WRITE_BYTE
        XDEF STR_PTR

        XREF LED_DATA
        XREF WDC_WRITE_BYTE

;WRITE_BYTE              EQU        $F803 ; WDC ROM serial out
STR_PTR                 EQU        $0036


        ; Linker places CODE at $1000 (see Makefile hello target -C1000).
START:
        PRT_CSTRING HELLO_WORLD
        BRK 00
	ldx #00
?loop	txa
	jsr PRT_HEX
	inx
	bne ?loop
	brk 01
	PRT_CSTRING CRLF
        WAI
        JMP START

HELLO_WORLD: DB $0D,$0A,"HELLO, WORLD",0
CRLF:		DB $0d, $0a, $00
