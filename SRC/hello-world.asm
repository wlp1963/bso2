; HELLO-WORLD.ASM
;  hand written
                        CHIP       65C02
                        PL         60
                        PW         132
                        TITLE      HELLO-WORLD
			MACLIST	   OFF

			INCLUDE	   MACROS.INC

PRT_C_STRING:		EQU	   $AEC7
WDC_WRITE_BYTE          EQU        $F803 ; ROM SERIAL OUT
STR_PTR  	        EQU	   $0036


        ORG $1000
START:
        PRT_CSTRING HELLO_WORLD
        BRK 00
	ldx #00
?loop	txa
	jsr $aea6 ; prt_hex
	inx
	bne ?loop
	brk 01
	PRT_CSTRING CRLF
        WAI
        JMP START

HELLO_WORLD: DB $0D,$0A,"HELLO, WORLD",0
CRLF:		DB $0d, $0a, $00
