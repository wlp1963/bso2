# SPDX-License-Identifier: MIT
# Copyright (c) 2026 95west.us
# See LICENSE for full license text.

all:		modules bso2 hello games

games:		modules life mastermind chess adventure

modules:	lib.asm ../INCLUDES/equates.inc ../INCLUDES/macros.inc
			del /f lib.obj lib.lib lib.lst
			WDC02AS -G -i ../INCLUDES/ -l -s  lib.asm
			WDCLIB -a lib.lib lib.obj 
			del /f lib.obj


bso2:	bso2.asm ../INCLUDES/equates.inc ../INCLUDES/macros.inc lib.lib
	WDC02AS -G -i ../INCLUDES/ -l -s bso2.asm
	WDCLN -G -sz -t -HZ -PFF -J -C8000 -O bso2.bin  bso2.obj -L lib
#	del /f *.obj

WDC_UPLOADER = C:\Program Files\wdc\tools\bin\wdc_interface.py
WDC_EDU_ARCHIVE_URL = https://wdc65xx.com/wp-content/uploads/2022/09/EDU_GettingStarted.zip
HOST_NAME := $(strip $(COMPUTERNAME))
ifeq ($(origin WDC_PORT), undefined)
ifeq ($(HOST_NAME),UDOO)
WDC_PORT = COM4
else ifeq ($(HOST_NAME),LAPTOP)
WDC_PORT = COM3
else
WDC_PORT = COM3
endif
endif

# wdc_uploader_term.py and wdc_interface.py are the same WDC uploader script
# under different package revisions (often distributed via W65C02SXB.zip /
# EDU_GettingStarted.zip).
upload:	bso2 check-uploader
	python "$(WDC_UPLOADER)" -d$(WDC_PORT) bso2.bin -m write -k -x -t
#	del /f *.bin *.obj *.lst *.sym *.map

check-uploader:
	@if not exist "$(WDC_UPLOADER)" (echo ERROR: Missing uploader script: "$(WDC_UPLOADER)" & echo INFO: Download WDC EDU package: "$(WDC_EDU_ARCHIVE_URL)" & exit /b 1)

hello: modules
	wdc02as -i ../INCLUDES -g -l -s hello-world.asm
	WDCLN -g -sz -t -HM19 -PFF -J -Q hello-world -Llib -C1000

clean:
	del /f *.obj *.lst *.sym *.map *.bin

realclean:
	del /f *.obj *.lst *.sym *.map *.lib *.bin *.s28 *.s19
	
term:
	python -m serial.tools.miniterm COM3 1042200

mastermind: modules
	wdc02as -i ../INCLUDES -g -l -s mastermind.asm
	WDCLN -g -sz -t -HM19 -PFF -J -Q mastermind -Llib -C1000

life: modules
	wdc02as -i ../INCLUDES -g -l -s life.asm
	WDCLN -g -sz -t -HM19 -PFF -J -Q life -Llib -C5000

chess: modules
	wdc02as -i ../INCLUDES -g -l -s chess.asm
	WDCLN -g -sz -t -HM19 -PFF -J -Q chess -Llib -C6000

adventure: modules
	wdc02as -i ../INCLUDES -g -l -s adventure.asm
	WDCLN -g -sz -t -HM19 -PFF -J -Q adventure -Llib -C7000



