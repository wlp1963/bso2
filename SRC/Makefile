# SPDX-License-Identifier: MIT
# Copyright (c) 2026 95west.us
# See LICENSE for full license text.

all:	bso2

bso2:	bso2.asm macros.inc
	WDC02AS -g -l -s -DUSING_02 bso2.asm
	WDCLN -g -sz -t -HZ -PFF -J -Q bso2 -C8000
	del /f *.obj *.sym

WDC_UPLOADER = C:\Program Files\wdc\tools\bin\wdc_interface.py
WDC_EDU_ARCHIVE_URL = https://wdc65xx.com/wp-content/uploads/2022/09/EDU_GettingStarted.zip

# wdc_uploader_term.py and wdc_interface.py are the same WDC uploader script
# under different package revisions (often distributed via W65C02SXB.zip /
# EDU_GettingStarted.zip).
upload:	bso2 check-uploader
	python "$(WDC_UPLOADER)" -dCOM3 bso2.bin -m write -k -x -t
	del /f *.bin *.obj *.lst *.sym *.map

check-uploader:
	@if not exist "$(WDC_UPLOADER)" (echo ERROR: Missing uploader script: "$(WDC_UPLOADER)" & echo INFO: Download WDC EDU package: "$(WDC_EDU_ARCHIVE_URL)" & exit /b 1)

hello:
	wdc02as -g -l -s hello-world.asm
	WDCLN -g -sz -t -HM28 -PFF -J -Q hello-world

clean:
	del /f *.obj *.lst *.sym *.map
	
term:
	python -m serial.tools.miniterm COM3 1042200
