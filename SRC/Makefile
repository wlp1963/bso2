# SPDX-License-Identifier: MIT
# Copyright (c) 2026 95west.us
# See LICENSE for full license text.

all:	bso2

bso2:	bso2.asm macros.inc
	WDC02AS -g -l -s -DUSING_02 bso2.asm
	WDCLN -g -sz -t -HZ -PFF -J -Q bso2 -C8000
	del /f *.obj *.sym

upload:	
	python "C:\Program Files\wdc\Tools\bin\wdc_interface.py" -dCOM3 bso2.bin -m write -k -x -t
	del /f *.bin *.obj *.lst *.sym *.map

hello:
	wdc02as -g -l -s hello-world.asm
	WDCLN -g -sz -t -HM28 -PFF -J -Q hello-world

clean:
	del /f *.obj *.lst *.sym *.map
	
