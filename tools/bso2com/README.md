# bso2com (GNU C)

Status: TOTALLY UNTESTED.

Unified Linux GNU C host utility for bso2 serial workflows.

## Capabilities

- Terminal mode (`term`) with local quit char (default `0x1D`, Ctrl-])
- Raw binary load (`lb-send`) for monitor command `L B ADDR LEN`
- WDCMONv2 flash protocol helpers:
- `flash-read`
- `flash-write`
- `flash-clear`
- `flash-check`
- `flash-exec`
- `flash-update-monitor`

## Build

```bash
make -C tools/bso2com
```

## Linux Dependencies

- GNU C toolchain: `gcc`, `make`
- POSIX userspace headers/libs (for `termios`, `select`, `unistd`)

Examples:

```bash
# Debian/Ubuntu
sudo apt-get install build-essential

# Fedora
sudo dnf install gcc make glibc-devel
```

## WSL2 Quick Start (`Ubuntu` on `Win11 Pro`)

```powershell
# Windows PowerShell (admin)
usbipd list
usbipd bind --busid <BUSID>
usbipd attach --wsl --busid <BUSID>
```

```bash
# Ubuntu (WSL)
ls /dev/ttyUSB* /dev/ttyACM* /dev/ttyS* 2>/dev/null
make -C tools/bso2com
tools/bso2com/bso2com --port /dev/ttyUSB0 --baud 115200 term
```

## Examples

```bash
tools/bso2com/bso2com --port /dev/ttyUSB0 --baud 115200 term
tools/bso2com/bso2com lb-send payload.bin --addr 1000
tools/bso2com/bso2com flash-check
tools/bso2com/bso2com flash-clear --force --confirm ERASE
tools/bso2com/bso2com flash-write --addr 8000 --in app.bin --force --confirm WRITE
tools/bso2com/bso2com flash-read --addr 8000 --len 0100 --out dump.bin
tools/bso2com/bso2com flash-update-monitor --in monitor.bin --force --confirm UPDATE
```
