# bso2com (GNU C)

Status: TOTALLY UNTESTED.

Poor, poor, poor man's 5250 data stream / terminal emulation / file transfer for bso2 serial workflows.

## Capabilities

- Terminal mode (`term`) with local quit char (default `0x1D`, Ctrl-])
- Raw binary load (`lb-send`) for monitor command `L B ADDR LEN`
- S-record load from host file (`ls-send`) for monitor `L S`
- S-record load+go from host file (`lgs-send`) for monitor `L G S`
- IBM-ish data-stream / terminal emulation / file-transfer direction (experimental)

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
tools/bso2com/bso2com ls-send game.s28
tools/bso2com/bso2com lgs-send game.s28
```

Notes:

- `ls-send`/`lgs-send` wait for monitor text `L S READY`, stream the `.s28` file,
  then wait for `L S LOAD COMPLETE` by default.
- Use `--no-wait-done` if you do not want to wait for completion text.
