#define _POSIX_C_SOURCE 200809L

/*
 * bso2com.c
 * TOTALLY UNTESTED
 */

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#define HSYNC 0x55
#define HREQ 0xAA
#define HACK 0xCC

#define CMD_WRITE_FLASH 0x07
#define CMD_READ_FLASH 0x08
#define CMD_CLEAR_FLASH 0x09
#define CMD_CHECK_FLASH 0x0A
#define CMD_EXEC_FLASH 0x0B
#define CMD_UPDATE_MON 0x0D

#define STATUS_OK 0x00
#define STATUS_STAGE1 0x01
#define STATUS_STAGE2 0x02
#define STATUS_DONE 0x03
#define STATUS_FAIL 0xFF
#define STATUS_FLASH_FAIL 0xBF

struct common_opts {
    const char *port;
    int baud;
    double timeout_s;
};

struct tty_guard {
    bool active;
    struct termios saved;
};

static void make_raw_mode(struct termios *tio) {
    tio->c_iflag &= (tcflag_t) ~(IGNBRK | BRKINT | PARMRK | ISTRIP |
                                  INLCR | IGNCR | ICRNL | IXON);
    tio->c_oflag &= (tcflag_t) ~OPOST;
    tio->c_lflag &= (tcflag_t) ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    tio->c_cflag &= (tcflag_t) ~(CSIZE | PARENB);
    tio->c_cflag |= CS8;
}

static void usage(void) {
    fprintf(stderr,
            "bso2com - bso2 communication utility (GNU C)\n\n"
            "Global options (before command):\n"
            "  --port PATH         Serial path (default: /dev/ttyUSB0)\n"
            "  --baud N            Baud rate (default: 115200)\n"
            "  --timeout S         Base read timeout seconds (default: 1.0)\n\n"
            "Commands:\n"
            "  term [--quit-char HEX]\n"
            "  lb-send BIN --addr HEX [--ready-timeout S] [--done-timeout S] [--no-wait-done]\n"
            "  ls-send S28 [--ready-timeout S] [--done-timeout S] [--no-wait-done]\n"
            "  lgs-send S28 [--ready-timeout S] [--done-timeout S] [--no-wait-done]\n"
            "  flash-read --addr HEX --len HEX --out FILE [--bank HEX]\n"
            "  flash-write --addr HEX --in FILE [--bank HEX] --force --confirm WRITE\n"
            "  flash-clear --force --confirm ERASE\n"
            "  flash-check\n"
            "  flash-exec\n"
            "  flash-update-monitor --in FILE --force --confirm UPDATE\n");
}

static int parse_u16_hex(const char *text, uint16_t *out) {
    const char *p = text;
    char *end = NULL;
    unsigned long value;

    if (p[0] == '$') {
        p++;
    } else if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
        p += 2;
    }
    if (*p == '\0') {
        return -1;
    }

    errno = 0;
    value = strtoul(p, &end, 16);
    if (errno != 0 || end == p || *end != '\0' || value > 0xFFFFUL) {
        return -1;
    }
    *out = (uint16_t)value;
    return 0;
}

static int parse_u8_hex(const char *text, uint8_t *out) {
    uint16_t tmp;
    if (parse_u16_hex(text, &tmp) != 0 || tmp > 0xFFu) {
        return -1;
    }
    *out = (uint8_t)tmp;
    return 0;
}

static int parse_int_range(const char *text, int min_v, int max_v, int *out) {
    char *end = NULL;
    long value;

    errno = 0;
    value = strtol(text, &end, 10);
    if (errno != 0 || end == text || *end != '\0') {
        return -1;
    }
    if (value < min_v || value > max_v) {
        return -1;
    }
    *out = (int)value;
    return 0;
}

static int parse_double_pos(const char *text, double *out) {
    char *end = NULL;
    double value;

    errno = 0;
    value = strtod(text, &end);
    if (errno != 0 || end == text || *end != '\0' || value <= 0.0) {
        return -1;
    }
    *out = value;
    return 0;
}

static speed_t baud_to_termios(int baud) {
    switch (baud) {
    case 9600: return B9600;
    case 19200: return B19200;
    case 38400: return B38400;
    case 57600: return B57600;
    case 115200: return B115200;
#ifdef B230400
    case 230400: return B230400;
#endif
#ifdef B460800
    case 460800: return B460800;
#endif
    default: return (speed_t)0;
    }
}

static int open_serial(const char *port, int baud) {
    int fd;
    struct termios tio;
    speed_t speed = baud_to_termios(baud);

    if (speed == 0) {
        fprintf(stderr, "error: unsupported baud rate: %d\n", baud);
        return -1;
    }

    fd = open(port, O_RDWR | O_NOCTTY | O_SYNC);
    if (fd < 0) {
        perror("open serial");
        return -1;
    }

    if (tcgetattr(fd, &tio) != 0) {
        perror("tcgetattr");
        close(fd);
        return -1;
    }

    make_raw_mode(&tio);
    tio.c_cflag |= (CLOCAL | CREAD);
    tio.c_cflag &= ~CSIZE;
    tio.c_cflag |= CS8;
    tio.c_cflag &= ~(PARENB | PARODD | CSTOPB);
#ifdef CRTSCTS
    tio.c_cflag &= ~CRTSCTS;
#endif
    tio.c_cc[VMIN] = 0;
    tio.c_cc[VTIME] = 1;

    if (cfsetispeed(&tio, speed) != 0 || cfsetospeed(&tio, speed) != 0) {
        perror("cfset*speed");
        close(fd);
        return -1;
    }

    if (tcsetattr(fd, TCSANOW, &tio) != 0) {
        perror("tcsetattr");
        close(fd);
        return -1;
    }

    tcflush(fd, TCIOFLUSH);
    return fd;
}

static int write_all_fd(int fd, const uint8_t *buf, size_t len) {
    size_t off = 0;
    while (off < len) {
        ssize_t n = write(fd, buf + off, len - off);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            perror("write");
            return -1;
        }
        off += (size_t)n;
    }
    return 0;
}

static double now_s(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
}

static int read_exact_timeout(int fd, uint8_t *buf, size_t need, double timeout_s) {
    size_t off = 0;
    double deadline = now_s() + timeout_s;

    while (off < need) {
        ssize_t n = read(fd, buf + off, need - off);
        if (n > 0) {
            off += (size_t)n;
            continue;
        }
        if (n < 0 && errno != EINTR) {
            perror("read");
            return -1;
        }
        if (now_s() >= deadline) {
            fprintf(stderr, "error: timeout while reading serial data\n");
            return -1;
        }
    }
    return 0;
}

static int token_found(const uint8_t *buf, size_t len, const char *token) {
    size_t tok_len = strlen(token);
    size_t i;
    if (tok_len == 0 || tok_len > len) {
        return 0;
    }
    for (i = 0; i + tok_len <= len; i++) {
        if (memcmp(buf + i, token, tok_len) == 0) {
            return 1;
        }
    }
    return 0;
}

static int read_until_token(int fd,
                            const char *token_ok,
                            const char *token_abort,
                            double timeout_s) {
    uint8_t chunk[256];
    uint8_t *accum = NULL;
    size_t accum_len = 0;
    size_t accum_cap = 0;
    double deadline = now_s() + timeout_s;

    while (now_s() < deadline) {
        ssize_t n = read(fd, chunk, sizeof(chunk));
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            perror("read");
            free(accum);
            return -1;
        }
        if (n == 0) {
            continue;
        }

        if (write_all_fd(STDOUT_FILENO, chunk, (size_t)n) != 0) {
            free(accum);
            return -1;
        }

        if (accum_len + (size_t)n > accum_cap) {
            size_t new_cap = (accum_cap == 0) ? 1024 : accum_cap * 2;
            while (new_cap < accum_len + (size_t)n) {
                new_cap *= 2;
            }
            uint8_t *tmp = realloc(accum, new_cap);
            if (tmp == NULL) {
                fprintf(stderr, "error: out of memory\n");
                free(accum);
                return -1;
            }
            accum = tmp;
            accum_cap = new_cap;
        }

        memcpy(accum + accum_len, chunk, (size_t)n);
        accum_len += (size_t)n;

        if (token_found(accum, accum_len, token_ok)) {
            free(accum);
            return 1;
        }
        if (token_abort != NULL && token_found(accum, accum_len, token_abort)) {
            free(accum);
            return 2;
        }
    }

    free(accum);
    return 0;
}

static int read_file(const char *path, uint8_t **buf, size_t *len) {
    FILE *f = fopen(path, "rb");
    long size;
    uint8_t *data;

    if (!f) {
        perror("fopen");
        return -1;
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        perror("fseek");
        fclose(f);
        return -1;
    }
    size = ftell(f);
    if (size < 0) {
        perror("ftell");
        fclose(f);
        return -1;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        perror("fseek");
        fclose(f);
        return -1;
    }
    if (size == 0) {
        fprintf(stderr, "error: input file is empty\n");
        fclose(f);
        return -1;
    }
    data = malloc((size_t)size);
    if (!data) {
        fprintf(stderr, "error: out of memory\n");
        fclose(f);
        return -1;
    }
    if (fread(data, 1, (size_t)size, f) != (size_t)size) {
        perror("fread");
        free(data);
        fclose(f);
        return -1;
    }
    fclose(f);
    *buf = data;
    *len = (size_t)size;
    return 0;
}

static int write_file(const char *path, const uint8_t *buf, size_t len) {
    FILE *f = fopen(path, "wb");
    if (!f) {
        perror("fopen");
        return -1;
    }
    if (fwrite(buf, 1, len, f) != len) {
        perror("fwrite");
        fclose(f);
        return -1;
    }
    fclose(f);
    return 0;
}

static const char *status_text(uint8_t s) {
    switch (s) {
    case STATUS_OK: return "OK";
    case STATUS_STAGE1: return "STAGE1_OK";
    case STATUS_STAGE2: return "STAGE2_OK";
    case STATUS_DONE: return "DONE";
    case STATUS_FLASH_FAIL: return "FLASH_FAIL";
    case STATUS_FAIL: return "FAIL";
    default: return "UNKNOWN";
    }
}

static int mon_sync(int fd, double timeout_s) {
    uint8_t tx[2] = {HSYNC, HREQ};
    uint8_t rx = 0;
    if (write_all_fd(fd, tx, sizeof(tx)) != 0) {
        return -1;
    }
    if (read_exact_timeout(fd, &rx, 1, timeout_s) != 0) {
        return -1;
    }
    if (rx != HACK) {
        fprintf(stderr, "error: sync failed (expected CC, got %02X)\n", rx);
        return -1;
    }
    return 0;
}

static int mon_start_cmd(int fd, double timeout_s, uint8_t cmd) {
    if (mon_sync(fd, timeout_s) != 0) {
        return -1;
    }
    return write_all_fd(fd, &cmd, 1);
}

static int mon_send_info(int fd, uint16_t addr, uint8_t bank, uint16_t len) {
    uint8_t info[6];
    info[0] = (uint8_t)(addr & 0xFF);
    info[1] = (uint8_t)((addr >> 8) & 0xFF);
    info[2] = bank;
    info[3] = (uint8_t)(len & 0xFF);
    info[4] = (uint8_t)((len >> 8) & 0xFF);
    info[5] = 0x00;
    return write_all_fd(fd, info, sizeof(info));
}

static int mon_read_status(int fd, double timeout_s, uint8_t *status) {
    return read_exact_timeout(fd, status, 1, timeout_s);
}

static int require_force_confirm(bool force,
                                 const char *confirm,
                                 const char *required_token,
                                 const char *op_name) {
    if (!force) {
        fprintf(stderr, "error: %s requires --force\n", op_name);
        return -1;
    }
    if (confirm == NULL || strcmp(confirm, required_token) != 0) {
        fprintf(stderr, "error: %s requires --confirm %s\n", op_name, required_token);
        return -1;
    }
    return 0;
}

static int tty_raw_begin(struct tty_guard *guard) {
    struct termios t;
    guard->active = false;
    if (!isatty(STDIN_FILENO)) {
        return 0;
    }
    if (tcgetattr(STDIN_FILENO, &guard->saved) != 0) {
        perror("tcgetattr(stdin)");
        return -1;
    }
    t = guard->saved;
    make_raw_mode(&t);
    if (tcsetattr(STDIN_FILENO, TCSANOW, &t) != 0) {
        perror("tcsetattr(stdin)");
        return -1;
    }
    guard->active = true;
    return 0;
}

static void tty_raw_end(struct tty_guard *guard) {
    if (guard->active) {
        tcsetattr(STDIN_FILENO, TCSANOW, &guard->saved);
        guard->active = false;
    }
}

static int cmd_term(int argc, char **argv, const struct common_opts *common) {
    int fd;
    uint8_t quit_char = 0x1D; /* Ctrl-] */
    int i;
    struct tty_guard guard;

    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "--quit-char") == 0) {
            if (++i >= argc || parse_u8_hex(argv[i], &quit_char) != 0) {
                fprintf(stderr, "error: invalid --quit-char\n");
                return 2;
            }
            continue;
        }
        fprintf(stderr, "error: unknown term option: %s\n", argv[i]);
        return 2;
    }

    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        return 1;
    }

    if (tty_raw_begin(&guard) != 0) {
        close(fd);
        return 1;
    }

    fprintf(stderr, "term mode: %s @ %d, quit char 0x%02X\n",
            common->port, common->baud, quit_char);

    for (;;) {
        fd_set rfds;
        int nfds = (fd > STDIN_FILENO ? fd : STDIN_FILENO) + 1;
        int rc;

        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        FD_SET(STDIN_FILENO, &rfds);

        rc = select(nfds, &rfds, NULL, NULL, NULL);
        if (rc < 0) {
            if (errno == EINTR) {
                continue;
            }
            perror("select");
            tty_raw_end(&guard);
            close(fd);
            return 1;
        }

        if (FD_ISSET(fd, &rfds)) {
            uint8_t buf[512];
            ssize_t n = read(fd, buf, sizeof(buf));
            if (n > 0) {
                if (write_all_fd(STDOUT_FILENO, buf, (size_t)n) != 0) {
                    tty_raw_end(&guard);
                    close(fd);
                    return 1;
                }
            }
        }

        if (FD_ISSET(STDIN_FILENO, &rfds)) {
            uint8_t buf[256];
            ssize_t n = read(STDIN_FILENO, buf, sizeof(buf));
            ssize_t k;
            if (n <= 0) {
                continue;
            }
            for (k = 0; k < n; k++) {
                if (buf[k] == quit_char) {
                    tty_raw_end(&guard);
                    close(fd);
                    fprintf(stderr, "\nterm mode ended\n");
                    return 0;
                }
            }
            if (write_all_fd(fd, buf, (size_t)n) != 0) {
                tty_raw_end(&guard);
                close(fd);
                return 1;
            }
        }
    }
}

static int cmd_lb_send(int argc, char **argv, const struct common_opts *common) {
    const char *bin_file = NULL;
    uint16_t addr = 0;
    bool have_addr = false;
    double ready_timeout = 5.0;
    double done_timeout = 5.0;
    bool no_wait_done = false;
    uint8_t *payload = NULL;
    size_t payload_len = 0;
    char cmd[64];
    int i;
    int fd;
    int state;

    if (argc < 1) {
        fprintf(stderr, "error: lb-send requires BIN file\n");
        return 2;
    }
    bin_file = argv[0];

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--addr") == 0) {
            if (++i >= argc || parse_u16_hex(argv[i], &addr) != 0) {
                fprintf(stderr, "error: invalid --addr\n");
                return 2;
            }
            have_addr = true;
            continue;
        }
        if (strcmp(argv[i], "--ready-timeout") == 0) {
            if (++i >= argc || parse_double_pos(argv[i], &ready_timeout) != 0) {
                fprintf(stderr, "error: invalid --ready-timeout\n");
                return 2;
            }
            continue;
        }
        if (strcmp(argv[i], "--done-timeout") == 0) {
            if (++i >= argc || parse_double_pos(argv[i], &done_timeout) != 0) {
                fprintf(stderr, "error: invalid --done-timeout\n");
                return 2;
            }
            continue;
        }
        if (strcmp(argv[i], "--no-wait-done") == 0) {
            no_wait_done = true;
            continue;
        }
        fprintf(stderr, "error: unknown lb-send option: %s\n", argv[i]);
        return 2;
    }

    if (!have_addr) {
        fprintf(stderr, "error: lb-send requires --addr\n");
        return 2;
    }
    if (read_file(bin_file, &payload, &payload_len) != 0) {
        return 1;
    }
    if (payload_len > 0xFFFFu) {
        fprintf(stderr, "error: payload too large for L B LEN (max FFFF)\n");
        free(payload);
        return 2;
    }
    if ((unsigned long)addr + (unsigned long)payload_len > 0x10000UL) {
        fprintf(stderr, "error: address range exceeds 64K memory space\n");
        free(payload);
        return 2;
    }

    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        free(payload);
        return 1;
    }

    snprintf(cmd, sizeof(cmd), "L B %04X %04X\r", addr, (unsigned)payload_len);
    if (write_all_fd(fd, (const uint8_t *)cmd, strlen(cmd)) != 0) {
        close(fd);
        free(payload);
        return 1;
    }

    state = read_until_token(fd, "L B READY", "L B ABORTED", ready_timeout);
    if (state == 2) {
        fprintf(stderr, "\nerror: monitor aborted before ready\n");
        close(fd);
        free(payload);
        return 1;
    }
    if (state != 1) {
        fprintf(stderr, "\nerror: did not receive L B READY\n");
        close(fd);
        free(payload);
        return 1;
    }

    if (write_all_fd(fd, payload, payload_len) != 0) {
        close(fd);
        free(payload);
        return 1;
    }

    if (!no_wait_done) {
        state = read_until_token(fd, "L B LOAD COMPLETE", "L B ABORTED", done_timeout);
        if (state != 1) {
            fprintf(stderr, "\nerror: load did not complete\n");
            close(fd);
            free(payload);
            return 1;
        }
    }

    close(fd);
    free(payload);
    return 0;
}

static int cmd_ls_send_common(int argc,
                              char **argv,
                              const struct common_opts *common,
                              bool auto_go) {
    const char *srec_file = NULL;
    double ready_timeout = 5.0;
    double done_timeout = 10.0;
    bool no_wait_done = false;
    uint8_t *payload = NULL;
    size_t payload_len = 0;
    int i;
    int fd;
    int state;
    const char *cmd = auto_go ? "L G S\r" : "L S\r";
    const uint8_t line_end[2] = {'\r', '\n'};

    if (argc < 1) {
        fprintf(stderr, "error: %s requires S28 file\n", auto_go ? "lgs-send" : "ls-send");
        return 2;
    }
    srec_file = argv[0];

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--ready-timeout") == 0) {
            if (++i >= argc || parse_double_pos(argv[i], &ready_timeout) != 0) {
                fprintf(stderr, "error: invalid --ready-timeout\n");
                return 2;
            }
            continue;
        }
        if (strcmp(argv[i], "--done-timeout") == 0) {
            if (++i >= argc || parse_double_pos(argv[i], &done_timeout) != 0) {
                fprintf(stderr, "error: invalid --done-timeout\n");
                return 2;
            }
            continue;
        }
        if (strcmp(argv[i], "--no-wait-done") == 0) {
            no_wait_done = true;
            continue;
        }
        fprintf(stderr, "error: unknown %s option: %s\n",
                auto_go ? "lgs-send" : "ls-send", argv[i]);
        return 2;
    }

    if (read_file(srec_file, &payload, &payload_len) != 0) {
        return 1;
    }

    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        free(payload);
        return 1;
    }

    if (write_all_fd(fd, (const uint8_t *)cmd, strlen(cmd)) != 0) {
        close(fd);
        free(payload);
        return 1;
    }

    state = read_until_token(fd, "L S READY", "L S ABORTED", ready_timeout);
    if (state == 2) {
        fprintf(stderr, "\nerror: monitor aborted before S-record ready\n");
        close(fd);
        free(payload);
        return 1;
    }
    if (state != 1) {
        fprintf(stderr, "\nerror: did not receive L S READY\n");
        close(fd);
        free(payload);
        return 1;
    }

    if (write_all_fd(fd, payload, payload_len) != 0) {
        close(fd);
        free(payload);
        return 1;
    }

    if (payload_len > 0 &&
        payload[payload_len - 1] != '\n' &&
        payload[payload_len - 1] != '\r') {
        if (write_all_fd(fd, line_end, sizeof(line_end)) != 0) {
            close(fd);
            free(payload);
            return 1;
        }
    }

    if (!no_wait_done) {
        state = read_until_token(fd, "L S LOAD COMPLETE", "L S ABORTED", done_timeout);
        if (state != 1) {
            fprintf(stderr, "\nerror: S-record load did not complete\n");
            close(fd);
            free(payload);
            return 1;
        }
    }

    close(fd);
    free(payload);
    return 0;
}

static int cmd_ls_send(int argc, char **argv, const struct common_opts *common) {
    return cmd_ls_send_common(argc, argv, common, false);
}

static int cmd_lgs_send(int argc, char **argv, const struct common_opts *common) {
    return cmd_ls_send_common(argc, argv, common, true);
}

static int cmd_flash_read(int argc, char **argv, const struct common_opts *common) {
    uint16_t addr = 0;
    uint16_t len = 0;
    uint8_t bank = 0;
    const char *out_file = NULL;
    bool have_addr = false;
    bool have_len = false;
    int i;
    int fd;
    uint8_t *data = NULL;

    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "--addr") == 0) {
            if (++i >= argc || parse_u16_hex(argv[i], &addr) != 0) return 2;
            have_addr = true;
            continue;
        }
        if (strcmp(argv[i], "--len") == 0) {
            if (++i >= argc || parse_u16_hex(argv[i], &len) != 0) return 2;
            have_len = true;
            continue;
        }
        if (strcmp(argv[i], "--bank") == 0) {
            if (++i >= argc || parse_u8_hex(argv[i], &bank) != 0) return 2;
            continue;
        }
        if (strcmp(argv[i], "--out") == 0) {
            if (++i >= argc) return 2;
            out_file = argv[i];
            continue;
        }
        fprintf(stderr, "error: unknown flash-read option: %s\n", argv[i]);
        return 2;
    }
    if (!have_addr || !have_len || out_file == NULL) {
        fprintf(stderr, "error: flash-read requires --addr --len --out\n");
        return 2;
    }
    if ((unsigned long)addr + (unsigned long)len > 0x10000UL) {
        fprintf(stderr, "error: address range exceeds 64K in selected bank\n");
        return 2;
    }

    data = malloc(len ? len : 1);
    if (!data) {
        fprintf(stderr, "error: out of memory\n");
        return 1;
    }

    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        free(data);
        return 1;
    }
    if (mon_start_cmd(fd, common->timeout_s, CMD_READ_FLASH) != 0 ||
        mon_send_info(fd, addr, bank, len) != 0 ||
        (len && read_exact_timeout(fd, data, len, common->timeout_s * 10.0) != 0)) {
        close(fd);
        free(data);
        return 1;
    }

    if (write_file(out_file, data, len) != 0) {
        close(fd);
        free(data);
        return 1;
    }

    printf("read %u bytes from bank %02X:%04X to %s\n", len, bank, addr, out_file);
    close(fd);
    free(data);
    return 0;
}

static int cmd_flash_write(int argc, char **argv, const struct common_opts *common) {
    uint16_t addr = 0;
    uint8_t bank = 0;
    const char *in_file = NULL;
    bool have_addr = false;
    bool force = false;
    const char *confirm = NULL;
    int i;
    int fd;
    uint8_t status = 0;
    uint8_t *payload = NULL;
    size_t payload_len = 0;

    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "--addr") == 0) {
            if (++i >= argc || parse_u16_hex(argv[i], &addr) != 0) return 2;
            have_addr = true;
            continue;
        }
        if (strcmp(argv[i], "--bank") == 0) {
            if (++i >= argc || parse_u8_hex(argv[i], &bank) != 0) return 2;
            continue;
        }
        if (strcmp(argv[i], "--in") == 0) {
            if (++i >= argc) return 2;
            in_file = argv[i];
            continue;
        }
        if (strcmp(argv[i], "--force") == 0) {
            force = true;
            continue;
        }
        if (strcmp(argv[i], "--confirm") == 0) {
            if (++i >= argc) return 2;
            confirm = argv[i];
            continue;
        }
        fprintf(stderr, "error: unknown flash-write option: %s\n", argv[i]);
        return 2;
    }
    if (!have_addr || in_file == NULL) {
        fprintf(stderr, "error: flash-write requires --addr and --in\n");
        return 2;
    }
    if (require_force_confirm(force, confirm, "WRITE", "flash-write") != 0) {
        return 2;
    }
    if (read_file(in_file, &payload, &payload_len) != 0) {
        return 1;
    }
    if (payload_len > 0xFFFFu) {
        fprintf(stderr, "error: payload too large (max FFFF)\n");
        free(payload);
        return 2;
    }
    if ((unsigned long)addr + (unsigned long)payload_len > 0x10000UL) {
        fprintf(stderr, "error: address range exceeds 64K in selected bank\n");
        free(payload);
        return 2;
    }

    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        free(payload);
        return 1;
    }

    if (mon_start_cmd(fd, common->timeout_s, CMD_WRITE_FLASH) != 0 ||
        mon_send_info(fd, addr, bank, (uint16_t)payload_len) != 0 ||
        write_all_fd(fd, payload, payload_len) != 0 ||
        mon_read_status(fd, common->timeout_s * 10.0, &status) != 0) {
        close(fd);
        free(payload);
        return 1;
    }

    printf("write status: %02X (%s)\n", status, status_text(status));
    close(fd);
    free(payload);
    return status == STATUS_OK ? 0 : 1;
}

static int cmd_flash_clear(int argc, char **argv, const struct common_opts *common) {
    bool force = false;
    const char *confirm = NULL;
    int i;
    int fd;
    uint8_t status = 0;

    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "--force") == 0) {
            force = true;
            continue;
        }
        if (strcmp(argv[i], "--confirm") == 0) {
            if (++i >= argc) return 2;
            confirm = argv[i];
            continue;
        }
        fprintf(stderr, "error: unknown flash-clear option: %s\n", argv[i]);
        return 2;
    }
    if (require_force_confirm(force, confirm, "ERASE", "flash-clear") != 0) {
        return 2;
    }

    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        return 1;
    }
    if (mon_start_cmd(fd, common->timeout_s, CMD_CLEAR_FLASH) != 0 ||
        mon_read_status(fd, common->timeout_s * 30.0, &status) != 0) {
        close(fd);
        return 1;
    }
    printf("clear status: %02X (%s)\n", status, status_text(status));
    close(fd);
    return status == STATUS_OK ? 0 : 1;
}

static int cmd_flash_check(int argc, char **argv, const struct common_opts *common) {
    int fd;
    uint8_t status = 0;
    if (argc != 0) {
        fprintf(stderr, "error: flash-check takes no arguments\n");
        return 2;
    }
    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        return 1;
    }
    if (mon_start_cmd(fd, common->timeout_s, CMD_CHECK_FLASH) != 0 ||
        mon_read_status(fd, common->timeout_s * 10.0, &status) != 0) {
        close(fd);
        return 1;
    }
    printf("check status: %02X (%s)\n", status, status_text(status));
    close(fd);
    return status == STATUS_OK ? 0 : 1;
}

static int cmd_flash_exec(int argc, char **argv, const struct common_opts *common) {
    int fd;
    if (argc != 0) {
        fprintf(stderr, "error: flash-exec takes no arguments\n");
        return 2;
    }
    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        return 1;
    }
    if (mon_start_cmd(fd, common->timeout_s, CMD_EXEC_FLASH) != 0) {
        close(fd);
        return 1;
    }
    printf("execute-from-flash command sent\n");
    close(fd);
    return 0;
}

static int cmd_flash_update_monitor(int argc, char **argv, const struct common_opts *common) {
    const char *in_file = NULL;
    bool force = false;
    const char *confirm = NULL;
    int i;
    int fd;
    uint8_t status = 0;
    uint8_t *image = NULL;
    size_t image_len = 0;
    const uint8_t key[3] = {0x55, 0xAA, 0xCC};

    for (i = 0; i < argc; i++) {
        if (strcmp(argv[i], "--in") == 0) {
            if (++i >= argc) return 2;
            in_file = argv[i];
            continue;
        }
        if (strcmp(argv[i], "--force") == 0) {
            force = true;
            continue;
        }
        if (strcmp(argv[i], "--confirm") == 0) {
            if (++i >= argc) return 2;
            confirm = argv[i];
            continue;
        }
        fprintf(stderr, "error: unknown flash-update-monitor option: %s\n", argv[i]);
        return 2;
    }
    if (in_file == NULL) {
        fprintf(stderr, "error: flash-update-monitor requires --in FILE\n");
        return 2;
    }
    if (require_force_confirm(force, confirm, "UPDATE", "flash-update-monitor") != 0) {
        return 2;
    }
    if (read_file(in_file, &image, &image_len) != 0) {
        return 1;
    }
    if (image_len != 0x1000u) {
        fprintf(stderr, "error: monitor image must be exactly 1000h bytes\n");
        free(image);
        return 2;
    }

    fd = open_serial(common->port, common->baud);
    if (fd < 0) {
        free(image);
        return 1;
    }

    if (mon_start_cmd(fd, common->timeout_s, CMD_UPDATE_MON) != 0 ||
        mon_read_status(fd, common->timeout_s * 10.0, &status) != 0) {
        close(fd);
        free(image);
        return 1;
    }
    if (status != STATUS_OK) {
        fprintf(stderr, "error: update stage0 rejected: %02X (%s)\n", status, status_text(status));
        close(fd);
        free(image);
        return 1;
    }

    if (write_all_fd(fd, key, sizeof(key)) != 0 ||
        mon_send_info(fd, 0xF000u, 0x00u, 0x1000u) != 0 ||
        mon_read_status(fd, common->timeout_s * 10.0, &status) != 0) {
        close(fd);
        free(image);
        return 1;
    }
    if (status != STATUS_STAGE1) {
        fprintf(stderr, "error: update stage1 rejected: %02X (%s)\n", status, status_text(status));
        close(fd);
        free(image);
        return 1;
    }

    if (write_all_fd(fd, image, image_len) != 0 ||
        mon_read_status(fd, common->timeout_s * 30.0, &status) != 0) {
        close(fd);
        free(image);
        return 1;
    }
    if (status != STATUS_STAGE2) {
        fprintf(stderr, "error: update stage2 rejected: %02X (%s)\n", status, status_text(status));
        close(fd);
        free(image);
        return 1;
    }

    if (write_all_fd(fd, key, sizeof(key)) != 0 ||
        mon_read_status(fd, common->timeout_s * 60.0, &status) != 0) {
        close(fd);
        free(image);
        return 1;
    }

    printf("update status: %02X (%s)\n", status, status_text(status));
    close(fd);
    free(image);
    return status == STATUS_DONE ? 0 : 1;
}

int main(int argc, char **argv) {
    struct common_opts common;
    int i = 1;
    const char *cmd;

    common.port = "/dev/ttyUSB0";
    common.baud = 115200;
    common.timeout_s = 1.0;

    while (i < argc) {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            usage();
            return 0;
        }
        if (strcmp(argv[i], "--port") == 0) {
            if (++i >= argc) {
                usage();
                return 2;
            }
            common.port = argv[i++];
            continue;
        }
        if (strcmp(argv[i], "--baud") == 0) {
            if (++i >= argc || parse_int_range(argv[i], 1, 4000000, &common.baud) != 0) {
                usage();
                return 2;
            }
            i++;
            continue;
        }
        if (strcmp(argv[i], "--timeout") == 0) {
            if (++i >= argc || parse_double_pos(argv[i], &common.timeout_s) != 0) {
                usage();
                return 2;
            }
            i++;
            continue;
        }
        break;
    }

    if (i >= argc) {
        usage();
        return 2;
    }

    cmd = argv[i++];

    if (strcmp(cmd, "term") == 0) {
        return cmd_term(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "lb-send") == 0) {
        return cmd_lb_send(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "ls-send") == 0) {
        return cmd_ls_send(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "lgs-send") == 0) {
        return cmd_lgs_send(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "flash-read") == 0) {
        return cmd_flash_read(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "flash-write") == 0) {
        return cmd_flash_write(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "flash-clear") == 0) {
        return cmd_flash_clear(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "flash-check") == 0) {
        return cmd_flash_check(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "flash-exec") == 0) {
        return cmd_flash_exec(argc - i, &argv[i], &common);
    }
    if (strcmp(cmd, "flash-update-monitor") == 0) {
        return cmd_flash_update_monitor(argc - i, &argv[i], &common);
    }

    fprintf(stderr, "error: unknown command: %s\n", cmd);
    usage();
    return 2;
}
