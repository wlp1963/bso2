# SPDX-License-Identifier: MIT
# Copyright (c) 2026 95west.us
# See LICENSE for full license text.

CC = gcc
CFLAGS ?= -O2 -Wall -Wextra -std=c11
LDFLAGS ?=

TARGET := bso2com
SRC := bso2com.c

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -o $@ $(SRC) $(LDFLAGS)

clean:
	rm -f $(TARGET)
