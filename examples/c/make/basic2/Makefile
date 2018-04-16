SRCDIR 	= src
INCDIR 	= include
BINDIR = /tmp/make_example/bin

CC     ?= gcc
CFLAGS := -I$(INCDIR)

TARGET 	= $(BINDIR)/main

.PHONY: clean

all: $(TARGET)

$(TARGET): $(SRCDIR)/main.c $(SRCDIR)/lib.c | $(BINDIR)
	$(CC) $(CFLAGS) $^ -o $@

$(BINDIR):
	mkdir -p $(BINDIR)

clean:
	rm -f $(TARGET) $(BINDIR)/*.o
