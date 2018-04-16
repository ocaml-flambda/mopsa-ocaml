SRCDIR 	= src
INCDIR 	= include
OBJDIR 	= /tmp/make_example/obj
BINDIR = /tmp/make_example/bin

CC     ?= gcc
CFLAGS := -I$(INCDIR)

INC 	= $(wildcard $(INCDIR)/*.h)
SRC	= $(wildcard $(SRCDIR)/*.c)
OBJ 	= $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SRC))

TARGET 	= $(BINDIR)/main

.PHONY: clean

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) $^ -o $@

$(OBJ): $(OBJDIR)/%.o : $(SRCDIR)/%.c | $(INC) $(OBJDIR) $(BINDIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

clean:
	rm -f $(TARGET) $(OBJDIR)/*.o
