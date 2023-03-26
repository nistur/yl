.phony: run dirs clean

CC=gcc

OUTDIR=out
OBJDIR=obj
SRCDIR=src

STAGE0_OBJS=$(OBJDIR)/stage0.o
STAGE0=$(OUTDIR)/stage0

CFLAGS=-g -Wall -Wextra
LDFLAGS=$(CFLAGS)

run: $(STAGE0)
	$(STAGE0) stage1.yl

$(STAGE0): dirs $(STAGE0_OBJS)
	$(CC) -o $@ $(STAGE0_OBJS) $(LDFLAGS)

$(OBJDIR)/%.o:$(SRCDIR)/stage0/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

dirs:
	mkdir -p $(OUTDIR)
	mkdir -p $(OBJDIR)

clean:
	rm $(OBJDIR)/*.o
	rm $(STAGE0)
