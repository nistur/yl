.phony: run dirs clean

CC=gcc

OUTDIR=out
OBJDIR=obj
SRCDIR=src

STAGE0_OBJS=$(OBJDIR)/stage0.o
STAGE0=$(OUTDIR)/stage0

STAGE1_OBJS=$(OBJDIR)/stage1_generated.o
STAGE1=$(OUTDIR)/stage1

CFLAGS=-g -Wall -Wextra
LDFLAGS=$(CFLAGS)

run: $(STAGE1)
	@$(STAGE1)

$(STAGE0): dirs $(STAGE0_OBJS)
	@$(CC) -o $@ $(STAGE0_OBJS) $(LDFLAGS)

$(OBJDIR)/%.o:$(SRCDIR)/stage0/%.c
	@$(CC) $(CFLAGS) -c -o $@ $<

$(OUTDIR)/%:%.yl $(STAGE0)
	@$(STAGE0) stage1.yl $<

dirs:
	@mkdir -p $(OUTDIR)
	@mkdir -p $(OBJDIR)

clean:
	@rm -f $(OBJDIR)/*.o
	@rm -f $(OBJDIR)/*.c
	@rm -f $(STAGE0)
	@rm -f $(STAGE1)
