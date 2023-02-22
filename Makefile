.phony: stage0 run

CC=gcc

STAGE0_OBJS=main.o lisp.o

CFLAGS=-g -Wall -Wextra
LDFLAGS=$(CFLAGS)

run: stage0
	./stage0 test.l

stage0: $(STAGE0_OBJS)
	$(CC) -o $@ $(STAGE0_OBJS) $(LDFLAGS)

%.o:%.c
	$(CC) $(CFLAGS) -c -o $@ $<
