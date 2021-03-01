RM ?= rm -f

jeezson.a : jeezson.c jeezson.h
	$(CC) -c -o $@ $< $(CFLAGS)

check :
	CFLAGS=-std=c99 $(MAKE) -C t

clean :
	$(RM) jeezson.a
	$(MAKE) -C t clean

.PHONY: bootstrap check clean
