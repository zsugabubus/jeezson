RM ?= rm -f

jeezson.a : jeezson.c jeezson.h
	$(CC) -c -o $@ $<

check :
	$(MAKE) -C t

clean :
	$(RM) jeezson.a
	$(MAKE) -C t clean

.PHONY: bootstrap check clean
