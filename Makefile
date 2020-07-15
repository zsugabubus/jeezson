RM ?= rm -f

jeezson.a : jeezson.c jeezson.h
	$(CC) -c -o $@ $<

bootstrap :
	git submodule update --init --recursive

check :
	$(MAKE) -C t

clean :
	$(RM) jeezson.a

.PHONY: bootstrap check clean
