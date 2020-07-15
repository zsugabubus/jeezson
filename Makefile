RM ?= rm -f

jeezson.a : jeezson.c jeezson.h
	$(CC) -c -o $@ $<

bootstrap :
	git submodule update --init --recursive

check :
	$(MAKE) -C tests

clean :
	$(RM) jeezson.a

.PHONY: bootstrap check clean
