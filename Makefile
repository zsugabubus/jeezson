jeezson.a: jeezson.c jeezson.h
	$(CC) -c -o $@ $<

.PHONY: tests
tests:
	$(MAKE) -c tests
