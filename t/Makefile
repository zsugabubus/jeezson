RM ?= rm -f

run : check
	./$^

c1/% :
	git submodule update --init c1

check : c1/test.? . *.c ../jeezson.?
	# -fsanitize=undefined,null
	$(CC) -o $@ -g -Wall -ansi -Wno-format -O2 -Ic1 -I.. c1/test.c *.c $(CFLAGS)

clean :
	$(RM) check

.PHONY: run clean
