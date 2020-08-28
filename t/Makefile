RM ?= rm -f

run : check
	./$^

check : ../jeezson.? c1/test.h c1/test.c *.c
	# -fsanitize=undefined,null
	$(CC) -o $@ -g -Wall -ansi -Wno-format -O2 -Ic1 -I.. c1/test.h c1/test.c *.c

clean :
	$(RM) check

.PHONY: run clean
