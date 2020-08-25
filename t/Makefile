RM ?= rm -f

run : check.out
	./$^

check.out: ../jeezson.? c1/test.h c1/test.c *.c
	# -fsanitize=undefined,null
	$(CC) -o $@ -g -Wall -ansi -Wno-format -ldl -O2 -Ic1 -I.. c1/test.h c1/test.c *.c

debug : check.out
	gdb run $^

clean :
	$(RM) check.out

.PHONY: run debug clean
