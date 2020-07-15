.PHONY: tests
tests: test
	./$^

test: ../jeezson.? c1/test.h c1/test.c *.c
	# -fsanitize=undefined,null
	$(CC) -o $@ -g -Wall -ansi -Wno-format -ldl -O2 -Ic1 -I.. c1/test.h c1/test.c *.c

debug: test
	gdb run $^

clean:
	$(RM) test
