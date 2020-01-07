.PHONY: tests
tests: test
	./$^

test: c1/test.h c1/test.c *.c
	# -fsanitize=undefined,null
	$(CC) -o $@ -g -Wall -ansi -Wno-format -ldl -O1 -Ic1 -I.. $^

clean:
	$(RM) test
