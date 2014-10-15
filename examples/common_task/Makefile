all: main

main: main.c
	$(CC) -o $@ $<

.PHONY: test

test:
	./main <test.in >temp.out
	cmp test.out temp.out
