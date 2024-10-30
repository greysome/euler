CFLAGS = -Wall -Wextra -Wpedantic -O3 -march=native -lm
ifdef DEBUG
CFLAGS += -g -fanalyzer -fsanitize=address -fsanitize=undefined
endif
SOURCES = $(wildcard [0-9]*_*.c)

.PHONY: all
all:
	@echo "Give a specific number to build!"

util.o: util.c util.h
	gcc $(CFLAGS) -c $< -o $@

%: %*.c util.o
	gcc $(CFLAGS) $^ -o $@; \
	./$@

test: test.c util.o
	gcc $(CFLAGS) $^ -o $@; \
	./$@

clean:
	for exec in $(shell ls --color=never *.c | grep '^[0-9]' | sed 's/_.*//g'); do \
		rm -f $$exec; \
	done; \
	rm -f *.o
