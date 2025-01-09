MAKEFLAGS += --no-print-directory

CC = g++ -Wall -g -std=c++17

TARGETS = $(patsubst %/, %, $(filter %/, $(wildcard */)))

.PHONY: all clean test $(TARGETS)

all: test

clean:
	rm -rf a.out

test: $(TARGETS)

$(foreach target, $(TARGETS), $(eval $(target): ; @make test_target TARGET=$(target))):

test_target:
	@[ -n "$(TARGET)" ] || (echo "usage: make test_target TARGET=<target>" && false)
	@$(CC) $(TARGET)/test.cc
	@./a.out
	@echo "all tests passed for $(TARGET)"
