include makefile_utils/defaults.mk

CC = g++ -Wall -g -std=c++17

TARGETS = $(filter-out makefile_utils, $(patsubst %/, %, $(filter %/, $(wildcard */))))

.PHONY: all clean setup update test $(TARGETS)

all: test

clean:
	rm -rf a.out

setup: git-hook-install

update: git-submodule-update

test: $(TARGETS)

$(foreach target, $(TARGETS), $(eval $(target): ; @make test_target TARGET=$(target))):

test_target:
	@[ -n "$(TARGET)" ] || (echo "usage: make test_target TARGET=<target>" && false)
	@$(CC) $(TARGET)/test.cc
	@./a.out
	@echo "all tests passed for $(TARGET)"

include makefile_utils/git.mk
