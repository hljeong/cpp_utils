include makefile_utils/defaults.mk

CC = g++ -Wall -g -std=c++17

.PHONY: all clean setup update test $(TARGETS)

all: test

clean:
	rm -rf a.out

setup: git-hook-install

update: git-submodule-update

test:
	@ ${CC} hlj/test.cc
	@ ./a.out
	@ echo all tests passed

include makefile_utils/git.mk
