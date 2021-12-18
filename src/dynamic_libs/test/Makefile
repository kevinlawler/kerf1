# Makefile for automated tests of the Kerf Native API

OS := $(shell uname -s | tr "[:upper:]" "[:lower:]")
ifeq (linux,$(OS))
FLAGS := -rdynamic -shared -fPIC
endif
ifeq (darwin,$(OS))
FLAGS := -flat_namespace -undefined suppress -dynamiclib
endif

compile:
	clang -m64 ${FLAGS} native.c -o native.dylib

clean:
	rm native.dylib

test: compile
	../../kerf_test -q native.kerf
	rm -rf breakfast.bin # clean up table building
