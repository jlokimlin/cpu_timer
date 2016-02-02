
include make.inc

#EXTERNAL_LIBRARY_PATH = /usr/local/lib
EXTERNAL_LIBRARY_PATH = /usr/local/lib64

BIN_PATH = /usr/local/bin

all: lib testlib

lib: 
	cd ./src; make all

testlib:
	cd ./test; make run

install:
	cp ./lib/libcpu_timer.a $(EXTERNAL_LIBRARY_PATH)
	cp -r ../CPU_timer $(BIN_PATH)

.PHONY: all lib testlib install
