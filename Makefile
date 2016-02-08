
include make.inc

all: lib testlib

lib: 
	mkdir -p ./lib
	mkdir -p ./objs
	cd ./src; make all

testlib:
	cd ./test; make run

install:
	cp ./lib/libcpu_timer.a $(EXTERNAL_LIBRARY_PATH)
	cp -r ../cpu_timer $(BIN_PATH)

.PHONY: all lib testlib install
