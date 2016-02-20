
include make.inc

all: lib testlib

lib: 
	mkdir -p ./lib
	mkdir -p ./objs
	( cd ./src; $(MAKE) all )

testlib:
	( cd ./test; $(MAKE) run )

install:
	cp ./lib/libcpu_timer.a $(EXTERNAL_LIBRARY_PATH)
	cp -r ../cpu_timer $(BIN_PATH)

clean: 
	( cd ./src; $(MAKE) clean; cd ../test; $(MAKE) clean )

.PHONY: all lib testlib install
