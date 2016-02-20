
include ../make.inc

MODULE_PATH = $(LIB_DIRECTORY)

LINKS = \
-L$(LIB_DIRECTORY) -lcpu_timer \
-I$(MODULE_PATH)

EXECUTABLE = ./test.exe

test.o: \
test.f90
	$(FC) $(FFLAGS) -c test.f90 $(LINKS)\
	 -o $(OBJ_DIRECTORY)/$@

exec: $(OBJ)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJS) $(LINKS)

run:
	make clean
	make exec; $(EXECUTABLE)
	 
clean:
	rm -f $(OBJS)
	rm -f $(EXECUTABLE)
