include ../compiler_flags.inc
include ./relative_directories.inc

LINKS = \
-I$(OBJ_DIRECTORY) \
-lcpu_timer -L$(LIB_DIRECTORY)

EXECUTABLE = ./exe
	 
test.o: \
test.f90
	$(FC) $(FFLAGS) -c test.f90 $(LINKS)\
	 -o $(OBJ_DIRECTORY)/$@

exec: $(OBJECTS)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJECTS_WITH_PREFIX) $(LINKS)

run:
	make clean
	make exec
	./exe
	 
clean:
	rm -f $(OBJECTS)
	rm -f $(EXECUTABLE)