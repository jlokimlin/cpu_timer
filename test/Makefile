
include ../make.inc

LINKS = \
-I$(OBJ_DIRECTORY) \
-L$(LIB_DIRECTORY) -lcputimer

EXECUTABLE = ./exe

test.o: \
test.f90
	$(FC) $(FFLAGS) -c test.f90 $(LINKS)\
	 -o $(OBJ_DIRECTORY)/$@

exec: $(OBJECTS)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJECTS_WITH_PREFIX) $(LINKS)

run:
	make clean
	make exec; ./exe
	 
clean:
	rm -f $(OBJECTS)
	rm -f $(EXECUTABLE)
