
include ../make.inc

MODULE_PATH = $(LIB_DIRECTORY)

LINKS = \
-L$(LIB_DIRECTORY) -l$(LIB_NAME) \
-I$(MODULE_PATH)

EXECUTABLE = ./test.exe

FC = gfortran-6 -O3 

test.o: \
test.f90
	$(FC) $(FFLAGS) -c test.f90 $(LINKS)\
	 -o $(OBJ_DIRECTORY)/$@

exec: $(OBJ)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJS) $(LINKS)

run:
	$(MAKE) clean
	$(MAKE) exec; $(EXECUTABLE)
	 
clean:
	rm -f $(OBJS) $(EXECUTABLE)
