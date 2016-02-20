
include ../make.inc

LINKS = \
-I$(OBJ_DIRECTORY) \
-I$(LIB_DIRECTORY)

EXECUTABLE = ./build_lib.exe

type_CpuTimer.o: \
type_CpuTimer.f90
	$(FC) $(FFLAGS) -c type_CpuTimer.f90 $(LINKS) \
	 -o $(OBJ_DIRECTORY)/$@ -J$(LIB_DIRECTORY)

main.o: \
type_CpuTimer.o \
main.f90
	$(FC) $(FFLAGS) -c main.f90 $(LINKS) \
	-o $(OBJ_DIRECTORY)/$@

exec: $(OBJ)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJS) $(LINKS)

lib: 
	ar rc libcpu_timer.a $(OBJ_DIRECTORY)/*.o
	mv libcpu_timer.a $(LIB_DIRECTORY)

all:
	make clean
	make exec; $(EXECUTABLE)
	make lib

clean:
	rm -f $(OBJS) $(LIB) $(LIB_DIRECTORY)/*.mod
	rm -f $(EXECUTABLE)