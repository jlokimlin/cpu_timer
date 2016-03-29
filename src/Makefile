
include ../make.inc

LINKS = \
-I$(OBJ_DIRECTORY) \
-I$(LIB_DIRECTORY)

EXECUTABLE = ./build_lib.exe

type_CpuTimer.o: \
type_CpuTimer.f90
	$(FC) $(FFLAGS) -c type_CpuTimer.f90 $(LINKS) \
	 -o $(OBJ_DIRECTORY)/$@ -J$(LIB_DIRECTORY)

cpu_timer_library.o: \
type_CpuTimer.o \
cpu_timer_library.f90
	$(FC) $(FFLAGS) -c cpu_timer_library.f90 $(LINKS) \
	 -o $(OBJ_DIRECTORY)/$@ -J$(LIB_DIRECTORY)

main.o: \
type_CpuTimer.o \
main.f90
	$(FC) $(FFLAGS) -c main.f90 $(LINKS) \
	-o $(OBJ_DIRECTORY)/$@

exec: $(OBJ)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJS) $(LINKS)

lib: 
	$(AR) rc lib$(LIB_NAME).a $(OBJS)
	mv lib$(LIB_NAME).a $(LIB_DIRECTORY)

all:
	$(MAKE) clean
	$(MAKE) exec; $(EXECUTABLE)
	$(MAKE) lib

clean:
	rm -f $(OBJS) $(LIB) $(LIB_DIRECTORY)/*.mod
	rm -f $(EXECUTABLE)