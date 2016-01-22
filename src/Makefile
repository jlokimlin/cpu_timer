
include ../make.inc

#EXTERNAL_LIBRARY_PATH = /usr/local/lib
EXTERNAL_LIBRARY_PATH = /usr/local/lib64

BIN_PATH = /usr/local/bin

LINKS = \
-I$(OBJ_DIRECTORY)

EXECUTABLE = ./exe

type_cpu_timer_mod.o: \
type_cpu_timer_mod.f90
	$(FC) $(FFLAGS) -c type_cpu_timer_mod.f90 $(LINKS) \
	 -o $(OBJ_DIRECTORY)/$@ -J$(OBJ_DIRECTORY)

main.o: \
type_cpu_timer_mod.o \
main.f90
	$(FC) $(FFLAGS) -c main.f90 $(LINKS) \
	-o $(OBJ_DIRECTORY)/$@

exec: $(OBJECTS)
	$(FC) $(FFLAGS) -o $(EXECUTABLE) $(OBJECTS_WITH_PREFIX) $(LINKS)

lib: 
	ar rc libcpu_timer.a \
	$(OBJ_DIRECTORY)/type_cpu_timer_mod.o

	mv libcpu_timer.a $(LIB_DIRECTORY)

all:
	make clean
	make exec; ./exe
	make lib

clean:
	rm -f $(OBJ_DIRECTORY)/*.o $(OBJ_DIRECTORY)/*.mod $(LIB_DIRECTORY)/*.a
	rm -f $(EXECUTABLE)
