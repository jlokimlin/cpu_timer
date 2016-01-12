include ./compiler_flags.inc
include ./directories.inc

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

build_lib: 
	ar rc libcpu_timer.a \
	$(OBJ_DIRECTORY)/type_cpu_timer_mod.o
	
	cp libcpu_timer.a /usr/local/lib
	mv libcpu_timer.a $(LIB_DIRECTORY)

run:
	make clean
	make exec
	make build_lib	
	./exe
	 
clean:
	rm -f $(OBJECTS)
	rm -f $(EXECUTABLE)