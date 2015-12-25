include ./fcomp.inc
include ./directories.inc

# Add the required internal libraries
libInclude = \
-I$(obj_directory)

executable = ./exe

type_cpu_timer_mod.o: \
type_cpu_timer_mod.f90
	$(fcomp) -c type_cpu_timer_mod.f90 $(libInclude)\
	 -o $(obj_directory)/$@ -J$(obj_directory)

main.o: \
type_cpu_timer_mod.o \
main.f90
	$(fcomp) -c main.f90 $(libInclude)\
	 -o $(obj_directory)/$@

exec: $(objects)
	$(fcomp) -o $(executable) $(objects_with_prefix) $(libInclude)

build_lib: 
	ar rc libcpu_timer.a \
	$(obj_directory)/type_cpu_timer_mod.o
	
	mv libcpu_timer.a $(lib_directory)

run:
	make clean
	make exec
	make build_lib	
	./exe
	 
clean:
	rm -f $(objects)
	rm -f $(executable)