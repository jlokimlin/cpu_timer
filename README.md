# **cpu\_timer**

This Fortran project implements a class for computing elapsed CPU time. More specifically, you can determine the CPU time consumed by a particular piece of your code, that is:

```fortran

	!... start timer
	
	do i = 1, n
		
		!...some big calculation...
	
	end do
	
	!...stop timer
	
	print *, 'Elapsed CPU time = ', blah
```

-----------------------------------------------------------------------------

## Requirements
* The GNU Make tool https://www.gnu.org/software/make/
* The GNU gfortran compiler https://gcc.gnu.org/wiki/GFortran

-----------------------------------------------------------------------------

## To build the project

Type the following command line arguments
```
git clone https://github.com/jlokimlin/cpu_timer.git

cd cpu_timer; make all
```

-----------------------------------------------------------------------------

## Usage:

```fortran

use, intrinsic :: iso_fortran_env, only: &
    wp => REAL64, &
    ip => INT32

use type_CpuTimer, only: &
    CpuTimer

! Explicit typing only
implicit none

type (CpuTimer)         :: timer
real (wp)               :: wall_clock_time
real (wp)	            :: total_processor_time
integer (ip), parameter :: UNITS = 0 ! (optional argument) = 0 for seconds, or 1 for minutes, or 2 for hours

! Starting the timer
call timer%start()

! Stopping the timer
call timer%stop()

! Reading the time
wall_clock_time = timer%get_elapsed_time( UNITS )

total_processor_time = timer%get_total_cpu_time( UNITS )

! Put a time stamp
call timer%print_time_stamp()

```

-----------------------------------------------------------------------------

## Result

```
```